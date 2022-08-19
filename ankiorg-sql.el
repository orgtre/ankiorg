;;; ankiorg-sql.el --- SQLite interface to Anki  -*- lexical-binding: t -*-
;; * Preamble

;; Copyright (C) 2022 orgtre <orgtre\a.t/posteo.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;;; Code:

;; * Setup

(require 'ankiorg)
(require 'sqlite3) ; #TODO several commonly used packages are named like this

(defgroup ankiorg-sql nil
  "Customizations for ankiorg-sql."
  :group 'ankiorg)

(defcustom ankiorg-sql-database nil
  "Path to the Anki SQLite database.
Please set this to a (recent) copy of the original or at least back it
up. Currently only absolute paths are supported. The Anki SQLite
database is a file named 'collection.anki2' at the path where Anki
stores user files, see URL
`https://docs.ankiweb.net/files.html#file-locations'."
  :type 'string
  :group 'ankiorg-sql)


;; * Minor-mode definition

;;;###autoload
(define-minor-mode ankiorg-sql-minor-mode
  "Interface directly with Anki's SQLite database.

This uses emacs-sqlite3-api to directly access to the core SQLite3 C API
from Emacs Lisp. It requires that Anki is not running (or that one works
with a copy of the database) and that `ankiorg-sql-database' is set.

If this mode is not enabled, Anki-Connect's HTTP API is used via curl,
which requires that Anki is running. emacs-sqlite3-api used to be much
faster, but after upgrades to anki-editor/Anki-Connect there isn't much
of a difference anymore."
  :global t
  (if ankiorg-sql-minor-mode
      (setq ankiorg-pick-deck-function 'ankiorg-sql-pick-deck
	    ankiorg-anki-note-ids-function 'ankiorg-sql-note-ids
	    ankiorg-anki-notes-function 'ankiorg-sql-get-notes)
    (setq ankiorg-pick-deck-function 'ankiorg-ancon-pick-deck
	  ankiorg-anki-note-ids-function 'ankiorg-ancon-note-ids
	  ankiorg-anki-notes-function 'ankiorg-ancon-get-notes)))
;; TODO and ankiorg-sql-pick-tag?


;; * SQL interface functions

(defun ankiorg-sql-exec (query callback)
  "Run SQLite QUERY (a string) on the Anki database.
Function CALLBACK is called for each row returned by QUERY. Wrapper
around `sqlite3-exec' which opens and closes `ankiorg-sql-database'."
  ;; TODO Why does it randomly work or not with sqlite-open-readonly?
  (let ((db (sqlite3-open ankiorg-sql-database sqlite-open-readwrite))
	(result nil))
    (sqlite3-exec db query callback)
    (sqlite3-close db)))


(defun ankiorg-sql-pick-deck ()
  "Select a deck from the list of decks used in `ankiorg-sql-database'."
  (let* (decks
	 (query "select name from decks")
	 (callback (lambda (_ncols row _colnames)
		     (let ((deck (car row)))
		       (push (replace-regexp-in-string "\^_" "::" deck)
			     decks)))))
    (ankiorg-sql-exec query callback)
    (completing-read "Deck: "
                     (sort decks #'string-lessp))))


(defun ankiorg-sql-note-ids (&optional deck)
  "Return list of all note-id's in Anki.
If DECK is non-nil, return only note-id's in DECK."
  (let* (note-ids
	 (query (if deck
		    (let ((deck (replace-regexp-in-string "::" "" deck)))
		      (format
		       ;; the 'or' is used to also match subdecks
		       "pragma case_sensitive_like=on;
		      select distinct nid from cards where did in
                      (select id from decks where name like
                      '%s' or name like '%s%%')" deck deck))
		  "select distinct nid from cards"))
	 (callback (lambda (_ncols row _colnames)
                     (push (string-to-number (car row)) note-ids))))
    (ankiorg-sql-exec query callback)
    note-ids))


(defun ankiorg-sql-exec-get-alists (query)
  "Run SQLite QUERY (a string) and return results as list of alists.
Wrapper around `ankiorg-sql-exec' which in turn wraps `sqlite3-exec'."
  (let* (result
	 (callback (lambda (ncols row names)
                     (let ((i 0)
                           (li nil))
                       (while (< i ncols)
                         (push (cons (intern (nth i names)) (nth i row)) li)
                         (setq i (1+ i)))
                       (setq li (reverse li))
                       (push li result)))))
    (ankiorg-sql-exec query callback)
    result))


(defun ankiorg-sql-get-notes (note-ids &optional _deck)
  "Return list of notes in alist format with note-ids in NOTE-IDS.
Makes sure that field names are added properly."
  ;; #TODO better progress indication
  ;; #TODO this runs two sql queries and one loop across fields per note-id,
  ;; hence this might be slow, but how to handle the field names,
  ;; which are stored on multiple rows and whose order matters, otherwise?
  ;; In MySQL one can specify ORDER BY within group_concat,
  ;; but SQLite doesn't support this - instead the order of elements
  ;; returned by group_concat is arbitrary...
  ;; The ord column could be used for this: GROUP_CONCAT(ord || ':' || name)
  ;; but this doesn't seem to become faster and less complex than currently.
   (let (note notes)
     (dolist (note-id note-ids notes)
       (setq note (ankiorg-sql-note-without-field-alist note-id))
       (setq note (ankiorg-sql-note-add-field-alist note))
       (assq-delete-all 'mid note)
       (push note notes))))


(defun ankiorg-sql-note-without-field-alist (note-id)
  "Get note NOTE-ID in alist form, but without field alist.
Instead only a concatenated string of field contents is returned.
In this string fields are separated by ''."
  ;; TODO check the SQL, can it be simplified?
  ;; TODO can cards really be in multiple non-nested decks?
  ;;      Or why group_concat(decks.name, '\x1e')?
  ;; TODO does the deck get set correctly in all cases?
  (let ((note
	 (car (ankiorg-sql-exec-get-alists
	       (format "SELECT notes.id AS 'note-id', mid, notetypes.name AS
	               'note-type', a.deck, trim(tags) AS tags, flds AS fields
	               FROM notes INNER JOIN (SELECT cards.nid, 
	               group_concat(decks.name, '\x1e') AS deck FROM cards 
	               INNER JOIN decks ON cards.did = decks.id 
	               WHERE cards.nid = %s) AS a ON notes.id = a.nid 
	               INNER JOIN notetypes ON notes.mid = notetypes.id"
		       note-id)))))
    (setcdr (assoc 'note-id note)
	    (string-to-number (alist-get 'note-id note)))
    (ankiorg-sql-warn-if-multiple-decks note)
    (setcdr (assoc 'deck note)
	    (replace-regexp-in-string "\^_" "::" (alist-get 'deck note)))
    note))


(defun ankiorg-sql-warn-if-multiple-decks (note)
  "Show warning if NOTE has cards in multiple decks."
  (when (> (length (split-string (cdr (assoc 'deck note)) "\x1e")) 1)
    (display-warning
     'ankiorg
     (format
      (concat "Note with note-id %s has cards in multiple decks. "
	      "Anki-editor does not handle this properly, hence the deck "
	      "property of this org note will be unreliable.")
      (cdr (assoc 'note-id note))))))


(defun ankiorg-sql-note-add-field-alist (note)
  "Add field alist to alist NOTE."
  (let* ((field-names
	  (ankiorg-sql-field-names (alist-get 'mid note)))
	 (field-contents
	  (split-string (alist-get 'fields note) "\^_"))
	 (field-alist
	  (cl-mapcar 'cons field-names field-contents)))
    (setcdr (assoc 'fields note)
	    field-alist)
    note))


(defun ankiorg-sql-field-names (model-id)
  "Get field names belonging to note type (note model) with MODEL-ID."
  (let* (fields
	 (query (format "select name from fields where ntid = %s
                        order by ord desc"
			model-id))
	 (callback (lambda (_ncols row _colnames)
		     (push (car row) fields))))
    (ankiorg-sql-exec query callback)
    fields))


(defun ankiorg-sql-pick-tag ()
  "Select a tag from the list of tags used in `ankiorg-sql-database'."
  (interactive)
  (let* (result
	 (query
	  ;; sql query taken from here:
	  ;; https://stackoverflow.com/a/32051164
	  "WITH split(word, str) AS
          (SELECT '', trim(tags)||' ' FROM notes
          -- SELECT '', '  A B CC DDD 101 fff '||' '
          UNION ALL SELECT
          substr(str, 0, instr(str, ' ')),
          substr(str, instr(str, ' ')+1)
          FROM split WHERE str!='')
          SELECT DISTINCT word
          FROM split WHERE word!=''
          ORDER BY word COLLATE NOCASE DESC")
	 (callback (lambda (_ncols row _colnames) (push row result))))
    (ankiorg-sql-exec query callback)
    (completing-read "Choose a deck: " result)))


(provide 'ankiorg-sql)
;;; ankiorg-sql.el ends here
