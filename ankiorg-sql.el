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

;;; Commentary:

;; #TODO Why does sqlite-open-readonly sometimes not work?
;;       Only works with sqlite-open-readwrite.
;;       Try again with sqlite-open-readonly.


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


;; * ankiorg-sql-exec-get-alists: general query utility function

(defun ankiorg-sql-exec-get-alists (query)
  "Execute sqlite3 QUERY (a string) and return results as list of alists.
Wrapper around `sqlite3-exec'. Runs against the database given in
`ankiorg-sql-database'."
  (let ((db (sqlite3-open ankiorg-sql-database sqlite-open-readwrite))
	(res))
    (sqlite3-exec db query
                  (lambda (ncols row names)
                    (let ((i 0)
                          (li nil))
                      (while (< i ncols)
                        (push (cons (intern (nth i names)) (nth i row)) li)
                        (setq i (1+ i)))
                      (setq li (reverse li))
                      (push li res))))
    (sqlite3-close db)
    res))


;; * ankiorg-sql-note-ids

(defun ankiorg-sql-note-ids (&optional deck)
  "Return list of all note-id's in Anki.
If DECK is non-nil, return only note-id's in DECK."
  (let ((db (sqlite3-open ankiorg-sql-database sqlite-open-readwrite))
	;; the 'or' is used to also match subdecks
	(query (if deck
		   (let ((deck (replace-regexp-in-string "::" "" deck)))
		     (format
		      "pragma case_sensitive_like=on;
		      select distinct nid from cards where did in
                      (select id from decks where name like
                      '%s' or name like '%s%%')" deck deck))
		 "select distinct nid from cards"))
	(ids-in-anki))
    (sqlite3-exec db query
                  (lambda (ncols row names)
                    (push (string-to-number (car row)) ids-in-anki)))
    (sqlite3-close db)
    ids-in-anki))


;; * ankiorg-sql-get-notes

(defun ankiorg-sql-get-notes (note-ids &optional _deck)
  "Queries sqlite3 for note data given a list of NOTE-IDS.
Makes sure that field names are added properly. Returns a list of notes in
alist format."

  ;; #TODO this runs two sql queries and one loop across fields per note-id --
  ;; might be slow, but how to handle the field names, which are stored on
  ;; multiple rows and whose order matters, otherwise? In MySQL one can specify
  ;; ORDER BY within group_concat, but SQLite doesn't support this

  ;; - instead the order of elements returned by group_concat is arbitrary...
  ;; The ord column could be used for this: GROUP_CONCAT(ord || ':' || name)
  ;; but this doesn't seem to become faster and less complex than currently.
  
  (setq
   final
   (let ((value)(res)(mres)(out))    
     (dolist (note-id note-ids value)
       (setq res
	     (ankiorg-sql-exec-get-alists
	      (format
	       (concat "SELECT notes.id AS 'note-id', mid, notetypes.name AS "
		       "'note-type', a.deck, trim(tags) AS tags, flds AS fields"
		       " FROM notes INNER JOIN (SELECT cards.nid, "
		       "group_concat(decks.name, '\x1f') AS deck FROM cards "
		       "INNER JOIN decks ON cards.did = decks.id "
		       "WHERE cards.nid = %s) AS a ON notes.id = a.nid "
		       "INNER JOIN notetypes ON notes.mid = notetypes.id")
	       note-id)))
       ;; could select mod here too
       ;; old query without decks and renaming:
       ;; (concat "select notes.id, mid, notetypes.name, trim(tags), flds from "
       ;; 	       "notes left join notetypes on notes.mid = notetypes.id "
       ;; 	       "where notes.id = %s"
       
       (setq mres
	     (reverse
	      (ankiorg-sql-exec-get-alists
	       (format "select name from fields where ntid = %s order by ord"
		       (cdr (assoc 'mid (car res)))))))
       ;; display warning when note has cards in multiple decks
       ;; #TODO this shouldn't display for nested decks
       (when (> (length (split-string (cdr (assoc 'deck (car res))) "\^_")) 1)
	 (display-warning
	  'ankiorg
	  (format
	   (concat "Note with note-id %s has cards in multiple decks. "
		   "Anki-editor does not handle this properly, hence the deck "
		   "property of this org note will be unreliable.")
	   (cdr (assoc 'note-id (car res))))))
       
       ;; given the above lists this will output the fields in the right format
       (setq out
	     (let ((i 0)
                   (flds-list (split-string
			       (cdr (assq 'fields (car res)))
			       "\^_"))
                   (li nil)
                   (elem))
	       
               (while (< i (length mres))
		 (setq elem (cons (cdr (car (nth i  mres))) (nth i flds-list)))
		 (push elem li)
		 (setq i (1+ i)))
	       
               (setq li (reverse li))))

       (setcdr (assoc 'fields (car res)) out)
       (setcdr (assoc 'note-id (car res))
	       (string-to-number (cdr (assoc 'note-id (car res)))))
       (assq-delete-all 'mid (car res))
       (push (car res) value)
       ))))


;; * ankiorg-sql-pick-deck

(defun ankiorg-sql-pick-deck ()
  "Select a deck from the list of decks used in `ankiorg-sql-database'."
  (interactive)
  (setq res nil)
  (setq db (sqlite3-open ankiorg-sql-database sqlite-open-readwrite))
  (sqlite3-exec db "select id, name from decks"
		(lambda (ncols row names)
		  (let ((id (car row))
			(flds (cdr row)))
		    (push (replace-regexp-in-string
			   "\^_" "::" (car flds))
			  res))))
  (sqlite3-close db)
  (completing-read "Deck: "
                   (sort res #'string-lessp)))


;; * ankiorg-sql-pick-tag

(defun ankiorg-sql-pick-tag ()
  "Select a tag from the list of tags used in `ankiorg-sql-database'."
  (interactive)
  (setq res nil)
  (setq db (sqlite3-open ankiorg-sql-database sqlite-open-readwrite))
  ;; sql query taken from here: https://stackoverflow.com/a/32051164
  (sqlite3-exec db "WITH split(word, str) AS (
    SELECT '', trim(tags)||' ' FROM notes
    -- SELECT '', '  A B CC DDD 101 fff '||' '
    UNION ALL SELECT
    substr(str, 0, instr(str, ' ')),
    substr(str, instr(str, ' ')+1)
    FROM split WHERE str!='') SELECT DISTINCT word FROM split WHERE word!=''
    ORDER BY word COLLATE NOCASE DESC"
		(lambda (ncols row names) (push row res)))
  (sqlite3-close db)
  (completing-read "Choose a deck: " res))


(provide 'ankiorg-sql)
;;; ankiorg-sql.el ends here
