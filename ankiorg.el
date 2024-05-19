;;; ankiorg.el --- Pull notes from Anki to org  -*- lexical-binding: t; -*-
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

;; ankiorg.el is an add-on to anki-editor.el that adds functionality
;; to sync notes from Anki to org, i.e. for pulling notes from Anki.

;; In this code a distinction is made between three different
;; representations of a "note":
;; 1. An "Anki note" is the note as it exists within Anki and its
;;    sql database,
;; 2. an "org note" is the note as it exits in Emacs org-mode, and
;; 3. a note in "anki-editor alist format" or an "alist note" is an
;;    intermediate representation of a note as an alist such as:
;;    '((deck . "Deck name")
;;      (note-id . 1612629272499)
;;      (note-type . "Note type")
;;      (tags . "Space-separated string of tags")
;;      (fields ("Field 1 name" . "Field 1 content")
;; 	        ("Field 2 name" . "Field 2 content"))
;;      (cards 1612629272499))


;;; Code:

;; * Setup

(require 'json)
(require 'cl-lib)
(require 'anki-editor)


(defgroup ankiorg nil
  "Customizations for ankiorg."
  :group 'anki-editor)

(defcustom ankiorg-media-directory nil
  "Directory where media files retreived from Anki should be stored.
This is used when calling `ankiorg-buffer-get-media-files' and may be set before
each call if several different folders are desired."
  :type 'string
  :group 'ankiorg)

(defcustom ankiorg-pull-notes-ask-confirmation t
  "If non-nil, command `ankiorg-pull-notes' will ask for confirmation.
It also displays a summary of what it thinks it should do when doing so."
  :type 'boolean
  :group 'ankiorg)

(defcustom ankiorg-truncate-headings-at 60
  "Character at which to truncate when creating note headings.
Note headings are created from the contents of the first field."
  :type 'int
  :group 'ankiorg)

(defcustom ankiorg-new-from-anki-heading "new-from-anki"
  "The heading under which new cards from Anki should be inserted."
  :type 'string
  :group 'ankiorg)

(defcustom ankiorg-pick-deck-all-directly nil
  "If non-nil, `ankiorg-pick-deck' shows all Anki decks directly.
Otherwise it first only shows deck within its scope."
  :type 'boolean
  :group 'ankiorg)

(defcustom ankiorg-org-note-files nil
  "List of files containing org notes compatible with anki-editor.
If specified, this can be selected as a value for scope in
`ankiorg-pull-notes'."
  :type 'list
  :group 'ankiorg)


(defvar ankiorg-pick-deck-function 'ankiorg-ancon-pick-deck
  "Function used to pick among Anki decks.")

(defvar ankiorg-anki-note-ids-function 'ankiorg-ancon-note-ids
  "Function used to get note-ids in Anki deck.")

(defvar ankiorg-anki-notes-function 'ankiorg-ancon-get-notes
  "Function used to get note-ids in Anki deck.")



;; * Main command to pull notes from Anki to org

;;;###autoload
(defun ankiorg-pull-notes (deck &optional scope)
  "Pull notes from Anki to org.

When called interactively, `ankiorg-pick-deck' asks for a DECK,
and with a prefix argument, `ankiorg-pick-scope' asks for a SCOPE.

SCOPE determines where to look for decks and notes; it is as in
`org-map-entries', defaulting to current buffer respecting restrictions.

If org notes with `anki-editor-prop-deck' property matching DECK already
exist within SCOPE, try to update the org representation of DECK in a
reasonable way by doing all of the following:

- delete org notes not existing in Anki, unless they have no
  `anki-editor-prop-note-id' property assigned yet,
- create new org note entries for notes existing only in Anki, and
- update org notes already in org using the version in Anki.

In more detail, there are five actions within Anki between which we need
to distinguish. Since the last sync, a note has been either

(a1) moved from the target deck to another deck
     => update note in org from Anki, including its deck,
(a2) deleted
     => delete note from org, 
(b1) moved to the target deck from another deck
     => update note in org from Anki, including its deck,
(b2) created
     => create note in org,
(c)  not changed in any of the above ways
     => update note in org from Anki."
  ;; #TODO refactor more
  ;; #TODO only update notes if modification time in Anki is after 
  ;;       modification time in org?
  ;; #TODO a note can have cards in several different decks!
  ;;       how does anki-editor handle this?
  ;; #TODO better if in case (a1) only the deck is updated and
  ;;       nothing else?
  (interactive (list (ankiorg-pick-deck)
		     (when current-prefix-arg
		       (ankiorg-pick-scope))))
  ;; To distinguish the above cases we need the following lists:
  (let ((ids-in-org-deck (ankiorg-org-note-ids deck scope))
	(ids-in-org (ankiorg-all-org-note-ids))
	(ids-in-anki-deck (funcall ankiorg-anki-note-ids-function deck))
	(ids-in-anki (funcall ankiorg-anki-note-ids-function)))

    (ankiorg--display-warnings ids-in-org-deck ids-in-org
			       ids-in-anki-deck ids-in-anki)
    (delete-dups ids-in-org-deck)     
    ;; Use set operations to get lists of id's for each case we distinguish:
    ;; a) note id's which are in deck in org but not in deck in Anki
    (setq ids-deck-org-not-anki
	  (cl-set-difference ids-in-org-deck ids-in-anki-deck))
    ;; a1) if in Anki but another deck, assume user changed away from
    ;;     deck in Anki
    (setq ids-deck-changed-away
	  (cl-intersection ids-deck-org-not-anki ids-in-anki))
    ;; a2) if not in Anki, then assume the note was deleted in Anki
    (setq ids-deck-deleted
	  (cl-set-difference ids-deck-org-not-anki ids-in-anki))  
    ;; b) note id's which are in deck in Anki but not in deck in org
    (setq ids-deck-anki-not-org
	  (cl-set-difference ids-in-anki-deck ids-in-org-deck))
    ;; b1) if in org but another deck, assume user changed to the deck in Anki
    (setq ids-deck-changed-to
	  (cl-intersection ids-deck-anki-not-org ids-in-org))
    ;; b2) if not in org, then assume the note was created in Anki
    (setq ids-deck-created
	  (cl-set-difference ids-deck-anki-not-org ids-in-org))
    ;; c) note id's which are in deck in Anki and in deck in org
    (setq ids-deck-anki-org
	  (cl-intersection ids-in-anki-deck ids-in-org-deck))
    ;; combined list of id's to be updated
    (setq ids-to-update
	  (append ids-deck-changed-away
		  ids-deck-changed-to
		  ids-deck-anki-org))  
    ;; Display summary of what will be done and
    ;; optionally ask for confirmation
    (let ((summary-message
	   (format
	    (concat
	     "The following changes to deck '%s' will be pulled from Anki:\n\n"
	     "%d notes moved to another deck -- to be updated in org.\n"
	     "%d notes deleted -- to be deleted in org.\n"
	     "%d notes added from another deck -- to be updated in org.\n"
	     "%d notes created -- to be created in org.\n"
	     "%d notes unchanged in the above ways -- to be updated in org.\n")
	    deck
	    (length ids-deck-changed-away)
	    (length ids-deck-deleted)
	    (length ids-deck-changed-to)
	    (length ids-deck-created)
	    (length ids-deck-anki-org))))
      (if ankiorg-pull-notes-ask-confirmation
	  (when (not (yes-or-no-p (concat summary-message "\nContinue? ")))
	    (signal 'quit nil))
	(message summary-message)))
    ;; Call functions to delete, create, and update notes  
    (ankiorg-delete-org-notes ids-deck-deleted scope)
    (ankiorg-create-org-notes ids-deck-created deck)
    (ankiorg-update-org-notes ids-to-update scope)))



(defun ankiorg-pick-deck (&optional scope)
  "Pick a deck with presets based on decks in org.

If `ankiorg-pick-deck-all-directly' is non-nil, presets are populated
with all values of the `anki-editor-prop-deck' property found within
SCOPE and an option to 'Show all Anki decks...'. Otherwise all Anki
decks are shown directly.

If `ankiorg-use-sql-api' is non-nil, the list of all Anki decks is
fetched by interfacing via SQLite. Else Anki-Connect is used.

SCOPE is as in `org-map-entries', defaulting to current buffer
respecting restrictions."
  (let (deck)
    (unless ankiorg-pick-deck-all-directly
      (setq deck
	    (completing-read "Deck: "
			     (cons "Show all Anki decks..."
				   (sort (ankiorg-org-deck-names scope)
					 #'string-lessp)))))
    (if (or ankiorg-pick-deck-all-directly
	    (string= deck "Show all Anki decks..."))
	(funcall ankiorg-pick-deck-function)
      deck)))


(defun ankiorg-pick-scope ()
  "Pick a scope as used by `org-map-entries'.
When `ankiorg-org-note-files' is non-nil, present it as an alternative."
  (let ((alternatives '("nil" "tree" "region"
			"region-start-level" "file" "agenda"))
	choice)
    (when ankiorg-org-note-files
	(setq alternatives (cons "ankiorg-org-note-files" alternatives)))
    (setq choice (completing-read "Scope: " alternatives))
    (if (string= choice "ankiorg-org-note-files")
	ankiorg-org-note-files
      (intern choice))))


(defun ankiorg-org-deck-names (&optional scope)
  "Get all deck names occuring among org notes in SCOPE.
Simple wrapper to `anki-editor-map-note-entries' which in turn wrapys
`org-map-entries'. SCOPE defaults to current buffer respecting
restrictions."
  ;; We don't match on anki-editor-prop-deck when calling
  ;; org-map-entries and specify t, in order to retrieve the property
  ;; with inheritance. Otherwise org-map-entries misses the deck of
  ;; notes inheriting from properties at the top of a file
  ;; (i.e. not under any heading). Also we don't want to match
  ;; subheadings of a note, which is why we first disable inheritance
  ;; for the match and then reenable it in org-entry-get.
  (delete-dups
   (anki-editor-map-note-entries
    (lambda () (org-entry-get nil anki-editor-prop-deck t t))
    nil
    scope)))


(defun ankiorg-ancon-pick-deck ()
  "Pick a deck with presets fetched via Anki-Connect."
  (completing-read
   "Deck: "
   (sort (anki-editor-deck-names) #'string-lessp)))


(defun ankiorg-org-note-ids (deck &optional scope)
  "Get list of note-ids of notes within DECK and SCOPE in org."
  ;; See ankiorg-org-deck-names for some explanation.
  (remove nil
	  (anki-editor-map-note-entries
	   (lambda ()
	     (let ((note-deck (org-entry-get nil anki-editor-prop-deck t t))
		   node-id)
	       (setq note-id
		     (if (string= note-deck deck)
			 (car (org--property-local-values
			       anki-editor-prop-note-id nil))
		       nil))
	       (when (not (equal note-id nil))
		 (string-to-number note-id))))
	   nil
	   scope)))


(defun ankiorg-all-org-note-ids (&optional scope)
  "Get list of all note-ids of notes within SCOPE in org."
  ;; See ankiorg-org-deck-names for some explanation.
  (remove nil
	  (anki-editor-map-note-entries
	   (lambda ()
	     (let ((note-id (car (org--property-local-values
				  anki-editor-prop-note-id nil))))
	       (when (not (equal note-id nil))
		 (string-to-number note-id))))
	   nil
	   scope)))


(defun ankiorg-ancon-note-ids (&optional deck)
  "Get list of all note-ids in Anki using Anki-Connect.
If DECK is non-nil, only get note-ids in DECK."
  ;; #TODO Replaces spaces in deck name with "_" (read as wildcard
  ;;       character), since otherwise currently not working due to
  ;;       error in Anki/Anki-Connect.
  ;;       Otherwise we could also just use `anki-editor-find-notes'.
  (anki-editor-api-call-result
   'findNotes
   :query (if deck
	      (concat "deck:" (replace-regexp-in-string " " "_" deck))
	    "")))


(defun ankiorg--display-warnings (ids-in-org-deck ids-in-org
						  ids-in-anki-deck ids-in-anki)
  "Displays warnings."
  ;; Warn if duplicate note-id's in org (within deck and scope)
  (unless (equal (length ids-in-org-deck)
		 (length (cl-remove-duplicates
			  ids-in-org-deck)))
    (display-warning
     'ankiorg
     (concat "Duplicate Anki note IDs detected in org. "
	     "Only the first one in scope will be updated."))
  ;; Warn if notes with matching deck but no note-id in scope in org
  (unless (equal (length ids-in-org-deck)
		 (length (anki-editor-map-note-entries
			  nil
			  (concat anki-editor-prop-deck "=" "\"" deck "\"")
			  scope)))
    (display-warning
     'ankiorg
     (concat "Org notes without note ID found for deck. "
	     "These have likely been created in org but not"
             "synced to Anki yet;"
	     " they will be ignored.")))))



;; * Functions to delete, create, and update notes in org

;; ** Delete notes matching note-ids from org

(defun ankiorg-delete-org-notes (note-ids &optional scope)
  "Kill the org notes with note-id in list NOTE-IDS.
SCOPE is passed on to `org-map-entries'."
  ;; #TODO better delete or archieve than kill?
  ;; #TODO allow users to set ankiorg-note-remove-action?
  ;; if several matching entries, all will be removed
  ;; #TODO org-mark-subtree sets the mark which is not good
  ;;       see set-mark docstring
  (let ((org-map-return))
    (dolist (note-id note-ids)
      (setq org-map-return
	    (append org-map-return
		    (anki-editor-map-note-entries
		     (lambda ()
		       (org-mark-subtree)
		       (kill-region (region-beginning) (region-end))
		       (deactivate-mark)
		       (setq org-map-continue-from (point)))
		     (concat anki-editor-prop-note-id
			     "="
			     (number-to-string note-id))
		     scope))))
    (message "Done removing %d of %d notes expected from org."
	     (length org-map-return)
	     (length note-ids))))


;; ** Create notes with note-ids in org from Anki

(defun ankiorg-create-org-notes (note-ids &optional deck)
  "Create org notes corresponding to Anki notes with note-id in list NOTE-IDS.
DECK is passed on to `ankiorg-get-convert-notes-from-anki'."
  
  (setq notes
	(ankiorg-get-convert-notes-from-anki
	 note-ids deck))

  (setq number-of-notes
	(length notes))

  (let ((i 1))
    (dolist (note notes)
      (message "Creating org note %d/%d from Anki." i number-of-notes)
      (ankiorg--create-org-note note)
      (setq i (1+ i))))
  
  (message "Done creating %d new org notes from Anki." number-of-notes))


(defun ankiorg--create-org-note (note)
  "Create the org note corresponding to alist NOTE using data from it."

  (save-excursion
    
    (ankiorg--create-goto-new-from-anki-heading)
    ;; #TODO or prefix: '(4)?
    (ankiorg--insert-note-from-alist note nil)))


(defun ankiorg--create-goto-new-from-anki-heading ()
  "Create and/or go to the `ankiorg-new-from-anki-heading'.
This is the heading under which new notes from Anki should be created.
Looks for/inserts `ankiorg-new-from-anki-heading' in current buffer only;
change this function to get another behavior."

  ;; #TODO deck and property inheritance?
  (beginning-of-buffer)
  
  (unless (re-search-forward
	   (concat "[\\\\*]? " ankiorg-new-from-anki-heading) nil "end")
    (end-of-buffer)
    ;;(beginning-of-buffer)
    ;;(goto-char (org-entry-beginning-position))
    ;; #TODO something goes wrong here when deleting an entry before
    ;; want to use org-insert-heading so that its hook is run
    ;;(org-insert-heading)
    (org-insert-heading nil nil t)
    (insert ankiorg-new-from-anki-heading)))


(defun ankiorg--insert-note-from-alist (note prefix)
  "Insert an org-note from alist NOTE as a subtree to the heading at point.
Where the subtree is created depends on PREFIX."

  ;; get values from alist
  (let* ((deck (alist-get 'deck note))
	 (note-id (number-to-string (alist-get 'note-id note)))
	 (note-type (alist-get 'note-type note))
	 (tags (alist-get 'tags note))
	 (fields (alist-get 'fields note))
	 ;;(heading (number-to-string (alist-get 'note-id note)))
	 ;; FIXME need to only match until first line-break
	 (first-field (cdr (nth 0 fields)))
	 (truncate-at ankiorg-truncate-headings-at)
	 (match-pos (string-match "$" first-field))
	 (heading (if (< match-pos truncate-at)
		      (substring first-field 0 (match-beginning 0))
		    (substring first-field 0 truncate-at))))

    ;; if the whole first field was inserted as heading, skip it
    (if (and (equal (length first-field) match-pos)
	     (< match-pos truncate-at))
	(setq fields (cdr fields)))
        
    (org-insert-subheading prefix)
    (insert heading)

    ;; nil is translated to "" since otherwise it will prompt for a deck
    ;; #TODO handle this better?
    (unless deck
      (setq deck ""))
    
    (unless
	(save-excursion
          (org-up-heading-safe)
          ;; don't insert ANKI_DECK if some ancestor already has the same value
          (and (not (string-blank-p deck))
               (string=
		deck
		(org-entry-get-with-inheritance anki-editor-prop-deck))))
      (org-set-property anki-editor-prop-deck deck))

    (org-set-property anki-editor-prop-note-type note-type)
    ;;(when (and tags (not (equal tags "")))
    ;;  (org-set-property anki-editor-prop-tags tags))
    (when (and tags (not (equal tags "")))
      (dolist (tag (split-string tags))
    	(org-set-property anki-editor-prop-tags tag)))
    (org-set-property anki-editor-prop-note-id note-id)

    ;; field now contains a list of field-name and field-value
    ;; #TODO need to skip property drawers etc. like in better org return
    ;; before inserting contents
    ;; maybe can look at how org-capture does things too
    ;; #TODO use ankiorg-org-step-into-entry for now

    (if (equal (length fields) 1)
	(save-excursion
	  (let ((field (nth 0 fields)))
	    (ankiorg-org-step-into-entry)
	    (insert (cdr field))))

      (progn

	(if (and (or (not (equal (length first-field) match-pos))
		     (> match-pos truncate-at))
		 (equal (length fields) 2))
	    (save-excursion
	      (let ((field (nth 0 fields)))
		(ankiorg-org-step-into-entry)
		(insert (cdr field)))
	      (setq fields (cdr fields))))
	
	(dolist (field fields)
	  (save-excursion
	    (org-insert-heading-respect-content)
	    (org-do-demote)
	    (insert (car field))
	    (ankiorg-org-step-into-entry)
	    (insert (cdr field))
	    ))))
    
    ))


(defun ankiorg-org-step-into-entry ()
  "Move from heading to position after org entry content.
Makes sure content is inserted after drawers and planning."
  ;; Taken from John Kitchin's blog (2017-04-09) "A better return in org-mode"
  ;; #TODO can probably be done much simpler:
  ;;       just goto entry-end and insert newlines...
  (let ((heading-start (org-entry-beginning-position)))
    (goto-char (org-entry-end-position))
    (cond ((and (org-at-heading-p)
                (= heading-start (org-entry-beginning-position)))
           ;; Entry ends on its heading; add newline after
           (end-of-line)
           (insert "\n\n"))
          (t
           ;; Entry ends after its heading; back up
           (forward-line -1)
           (end-of-line)
           (when (org-at-heading-p)
             ;; At the same heading
             (forward-line)
             (insert "\n")
             (forward-line -1))
           ;; FIXME: looking-back is supposed to be called with more arguments.
           (while (not (looking-back
			(rx (repeat 3 (seq (optional blank) "\n")))))
             (insert "\n"))
           (forward-line -1)))))


;; ** Update notes with note-ids in org from Anki

(defun ankiorg-update-org-notes (note-ids &optional scope)
  "Update the org notes with note-id in list NOTE-IDS with data from Anki.
Searches for corresponding org notes in SCOPE."
  (setq notes (ankiorg-get-convert-notes-from-anki note-ids))
  (setq number-of-notes (length notes))
  
  (let ((i 1))
    (dolist (note notes)
      (message "Updating org note %d/%d from Anki." i number-of-notes)
      (ankiorg--update-org-note note scope)
      (setq i (1+ i))))
  
  (message "Done updating %d org notes from Anki." number-of-notes))


(defun ankiorg--update-org-note (note &optional scope)
  "Update the org note corresponding to alist NOTE using data from it.
Searches for a corresponding org note in SCOPE."
  (save-excursion
    (ankiorg--goto-org-note-heading note scope)
    (ankiorg--update-org-note-metadata note)
    (ankiorg--update-org-note-fields note)))


(defun ankiorg--goto-org-note-heading (note &optional scope)
  "Search for org note matching alist-representation given by NOTE in SCOPE.
And goes to its heading."
  ;; #TODO fix
  ;; (unless buffer
  ;;   (setq buffer (current-buffer)))
  ;; #TODO how allow scope like in org-map-entries?
  ;; Just put below things into into a function and use it.
  ;; However org-map-entries doesn't pass arguments to the function so would
  ;; have use a globale varible or lexical let to save note alist.
  
  ;; Kill old note contents and insert new ones #TODO delete instead?
  ;;(set-buffer buffer)
  ;;(switch-to-buffer "ankiorg.org") ; #TODO remove later and use current buffer
  (beginning-of-buffer)

  ;; #TODO currently it simply updates first match in case there are several
  (re-search-forward
   (concat ":" anki-editor-prop-note-id ": "
	   (number-to-string (alist-get 'note-id note)))))


(defun ankiorg--update-org-note-metadata (note)
  "Update the metadata of the org note at point using the data in alist NOTE."
  ;; #TODO updating deck accounting for both property inheritance and
  ;; multiple decks is quite involved - not done yet
  
  ;; update note properties
  (org-set-property anki-editor-prop-note-type (alist-get 'note-type note))
  ;; don't remove tags in anki-editor-ignored-org-tags
  (let* ((tags
	  (split-string (alist-get 'tags note)))
	 (existing-tags
	  (split-string (or (org-entry-get nil anki-editor-prop-tags) "")))
	 (tags-to-keep
	  (-intersection (-difference existing-tags tags)
			 anki-editor-ignored-org-tags))
	 (final-tags
	  (append tags tags-to-keep)))
    (org-entry-put nil anki-editor-prop-tags
		   (mapconcat 'identity final-tags " "))))


(defun ankiorg--update-org-note-fields (note)
  "Update the fields of the org note at point using the data in alist NOTE."

  ;; remove old note contents

  ;; Would be easier to just replace the whole tree from heading,
  ;; but we don't do this in order to preserve properties, drawers, planning
  ;; and timestamps that might have been added to the note org entry.
  ;; Such things are not preserved for field headings though. #TODO?

  (org-mark-subtree) ; mark whole subtree with point at its start
  ;; (org-end-of-subtree)
  
  ;; Skip headline
  (forward-line)

  ;; Skip planning lines
  (while (and (org-at-planning-p)
              (< (point) (point-max)))
    (forward-line))

  ;; Skip drawers
  (let ((end (org-entry-end-position)))
    (while (re-search-forward org-drawer-regexp end t)
      (re-search-forward "^[ \t]*:END:.*\n?" end t)))
  
  ;; Skip one timestamp
  (if (looking-at org-element--timestamp-regexp)
      (forward-line))
  
  ;; Kill the remainder of the subtree
  (kill-region (region-beginning) (region-end))
  (deactivate-mark)
  
  ;; and move into right position for inserting new content
  (newline 2) ;; #TODO allow customization? or use org setting
  (forward-line -1)
  
  ;; insert updated note contents
  (ankiorg--insert-note-fields-from-alist note))


(defun ankiorg--insert-note-fields-from-alist (note)
  "Insert the note fields in alist NOTE into the org note at point."

  (let ((level (nth 1 (org-heading-components)))
	(first-field (car (alist-get 'fields note)))
	(other-fields (cdr (alist-get 'fields note))))

    (org-insert-heading nil)
    (org-demote)
    (insert (car first-field))
    (save-excursion
      (ankiorg-org-step-into-entry)
      (insert (cdr first-field)))
    
    (dolist (item other-fields)
      (org-insert-heading '(4))
      (insert (car item))
      (save-excursion
	(ankiorg-org-step-into-entry)
	(insert (cdr item))))))


;; #TODO think of a better way to update notes

;; just regex search for this?!: :ANKI_NOTE_ID: 1617090699160

;; note-id needs to first be both
;; (a) converted from org to alist and
;; (b) from Anki to alist.
;; Then we can (c) compare them, (d) decide what to do, and (e) update.

;; Better to convert to org and then compare using M-x ediff-buffers
;; Maybe better to just backup whole org file, replace all notes
;; and then run ediff on whole thing?!

;; Is there any way to be smart about this?
;; Using dates modified or something like that? Hashes?


;; * Function to get and convert notes from Anki

(defun ankiorg-get-convert-notes-from-anki (note-ids &optional deck)
  "Get notes with note-id in NOTE-IDS from Anki and convert their html to org.
Just a wrapper that passes on NOTE-IDS to either `ankiorg-sql-get-notes' or
`ankiorg-ancon-get-notes' depending on the setting of `ankiorg-use-sql-api',
and then calls `ankiorg--notes-html-to-org-with-pandoc' on what they return.
The return value is a list of alists in standard anki-editor format. The idea is
that alternative functions to get notes from Anki and convert their html could
be easily added here.
If DECK is given it is used by `ankiorg-ancon-get-notes'."
  
  (setq raw-notes
	(funcall ankiorg-anki-notes-function note-ids deck))
  (ankiorg--notes-html-to-org-with-pandoc raw-notes))


;; * Interact with Anki using Anki-Connect API

(defun ankiorg-ancon-get-notes (note-ids &optional deck)
  "Pulls the notes with note-id in list NOTE-IDS from Anki using AnkiConnect.
Returns a list of notes in anki-editors alist format.
If DECK is given it will be used for the deck value, otherwise it is nil."

  ;; #TODO fix deck - need to get it from Anki and not via argument
  ;; in order for deck changes to by synced
  
  ;; request notes info from AnkiConnect
  (message "Getting %d notes from Anki..." (length note-ids))
  (let ((notes-raw
	 (unless (not note-ids)
	   (anki-editor-api-call-result 'notesInfo :notes note-ids)))
	(notes))
    
    ;; get values out of response and store them in properly structured alist
    (dolist (note-raw notes-raw notes)
      
      (let ((adeck deck)
	    (note-id (alist-get 'noteId note-raw))
	    (note-type (alist-get 'modelName note-raw))
	    (tags (mapconcat 'identity (alist-get 'tags note-raw) " "))
	    (fields (ankiorg--build-fields-from-anki note-raw))
	    (cards (alist-get 'cards note-raw)))

	(push `((note-id . ,note-id)
		(note-type . ,note-type)
		(deck . ,adeck)
		(tags . ,tags)
		(fields . ,fields)
		(cards . ,cards))
	      notes)))))


(defun ankiorg--build-fields-from-anki (response)
  "Bring fields returned from Anki into the format used by anki-editor.
RESPONSE should be what is returned from a note query to Anki-Connect."
  (let ((fields (alist-get 'fields response)))
    (setq fields ())
    (dolist (fieldinfo (alist-get 'fields response))
      ;;(message "%s" (cons (car fieldinfo) (alist-get 'value fieldinfo)))
      (push (cons (symbol-name (car fieldinfo))
		  (alist-get 'value fieldinfo))
	    fields))
    (reverse fields)))


;; * Convert html to org using pandoc

;; #TODO move and fix with better docstring

;; replace non-breaking space with space
(defcustom ankiorg-pandoc-replacements (list (cons " " ""))
  "A list of elisp regex pattern-replacement pairs.
They are applied right after pandoc runs to convert Anki html to org."
  :type 'list
  :group 'ankiorg)

;; replace two and only two backslashes with nothing
(add-to-list
 'ankiorg-pandoc-replacements
 (cons "\\([^\\\\]\\|^\\)\\\\\\\\\\([^\\\\]\\|$\\)" "\\1\\2") t)

;; replace four and only four backslashes with two backslashes
(add-to-list
 'ankiorg-pandoc-replacements
 (cons "\\([^\\\\]\\|^\\)\\\\\\\\\\\\\\\\\\([^\\\\]\\|$\\)" "\\1\\\\\\\\\\2") t)

;; remove whitespace at beginning of buffer
(add-to-list
 'ankiorg-pandoc-replacements
 (cons "\\`[ \n\t]+" "") t)

;; remove whitespace at end of buffer
(add-to-list
 'ankiorg-pandoc-replacements
 (cons "[ \n\t]+\\'" "") t)

;; replace [sound:x] with file:x
(add-to-list
 'ankiorg-pandoc-replacements
 (cons "\\[sound:\\(.*\\)\\]" "file:\\1") t)

;; risky! attempt to repair invalid html for nested lists produced by
;; e.g. the mini-format-pack add-on; seems to work for one level of nesting
(defcustom ankiorg-anki-replacements
  (list (cons (concat "</li>\\(<ul>\\([^<]\\|<\\([^uo]\\|u\\([^l]\\|l\\([^>]"
		      "\\)\\)\\|o\\([^l]\\|l\\([^>]\\)\\)\\)\\)*?</ul>\\)")
	      "\\1</li>"))
  "A list of elisp regex pattern-replacement pairs.
They are applied just before pandoc runs to convert Anki html to org."
  :type 'list
  :group 'ankiorg)
;; #TODO add the same for </li><ol>


(defun ankiorg--notes-html-to-org-with-pandoc (notes-raw)
  "Convert all field values from html to org using pandoc.
Takes a list NOTES-RAW of notes in anki-editor alist form."
  ;; makes use of a deep copy and setcdr #TODO is this good?

  (let ((note)
	(notes))
    
    (dolist (note-raw notes-raw)

      (setq note (copy-tree note-raw))
      
      (dolist (item (alist-get 'fields note))
	(setcdr item (ankiorg--html-to-org-with-pandoc (cdr item))))

      (push note notes))
    notes))


(defun ankiorg--html-to-org-with-pandoc (html)
  "Take string of HTML and convert it to org with pandoc.
Requires at least pandoc version >= 1.16.
Taken from org-web-tools and modified."
  (with-temp-buffer
    (insert html)
    (ankiorg--clean-anki)
    (unless (zerop
	     (call-process-region (point-min) (point-max) "pandoc"
                                  t t nil
                                  "--wrap=none"
                                  "-f" "html-raw_html-native_divs" "-t" "org"))
      ;; #TODO: Add error output, see org-protocol-capture-html
      (error "pandoc failed"))
    (ankiorg--clean-pandoc-output)
    (buffer-string)))


(defun ankiorg--clean-pandoc-output ()
  "Remove unwanted characters from current buffer.
Bad characters are matched by `ankiorg-pandoc-replacements'.
Taken from org-web-tools."
  (save-excursion
    (cl-loop for (re . replacement) in ankiorg-pandoc-replacements
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (replace-match replacement))))))

(defun ankiorg--clean-anki ()
  "Clean buffer containing html note field from Anki.
Bad patterns are matched and replaced using `ankiorg-anki-replacements'.
Taken from org-web-tools."
  (save-excursion
    (cl-loop for (re . replacement) in ankiorg-anki-replacements
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (replace-match replacement))))))



;; * Pull media files and tags from Anki to org

;;;###autoload
(defun ankiorg-buffer-get-media-files ()
  "Copies files linked in buffer from the Anki media folder.
The destination is controlled by `ankiorg-media-directory'."
  ;; TODO show progress as this might take quite long
  (interactive)
  (let ((filenames
	 (delete-dups
	  (org-element-map (org-element-parse-buffer) 'link
	    (lambda (link)
	      (when (string= (org-element-property :type link) "file")
		(org-element-property :path link)))))))
    (dolist (filename filenames)
      (ankiorg-get-media-file filename))))

(defun ankiorg-get-media-file (filename)
  "Copies media file with FILENAME from the Anki media folder.
The destination is controlled by `ankiorg-media-directory'."
  (let ((new-filename
	 (if (string-prefix-p
	      ankiorg-media-directory filename)
	     filename
	   (concat
	    ankiorg-media-directory
	    filename)))
	file-returned)

    ;; retrieve file from Anki
    (unless (file-exists-p new-filename)
      (setq file-returned
	    (anki-editor-api-call-result
	     'retrieveMediaFile :filename filename)))

    ;; save file to ankiorg-media-directory
    (unless (or (equal file-returned ':json-false)
		(not file-returned))
      (with-temp-buffer
	(toggle-enable-multibyte-characters)
	(set-buffer-file-coding-system 'raw-text)
	(seq-doseq (char (base64-decode-string file-returned))
	  (insert char))
	(write-region nil nil new-filename)))

    ;; update file paths
    (unless (and (equal filename new-filename)
		 (not (file-exists-p new-filename)))
      (save-excursion
	(beginning-of-buffer)
	(while (search-forward filename nil t)
	  (replace-match new-filename))))))



;;;###autoload
(defun ankiorg-pull-tags (&optional scope match)
  "Pull tags from Anki for org notes from headings that MATCH within SCOPE.

The default search condition `&ANKI_NOTE_TYPE<>\"\"' will always
be appended to MATCH.

If SCOPE is not specified, the following rules are applied to
determine the scope:

- If there's an active region, it will be set to `region'
- If called with prefix `C-u', it will be set to `tree'
- If called with prefix double `C-u', it will be set to `file'
- If called with prefix triple `C-u', will be set to `agenda'

See doc string of `org-map-entries' for what these different options mean.

If one fails, the failure reason will be set in property drawer
of that heading."
  (interactive (list (cond
                      ((region-active-p) 'region)
                      ((equal current-prefix-arg '(4)) 'tree)
                      ((equal current-prefix-arg '(16)) 'file)
                      ((equal current-prefix-arg '(64)) 'agenda)
                      (t nil))))
  (unwind-protect
      (progn
        (anki-editor-map-note-entries #'anki-editor--collect-note-marker
				      match scope)
        (setq anki-editor--note-markers (reverse anki-editor--note-markers))
        (let ((count 0)
              (failed 0))
          (save-excursion
            (anki-editor--with-collection-data-updated
              (cl-loop with bar-width = 30
                       for marker in anki-editor--note-markers
                       for progress = (/ (float (cl-incf count))
					 (length anki-editor--note-markers))
                       do
                       (goto-char marker)
                       (message (concat "Pulling tags for notes in buffer "
					"%s%s [%s%s] %d/%d (%.2f%%)")
                                (marker-buffer marker)
                                (if (zerop failed)
                                    ""
                                  (propertize (format " %d failed" failed)
                                              'face `(:foreground "red")))
                                (make-string
				 (truncate (* bar-width progress))
				 ?#)
                                (make-string
				 (- bar-width
				    (truncate (* bar-width progress)))
				 ?.)
                                count
                                (length anki-editor--note-markers)
                                (* 100 progress))
                       (anki-editor--clear-failure-reason)
                       (condition-case-unless-debug err
                           (ankiorg-pull-tags-sub (anki-editor-note-at-point))
                         (error (cl-incf failed)
                                (anki-editor--set-failure-reason
				 (error-message-string err))))
                       ;; free marker
                       (set-marker marker nil))))
          (message
           (cond
            ((zerop (length anki-editor--note-markers))
	     "No notes to update")
            ((zerop failed)
	     (format "Successfully pulled tags for %d notes from Anki" count))
            (t
	     (format (concat "Pulled tags of %d notes from Anki, "
			     "with %d failed. "
			     "Check property drawers for details.")
                       count failed))))))
    ;; clean up markers
    (cl-loop for m in anki-editor--note-markers
             do (set-marker m nil)
             finally do (setq anki-editor--note-markers nil))))


(defun ankiorg-pull-tags-sub (note)
  "Does the main work of `ankiorg-pull-tags'.
NOTE should be in anki-editor alist format."
  (let* ((oldnote
	  (car
	   (cdr
	    (car
	     (anki-editor-api-call 'notesInfo
				   :notes (list
					   (string-to-number
					    (anki-editor-note-id note))))))))
	 (tagsadd (cl-set-difference (alist-get 'tags oldnote)
				     (anki-editor-note-tags note)
				     :test 'string=))
	 (tagsdel (cl-set-difference (anki-editor-note-tags note)
				     (alist-get 'tags oldnote)
				     :test 'string=)))
    (message "anki tags: %s" (alist-get 'tags oldnote))
    (message "org tags: %s" (anki-editor-note-tags note))
    (message "tagsadd: %s" tagsadd)
    (message "tagsdel: %s" tagsdel)
    (dolist (tag tagsadd)
      (org-entry-add-to-multivalued-property nil "ANKI_TAGS+" tag))
    (dolist (tag tagsdel)
      ;; deleting tags works only for non-inherited tags
      (org-entry-remove-from-multivalued-property nil "ANKI_TAGS+" tag))))


(provide 'ankiorg)
;;; ankiorg.el ends here
