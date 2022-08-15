;;; ankiorg.el --- Pull notes from Anki to org  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ankiorg.el is an add-on to anki-editor.el that adds functionality
;; to sync notes from Anki to org, i.e. for pulling notes from Anki.
;;
;; In the code we distinguish between three different representations of a "note":
;; An "Anki note" is the note as it exists within Anki and its sql database,
;; an "org note" is the note as it exits in Emacs org-mode,
;; and a note in "anki-editor alist format" or an "alist note" is an intermediate 
;; representation of a note as an alist such as:
;;  '((deck . "Deck name") (note-id . 1612629272499) (note-type . "Note type")
;;   (tags . "Space-separated string of tags")
;;   (fields ("Field 1 name" . "Field 1 content") ("Field 2 name" . "Field 2 content"))
;;   (cards 1612629272499))
;; 
;; 
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'sqlite3)
;; make sure the correct version of anki-editor is loaded
(load-file 
 (expand-file-name "anki-editor.el"
		   (file-name-directory (buffer-file-name))))

(defgroup ankiorg nil
  "Customizations for ankiorg."
  :group 'anki-editor)

(defcustom ankiorg-notes-ask-confirmation t
  "If non-nil the command `ankiorg' will display a summary of what it thinks it should do and ask for confirmation before taking any action.")

(defcustom ankiorg-truncate-headings-at 60
  "Character at which to truncate when creating note headings.
Note headings are created from the contents of the first field."
  :type 'int
  :group 'ankiorg)

(defcustom ankiorg-media-directory "./img/"
  "Directory where media files retreived from Anki should be stored."
  :type 'string
  :group 'ankiorg)

(defcustom anki-editor-use-sql-api t
  "If non-nil elisp bindings to the sqlite3 C API will be used to directly interact with the Anki database; this requires that Anki is not running. Otherwise AnkiConnect's HTTP API is used via curl, which requires that Anki is running.")
;; #TODO add auto option

(defcustom anki-editor-sql-database "~/main/projects/AnkiOrg/collectiontest.anki2"
  "Path to the Anki sqlite database. Please back it up before setting this.")

(defcustom anki-editor-new-from-anki-heading "new-from-anki"
  "Name of the heading under which new cards from Anki should be inserted.")


;;; Main command to pull notes from Anki to org


(defun ankiorg-notes (&optional deck scope)
  "Update notes in the org representation of a DECK by pulling notes from Anki. 
This both deletes notes not existing in Anki (unless they have no note-id assigned yet), creates new entries for notes existing only in Anki, and updates notes already in org using the version in Anki. SCOPE determines where to look for decks and notes; it is as in `org-map-entries', defaulting to current buffer respecting restrictions."

  (interactive)

  
;;;;; Deck selection
  
  ;; If called interactively, pick deck from among decks in scope:
  (when (called-interactively-p 'any)
    (setq deck
	  (completing-read "Choose a deck: "
			   (cons "Show all decks in Anki..."
				 (sort
				  (anki-editor-org-deck-names scope) #'string-lessp))))
    (when (equal deck "Show all decks in Anki...")
      (if anki-editor-use-sql-api
	  (setq deck (anki-editor-sql-pick-deck))
	(progn
	  (message "Fetching decks...")
	  (setq deck
		(completing-read "Choose a deck: "
				 (sort (anki-editor-deck-names) #'string-lessp)))))))
  ;;#TODO fix seems strange
  
  ;; If not called interatively and without deck, complain
  (when (not (called-interactively-p 'any))
    (user-error "A deck name is required when calling non-interactively."))
  
  ;; Default target argument
  ;;(unless target
  ;;  (setq target (current-buffer)))
  
  
;;;;; Sorting out which notes should be deleted, created, and updated

;;;;;; Five actions within Anki between which we need to distinguish:
  
  ;; Since the last sync, a note has been either
  ;; (a1) moved from the target deck to another deck,
  ;; (a2) deleted,
  ;; (b1) moved to the target deck from another deck,
  ;; (b2) created, or
  ;; (c)  unchanged in any of the above ways.

  ;; Action taken in each case:
  ;; (a1) => update note in org from Anki, including its deck
  ;; (a2) => delete note from org
  ;; (b1) => update note in org from Anki, including its deck
  ;; (b2) => create note in org
  ;; (c)  => update note in org from Anki

  ;; #TODO only update notes if modification time in Anki is after modification time in org?
  ;; #TODO a note can have cards in several different decks! how does anki-editor handle this?
  ;; #TODO better if in case (a1) only the deck is updated and nothing else?

  
;;;;;; To distinguish these cases we need the following lists:
  
  ;; List of note-id's of notes within deck and scope in org
  (setq anki-editor-ids-in-org-deck
	(remove nil (anki-editor-map-note-entries
		     (lambda () (let ((note-id (car (org--property-local-values anki-editor-prop-note-id nil)))) (when (not (equal note-id nil)) (string-to-number note-id)))) (concat anki-editor-prop-deck "=" "\"" deck "\"") scope)))

  ;; List of note-id's of all notes within scope in org
  (setq anki-editor-ids-in-org
	(remove nil (anki-editor-map-note-entries
		     (lambda () (let ((note-id (car (org--property-local-values anki-editor-prop-note-id nil)))) (when (not (equal note-id nil)) (string-to-number note-id)))) nil scope)))

  ;; List of note-id's of notes within deck in Anki
  (message "Getting list of note IDs in deck %s from Anki..." deck)
  (setq anki-editor-ids-in-anki-deck
	(if anki-editor-use-sql-api
	    (anki-editor-sql-get-ids-in-deck deck)
	  (anki-editor--anki-connect-invoke-result "findNotes" `(("query" . ,(concat "deck:" (replace-regexp-in-string " " "_" deck)))))))
  ;; Replaces spaces in deck name with "_" (read as wildcard character), since otherwise currently not working due to error in Anki/AnkiConnect.

  ;; List of all note-id's in Anki
  (message "Getting list of all note IDs from Anki...")
  (setq anki-editor-ids-in-anki
	(if anki-editor-use-sql-api
	    (anki-editor-sql-get-ids-in-anki)
	  (anki-editor--anki-connect-invoke-result "findNotes" '(("query" . "deck:*")))))
  ;; #TODO does this list get too large? (garbage-collect)
  ;; nah, should easily be able to handle whole Anki sql database in memory


;;;;;; Some warnings:
  
  ;; Warn if duplicate note-id's in org (within deck and scope)
  (unless (equal (length anki-editor-ids-in-org-deck)
		 (length (cl-remove-duplicates anki-editor-ids-in-org-deck)))
    (display-warning 'anki-editor "Duplicate Anki note IDs detected in org. Only the first one in scope will be updated."))
  (delete-dups anki-editor-ids-in-org-deck)  ; delete duplicate id's
  
  ;; Warn if notes with matching deck but no note-id in scope in org
  (unless (equal (length anki-editor-ids-in-org-deck)
		 (length (anki-editor-map-note-entries nil (concat anki-editor-prop-deck "=" "\"" deck "\"") scope)))
    (display-warning 'anki-editor "Org notes without note ID found for deck. These have likely been created in org but not synced to Anki yet; they will be ignored."))
  
  
;;;;;; Now use set operations to get lists of id's for each case we distinguish:
  
  ;; a) note id's which are in deck in org but not in deck in Anki
  (setq anki-editor-ids-deck-org-not-anki (cl-set-difference anki-editor-ids-in-org-deck anki-editor-ids-in-anki-deck))
  ;; a1) if in Anki (but another deck), then assume user changed away from deck in Anki
  (setq anki-editor-ids-deck-changed-away (cl-intersection anki-editor-ids-deck-org-not-anki anki-editor-ids-in-anki))
  ;; a2) if not in Anki, then assume the note was deleted in Anki
  (setq anki-editor-ids-deck-deleted (cl-set-difference anki-editor-ids-deck-org-not-anki anki-editor-ids-in-anki))
  
  ;; b) note id's which are in deck in Anki but not in deck in org
  (setq anki-editor-ids-deck-anki-not-org (cl-set-difference anki-editor-ids-in-anki-deck anki-editor-ids-in-org-deck))
  ;; b1) if in org (but another deck), then assume user changed to the deck in Anki
  (setq anki-editor-ids-deck-changed-to (cl-intersection anki-editor-ids-deck-anki-not-org anki-editor-ids-in-org))
  ;; b2) if not in org, then assume the note was created in Anki
  (setq anki-editor-ids-deck-created (cl-set-difference anki-editor-ids-deck-anki-not-org anki-editor-ids-in-org))

  ;; c) note id's which are in deck in Anki and in deck in org
  (setq anki-editor-ids-deck-anki-org (cl-intersection anki-editor-ids-in-anki-deck anki-editor-ids-in-org-deck))


  ;; combined list of id's to be updated
  (setq anki-editor-ids-to-update (append anki-editor-ids-deck-changed-away anki-editor-ids-deck-changed-to anki-editor-ids-deck-anki-org))

  
;;;;;; Display summary of what will be done and optionally ask for confirmation
  
  (let ((summary-message (format "The following changes to deck '%s' will be pulled from Anki:\n\n%d notes moved to another deck -- to be updated in org.\n%d notes deleted -- to be deleted in org.\n%d notes added from another deck -- to be updated in org.\n%d notes created -- to be created in org.\n%d notes unchanged in the above ways -- to be updated in org.\n" deck
				 (length anki-editor-ids-deck-changed-away)
				 (length anki-editor-ids-deck-deleted)
				 (length anki-editor-ids-deck-changed-to)
				 (length anki-editor-ids-deck-created)
				 (length anki-editor-ids-deck-anki-org))))

    (if ankiorg-notes-ask-confirmation
	(when (not (yes-or-no-p (concat summary-message "\nContinue? ")))
	  (signal 'quit nil))
      (message summary-message)))

  
;;;;; Calling functions to delete, create, and update notes

  
  (anki-editor-delete-org-notes anki-editor-ids-deck-deleted scope)

  (anki-editor-create-org-notes anki-editor-ids-deck-created deck)

  (anki-editor-update-org-notes anki-editor-ids-to-update scope))



(defun anki-editor-org-deck-names (&optional scope)
  "Get all deck names occuring among anki-editor org notes. 
Simple wrapper to `org-map-entries'. SCOPE defaults to current buffer respecting restrictions."

  (delete-dups (org-map-entries
		(lambda () (org-entry-get nil anki-editor-prop-deck))
		(concat anki-editor-prop-deck "<>\"\"")
		scope))
  )


;;; Functions to delete, create, and update notes in org

;;;; Delete notes matching note-ids from org

(defun anki-editor-delete-org-notes (note-ids &optional scope)
  "Kills the org notes with note-id in list NOTE-IDS.
SCOPE is passed on to `org-map-entries'."
  ;; #TODO better delete or archieve than kill?
  ;; #TODO allow users to set anki-editor-note-remove-action?
  ;; if several matching entries, all will be removed
  ;; #TODO org-mark-subtree sets the mark which is not good
  ;; see set-mark docstring
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


;;;; Create notes with note-ids in org from Anki

(defun anki-editor-create-org-notes (note-ids &optional deck)
  "Creates new org notes corresponding to Anki notes with note-id in list NOTE-IDS."
  
  (setq notes (anki-editor-get-convert-notes-from-anki note-ids deck))

  (setq number-of-notes (length notes))

  (let ((i 1))
    (dolist (note notes)
      (message "Creating org note %d/%d from Anki." i number-of-notes)
      (anki-editor--create-org-note note)
      (setq i (1+ i))))
  
  (message "Done creating %d new org notes from Anki." number-of-notes))


(defun anki-editor--create-org-note (note)
  "Create the org note corresponding to alist NOTE using data from it."

  (save-excursion
    
    (anki-editor--create-goto-new-from-anki-heading)
    ;; #TODO or prefix: '(4)?
    (anki-editor--insert-note-from-alist note nil)))


(defun anki-editor--create-goto-new-from-anki-heading ()
  "Creates and/or goes to the `anki-editor-new-from-anki-heading' under which new notes from Anki should be created.
Looks for/inserts `anki-editor-new-from-anki-heading' in current buffer only; change this function to get another behavior."

  ;; #TODO deck and property inheritance?
  (beginning-of-buffer)
  
  (unless (re-search-forward (concat "[\\\\*]? " anki-editor-new-from-anki-heading) nil "end")
    (end-of-buffer)
    ;;(beginning-of-buffer)
    ;;(goto-char (org-entry-beginning-position))
    ;; #TODO something goes wrong here when deleting an entry before
    ;; want to use org-insert-heading so that its hook is run
    ;;(org-insert-heading)
    (org-insert-heading nil nil t)
    (insert anki-editor-new-from-anki-heading)))


(defun anki-editor--insert-note-from-alist (note prefix)
  "Inserts an org-note from alist NOTE as a subtree to the heading at point.
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

    ;; nil is translated to "" since otherwise it will prompt for a deck - #TODO better?
    (unless deck
      (setq deck ""))   
    
    (unless (save-excursion
              (org-up-heading-safe)
              ;; don't insert `ANKI_DECK' if some ancestor already has the same value
              (and (not (string-blank-p deck))
                   (string= deck (org-entry-get-with-inheritance anki-editor-prop-deck))))
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
    ;; #TODO use anki-editor-org-step-into-entry for now

    (if (equal (length fields) 1)
	(save-excursion
	  (let ((field (nth 0 fields)))
	    (anki-editor-org-step-into-entry)
	    (insert (cdr field))))

      (progn

	(if (and (or (not (equal (length first-field) match-pos))
		     (> match-pos truncate-at))
		 (equal (length fields) 2))
	    (save-excursion
	      (let ((field (nth 0 fields)))
		(anki-editor-org-step-into-entry)
		(insert (cdr field)))
	      (setq fields (cdr fields))))
	
	(dolist (field fields)
	  (save-excursion
	    (org-insert-heading-respect-content)
	    (org-do-demote)
	    (insert (car field))
	    (anki-editor-org-step-into-entry)
	    (insert (cdr field))
	    ))))
    
    ))


(defun anki-editor-org-step-into-entry ()
  "Move from heading to position after org entry content. 
Makes sure content is inserted after drawers and planning."
  ;; Taken from John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  ;; can probably be done much simpler #TODO just goto entry-end and insert newlines...
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
           (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n")))))
             (insert "\n"))
           (forward-line -1)))))


;;;; Update notes with note-ids in org from Anki

(defun anki-editor-update-org-notes (note-ids &optional scope)
  "Updates the org notes with note-id in list NOTE-IDS with data from Anki.
Searches for corresponding org notes in SCOPE."
  
  (setq notes (anki-editor-get-convert-notes-from-anki note-ids))

  (setq number-of-notes (length notes))

  (let ((i 1))
    (dolist (note notes)
      (message "Updating org note %d/%d from Anki." i number-of-notes)
      (anki-editor--update-org-note note scope)
      (setq i (1+ i))))
  
  (message "Done updating %d org notes from Anki." number-of-notes))


(defun anki-editor--update-org-note (note &optional scope)
  "Updates the org note corresponding to alist NOTE using data from it.
Searches for a corresponding org note in SCOPE."

  (save-excursion 

    (anki-editor--goto-org-note-heading note scope)

    (anki-editor--update-org-note-metadata note)

    (anki-editor--update-org-note-fields note)))


(defun anki-editor--goto-org-note-heading (note &optional scope)
  "Searches for an org note matching the alist-representation given by NOTE in SCOPE and goes to its heading."

  ;; #TODO fix
  ;;(unless buffer
  ;;  (setq buffer (current-buffer)))
  ;; #TODO how allow scope like in org-map-entries? Just put below things into into a function and use it. However org-map-entries doesn't pass arguments to the function so would have use a globale varible or lexical let to save note alist.
  
  ;; Kill old note contents and insert new ones #TODO delete instead?
  ;;(set-buffer buffer)
  ;;(switch-to-buffer "ankiorg.org") ; #TODO remove later and use current buffer
  (beginning-of-buffer)
  
  (re-search-forward
   (concat ":" anki-editor-prop-note-id ": " (number-to-string (alist-get 'note-id note))))
  ;; #TODO currently it simply updates first match in case there are several
  )


(defun anki-editor--update-org-note-metadata (note)
  "Updates the metadata of the org note at point using the data in alist NOTE."

  ;; update note properties
  (org-set-property anki-editor-prop-note-type (alist-get 'note-type note))
  ;; don't remove tags in anki-editor-ignored-org-tags
  (let* ((tags (split-string (alist-get 'tags note)))
	 (existing-tags (split-string (org-entry-get nil anki-editor-prop-tags)))
	 (tags-to-keep (-intersection (-difference existing-tags tags) anki-editor-ignored-org-tags))
	 (final-tags (append tags tags-to-keep))
	 )
    (org-entry-put nil anki-editor-prop-tags
		   (mapconcat 'identity final-tags " ")))

  ;; #TODO updating deck accounting for both property inheritance and multiple decks is quite involved - not done
  
  )


(defun anki-editor--update-org-note-fields (note)
  "Updates the fields of the org note at point using the data in alist NOTE."

  ;; remove old note contents

  ;; Would be easier to just replace the whole tree from heading, but we don't do this in order to preserve properties, drawers, planning and timestamps that might have been added to the note org entry. Such things are not preserved for field headings though. #TODO?

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
      (re-search-forward "^[ \t]*:END:.*\n?" end t)
      ))
  
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
  (anki-editor--insert-note-fields-from-alist note))


(defun anki-editor--insert-note-fields-from-alist (note)
  "Inserts the note fields in alist NOTE into the org note at point."

  (let ((level (nth 1 (org-heading-components)))
	(first-field (car (alist-get 'fields note)))
	(other-fields (cdr (alist-get 'fields note))))

    (org-insert-heading nil)
    (org-demote)
    (insert (car first-field))
    (save-excursion
      (anki-editor-org-step-into-entry)
      (insert (cdr first-field)))
    
    (dolist (item other-fields)
      (org-insert-heading '(4))
      (insert (car item))
      (save-excursion
	(anki-editor-org-step-into-entry)
	(insert (cdr item))))))


;; ;; old description

;;   "Updates an existing org note using the note with NOTE-ID in Anki.
;; First calls `anki-editor--pull-note' to download and convert the note into an alist, then calls `anki-editor--note-html-to-org-with-pandoc' to convert the note fields to org, navigates to the first matching note id property in BUFFER, and finally updates the corresponding subtree using values from the alist, while trying to preserve other org metadata. When BUFFER is not given it defaults to the current buffer. Note: The deck is not updated."





;; just regex search for this?!: :ANKI_NOTE_ID: 1617090699160

;; note-id needs to first be both (a) converted from org to alist and (b) from Anki to alist. Then we can (c) compare them, (d) decide what to do, and (e) update.

;; Better to convert to org and then compare using M-x ediff-buffers
;; Maybe better to just backup whole org file, replace all notes and then run ediff on whole thing?!

;; Is there any way to be smart about this? Using dates modified or something like that? Hashes?


;;; Function to get and convert notes from Anki

(defun anki-editor-get-convert-notes-from-anki (note-ids &optional deck)
  "Gets notes with note-id in list NOTE-IDS from Anki and converts their html to org. 
Just a wrapper that passes on NOTE-IDS to either `anki-editor-sql-get-notes' or `anki-editor-ancon-get-notes' depending on the setting of `anki-editor-use-sql-api', and then calls `anki-editor--notes-html-to-org-with-pandoc' on what they return. The return value is a list of alists in standard anki-editor format. The idea is that alternative functions to get notes from Anki and convert their html could be easily added here."
  
  (setq raw-notes
	(if anki-editor-use-sql-api
	    (anki-editor-sql-get-notes note-ids)
	  (anki-editor-ancon-get-notes note-ids deck)))

  (anki-editor--notes-html-to-org-with-pandoc raw-notes))


;;; Interact with Anki using sqlite3 API

;; #TODO establish and close connection to database less frequently?
;; #TODO Why does sqlite-open-readonly sometimes not work? Only works with sqlite-open-readwrite.


;;;; anki-editor-sql-exec-get-alists: general query utility function

(defun anki-editor-sql-exec-get-alists (query)
  "Executes sqlite3 query given as string QUERY and returns results as list of alists. Wrapper around sqlite3-exec. Runs against the database given in `anki-editor-sql-database'."

  (let ((db)(res))

    (setq db (sqlite3-open anki-editor-sql-database sqlite-open-readwrite))

    (setq res nil)

    (sqlite3-exec db query
                  (lambda (ncols row names)
                    (let ((i 0)
                          (li nil))
                      (while (< i ncols)
                        (push (cons (intern (nth i names)) (nth i row)) li)
                        (setq i (1+ i))
                        )
                      (setq li (reverse li))
                      (push li res)
                      )))
    
    (sqlite3-close db)
    res))


;;;; anki-editor-sql-get-ids-in-deck

;; ;; FIXME create decks table
;; (setq db (sqlite3-open anki-editor-sql-database sqlite-open-readwrite))
;; (sqlite3-exec db "CREATE TABLE FIELDS (  ntid integer NOT NULL,  ord integer NOT NULL,  name text NOT NULL COLLATE unicase,  config blob NOT NULL,  PRIMARY KEY (ntid, ord)) without rowid;CREATE UNIQUE INDEX idx_fields_name_ntid ON FIELDS (name, ntid);CREATE TABLE templates (  ntid integer NOT NULL,  ord integer NOT NULL,  name text NOT NULL COLLATE unicase,  mtime_secs integer NOT NULL,  usn integer NOT NULL,  config blob NOT NULL,  PRIMARY KEY (ntid, ord)) without rowid;CREATE UNIQUE INDEX idx_templates_name_ntid ON templates (name, ntid);CREATE INDEX idx_templates_usn ON templates (usn);CREATE TABLE notetypes (  id integer NOT NULL PRIMARY KEY,  name text NOT NULL COLLATE unicase,  mtime_secs integer NOT NULL,  usn integer NOT NULL,  config blob NOT NULL);CREATE UNIQUE INDEX idx_notetypes_name ON notetypes (name);CREATE INDEX idx_notetypes_usn ON notetypes (usn);CREATE TABLE decks (  id integer PRIMARY KEY NOT NULL,  name text NOT NULL COLLATE unicase,  mtime_secs integer NOT NULL,  usn integer NOT NULL,  common blob NOT NULL,  kind blob NOT NULL);CREATE UNIQUE INDEX idx_decks_name ON decks (name);CREATE INDEX idx_notes_mid ON notes (mid);CREATE INDEX idx_cards_odid ON cards (odid)WHERE odid != 0;UPDATE colSET ver = 15;ANALYZE;")
;; ;; progn: Database Error: "no such collation sequence: unicase", 1
;; (sqlite3-close db)

(defun anki-editor-sql-deck-to-did (deck)
  "Get deck id given DECK."
  (setq res nil)
  (setq db (sqlite3-open anki-editor-sql-database sqlite-open-readwrite))
  (sqlite3-exec db "select decks from col"
		(lambda (ncols row names)
		  (dolist (i (json-read-from-string (nth 0 row)) res)
		    (if (equal deck (replace-regexp-in-string
				     "\^_" "::"
				     (cdr (assoc 'name (cdr i)))))
			(message "inside if")
		      (setq res (cdr (assoc 'id (cdr i))))))
		  ))
  (sqlite3-close db)
  res
  )

(defun anki-editor-sql-get-ids-in-deck (deck)
  "Returns list of all note id's in Anki deck DECK."
  
  (let ((ids-in-anki-deck)
        (db (sqlite3-open anki-editor-sql-database sqlite-open-readwrite)))

    ;; old Anki query
    (sqlite3-exec db (concat "select id from notes where id in (select nid from cards where did = (select id from decks where name like '" (replace-regexp-in-string "::" "\x1f" deck) "'))")
                  (lambda (ncols row names)
                    (push (string-to-number (car row)) ids-in-anki-deck)))

    ;; new Anki
    ;; (sqlite3-exec db (concat "select id from notes where id in (select nid from cards where did = " (number-to-string (anki-editor-sql-deck-to-did deck)) ")")
    ;;               (lambda (ncols row names)
    ;;                 (push (string-to-number (car row)) ids-in-anki-deck)))


    (sqlite3-close db)
    ids-in-anki-deck)
  
  )

;;;; anki-editor-sql-get-ids-in-anki

(defun anki-editor-sql-get-ids-in-anki ()
  "Returns list of all note id's in `anki-editor-sql-database'."
  
  (let ((ids-in-anki)
        (db (sqlite3-open anki-editor-sql-database sqlite-open-readwrite)))

    (sqlite3-exec db "select distinct nid from cards"
                  (lambda (ncols row names)
                    (push (string-to-number (car row)) ids-in-anki)))

    (sqlite3-close db)
    ids-in-anki)
  
  )


;;;; anki-editor-sql-get-notes: sql to list of alists given list of note id's

(defun anki-editor-sql-get-notes (note-ids)
  "Queries sqlite3 for note data given a list of NOTE-IDS. 
Makes sure that field names are added properly. Returns a list of notes in alist format."

  ;; #TODO this runs two sql queries and one loop across fields per note-id -- might be slow, but how to handle the field names, which are stored on multiple rows and whose order matters, otherwise? In MySQL one can specify ORDER BY within group_concat, but SQLite doesn't support this - instead the order of elements returned by group_concat is arbitrary... The ord column could be used for this: GROUP_CONCAT(ord || ':' || name) but this doesn't seem to become faster and less complex than currently.
  
  (setq final
	(let ((value)(res)(mres)(out))
	  
	  (dolist (note-id note-ids value)

	    ;; FIXME needs to be updated with new Anki db format without decks table
            (setq res (anki-editor-sql-exec-get-alists
		       (format "SELECT notes.id AS 'note-id', mid, notetypes.name AS 'note-type', a.deck, trim(tags) AS tags, flds AS fields FROM notes INNER JOIN (SELECT cards.nid, group_concat(decks.name, '\x1f') AS deck FROM cards INNER JOIN decks ON cards.did = decks.id WHERE cards.nid = %s) AS a ON notes.id = a.nid INNER JOIN notetypes ON notes.mid = notetypes.id" note-id)))
	    ;; could select mod here too
	    ;; old query without decks and renaming: "select notes.id, mid, notetypes.name, trim(tags), flds from notes left join notetypes on notes.mid = notetypes.id where notes.id = %s"
	    
            (setq mres (reverse
			(anki-editor-sql-exec-get-alists
			 (format "select name from fields where ntid = %s order by ord"
				 (cdr (assoc 'mid (car res)))))))

	    ;; display warning when note has cards in multiple decks
	    (when (> (length (split-string (cdr (assoc 'deck (car res))) "\^_")) 1)
	      (display-warning 'anki-editor (format "Note with note-id %s has cards in multiple decks. Anki-editor does not handle this properly, hence the deck property of this org note will be unreliable." (cdr (assoc 'note-id (car res))))))
	    
            ;; given the above lists this will output the fields in the right format
            (setq out
		  (let ((i 0)
                        (flds-list (split-string (cdr (assq 'fields (car res))) "\^_"))
                        (li nil)
                        (elem))
		    
                    (while (< i (length mres))
		      (setq elem (cons (cdr (car (nth i  mres))) (nth i flds-list)))
		      (push elem li)
		      (setq i (1+ i)))	
		    
                    (setq li (reverse li))))                        

            (setcdr (assoc 'fields (car res)) out)
	    (setcdr (assoc 'note-id (car res)) (string-to-number (cdr (assoc 'note-id (car res)))))	    
	    (assq-delete-all 'mid (car res))
            (push (car res) value)
	    ))))


;;;; anki-editor-sql-pick-deck: select deck interactively

(defun anki-editor-sql-pick-deck ()
  "Select a deck from the list of decks used in `anki-editor-sql-database'."
  (interactive)
  (setq res nil)
  (setq db (sqlite3-open anki-editor-sql-database sqlite-open-readwrite))
  (sqlite3-exec db "select id, name from decks"
		;; for newer Anki versions:
		;; "select decks from col"
		;; for older Anki versions:
		;; "select id, name from decks"
		(lambda (ncols row names)
		  ;; for older Anki versions:
		  (let ((id (car row))
			(flds (cdr row)))
		    (push (replace-regexp-in-string "\^_" "::" (car flds)) res))
		  ;; for newer Anki versions:
		  ;; (dolist (i (json-read-from-string (nth 0 row)) res)
		  ;; 	(setq res (cons
		  ;; 		     (replace-regexp-in-string
		  ;; 		      "\^_" "::"
		  ;; 		      (cdr (assoc 'name (cdr i))))
		  ;; 		     res)))
		  ))
  (sqlite3-close db)
  (completing-read "Choose a deck: "
                   (sort res #'string-lessp)))


;;;; anki-editor-sql-pick-tag: select tag interactively

(defun anki-editor-sql-pick-tag ()
  "Select a tag from the list of tags used in `anki-editor-sql-database'."
  (interactive)
  (setq res nil)
  (setq db (sqlite3-open anki-editor-sql-database sqlite-open-readwrite))
  ;; sql query taken from here: https://stackoverflow.com/a/32051164
  (sqlite3-exec db "WITH split(word, str) AS (
    SELECT '', trim(tags)||' ' FROM notes
    -- SELECT '', '  A B CC DDD 101 fff '||' '
    UNION ALL SELECT
    substr(str, 0, instr(str, ' ')),
    substr(str, instr(str, ' ')+1)
    FROM split WHERE str!=''
) SELECT DISTINCT word FROM split WHERE word!='' ORDER BY word COLLATE NOCASE DESC"
		(lambda (ncols row names) (push row res)))
  (sqlite3-close db)
  (completing-read "Choose a deck: " res))


;;; Interact with Anki using AnkiConnect API

(defun anki-editor-ancon-get-notes (note-ids &optional deck)
  "Pulls the notes with note-id in list NOTE-IDS from Anki using AnkiConnect and returns a list of notes in anki-editors alist format.
If DECK is given it will be used for the deck value, otherwise it is nil."

  ;; #TODO fix deck - need to get it from Anki and not via argument
  ;; in order for deck changes to by synced
  
  ;; request notes info from AnkiConnect
  (message "Getting %d notes from Anki..." (length note-ids))
  (let ((notes-raw (unless (not note-ids) (anki-editor--anki-connect-invoke-result "notesInfo" `(("notes" . ,note-ids)))))
	(notes))

    ;; get values out of response and store them in properly structured alist
    (dolist (note-raw notes-raw notes)
      
      (let ((adeck deck)
	    (note-id (alist-get 'noteId note-raw))
	    (note-type (alist-get 'modelName note-raw))
	    (tags (mapconcat 'identity (alist-get 'tags note-raw) " "))
	    (fields (anki-editor--build-fields-from-anki note-raw))
	    (cards (alist-get 'cards note-raw)))

	(push `((note-id . ,note-id)
		(note-type . ,note-type)
		(deck . ,adeck)
		(tags . ,tags)
		(fields . ,fields)
		(cards . ,cards)) notes)))))


(defun anki-editor--build-fields-from-anki (response)
  "Bring fields returned from Anki into the format used by anki-editor."
  (let ((fields (alist-get 'fields response)))
    (setq fields ())
    (dolist (fieldinfo (alist-get 'fields response))
      ;;(message "%s" (cons (car fieldinfo) (alist-get 'value fieldinfo)))
      (push (cons (symbol-name (car fieldinfo)) (alist-get 'value fieldinfo)) fields))
    (reverse fields)))


;;; Convert html to org using pandoc

;; #TODO fix with better docstring

;; replace non-breaking space with space
(defcustom anki-editor-pandoc-replacements (list (cons " " ""))
  "A list of elisp regex pattern-replacement pairs which are applied right after pandoc runs to convert Anki html to org.")

;; replace two and only two backslashes with nothing
(add-to-list 'anki-editor-pandoc-replacements (cons "\\([^\\\\]\\|^\\)\\\\\\\\\\([^\\\\]\\|$\\)" "\\1\\2") t)

;; replace four and only four backslashes with two backslashes
(add-to-list 'anki-editor-pandoc-replacements (cons "\\([^\\\\]\\|^\\)\\\\\\\\\\\\\\\\\\([^\\\\]\\|$\\)" "\\1\\\\\\\\\\2") t)

;; remove whitespace at beginning of buffer
(add-to-list 'anki-editor-pandoc-replacements (cons "\\`[ \n\t]+" "") t)

;; remove whitespace at end of buffer
(add-to-list 'anki-editor-pandoc-replacements (cons "[ \n\t]+\\'" "") t)

;; risky! attempt to repair invalid html for nested lists produced by e.g. the mini-format-pack add-on; seems to work for one level of nesting
(defcustom anki-editor-anki-replacements (list (cons "</li>\\(<ul>\\([^<]\\|<\\([^uo]\\|u\\([^l]\\|l\\([^>]\\)\\)\\|o\\([^l]\\|l\\([^>]\\)\\)\\)\\)*?</ul>\\)" "\\1</li>"))
  "A list of elisp regex pattern-replacement pairs which are applied just before pandoc runs to convert Anki html to org.")
;; #TODO add the same for </li><ol>


(defun anki-editor--notes-html-to-org-with-pandoc (notes-raw)
  "Takes a list NOTES-RAW of notes in anki-editor alist form and converts all field values from html to org using pandoc."
  ;; makes use of a deep copy and setcdr #TODO is this good?

  (let ((note)
	(notes))
    
    (dolist (note-raw notes-raw)

      (setq note (copy-tree note-raw))
      
      (dolist (item (alist-get 'fields note))
	(setcdr item (anki-editor--html-to-org-with-pandoc (cdr item))))

      (push note notes))
    notes))


(defun anki-editor--html-to-org-with-pandoc (html)
  "Takes string of HTML and converts it to org with pandoc. 
Requires at least pandoc version >= 1.16. Taken from org-web-tools and modified."
  (with-temp-buffer
    (insert html)
    (anki-editor--clean-anki)
    (unless (zerop (call-process-region (point-min) (point-max) "pandoc"
                                        t t nil
                                        "--wrap=none"
                                        "-f" "html-raw_html-native_divs" "-t" "org"))
      ;; #TODO: Add error output, see org-protocol-capture-html
      (error "pandoc failed"))
    (anki-editor--clean-pandoc-output)
    (buffer-string)))


(defun anki-editor--clean-pandoc-output ()
  "Remove unwanted characters from current buffer.
Bad characters are matched by `anki-editor-pandoc-replacements'. Taken from org-web-tools."
  (save-excursion
    (cl-loop for (re . replacement) in anki-editor-pandoc-replacements
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (replace-match replacement))))))

(defun anki-editor--clean-anki ()
  "Clean buffer containing html note field from Anki.
Bad patterns are matched and replaced using `anki-editor-anki-replacements'. Taken from org-web-tools."
  (save-excursion
    (cl-loop for (re . replacement) in anki-editor-anki-replacements
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (replace-match replacement))))))



;;; Tests and misc:

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



(defun ankiorg-tags (&optional scope match)
  "Pull tags from Anki for org notes from headings that match MATCH within SCOPE.

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
        (anki-editor-map-note-entries #'anki-editor--collect-note-marker match scope)
        (setq anki-editor--note-markers (reverse anki-editor--note-markers))
        (let ((count 0)
              (failed 0))
          (save-excursion
            (anki-editor--with-collection-data-updated
              (cl-loop with bar-width = 30
                       for marker in anki-editor--note-markers
                       for progress = (/ (float (cl-incf count)) (length anki-editor--note-markers))
                       do
                       (goto-char marker)
                       (message "Pulling tags for notes in buffer %s%s [%s%s] %d/%d (%.2f%%)"
                                (marker-buffer marker)
                                (if (zerop failed)
                                    ""
                                  (propertize (format " %d failed" failed)
                                              'face `(:foreground "red")))
                                (make-string (truncate (* bar-width progress)) ?#)
                                (make-string (- bar-width (truncate (* bar-width progress))) ?.)
                                count
                                (length anki-editor--note-markers)
                                (* 100 progress))
                       (anki-editor--clear-failure-reason)
                       (condition-case-unless-debug err
                           (ankiorg-tags-sub (anki-editor-note-at-point))
                         (error (cl-incf failed)
                                (anki-editor--set-failure-reason (error-message-string err))))
                       ;; free marker
                       (set-marker marker nil))))
          (message
           (cond
            ((zerop (length anki-editor--note-markers)) "No notes to update")
            ((zerop failed) (format "Successfully pulled tags for %d notes from Anki" count))
            (t (format "Pulled tags of %d notes from Anki, with %d failed.  Check property drawers for details."
                       count failed))))))
    ;; clean up markers
    (cl-loop for m in anki-editor--note-markers
             do (set-marker m nil)
             finally do (setq anki-editor--note-markers nil))))


(defun ankiorg-tags-sub (note)
  "Does the main work of `ankiorg-tags'."
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
      (org-entry-remove-from-multivalued-property nil "ANKI_TAGS+" tag))
    ))

;;; ankiorg.el ends here
