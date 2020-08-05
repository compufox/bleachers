;;;; bleachers.lisp

(in-package #:bleachers)

;; make this KEY: teamid  VALUE: game object
(defvar *scores* (make-hash-table :test 'equal :size 14))
(defvar *team-names* nil)
(defvar *current-game-id* nil)
(defvar *update-mailbox* (safe-queue:make-mailbox :name "game updates"))

(defun main ()
  
  ;; get a mapping for the team names
  (handler-case
      (loop initially (setf *team-names* nil)
	    for team in (blaseball:get-all-teams)
	    do
	       (setf (gethash (blaseball:id team) *scores*) nil)
	       (push (cons (blaseball:id team)
			   (blaseball:full-name team))
		     *team-names*))
    (error ()
      (return-from main)))
  
  (let ((ws (blaseball-live:blaseball-websocket #'process-updates)))
    (handler-case 
	(wsd:start-connection ws)
      (error ()
	(wsd:remove-all-listeners ws)
	(wsd:close-connection ws)
	(return-from main)))
    
    (within-main-loop
      (let* ((builder (gtk-builder-new-from-file
		       (namestring (asdf:system-relative-pathname :bleachers "resources/interface.glade"))))
	     (window (gtk-builder-get-object builder "wnMain"))
	     (listbox (gtk-builder-get-object builder "lstGames"))
	     (away-team (gtk-builder-get-object builder "lblAwayTeam"))
	     (away-score (gtk-builder-get-object builder "lblAwayScore"))
	     (home-team (gtk-builder-get-object builder "lblHomeTeam"))
	     (home-score (gtk-builder-get-object builder "lblHomeScore"))
	     (update-label (gtk-builder-get-object builder "lblUpdate"))
	     (ball-label (gtk-builder-get-object builder "lblBalls"))
	     (out-label (gtk-builder-get-object builder "lblOuts"))
	     (strike-label (gtk-builder-get-object builder "lblStrikes"))
	     (inning-label (gtk-builder-get-object builder "lblInning")))

	(g-signal-connect window "destroy" #'quit-app)

	(g-signal-connect listbox "row-selected" #'process-selected)

	;; sets up a thread to rotate the update messages
	(let ((message-counter 0)
	      (update-messages (get-ticker-messages)))
	  (after-every (10 :seconds :async t :run-immediately t)
	    ;; update gui label
	    (setf (gtk-label-text update-label)
		  (aref update-messages (mod (incf message-counter)
					     (length update-messages))))
	    ;; update array of messages
	    (when (zerop (mod message-counter 50))
	      (setf update-messages (get-ticker-messages)))))


	;; make a thread to get the updates from the websocket
	(bt:make-thread
	 (lambda ()
	   (loop for update = (safe-queue:mailbox-receive-message *update-mailbox*
								  :timeout 10)
		 when update
		   do
		      (loop for game in update
			    do
			       (setf (gethash (blaseball:away-team game) *scores*)
				     game
				     
				     (gethash (blaseball:home-team game) *scores*)
				     game)))))

	(after-every (.5 :seconds :async t)
	  (when *current-game-id*
	    (let* ((game (get-team-score *current-game-id*))
		   (away (team-info-from-game game :away t))
		   (home (team-info-from-game game)))
	      (setf (gtk-label-text away-team) (convert-id-to-name (getf away :id))
		    (gtk-label-text away-score) (stringify (getf away :score))
		    
		    (gtk-label-text home-team) (convert-id-to-name (getf home :id))
		    (gtk-label-text home-score) (stringify (getf home :score))

		    (gtk-label-text inning-label)
		    (generate-inning-label (blaseball:inning game)
					   (blaseball:top-of-inning game))

		    (gtk-window-title window)
		    (generate-window-title game)))))
      
	;; populate the listbox with team names for now
	;; eventually moving to X vs Y
	(loop for team in *team-names*
	      do (gtk-list-box-prepend listbox
				       (gtk-label-new (cdr team))))
	(gtk-widget-show-all window)))
    (join-gtk-main)
    (wsd:remove-all-listeners ws)
    (unless (eq (wsd:ready-state ws) :closed)
      (wsd:close-connection ws))))

(defun process-updates (update)
  (let* ((yason:*parse-json-arrays-as-vectors* t)
	 (upd (yason:parse (ppcre:regex-replace "\\d+(\\[|\\{)" update "\\1")))
	 (schedule (and (vectorp upd)
			(find "gameDataUpdate" upd :test #'equal)
		        (map 'list #'json-to-game (gethash "schedule" (aref upd 1))))))
    (when schedule
      (safe-queue:mailbox-send-message *update-mailbox* schedule))))

(defun process-selected (w row)
  (declare (ignore w))
  (when row 
    (let* ((team-name (first (str:split " vs " (gtk-label-text
						(gtk-bin-get-child
						 (change-class row 'gtk-bin))))))
	   (game (gethash (convert-name-to-id team-name) *scores*)))
      (when game
	(setf *current-game-id* (blaseball:id game))))))
