;;;; bleachers.lisp

(in-package #:bleachers)

;; make this ((team-id (:game game-id :score team-score)))
(defvar *scores* (make-hash-table :test 'equal :size 14))
(defvar *team-names* nil)
(defvar *current-game-id* nil)
(defvar *update-mailbox* (safe-queue:make-mailbox :name "game updates"))

(defun main ()
  
  ;; get a mapping for the team names
  (loop initially (setf *team-names* nil)
    for team in (blaseball:get-all-teams)
	do
	   (setf (gethash (blaseball:id team) *scores*) nil)
	   (push (cons (blaseball:id team)
		       (blaseball:full-name team))
		 *team-names*))
  
  (let ((ws (blaseball-live:blaseball-websocket #'process-updates)))
    ;(wsd:start-connection ws)

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
	     (strike-label (gtk-builder-get-object builder "lblStriks"))
	     (inning-label (gtk-builder-get-object builder "lblInning")))

	(g-signal-connect window "destroy"
			  (lambda (w)
			    (declare (ignore w))
			    (leave-gtk-main)))

	(g-signal-connect listbox "row-selected"
			  (lambda (w row)
			    (declare (ignore w))
			    (when row 
			      (format t "~A"
				      (gtk-label-text (gtk-bin-get-child
						       (change-class row 'gtk-bin))))
			      (force-output))))

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
		      (loop for game across update
			    do
			       (setf (gethash (blaseball:away-team game) *scores*)
				     (list :game (blaseball:id game)
					   :score (blaseball:away-score game))
				     
				     (gethash (blaseball:home-team game) *scores*)
				     (list :game (blaseball:id game)
					   :score (blaseball:home-score game)))))))
      
	;; populate the listbox with team names for now
	;; eventually moving to X vs Y
	(loop for team in *team-names*
	      do (gtk-list-box-prepend listbox
				       (gtk-label-new (cdr team))))
	(gtk-widget-show-all window)
;;      (loop while (eq (wsd:ready-state ws) :open)
;;	    do (sleep 1)
;;	       
;;	    when *update-gui*
;;	      do (break)
;;	    )
	))
    (wsd:remove-all-listeners ws)
    (unless (eq (wsd:ready-state ws) :closed)
      (wsd:close-connection ws))))

(defun process-updates (update)
  (let* ((upd (yason:parse update))
	 (schedule (and (listp upd)
			(member "gameDataUpdate" upd :test #'equal)
			(loop for game across (gethash "schedule" (cadr upd))
			      collect (json-mop:json-to-clos game 'blaseball::game)))))
    (when schedule 
      (safe-queue:mailbox-send-message *update-mailbox* schedule))))
