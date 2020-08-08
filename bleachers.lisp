;;;; bleachers.lisp

(in-package #:bleachers)

;; make this KEY: teamid  VALUE: game object
(defvar *scores* (make-hash-table :test 'equal :size 14))
(defvar *team-names* nil)
(defvar *current-game-id* nil)
(defvar *update-mailbox* (safe-queue:make-mailbox :name "game updates"))
(defvar *resources*
  #+deploy (deploy:data-directory)
  #-deploy (asdf:system-source-directory :bleachers))
(defvar *threads* nil)

(defun main ()
  (within-main-loop
    (let* ((ws (blaseball-live:blaseball-websocket #'process-updates))

	   ;; gui stuff
	   (builder (gtk-builder-new-from-file
		     (namestring (merge-pathnames "resources/interface.glade"
						  *resources*))))
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
	   (inning-label (gtk-builder-get-object builder "lblInning"))

	   ;; error popup
	   (error-window (gtk-builder-get-object builder "wnErr"))
	   (error-label (gtk-builder-get-object builder "lblErr"))
	   (error-button (gtk-builder-get-object builder "btnErr")))
      
      ;; set up signals
      (g-signal-connect window "destroy" #'quit-app)
      (g-signal-connect listbox "row-selected" #'process-selected)
      (g-signal-connect error-button "clicked" #'quit-app)
      
      ;; sets up a thread to rotate the update messages
      (push
       (let ((message-counter 0)
	     (update-messages (get-ticker-messages)))
	 (after-every (10 :seconds :async t :run-immediately t)
	   ;; in case we've errored out and cant fetch ticker messages
	   (if update-messages
	       (progn
		 ;; update gui label
		 (setf (gtk-label-text update-label)
		       (aref update-messages (mod (incf message-counter)
						  (length update-messages))))
		 ;; update array of messages
		 (when (zerop (mod message-counter 50))
		   (format t "updating ticker~%")
		   (force-output)
		   (setf update-messages (get-ticker-messages))))
	       (setf (gtk-label-text update-label) ""))))
       *threads*)
      
      
      ;; make a thread to get the updates from the websocket
      (push
       (bt:make-thread
	(lambda ()
	  (loop for update = (safe-queue:mailbox-receive-message
			      *update-mailbox*
			      :timeout 10)
		when update
		  do
		     (loop for game in update
			   do
			      (setf (gethash (blaseball:away-team game)
					     *scores*)
				    game
				    
				    (gethash (blaseball:home-team game)
					     *scores*)
				    game)))))
       *threads*)
      
      (push
       (after-every (.1 :seconds :async t)
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
       *threads*)
      
      (handler-case
	  (progn
	    ;; get the team names/ids
	    (loop initially (setf *team-names* nil)
		  for team in (blaseball:get-all-teams)
		  do
		     (setf (gethash (blaseball:id team) *scores*) nil)
		     (push (cons (blaseball:id team)
				 (blaseball:full-name team))
			   *team-names*))
	    
	    ;; populate the listbox with team names for now
	    ;; eventually moving to X vs Y
	    (loop for team in *team-names*
		  do (gtk-list-box-prepend listbox
					   (gtk-label-new (cdr team))))
	    (gtk-widget-show-all window)
	    
	    (wsd:start-connection ws))

	;; if we've errored out here that means that we've
	;;  not been able to ping the site, or something.
	(error (e)
	  (setf (gtk-label-text error-label) (format nil "~A" e))
	  (wsd:remove-all-listeners ws)
	  (wsd:close-connection ws)
	  (gtk-widget-show-all error-window)))))
  (join-gtk-main)
  (mapcar #'bt:destroy-thread *threads*))
    
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
