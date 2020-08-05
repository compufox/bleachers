;;;; bleachers.lisp

(in-package #:bleachers)

;; make this ((team-id (:game game-id :score team-score)))
(defvar *scores* (make-hash-table :test 'equal :size 14))
(defvar *team-names* nil)
(defvar *selected-team* "")
(defvar *update-gui* nil)

(defun main ()
  (loop for team in (blaseball:get-all-teams)
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
	     (home-score (gtk-builder-get-object builder "lblHomeScore")))

	(g-signal-connect window "destroy"
			  (lambda (w)
			    (declare (ignore w))
			    (leave-gtk-main)))

	(g-signal-connect listbox "row-selected"
			  (lambda (w row)
			    (when row 
			      (format t "~A"
				      (gtk-label-text (gtk-bin-get-child
						       (change-class row 'gtk-bin)))))))
	
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
      
;;      (wsd:remove-all-listeners ws)
;;      (unless (eq (wsd:ready-state ws) :closed)
;;	(wsd:close-connection ws))
      
	))))

(defun convert-id-to-name (id)
  (alexandria:assoc-value *team-names* id :test 'equal))

(defun process-updates (update)
  (let* ((upd (yason:parse update))
	 (schedule (and (listp upd)
		      (member "gameDataUpdate" upd :test #'equal)
		      (loop for game across (gethash "schedule" (cadr upd))
			    collect (json-mop:json-to-clos game 'blaseball::game)))))
    (when schedule 
      (loop for game across schedule
	    do
	       (setf (gethash (blaseball:away-team game) *scores*)
		     (list :game (blaseball:id game)
			   :score (blaseball:away-score game))

		     (gethash (blaseball:home-team game) *scores*)
		     (list :game (blaseball:id game)
			   :score (blaseball:home-score game))

		     *update-gui* t)))))
