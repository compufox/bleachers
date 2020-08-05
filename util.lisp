;;;; util.lisp

(in-package :bleachers)

(declaim (inline get-ticker-messages convert-id-to-name
		 generate-game-label convert-name-to-id
		 json-to-game quit-app generate-inning-label
		 generate-window-title stringify))

(defun parse-time (amount duration)
  "parses AMOUNT of DURATION into seconds"
  (* amount (cond
	     ((or (eq duration :seconds)
		  (eq duration :second))
	      1)
	     ((or (eq duration :minutes)
		  (eq duration :minute))
	      60)
	     ((or (eq duration :hours)
		  (eq duration :hour))
	      3600)
	     ((or (eq duration :days)
		  (eq duration :day))
	      86400)
	     ((or (eq duration :weeks)
		  (eq duration :week))
	      604800)
	     (t (error "unknown duration")))))

(defmacro after-every ((amount duration &key async run-immediately) &body body)
  "runs BODY after every AMOUNT of DURATION

if ASYNC is non-nil, runs asynchronously
if RUN-IMMEDIATELY is non-nil, runs BODY once before waiting for next invocation"
  (let ((code `(loop ,@(when run-immediately `(initially ,@body))
		     do (sleep (parse-time ,amount ,duration))
		     ,@body)))
    (if async
	`(bt:make-thread
	  (lambda () ,code))
	code)))

(defun get-ticker-messages ()
  (map 'vector #'blaseball:msg (blaseball:get-global-events)))

(defun convert-id-to-name (id)
  (alexandria:assoc-value *team-names* id :test 'equal))

(defun convert-name-to-id (name)
  (car (find name *team-names* :test #'equal :key #'cdr)))

(defun generate-game-label (game)
  (format nil "~a vs ~a"
	  (convert-id-to-name (blaseball:away-team game))
	  (convert-id-to-name (blaseball:home-team game))))

(defun json-to-game (val)
  (json-mop:json-to-clos val 'blaseball::game))

(defun quit-app (w)
  (declare (ignore w))
  (leave-gtk-main))

(defun get-team-score (id &key away)
  (loop for k being the hash-key of *scores*
	for v being the hash-value of *scores*

	when (equal (blaseball:id v) id)
	  return v))

(defun team-info-from-game (game &key away)
  (list :id (if away
		(blaseball:away-team game)
		(blaseball:home-team game))
	:score (if away
		   (blaseball:away-score game)
		   (blaseball:home-score game))))

(defun generate-inning-label (number top)
  (format nil "~a of the ~a~a"
	  (if top "Top" "Bottom")
	  (1+ number)
	  (cond
	    ((= 0 number) "st")
	    ((= 1 number) "nd")
	    ((= 2 number) "rd")
	    (t "th"))))

(defun generate-window-title (game)
  (format nil "Bleachers - SEASON ~a DAY ~a"
	  (1+ (blaseball:season game))
	  (1+ (blaseball:day game))))

(defun stringify (thing)
  (format nil "~A" thing))
