;;;; util.lisp

(in-package :bleachers)

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
