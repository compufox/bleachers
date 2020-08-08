LISPS = ros sbcl clisp cmucl ccl
BUILD_CMDS = --eval "(handler-case (progn (push :deploy *features*) (ql:quickload :bleachers) (asdf:make :bleachers)) (error (e) (format t \"ERROR: ~a\" e)))"

ifeq ($(OS),Windows_NT)
	LISP := $(foreach lisp,$(LISPS), \
		$(shell where $(lisp)) \
		$(if $(.SHELLSTATUS),$(strip $(lisp)),))
else
	LISP := $(foreach lisp,$(LISPS), \
		$(if $(findstring $(lisp),"$(shell which $(lisp) 2>/dev/null)"), $(strip $(lisp)),))
endif

ifeq ($(LISP),)
	$(error "No lisps found")
endif

all:
	$(LISP) $(BUILD_CMDS)

