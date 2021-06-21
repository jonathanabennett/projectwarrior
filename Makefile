##
# GTD Review
#
# Dependencies:
#  * SBCL
#  * Quicklisp
#
# Author: Jonathan A. Bennett <doulos05@gmail.com>
# @file
# @version 0.0.1


LISP ?= sbcl

hook:
	mkdir -p ~/.cl-gtd
	touch ~/.cl-gtd/projects.txt
	$(LISP) --load gtd-review.asd \
		--eval '(ql:quickload :gtd-review/tw-hook)' \
		--eval '(asdf:make :gtd-review/tw-hook)' \
		--eval '(quit)'
	mv ./on-exit-projects-list ~/.task/hooks/on-exit-projects-list


# end
