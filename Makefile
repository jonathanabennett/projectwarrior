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
	mkdir -p ~/.projects
	touch ~/.projects/projects.txt
	touch ~/.projects/active.json
	touch ~/.projects/completed.json
	touch ~/.projects/deleted.json
	$(LISP) --eval "(require 'asdf)" \
		--load projectwarrior.asd \
		--eval '(ql:quickload :projectwarrior/tw-hook)' \
		--eval '(asdf:make :projectwarrior/tw-hook)' \
		--eval '(quit)'
	mv ./on-exit-projects-list ~/.task/hooks/on-exit-projects-list

build:
	mkdir -p ~/.projects
	touch ~/.projects/projects.txt
	touch ~/.projects/active.json
	touch ~/.projects/completed.json
	touch ~/.projects/deleted.json
	$(LISP) --eval "(require 'asdf)" \
		--load projectwarrior.asd \
		--eval '(ql:quickload :projectwarrior)' \
		--eval '(asdf:make :projectwarrior)' \
		--eval '(quit)'
		echo "Please move the project executable somewhere on your PATH"

install:
	mv projectwarrior ~/.bin/project
	cp -f default_config.lisp ~/.projectrc
# end
