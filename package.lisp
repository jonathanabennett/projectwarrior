;;;; package.lisp

(defpackage :projectwarrior
  (:use #:cl)
  (:export #:main
           #:merge-lists
           #:ask
           #:get-new-projects-list
           #:sync-projects-list
           #:*projects-filepath*
           #:get-list-from-file))

(defvar *active-projects-list* nil)
(defvar *completed-projects-list* nil)
(defvar *deleted-projects-list* nil)
