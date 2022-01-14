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
