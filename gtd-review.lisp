;;;; gtd-review.lisp

;;; Plan for this script
;;; This will be an on-exit hook for taskwarrior.
;;; Its purpose is to update my current list of projects.
;;; The first time it is run, it stores the list in a file.
;;; On subsequent runs, it adds any new projects to the file.
;;; The weekly review package will be the package where this data gets used.
;;; All data for this suite of tools lives by default in ~/.cl-gtd
;;; All data for this script is stored in ~/.cl-gtd/projects.txt


(in-package #:gtd-review)

(defparameter *projects-filepath* (uiop:native-namestring "~/.cl-gtd/projects.txt"))

(defun get-new-projects-list ()
  "Gets a list of the current projects from taskwarrior. rc.hooks=off is needed to prevent infinite loops."
 (inferior-shell:run/lines "task _projects rc.hooks=off"))

(defun get-current-projects-list (file)
  "Get the last list of projects for *projects-filepath*."
  (uiop:read-file-lines file))

(defun merge-projects-lists (curr new)
  "Merge two lists of strings."
  (union curr new :test `equal))

(defun main ()
  "This is the script entry point."
  (let* ((current-projects (get-current-projects-list *projects-filepath*))
         (new-projects (get-new-projects-list))
         (updated-projects (merge-projects-lists current-projects new-projects)))
    (with-open-file (file *projects-filepath* :direction :output :if-exists :supersede)
      (format file "~{~A~%~}" updated-projects))
    ()))
