;;;; gtd-review.lisp

;;; This program walked a user through a review of their projects.
;;; Projects are pulled from the task management program `taskwarrior`.
;;; Each project is presented, one by one, and the user is asked to classify
;;; the project as either (a)ctive, (c)ompleted, or (d)eleted.
;;;
;;; Active projects are entered into the projects list for the next review.
;;; Completed and deleted projects are removed from the projects list.
;;;
;;; Future versions will do something different with completed and deleted
;;; projects, but for now the two behave identically.
;;;
;;; Greet user
;;; Sync projects list
;;; Display project
;;; Sort project (keep or delete)
;;; When finished, say goodbye.


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
