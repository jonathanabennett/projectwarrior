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

(defun main ()
  "This is the script entry point."
  (format t "Welcome to your project review. Hold on while sync your projects.")
  (sync-projects-list *projects-filepath*)
  (let ((active-projects ())
        (review-list (get-list-from-file *projects-filepath*)))
    (dolist (project review-list)
      (progn
        (format t "Project: ~A~%" project)
        (uiop:run-program (format nil "task project:~A and '(status:PENDING or status:WAITING)' all rc.hooks=off" project) :ignore-error-status t :output *standard-output*)
        (let ((response (ask "Is your project [a]ctive, [c]ompleted, or [d]eleted? ")))
          (if (equal response "a")
              (push project active-projects)))
        ))
    (with-open-file (f *projects-filepath* :direction :output :if-exists :supersede)
      (format f "~{~A~%~}" active-projects))))
