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

(defun list-tasks (project)
  (let ((tasks (yason:parse (uiop:run-program (format nil "task project:~A and '(status:PENDING or status:WAITING)' export rc.hooks=off" project) :ignore-error-status t :output :string))))
    (print tasks)))

(defun add (project)
  "This adds a new project to the file identified in *projects-filepath*."
  (with-open-file (f *projects-filepath* :direction :output :if-exists :append)
    (format f "~&~A~%" project)))

(defun review ()
  "This guides a user through a review of the projects listed in their *projects-filepath* file."
  (format t "Welcome to your project review. Hold on while sync your projects.")
  (sync-projects-list *projects-filepath*)
  (let ((active-projects ())
        (review-list (get-list-from-file *projects-filepath*)))
    (dolist (project review-list)
      (progn
        (format t "~%~%~%~%Project: ~A~%" project)
        (uiop:run-program (format nil "task project.is:~A and '(status:PENDING or status:WAITING)' all rc.hooks=off" project) :ignore-error-status t :output *standard-output*)
        (let ((response (ask "Is your project [a]ctive, [c]ompleted, or [d]eleted? ")))
          (if (equal response "a")
              (push project active-projects)))
        ))
    (with-open-file (f *projects-filepath* :direction :output :if-exists :supersede)
      (format f "~{~A~%~}" active-projects))))

(defun help ()
  (format t "USAGE: ./gtd-review <subcommand> <project>")
  (format t "add <project>: Add <project> as a project.")
  (format t "Project should be in the same format as a taskwarrior project.")
  (format t "help: Display this message.")
  (format t "review: Review your tasks."))

(defun main (&rest argv)
  (declare (ignore argv))
  "This is the script entry point."
  (let ((args (uiop/image:command-line-arguments)))
     (cond
       ((equal (car args) "help") (help))
       ((equal (car args) "add") (add (car (cdr args))))
       ((equal (car args) "review") (review))
       (t (review)))))
