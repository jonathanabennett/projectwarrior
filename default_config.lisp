;;;; projectrc.lisp
;;;
;;; This is the configuration file for Projectwarrior.
;;; Here you can customize all the settings for projectwarrior.
;;; You will also be able to construct custom reports.
;;; This is written in Common Lisp.
;;; The basics of editing Common Lisp are not hard, though it does look
;;; different from most other languages.
;;; Every command you write should be wrapped in parentheses.
;;; (defvar my-variable "Foo!")
;;; Use two semi-colons to mark a comment.
;;; ;; (defvar not-real "This line will never run")
;;; The important commands you need to set the needed variables for
;;; projectwarrior to run are described below:
;;;
;;; defvar: Define a variable.
;;; (defvar variable-name variable-value "Documentation String")
;;; Most variables you define will look like *variable-name*.
;;; The "earmuffs" from the two asterisks are a Lisp convention to mark out
;;; global variables.
;;;
;;; uiop:native-namestring: Turn a string into a valid filepath.
;;; (uiop:native-namestring "path/to/file")
;;; This handles different home directories, / vs \ for *nix based vs Windows,
;;; and all the other oddities of different operating systems handling filepaths.
;;; You can also just type the file path in directly
;;; as a string "~/.projectrc.lisp", but using uiop:native-namestring lets you
;;; move your config across platforms more easily.
;;;

;;; Do not remove this line or all your configuration variables will not work because they will be in the wrong namespace.
(in-package #:projectwarrior)

;; Root folder for storing data
(defvar *data-folder* (uiop:native-namestring "~/.projects/"))
;; If you want to use taskwarrior, change this from `nil' to `t'.
(defvar *enable-taskwarrior-integration* nil)

;;; Reports
;; Below is the format for defining a custom report.
;; Reports are searched using the `name' field.
;; The `col-labels' field is a list of strings which must look like '("list" "of" "strings")
;; The `col-functions' field is a list of Common Lisp functions or lambda which should take a single
;; argument: a project. As above, it must begin with a ' character.
;; Finally `col-align' should be a list of `:left', `:center', or `:right' which is equal length to the other
;; two lists and defines whether to left, center, or right justify the fields.

;; This is the default report. It is hard-coded into the report formatter but can also be accessed
;; using the command "project <filter> view".
(register-report :name "view"
                 :col-labels '("#" "Slug" "Description" "Area of Focus" "tags")
                 :col-functions '(id slug description area-of-focus tags)
                 :col-align (loop for i from 1 to 5 collect :left))

;; Column functions are arbitrary Common Lisp functions. Here is one which returns the number
;; of tasks in taskwarrior which belong to a project (as defined by that project's `slug').
(defun task-count (project)
  "Returns the number of tasks having a `project' which matches `slug'."
  (uiop:run-program (format nil "task count project.is=~A" (slug project))))

;; And here is a report which uses that function
(register-report :name "count"
                 :col-labels '("#" "Slug" "Description" "Area of Focus" "Task Count")
                 :col-functions '(id slug description area-of-focus task-count)
                 :col-align (loop for i from 1 to 5 collect :left))
