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

(in-package #:projectwarrior)

;; Root folder for storing data
(defvar *data-folder* (uiop:native-namestring "~/.projects/"))
;; If you want to use taskwarrior, change this from `nil' to `t'.
(defvar *enable-taskwarrior-integration* nil)
