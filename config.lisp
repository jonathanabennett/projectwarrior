;;; Config.lisp Is the default configuration for project-warrior.
;;; All configuration is done through the setting of global variables.
;;; This configuration file is read first, then it checks for the existence of
;;; a user supplied file and calls that. This way, the default values are
;;; guaranteed to be loaded and the user values will overwrite the defaults.
;;;

(in-package #:projectwarrior)

;; General
(defvar *data-folder* (uiop:native-namestring "~/.cl-gtd/") "Folder where data is stored.")
(defvar *task-bin* "" "Location of taskwarrior binary.")
