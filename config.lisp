;;;; config.lisp
;;;
;;; Config.lisp is the default configuration for project-warrior. All configuration is done through
;;; the setting of global variables. This configuration file is read first, then it checks for the
;;; existence of a user supplied file and calls that. This way, the default values are guaranteed
;;; to be loaded and the user values will overwrite the defaults.


(in-package #:projectwarrior)

(defun check-for-configuration-file ()
  "Checks the following locations for configuration files.
1) ~/.projectrc.lisp
2) ~/.projects/projectrc.lisp
3) ~/.config/projects/projectrc.lisp

If it finds one, then it loads that file. If it doesn't find one, then it writes one with the
default configuration in ~/.projectrc.lisp"
  (let ((home-dir (uiop:native-namestring "~/.projectrc.lisp"))
        (projects-dir (uiop:native-namestring "~/.projects/projectrc.lisp"))
        (config-dir (uiop:native-namestring "~/.config/projects/projectrc.lisp")))
    (cond
      ((probe-file home-dir) (load home-dir))
      ((probe-file projects-dir) (load projects-dir))
      ((probe-file config-dir) (load configs-dir))
      (t (make-default-config)))))

(defun make-default-config ()
  "Prompt the user to see if they want to create a default config file. If they want to, write out
the default configuration options into the file at ~/.projectrc.lisp"
    (uiop:copy-file (uiop:native-namestring "./default_config.lisp") (uiop:native-namestring "~/.projectrc.lisp")))

(defvar *active-projects-filepath*)
(defvar *completed-projects-filepath*)
(defvar *deleted-projects-filepath*)
(defvar *active-projects-list* nil)
(defvar *completed-projects-list* nil)
(defvar *deleted-projects-list* nil)

;; Load Default Values
(defvar *data-folder* (uiop:native-namestring "~/.projects/") "Folder where data is stored.")
(defvar *enable-taskwarrior-integration* nil)

;; General
(defun load-configuration ()
  (check-for-configuration-file)
  (setf *active-projects-filepath* (uiop:native-namestring
                                    (concatenate 'string *data-folder* "active.json")))
  (setf *completed-projects-filepath* (uiop:native-namestring
                                       (concatenate 'string *data-folder* "completed.json")))
  (setf *deleted-projects-filepath* (uiop:native-namestring
                                     (concatenate 'string *data-folder* "deleted.json"))))
