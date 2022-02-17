;;;; projectwarrior.lisp

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


(in-package #:projectwarrior)

(defun add (project-data)
  "This adds a new project to the active.json project list after parsing the string into appropriate variables."
  (let ((user-description '())
        (user-slug "")
        (user-aof "")
        (user-tags '())
        (user-inherit-tags '()))
    ;; If the args include a string in the format "area:<area-of-focus>", make it the value of `user-aof'.
    ;; If the args include a string in the format "slug:<custom-slug>, make it the value of `user-slug'.
    ;; Collect all the args that begin with a single "+" and add them to the `user-tags' list
    ;; Collect all the args that begin with "++" and add them to `user-inherit-tags' list.
    ;; Any args not collected by the above filters are collected into the `user-description' variable.
    (dolist (str project-data)
      (cond
        ((search "area:" str) (setq user-aof (subseq str 5)))
        ((search "++" str) (push (subseq str 2) user-inherit-tags))
        ((search "+" str) (push (subseq str 1) user-tags))
        ((search "slug:" str) (setq user-slug (subseq str 5)))
        (t (setq user-description (add-to-end user-description str)))))
    (add-to-end *active-projects-list* (make-project :description (format nil "~{~a~^ ~}" user-description)
                                                     :slug user-slug :tags user-tags
                                                     :inherit-tags user-inherit-tags
                                                     :area-of-focus user-aof
                                                     :target-list *active-projects-list*))
    (save-projects *active-projects-list* *active-projects-filepath*)))

(defun view-projects (&optional (source :active))
  "This will filter projects based on a call to `where' before passing that
filtered list on to `list-projects' for display."
  (cond
    ((eq source :completed) (list-projects *completed-projects-list*))
    ((eq source :deleted)   (list-projects *deleted-projects-list*))
    (t                      (list-projects *active-projects-list*))))

(defun complete-project (project-num)
  "Find project `project-num' in the `*active-projects-list*', remove it, and append it to the
`*completed-projects-list*'. Then save both project lists to file."
  (let ((project (nth (- project-num 1) *active-projects-list*)))
    (setq *active-projects-list* (remove project *active-projects-list*))
    (setq *completed-projects-list* (add-to-end *completed-projects-list* project)))
  (save-projects *active-projects-list* *active-projects-filepath*)
  (save-projects *completed-projects-list* *completed-projects-filepath*))

(defun delete-project (project-num)
  "Find project `project-num' in the `*active-projects-list*', remove it, and append it to the
`*deleted-projects-list*'. Then save both project lists to file."
  (let ((project (nth (- project-num 1) *active-projects-list*)))
    (setq *active-projects-list* (remove project *active-projects-list*))
    (setq *deleted-projects-list (add-to-end *completed-projects-list* project)))
  (save-projects *active-projects-list* *active-projects-filepath*)
  (save-projeects *deleted-projects-list* *deleted-projects-filepath*))

;; TODO Rewrite this to use the new `PROJECT' class
;; Look up tasks using the `slug' field on the `PROJECT' object.
(defun projects-review ()
  "This guides a user through a review of the projects listed in their *projects-filepath* file."
  (format t "Welcome to your project review. Hold on while sync your projects.")
  (sync-projects-list *projects-filepath*)
  (let ((active-projects ())
        (review-list (get-list-from-file *projects-filepath*)))
    (dolist (project review-list)
      (progn
        (format t "~%~%~%~%Project: ~A~%" project)
        (list-tasks project)
        (let ((response (ask-until-valid '("a" "b" "d") "Is your project [a]ctive, [c]ompleted, or [d]eleted? ")))
          (cond
            ((equal response "a") (push project active-projects))
            ((equal response "c") ())
            ((equal response "d") ())
            (t (push project active-projects))))))
    (with-open-file (f *projects-filepath* :direction :output :if-exists :supersede)
      (format f "~{~A~%~}" active-projects))))

(defun help ()
  "Print out the help."
  (format t "USAGE: projectwarrior <subcommand> <project>")
  (format t "add <project>: Add <project> as a project.")
  (format t "Project should be in the same format as a taskwarrior project.")
  (format t "help: Display this message.")
  (format t "review: Conduct a guided weekly review.")
  (format t "projects: Review your projects."))

(defun main (&rest argv)
  "This is the script entry point."
  (declare (ignore argv))
  (load-projects *active-projects-filepath* :active)
  (load-projects *completed-projects-filepath* :completed)
  (load-projects *deleted-projects-filepath* :deleted)
  ;; Add code here to read in ~/.projectwarrior/config.lisp
  ;; If the file doesn't exist, create a default one from the template.
  ;; Primary initial contents will be the review options
  ;; Which will get registered as keyword options after review.
  (let ((args (uiop/image:command-line-arguments)))
     (cond
       ((equal (car args) "help") (help))
       ((equal (car args) "add") (add (cdr args)))
       ((equal (car args) "done") (complete-project (cdr args)))
       ((equal (car args) "delete") (delete-project (cdr args)))
       ((equal (car args) "review") (projects-review))
       ((equal (car args) "weekly") (weekly-review))
       (t (view-projects)))))
