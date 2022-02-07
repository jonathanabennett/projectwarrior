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

(defun list-projects (project-list)
  (let ((table (ascii-table:make-table `("#" "Description" "Area of Focus" "Tags") :header "Projects Report")))
    (loop for project in project-list
          for i from 1
          do (ascii-table:add-row table (list i (description project) (area-of-focus project) (tags project))))
    (ascii-table:display table)))

(defun list-tasks (project)
  "Retrieve a json list of tasks and parse them into Task objects."
  (let ((tasks (yason:parse (uiop:run-program (format nil "task project.is:~A and '(status:PENDING or status:WAITING)' export rc.hooks=off" project) :ignore-error-status t :output :string)))
        (table (ascii-table:make-table `("Description" "Status" "Urgency") :header project)))
    (dolist (task tasks)
       (ascii-table:add-row table (list (gethash "description" task) (gethash "status" task) (gethash "urgency" task))))
    (ascii-table:display table)))

(defun add (project-data)
  "This adds a new project to the active.json project list after parsing the string into appropriate variables."
  ;; Step 1 parse project-string into appropriate variables
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
      (let ((input-list (uiop:split-string str)))
        (dolist (token input-list)
          (cond
            ((search "area:" token) (setq user-aof (subseq token 5)))
            ((search "++" token) (push (subseq token 2) user-inherit-tags))
            ((search "+" token) (push (subseq token 1) user-tags))
            ((search "slug:" token) (setq user-aof (subseq token 5)))
            (t (setq user-description (append user-description (list token))))))
        ;; Step 2 `add-project'
        (add-project :description (format nil "~{~a~^ ~}" user-description)
                     :slug user-slug
                     :tags user-tags
                     :inherit-tags user-inherit-tags
                     :area-of-focus user-aof))
      ;; Step 3 `save-projects'
      (save-projects *active-projects-list* *active-projects-filepath*))))

(defun view-projects (&optional (source :active))
  "Display the list of projects."
  (cond
    ((eq source :active) (list-projects *active-projects-list*))))


(defun complete-project (project-num)
  "Complete a project.")

(defun delete-project (project-num)
  "Delete a project.")

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
            (t (push project active-projects))))
        ))
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
  ;; Add code here to read in ~/.gtd-revew/config.lisp
  ;; If the file doesn't exist, create a default one from the template.
  ;; Primary initial contents will be the review options
  ;; Which will get registered as keyword options after review.
  (let ((args (uiop/image:command-line-arguments)))
     (cond
       ((equal (car args) "help") (help))
       ((equal (car args) "add") (add (car (cdr args))))
       ((equal (car args) "projects") (projects-review))
       ((equal (car args) "review") (weekly-review))
       (t (help)))))

