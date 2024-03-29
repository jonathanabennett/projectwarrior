;;;; projectwarrior.lisp

;;; This program allows a user to track projects in the same way that taskwarrior allows you to
;;; track tasks.


(in-package #:projectwarrior)

(defvar *valid-commands* '("add" "done" "delete" "del" "modify" "mod" "review" "help" "tasks" "new" "open") "The list of valid commands.")

(defun add-from-string (project-data)
  "This is used when adding from a string rather than a list."
  (add (cl-utilities:split-sequence " " project-data :test #'string=))
  (save-projects *active-projects-list* *active-projects-filepath*))

(defun add (project-data)
  "This adds a new project to the active.json project list after parsing the string into appropriate variables."
  (add-to-end *active-projects-list* (project-from-list project-data)))

(defun complete-project (project-uuid)
  "Find project `project-uuid' in the `*active-projects-list*', remove it, and append it to the
`*completed-projects-list*'. Then save both project lists to file."
  (let ((project (find project-uuid *active-projects-list* :key #'uuid :test #'uuid:uuid=)))
    (setq *active-projects-list* (remove project *active-projects-list*))
    (add-to-end *completed-projects-list* project))
  (save-projects *active-projects-list* *active-projects-filepath*)
  (save-projects *completed-projects-list* *completed-projects-filepath*))

(defun delete-project (project-uuid)
  "Find project `project-uuid' in the `*active-projects-list*', remove it, and append it to the
`*deleted-projects-list*'. Then save both project lists to file."
  (let ((project (find project-uuid *active-projects-list* :key #'uuid :test #'uuid:uuid=)))
    (setq *active-projects-list* (remove project *active-projects-list*))
    (add-to-end *completed-projects-list* project))
  (save-projects *active-projects-list* *active-projects-filepath*)
  (save-projects *deleted-projects-list* *deleted-projects-filepath*))

(defun projects-review ()
  "This guides a user through a review of the projects listed in `*active-projects-list*' file."
  (format t "Welcome to your project review.")
  (let ((active-projects '())
        (completed-projects '())
        (deleted-projects '()))
    (loop for project in *active-projects-list*
          for i from 1
          do (format t "~%~%~%~%Project: ~A~%" (description project))
             (uiop:run-program (format nil "task project.is:~A and '(status:PENDING or status:WAITING)' rc.hooks=off" (slug project)) :ignore-error-status t :output t)
             (let ((response (ask-until-valid '("a" "c" "d") "Is this project [a]ctive, [c]ompleted, or [d]eleted? ")))
               (cond
                 ((equal response "c") (complete-project (uuid project)))
                 ((equal response "d") (delete-project (uuid project)))
                 (t (push project active-projects)
                    (add-until-enter (format nil "project:~a ~{+~a ~} " (slug project) (inherit-tags project)))))))))

(defun help ()
  "Print out the help."
  (write-string "USAGE: project <filter> <command> <information>")
  (write-string "add <information>: Add <information> as a project.")
  (write-string "<filter> mod <information>: Modify the projects selected by <filter> with <information>.")
  (write-string "<filter> view: Display projects matching <filter>.")
  (write-string "<filter> done: Mark projects matching <filter> complete.")
  (write-string "<filter> delete: Delete projects matching <filter>.")
  (write-string "review <projects|weekly|professional|personal: Conduct a guided review of the selected type.")
  (write-string "help: Display this message."))

(defun main (&rest argv)
  "This is the script entry point."
  (declare (ignore argv))
  ;; Add code here to read in ~/.projectwarrior/config.lisp
  ;; If the file doesn't exist, create a default one from the template.
  ;; Primary initial contents will be the review options
  ;; Which will get registered as keyword options after review.
  (load-configuration)

  (setf *active-projects-list* (load-projects *active-projects-filepath*))
  (setf *completed-projects-list* (load-projects *completed-projects-filepath*))
  (setf *deleted-projects-list* (load-projects *deleted-projects-filepath*))

  (setf *valid-commands* (union *valid-commands* (mapcar #'report-name *reports-list*) :test #'string=))
  (command-dispatcher (uiop/image:command-line-arguments))

  (save-projects *active-projects-list* *active-projects-filepath*)
  (save-projects *completed-projects-list* *completed-projects-filepath*)
  (save-projects *deleted-projects-list* *deleted-projects-filepath*)
  )

(defun command-dispatcher (user-input)
  "After `main' sets up the operational space, the command line args are passed on to this function
which will process them to determine what command to run and what arguments to pass to that command.
This way, the user can say things like 'project 4 done' instead of 'project done 4' so I can align
more closely to the taskwarrior interface style. The basic interface style is
project <filter string> <add|done|delete|modify|view> <project data (optional)>
So the command dispatcher should interpret all strings found before the first instance of a
command keyword as a filter and then filter the appropriate lists. It should then interpret the
command keyword as a funciton to call and pass anything after the keyword as arguments to apply to
the project(s) being modified."
  (let (filter command modifications)
    (dolist (term user-input)
      (if command
          (add-to-end modifications term)
          (if (member term *valid-commands* :test #'string=)
              (setf command term)
              (add-to-end filter term))))
    (cond
          ((string= command "add") (add modifications))
          ((string= command "mod") (update-projects (filter-projects filter) modifications))
          ((string= command "done") (complete-projects (filter-projects filter)))
          ((string= command "delete") (delete-projects (filter-projects filter)))
          ((string= command "new") (new (filter-projects filter) modifications))
          ((string= command "tasks") (tasks-reporter (filter-projects filter)))
          ((string= command "review") (review-dispatcher modifications))
          ((string= command "open") (open-supporting-documents (filter-projects filter)))
          ((string= command "view") (format-table (filter-projects filter) t))
          ((member command *reports-list* :test #'string= :key #'(lambda (x) (report-name x)))
           (format-table (filter-projects filter) t
                         :report-format (car (member command *reports-list* :test #'string=
                                                    :key #'(lambda (x) (report-name x))))))
          ((string= command "help") (help))
          (t (format-table (filter-projects filter) t)))))

(defun tasks-reporter (projects)
  (dolist (p projects)
    (tasks p)))

(defun review-dispatcher (input)
  "Select the review to conduct based on user input. In the case there is no input, give them the
`weekly-review'"
  (cond
    ((string= (car input) "projects") (projects-review))
    ((string= (car input) "weekly") (weekly-review))
    ((string= (car input) "personal") (personal-tickler))
    ((string= (car input) "professional") (professional-tickler))
    (t (weekly-review))))

(defun filter-projects (filter)
  (multiple-value-bind (filt block-tags block-inherit-tags source) (project-from-list filter)
    (cond
      ((string= source "active") (search-projects (where filt) *active-projects-list*))
      ((string= source "completed") (search-projects (where filt) *completed-projects-list*))
      ((string= source "deleted") (search-projects (where filt) *deleted-projects-list*))
      (t (search-projects (where filt) *active-projects-list*)))))

(defun complete-projects (projects)
  "Complete all projects in `projects'. If `projects' is more than 3 itmes, prompt for each
completion."
  (let ((prompt (>= (length projects) 3))
        (completep t))
    (dolist (p projects)
      (if prompt
          (setf completep (yes-or-no-p "Do you want to complete project ~a? " (description p))))
      (if completep
          (complete-project (uuid p))))))

(defun delete-projects (projects)
  "Delete all projects in `projects'. If `projects' is more than 3 itesm, prompt for each."
  (let ((prompt (>= (length projects) 3))
        (deletep t))
    (dolist (p projects)
      (if prompt
          (setf deletep (yes-or-no-p "Do you want to delete project ~a ?" (description p))))
      (if deletep
          (delete-project (uuid p))))))
