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

(defvar valid-commands '("add" "done" "delete" "del" "modify" "mod" "view" "review"))

(defun add-from-string (project-data)
  (add (cl-utilities:split-sequence " " project-data :test #'string=)))

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
    (setf *active-projects-list* (add-to-end *active-projects-list* (make-project :description (format nil "~{~a~^ ~}" user-description)
                                                     :slug user-slug :tags user-tags
                                                     :inherit-tags user-inherit-tags
                                                     :area-of-focus user-aof)))))


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
  "This guides a user through a review of the projects listed in `*active-projects-list*' file."
  (format t "Welcome to your project review.")
  (let ((active-projects '())
        (completed-projects '())
        (deleted-projects '()))
    (loop for project in *active-projects-list*
          for i from 1
          do (format t "~%~%~%~%Project: ~A~%" (description project))
             (list-tasks (slug project))
             (let ((response (ask-until-valid '("a" "c" "d") "Is this project [a]ctive, [c]ompleted, or [d]eleted? ")))
               (cond
                 ((equal response "c") (push project completed-projects))
                 ((equal response "d") (push project deleted-projects))
                 (t (push project active-projects)))))
    (complete-projects completed-projects)
    (delete-projects deleted-projects)))

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
  (setf *active-projects-list* (load-projects *active-projects-filepath* ))
  (setf *completed-projects-list* (load-projects *completed-projects-filepath* ))
  (setf *deleted-projects-list* (load-projects *deleted-projects-filepath* ))
  ;; Add code here to read in ~/.projectwarrior/config.lisp
  ;; If the file doesn't exist, create a default one from the template.
  ;; Primary initial contents will be the review options
  ;; Which will get registered as keyword options after review.
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
  (let ((filter '())
        (command)
        (modifications '()))
    (dolist (term user-input)
      (if command
          (setq modifications (add-to-end modifications term))
          (if (member term valid-commands :test #'string=)
              (setq command term)
              (setq filter (add-to-end filter term)))))
    (cond
          ((string= command "add") (add modifications))
          ((string= command "view") (list-projects (filter-projects filter)))
          ((string= command "mod") (modify-projects filter modifications))
          ((string= command "done") (complete-projects (filter-projects filter)))
          ((string= command "delete") (delete-projects (filter-projects filter)))
          ((string= command "review") (weekly-review))
          (t (list-projects (filter-projects filter))))))

;; TODO Rewrite this to create filters rather than apply filters. Then use `search-projects'
;; and `update-projects' to do the actual filtering
(defun filter-projects (filter)
  "Filters the list of projects based on the filter supplied"
  ;; Check which lists to pull from and assemble them into a master list.
  ;; Th `search-projects' over the list.
  (let ((aof)
        (tags '())
        (inherit-tags '())
        (slug)
        (id)
        (description)
        (source "active"))
    (dolist (term filter)
      (cond
        ((search "area:" term) (setq aof (subseq term 5)))
        ((search "++" term) (push (subseq term 2) inherit-tags))
        ((search "+" term) (push (subseq term 1) tags))
        ((search "slug:" term) (setq slug (subseq term 5)))
        ((search "status:" term) (setq source (subseq term 7)))
        (t (setq description (add-to-end description term)))))
    (cond
      ((string= source "active") (search-projects
                                  (where :aof aof :tags tags :inherit-tags inherit-tags :slug slug
                                         :description (format nil "~{~a~^ ~}" description)) *active-projects-list*))
      ((string= source "done") (search-projects
                                (where :aof aof :tags tags :inherit-tags inherit-tags :slug slug
                                       :description (format nil "~{~a~^ ~}" description)) *completed-projects-list*))
      ((string= source "deleted") (search-projects
                                   (where :aof aof :tags tags :inherit-tags inherit-tags :slug slug
                                          :description (format nil "~{~a~^ ~}" description)) *deleted-projects-list*)))))

(defun modify-projects (filter modifications)
  (let ((aof)
        (tags '())
        (inherit-tags '())
        (slug)
        (description))
    (dolist (term filter)
      (cond
        ((search "area:" term) (setq aof (subseq term 5)))
        ((search "++" term) (push (subseq term 2) inherit-tags))
        ((search "+" term) (push (subseq term 1) tags))
        ((search "slug:" term) (setq slug (subseq term 5)))
        (t (setq description (add-to-end description term)))))
    (update-projects (where :aof aof :tags tags :inherit-tags inherit-tags :slug slug :description (format nil "~{~a~^ ~}" description)) modifications)))

(defun complete-projects (projects)
  "Complete all projects in `projects'. If `projects' is more than 3 itmes, prompt for each
completion."
  (let ((prompt (>= (length projects) 3))
        (completep t))
    (dolist (p projects)
      (if prompt
          (setf completep (yes-or-no-p "Do you want to complete project %a? " (description p))))
      (if completep
          (complete-project (id p))))))

(defun delete-projects (projects)
  "Delete all projects in `projects'. If `projects' is more than 3 itesm, prompt for each."
  (let ((prompt (>= (length projects) 3))
        (deletep t))
    (dolist (p projects)
      (if prompt
          (setf deletep (yes-or-no-p "Do you want to delete project %a ?" (description p))))
      (if deletep
          (delete-project (id p))))))
