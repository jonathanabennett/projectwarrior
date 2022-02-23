;;;; project.lisp

;;; This file defines a project as seen by the gtd-review program.

(in-package #:projectwarrior)

(defclass project ()
  ((uuid
    :initarg :uuid
    :accessor uuid
    :documentation "The UUID that identifies a project.")
   (id
    :initarg :id
    :initform -1
    :accessor id
    :documentation "The project's numeric position within its JSON file when that file was last saved.
Defaults to `-1' as a flag for the saving function to update the number.")
   (description
    :initarg :description
    :accessor description
    :documentation "A sentence description of a project.")
   (slug
    :initarg :slug
    :initform ""
    :accessor slug
    :documentation "A string formatted to be useable in taskwarrior as a project name. Should contain no whitespace.")
   (tags
    :initarg :tags
    :initform '()
    :accessor tags
    :documentation "A list of strings representing the tags for the project.")
   (area-of-focus
    :initarg :area-of-focus
    :initform ""
    :accessor area-of-focus
    :documentation "The Area of Focus this project is tied to.")
   ;; (start-date
   ;;  :initarg :start-date
   ;;  :initform nil
   ;;  :accessor start-date
   ;;  :documentation "The date this was first entered into gtd-review. Stored as a date object.")
   ;; (last-review
   ;;  :initarg :last-review
   ;;  :initform nil
   ;;  :accessor last-review
   ;;  :documentation "The datetime that this was last reviewed. Stored as a datetime object.")
   ;; (deadline
   ;;  :initarg :deadline
   ;;  :initform nil
   ;;  :accessor deadline
   ;;  :documentation "The date after which this project can be considered late.")
   (inherit-tags
    :initarg :inherit-tags
    :initform '()
    :accessor inherit-tags
    :documentation "A list of strings representing the tags which all tasks created from within gtd-review for this project should have by default."))
  (:documentation "A class representing a project within projectwarrior."))

(defun project-from-taskwarrior (name)
  "When coming from taskwarrior, projects only have a desctiption and a slug, which are identical."
  (make-instance 'project
                 :description name
                 :slug name
                 :uuid (uuid::make-v5-uuid uuid::+namespace-dns+ name)))

(defun make-project (&key uuid description slug tags inherit-tags area-of-focus id)
  "Used to create projects for my filtered lists. This does not add it to one of the master lists."
  (if (eq slug nil)
      (setf slug (cl-slug::slugify description)))
  (if (eq uuid nil)
      (setf uuid (uuid:make-v5-uuid uuid::+namespace-dns+ slug))
      (setf uuid (uuid:make-uuid-from-string uuid)))
  (if (eq id nil)
      (setf id -1))
  (make-instance 'project
                       :description description
                       :uuid uuid :slug slug :id id
                       :area-of-focus area-of-focus
                       :tags tags :inherit-tags inherit-tags))

(defmethod slug= ((p project) str)
  "Check whether or not project `p' has the slug `str'"
  (string= (slug p) str))

(defmethod project= ((p project) (o project))
  "Two projects are equal if their `uuid' or `slug' are equal."
  (or (slug= p (slug o))
      (uuid::uuid= (uuid p) (uuid o))))

(defmethod print-object ((p project) out)
  "Display `p' on the screen in the format
slug: description"
  (print-unreadable-object (p out :type t)
    (format out "~a: ~a" (slug p) (description p))))

(defun save-projects (project-list filename)
  "Save the `project-list' to file.
Typically called with ~/.projects/active.json as the `filename'"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (cl-json:with-array (out)
      (loop for p in project-list
            for i from 1
            do (cl-json:as-array-member (out)
                 (cl-json:with-object (out)
                   (cl-json:encode-object-member "id" i out)
                   (cl-json:encode-object-member "uuid" (format nil "~a" (uuid p)) out)
                   (cl-json:encode-object-member "slug" (slug p) out)
                   (cl-json:encode-object-member "description" (description p) out)
                   (cl-json:encode-object-member "areaOfFocus" (area-of-focus p) out)
                   (cl-json:encode-object-member "tags" (tags p) out)
                   (cl-json:encode-object-member "inheritTags" (inherit-tags p) out)))
        (format out "~%")))
    (format out "~%")))

(defun load-projects (filename)
  (with-open-file (in filename :if-does-not-exist :create :direction :input)
    (let  ((data
             (if (listen in)
                 (cl-json:decode-json in)
                 (cl-json:decode-json-from-string "[]")))
           (output '()))
      (loop for p in data
            for i from 1
            do (add-to-end output (json->project p i)))
      output)))

(defun json->project (json-data id)
  (if (cdr (assoc :id json-data))
      (setf id (cdr (assoc :id json-data))))
  (make-project :uuid (cdr (assoc :uuid json-data))
                :id id
                :description (cdr (assoc :description json-data))
                :slug (cdr (assoc :slug json-data))
                :area-of-focus (cdr (assoc :AREA-OF-FOCUS json-data))
                :tags (cdr (assoc :TAGS json-data))
                :inherit-tags (cdr (assoc :inherit-tags json-data))))

;; TODO Refactor as a macro to make it easier to add new fields.
(defun where (&key aof tags inherit-tags slug description id)
  #'(lambda (project)
      (and
       (if aof          (search aof (area-of-focus project)) t)
       (if id           (eq id (id project)) t)
       (if tags         (member tags (tags project))         t)
       (if inherit-tags (member inherit-tags (inherit-tags project)) t)
       (if slug         (search slug (slug project)) t)
       (if description  (search description (description project)) t))))

(defun search-projects (search-fn project-list)
  (remove-if-not search-fn project-list))

;; TODO Refactor as a macro to make it easier to add new fields.
(defun update-projects (selector-fn modifications)
  (let (new-id new-description new-slug new-area new-tags remove-tags remove-inherit-tags new-inherit-tags)
    (dolist (term modifications)
      (cond
        ((search "area:" term) (setq new-aof (subseq term 5)))
        ((search "++" term) (push (subseq term 2) new-inherit-tags))
        ((search "+" term) (push (subseq term 1) new-tags))
        ((search "slug" term) (setq new-slug (subseq term 5)))
        ((search "id:" term) (setq new-id (parse-integer term :junk-allowed t)))
        ((search "--" term) (push (subseq term 2) remove-inherit-tags))
        ((search "-" term) (push (subseq term 1) remove-tags))
        (t (add-to-end new-description term))))
    (setf *completed-projects-list*
        (mapcar
         #'(lambda (project)
             (when (funcall selector-fn project)
               (if new-id (setf (id project) new-id))
               (if new-description (setf (description project) new-description))
               (if new-slug (setf (slug project) new-slug))
               (if new-area (setf (area-of-focus project) new-area))
               (if new-tags (setf (tags project) (pushnew (tags project) new-tags :test #'string=)))
               (if new-inherit-tags (setf (inherit-tags project) (pushnew (inherit-tags project) new-inherit-tags :test #'string=)))
               (if remove-tags (setf (tags project) (set-difference (tags project) remove-tags :test #'string=)))
               (if remove-tags (setf (inherit-tags project) (set-difference (inherit-tags project) remove-inherit-tags :test #'string=))))
             project) *completed-projects-list*))
  (setf *deleted-projects-list*
        (mapcar
         #'(lambda (project)
             (when (funcall selector-fn project)
               (if new-id (setf (id project) new-id))
               (if new-description (setf (description project) new-description))
               (if new-slug (setf (slug project) new-slug))
               (if new-area (setf (area-of-focus project) new-area))
               (if new-tags (setf (tags project) (pushnew (tags project) new-tags :test #'string=)))
               (if new-inherit-tags (setf (inherit-tags project) (pushnew (inherit-tags project) new-inherit-tags :test #'string=)))
               (if remove-tags (setf (tags project) (set-difference (tags project) remove-tags :test #'string=)))
               (if remove-tags (setf (inherit-tags project) (set-difference (inherit-tags project) remove-inherit-tags :test #'string=))))
             project) *deleted-projects-list*))
  (setf *active-projects-list*
        (mapcar
         #'(lambda (project)
             (when (funcall selector-fn project)
               (if new-id (setf (id project) new-id))
               (if new-description (setf (description project) new-description))
               (if new-slug (setf (slug project) new-slug))
               (if new-area (setf (area-of-focus project) new-area))
               (if new-tags (setf (tags project) (pushnew (tags project) new-tags :test #'string=)))
               (if new-inherit-tags (setf (inherit-tags project) (pushnew (inherit-tags project) new-inherit-tags :test #'string=)))
               (if remove-tags (setf (tags project) (set-difference (tags project) remove-tags :test #'string=)))
               (if remove-tags (setf (inherit-tags project) (set-difference (inherit-tags project) remove-inherit-tags :test #'string=))))
             project) *active-projects-list*))))

;; TODO Replace ascii-table with custom `format' calls so that I can control
;; the layout better. The current layout is far too bulky.
(defun list-projects (project-list)
  "This function builds an ascii-table table of the projects."
  (let ((table (ascii-table:make-table `("#" "Description" "Area of Focus" "Tags" "Slug") :header "Projects Report")))
    (dolist (project project-list)
      (ascii-table:add-row table (list (id project) (description project) (area-of-focus project) (tags project) (slug project))))
    (ascii-table:display table)))
