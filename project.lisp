;;;; project.lisp

;;; This file defines a project as seen by the gtd-review program.

(in-package #:gtd-review)

(defvar *active-projects-list* nil)

(defclass project ()
  ((uuid
    :initarg :uuid
    :accessor uuid
    :documentation "The UUID that identifies a project.")
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
  (:documentation "A class representing a project within gtd-review."))

(defun project-from-taskwarrior (name)
  (make-instance 'project
                 :description name
                 :slug name
                 :uuid (uuid::make-v5-uuid uuid::+namespace-dns+ name)))

;; Make project with optionals
(defun add-project (&key uuid description slug tags inherit-tags area-of-focus)
  (if (eq slug nil)
      (setf slug (cl-slug::slugify description)))
  (if (eq uuid nil)
      (setf uuid (uuid:make-v5-uuid uuid::+namespace-dns+ slug))
      (setf uuid (uuid:make-uuid-from-string uuid)))
  (push (make-instance 'project
                 :description description
                 :uuid uuid
                 :slug slug
                 :area-of-focus area-of-focus
                 :tags tags
                 :inherit-tags inherit-tags) *active-projects-list*))


(defmethod slug= ((p project) str)
  "Check whether or not project `p' has the slug `str'"
  (string= (slug p) str))

(defmethod project= ((p project) (o project))
  "Two projects are equal if their UUIDs are equal."
  (uuid::uuid= (uuid p) (uuid o)))

(defmethod print-object ((p project) out)
  "Display `p' on the screen in the format
Description: slug"
  (print-unreadable-object (p out :type t)
    (format out "~a: ~a" (slug p) (description p))))

(defun save-projects (project-list filename)
  "Save the `project-list' to file.
Typically called with ~/.cl-gtd/projects.db as the `filename'"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (cl-json:with-array (out)
      (dolist (p project-list)
        (cl-json:as-array-member (out) (cl-json:with-object (out)
          (cl-json:encode-object-member "uuid" (format nil "~a" (uuid p)) out)
          (cl-json:encode-object-member "slug" (slug p) out)
          (cl-json:encode-object-member "description" (description p) out)
          (cl-json:encode-object-member "areaOfFocus" (area-of-focus p) out)
          (cl-json:encode-object-member "tags" (tags p) out)
          (cl-json:encode-object-member "inheritTags" (inherit-tags p) out)))
        (format out "~%")))))

(defun load-projects (filename)
  (with-open-file (in filename
                      :direction :input)
    (let  ((data (cl-json:decode-json in)))
      (dolist (p data)
        (json->project p)))))

(defun json->project (json-data)
  (add-project :uuid (cdr (assoc :uuid json-data))
               :description (cdr (assoc :description json-data))
               :slug (cdr (assoc :slug json-data))
               :area-of-focus (cdr (assoc :AREA-OF-FOCUS json-data))
               :tags (cdr (assoc :TAGS json-data))
               :inherit-tags (cdr (assoc :inherit-tags json-data))))

