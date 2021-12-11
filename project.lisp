;;;; project.lisp

;;; This file defines a project as seen by the gtd-review program.

(in-package #:gtd-review)

(defclass project ()
  ((uuid
    :initarg :uuid
    :accessor uuid
    :documentation "The UUID that identifies a project.")
   (description
    :initarg :description
    :accessor description
    :documentation "A sentence description of a project.")
   (start-date
    :initarg :start-date
    :initform nil
    :accessor start-date
    :documentation "The date this was first entered into gtd-review. Stored as a date object.")
   (last-review
    :initarg :last-review
    :initform nil
    :initform nil
    :accessor last-review
    :documentation "The datetime that this was last reviewed. Stored as a datetime object.")
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
   (deadline
    :initarg :deadline
    :initform nil
    :accessor deadline
    :documentation "The date after which this project can be considered late.")
   (inherit-tags
    :initarg :inherit-tags
    :initform '()
    :accessor inherit-tags
    :documentation "A list of strings representing the tags which all tasks created from within gtd-review for this project should have by default."))
  (:documentation "A class representing a project within gtd-review."))

(defun project-from-taskwarrior (name)
  (make-instance 'project
                 :description name
                 :uuid (uuid::make-v5-uuid "gtd-review" name)))

(defmethod project-to-file (project p)
  "This function will write the project out to a file as a lisp form which, when evaluated, will create the project again.")
