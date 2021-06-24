;;;; project.lisp

;;; This file defines a project as seen by the gtd-review program.

(in-package #:gtd-review)

(defclass project ()
  ((description
    :initarg :description
    :accessor description)
   (start-date
    :initarg :start-date
    :accessor start-date)
   (last-review
    :initarg :last-review
    :initform nil
    :accessor last-review)
   (slug
    :initarg :slug
    :accessor slug)
   (tags
    :initarg :tags
    :accessor tags)
   (area-of-focus
    :initarg :area-of-focus
    :accessor area-of-focus)
   (deadline
    :initarg :deadline
    :accessor deadline)
   (inherit-tags
    :initarg :inherit-tags
    :accessor inherit-tags)))

(defgeneric new ())

(defmethod new (object project))
