;;;; task.lisp

;;; This file holds the internal representation of a task imported from
;;; taskwarrior.

(in-package :gtd-review)

(defclass task ()
  ((id
    :initarg :id
    :accessor task-id)
   (description
    :initarg :description
    :accessor task-desc)
   (entry
    :initarg :entry
    :accessor task-entry-date)
   (modified
    :initarg :modified
    :accessor task-mod-date)
   (project
    :initarg :project
    :accessor task-project)
   (reviewed
    :initarg :reviewed
    :accessor task-reviewed)
   (status
    :initarg :status
    :accessor task-status)
   (wait
    :initarg :wait
    :initform nil
    :accessor task-wait)
   (tags
    :initarg :tags
    :accessor task-tags)
   (uuid
    :initarg :uuid
    :accessor uuid)
   (urgency
    :initarg :urgency
    :accessor urgency)))

(defmethod compare ((t1 task) (t2 task))
  (> (urgency t1) (urgency t2)))

(defgeneric display (object))

(defmethod display ((tsk task))
  (format nil "~0,1,5A~0,1,5A~a" (task-desc tsk) (task-status tsk) (urgency tsk)))

(defun task-from-hash (t-hash)
  (make-instance 'task
                 :id (gethash "id" t-hash)
                 :description (gethash "description" t-hash)
                 :entry (gethash "entry" t-hash)
                 :modified (gethash "modified" t-hash)
                 :project (gethash "project" t-hash)
                 :reviewed (gethash "reviewed" t-hash)
                 :status (gethash "status" t-hash)
                 :tags (gethash "tags" t-hash)
                 :uuid (gethash "uuid" t-hash)
                 :urgency (gethash "urgency" t-hash)))
