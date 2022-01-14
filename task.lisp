;;;; task.lisp

;;; This file holds the internal representation of a task imported from
;;; taskwarrior.

(in-package :projectwarrior)

(defclass task ()
  ((id
    :initarg :id
    :accessor task-id
    :documentation "The id assigned by taskwarrior when this command was run.
This is kept to the user, but we would use the `UUID' to access this task programmatically.")
   (description
    :initarg :description
    :accessor task-desc
    :documentation "The description of the task when imported from taskwarrior.")
   (entry
    :initarg :entry
    :accessor task-entry-date
    :documentation "The date this task was entered into taskwarrior.
This is a string in the format YYYYMMDDTHHMMSS.")
   (modified
    :initarg :modified
    :accessor task-mod-date
    :documentation "The date this task was last modified. It follows the same string format as `ENTRY' above.")
   (project
    :initarg :project
    :accessor task-project
    :documentation "The project this task is assigned to in taskwarrior.
This will correspond to the SLUG in the `PROJECT' class.")
   (reviewed
    :initarg :reviewed
    :accessor task-reviewed
    :documentation "The last time this task was reviewed. It follows the same string format as `ENTRY' above.")
   (status
    :initarg :status
    :accessor task-status
    :documentation "This task's current status. Either 'PENDING' or 'WAITING'.")
   (wait
    :initarg :wait
    :initform nil
    :accessor task-wait
    :documentation "The date that this task is WAITed until. It follows the same string format as `ENTRY' above.")
   (tags
    :initarg :tags
    :accessor task-tags
    :documentation "A list of strings representing the tags associated with this task.")
   (uuid
    :initarg :uuid
    :accessor uuid
    :documentation "The UUID assigned by taskwarrior. We will use this when updating the task.")
   (urgency
    :initarg :urgency
    :accessor urgency
    :documentation "A FLOAT representing the urgency assigned by taskwarrior."))
  (:documentation "Holds the internal representation of a the task data we need to use which is passed in from taskwarrior.
Taskwarrior passes in all kinds of additional information, we ignore it as we do not need it."))

(defmethod compare ((t1 task) (t2 task))
  "Compare two tasks and sort them based on urgency."
  (> (urgency t1) (urgency t2)))

(defmethod display ((tsk task))
  "Formats tasks for display on the string TSK is a `TASK' object."
  (let ((width (uiop:getenv "COLUMNS")))
    (format nil "~0,1,10A~,0,1,10A~A" (task-desc tsk) (task-status tsk) (urgency tsk))))

(defun task-from-hash (t-hash)
  "Given a hash as produced by taskwarrior export, create a `TASK' instance. T-HASH must be a hash."
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
