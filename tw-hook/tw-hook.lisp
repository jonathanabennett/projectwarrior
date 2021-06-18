;;;; tw-hook.lisp

;;;  This on-exit hook for Taskwarrior keeps an up to date list of projects you
;;;  have had in your taskwarrior system recently. It only adds projects to the
;;;  list, it never deletes them. Deleting is done manually or through the
;;;  projectwarrior tool.

(in-package #:tw-hook)

(defparameter *projects-filepath* (uiop:native-namestring "~/.cl-gtd/projects.txt "))

(defun get-new-projects-list ()
  "Retrieve taskwarrior's current list of projects."
  (inferior-shell:run/lines "task _projects rc.hooks=off"))

(defun merge-projects-lists (curr new)
  "Produce the union of taskwarrior's list and the saved list."
  (union curr new :test `equal))

(defun main ()
  "Entry point for the hook."
  (let* ((current-projects (uiop:read-file-lines *projects-filepath*))
         (new-projects (get-new-projects-list))
         (updated-projects (merge-projects-lists current-projects new-projects)))
    (with-open-file (file *projects-filepath* :direction :output :if-exists :supersede)
      (format file "~{~A~%~}" updated-projects))
    ()))