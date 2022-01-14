;;;; tw-hook.lisp

;;;  This on-exit hook for Taskwarrior keeps an up to date list of projects you
;;;  have had in your taskwarrior system recently. It only adds projects to the
;;;  list, it never deletes them. Deleting is done manually or through the
;;;  projectwarrior tool.

(in-package #:tw-hook)

(defun main ()
  "Entry point for the hook."
  (PROJECTWARRIOR:sync-projects-list PROJECTWARRIOR:*projects-filepath*))
