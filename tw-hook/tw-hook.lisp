;;;; tw-hook.lisp

;;;  This on-exit hook for Taskwarrior keeps an up to date list of projects you
;;;  have had in your taskwarrior system recently. It only adds projects to the
;;;  list, it never deletes them. Deleting is done manually or through the
;;;  projectwarrior tool.

(in-package #:tw-hook)

(defun main ()
  "Entry point for the hook."
  (let* ((current-projects (GTD-REVIEW:get-list-from-file GTD-REVIEW:*projects-filepath*))
         (new-projects (GTD-REVIEW:get-new-projects-list))
         (updated-projects (GTD-REVIEW:merge-lists current-projects new-projects)))
    (with-open-file (file *projects-filepath* :direction :output :if-exists :supersede)
      (format file "~{~A~%~}" updated-projects))
    ()))
