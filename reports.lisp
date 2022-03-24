;;;; reports.lisp
;;;
;;; Generates reports for display to the user.

(defstruct report-layout
  name
  column-labels
  column-functions
  column-align)

(in-package :projectwarrior)

(defconstant +cell-formats+ '(:left   "~vA"
                              :center "~v:@<~A~>"
                              :right  "~v@A"))

(defmethod report-formatter ((p project) &optional (columns '(slug description area-of-focus tags inherit-tags)))
  (loop for col in columns
        collect (format nil "~A" (funcall col p))))

(defun format-table (projects stream &key (column-label '("#" "Slug" "Description" "Area of Focus" "tags"))
                                   (column-function '(id slug description area-of-focus tags))
                                   (column-align (loop for i from 1 to (length column-label)
                                                       collect :left)))
  "Code adapted from https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f"
  (let* ((col-count (length column-label))
         (strtable  (cons column-label ; CAR of string table is the table header as a list of strings
                          (loop for project in projects
                                collect (report-formatter project column-function))))
         (col-widths (loop with widths = (make-array col-count :initial-element 0)
                           for row in strtable
                           do (loop for cell in row
                                    for i from 0
                                    do (setf (aref widths i)
                                             (max (aref widths i) (length cell))))
                           finally (return widths))))
        ; Splice in the header separator
    (setq strtable
          (nconc (list (car strtable)
                       (loop for align in column-align
                             for width across col-widths
                           collect (case align
                                     (:left   (format nil ":~v@{~A~:*~}" (1- width) "-"))
                                     (:right  (format nil "~v@{~A~:*~}:" (1- width) "-"))
                                     (:center (format nil ":~v@{~A~:*~}:" (- width 2) "-")))))
                       (cdr strtable)))
        ; Generate the formatted table
    (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%"
                           (loop for align in column-align
                                 collect (getf +CELL-FORMATS+ align))))
          (widths (loop for w across col-widths collect w)))
      (loop for row-num from -1 to (length strtable)
            for row in strtable
            do (cond
                 ((< row-num 1) (apply #'format stream row-fmt (mapcan #'list widths row)))
                 ((= (mod row-num 2) 1) (with-color (:green :stream stream) (apply #'format stream row-fmt (mapcan #'list widths row))))
                 ((= (mod row-num 2) 0) (with-color (:blue :stream stream) (apply #'format stream row-fmt (mapcan #'list widths row)))))))))
