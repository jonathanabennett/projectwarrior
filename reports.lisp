;;;; reports.lisp
;;;
;;; Generates reports for display to the user.

(in-package :projectwarrior)

(defun has-file? (project)
  (if (filepath project)
      "True"
      "False"))

(defstruct report
  name
  column-labels
  column-functions
  column-align)

(defvar *reports-list* '())

(defun register-report (&key name col-labels col-functions col-align)
  (pushnew (make-report :name name
                     :column-labels col-labels
                     :column-functions col-functions
                     :column-align col-align)
        *reports-list* :test #'string= :key #'(lambda (x) (report-name x))))

(defvar *default-report* (make-report :name "Default"
                                      :column-labels '("#" "Slug" "Description" "Area of Focus" "tags" "file?")
                                      :column-functions '(id slug description area-of-focus tags has-file?)
                                      :column-align (loop for i from 1 to 6 collect :left)))

(alexandria:define-constant +cell-formats+ '(:left   "~vA"
                                             :center "~v:@<~A~>"
                                             :right  "~v@A")
  :test #'equal
  :documentation "These are the `FORMAT' recipes to align markdown tables left, right, and center.")

(defmethod report-formatter ((p project) &optional (columns '(slug description area-of-focus tags inherit-tags)))
  (loop for col in columns
        collect (format nil "~A" (funcall col p))))

(defun format-table (projects stream &key (report-format *default-report*))
  "Code adapted from https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f"
  (let* ((col-count (length (report-column-labels report-format)))
         (strtable  (cons (report-column-labels report-format) ; CAR of string table is the table header as a list of strings
                          (loop for project in projects
                                collect (report-formatter project (report-column-functions report-format)))))
         (col-widths (loop with widths = (make-array col-count :initial-element 0)
                           for row in strtable
                           do (loop for cell in row
                                    for i from 0
                                    do (setf (aref widths i)
                                             (max (aref widths i) (length cell))))
                           finally (return widths))))
        ; Splice in the header separator
    (setf strtable
          (nconc (list (car strtable)
                       (loop for align in (report-column-align report-format)
                             for width across col-widths
                           collect (case align
                                     (:left   (format nil ":~v@{~A~:*~}" (1- width) "-"))
                                     (:right  (format nil "~v@{~A~:*~}:" (1- width) "-"))
                                     (:center (format nil ":~v@{~A~:*~}:" (- width 2) "-")))))
                       (cdr strtable)))
        ; Generate the formatted table
    (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%"
                           (loop for align in (report-column-align report-format)
                                 collect (getf +CELL-FORMATS+ align))))
          (widths (loop for w across col-widths collect w)))
      (loop for row-num from -1 to (length strtable)
            for row in strtable
            do (cond
                 ((< row-num 1) (apply #'format stream row-fmt (mapcan #'list widths row)))
                 ((= (mod row-num 2) 1) (with-color (:green :stream stream) (apply #'format stream row-fmt (mapcan #'list widths row))))
                 ((= (mod row-num 2) 0) (with-color (:blue :stream stream) (apply #'format stream row-fmt (mapcan #'list widths row)))))))))
