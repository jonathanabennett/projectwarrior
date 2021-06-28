;;; Holds common functions used across the program

(in-package #:gtd-review)

(defparameter *projects-filepath* (uiop:native-namestring "~/.cl-gtd/projects.txt"))

(defun get-new-projects-list ()
  "Gets a list of the current projects from taskwarrior. rc.hooks=off is needed to prevent infinite loops."
 (uiop:run-program "task _projects rc.hooks=off" :ignore-error-status t :output :lines))

(defun merge-lists (lst1 lst2)
  "Merge two lists of strings together, returning the union of the two lists."
  (union lst1 lst2 :test 'equalp))

(defun get-list-from-file (file)
  "Retrieve a list of strings from a file, each line of the file as its own string."
  ;; This will get extended when I add Weekly Review templates,
  ;; For now, it just wraps around uiop:read-file-lines.
  (uiop:read-file-lines file))

(defun sync-projects-list (file)
  "Compare the list of projects stored in gtd-review to the list of projects from taskwarrior and keep the union of these two sets"
  (let* ((current-projects (get-list-from-file file))
         (new-projects (get-new-projects-list))
         (updated-projects (merge-lists current-projects new-projects)))
    (with-open-file (dest file :direction :output :if-exists :supersede)
      (format dest "~{~A~%~}" updated-projects))))

(defun ask-until-valid (valid-response-list prompt)
  "Ask the user to input until they give a response that is in the `VALID-RESPONSE-LIST'."
  (loop with answer = nil
        with response = nil
        while (null response)
        if (member answer valid-response-list :test 'equal)
          do (setq response answer)
        else do (clear-input)
                (write-string prompt)
                (finish-output)
                (setq answer (read-line))
        finally (return answer)))
