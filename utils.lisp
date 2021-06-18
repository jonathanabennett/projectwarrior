;;; Holds common functions used across the program

(defun merge-lists (lst1 lst2)
  "Merge two lists of strings together, returning the union of the two lists."
  (union lst1 lst2 :test `equal))

(defun get-list-from-file (file)
  "Retrieve a list of strings from a file, each line of the file as its own string."
  ;; This will get extended when I add Weekly Review templates,
  ;; For now, it just wraps around uiop:read-file-lines.
  (uiop:read-file-lines file))
