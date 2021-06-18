;;;; tw-hook.asd

(asdf:defsystem #:projects-list-hook
  :description "This simple hook for Taskwarrior adds newly created projects to a project.txt file for use in a weekly review."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("inferior-shell")
  :components ((:file "package")
               (:file "tw-hook"))
  :build-operation :program-op
  :build-pathname "on-exit-projects-list"
  :entry-point "gtd-review:main")
