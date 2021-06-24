;;;; gtd-review.asd

(asdf:defsystem #:gtd-review
  :description "GTD Review is a guided walk through your GTD Weekly Review that works with taskwarrior."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on ("yason")
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "gtd-review" :depends-on ("package")))
  :build-operation :program-op
  :build-pathname "gtd-review"
  :entry-point "gtd-review:main")

(asdf:defsystem #:gtd-review/tw-hook
  :description "This simple hook for Taskwarrior adds newly created projects to a projects.txt file for use in a weekly review."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("gtd-review")
  :components ((:file "tw-hook/package")
               (:file "tw-hook/tw-hook"))
  :build-operation :program-op
  :build-pathname "on-exit-projects-list"
  :entry-point "tw-hook:main")
