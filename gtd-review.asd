;;;; gtd-review.asd

(asdf:defsystem #:gtd-review
  :description "GTD Review is a guided walk through your GTD Weekly Review that works with taskwarrior."
  :author "Jonathan A. Bennett <doulos05@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("inferior-shell")
  :components ((:file "package")
               (:file "gtd-review"))
  :build-operation :program-op
  :build-pathname "on-exit-projects-list"
  :entry-point "gtd-review:main")
