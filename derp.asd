(asdf:defsystem #:derp
  :description "derp"
  :author "dptd <dptdescribe@gmail.com>"
  :license "MIT"
  :depends-on (#:jasa #:cl-json #:cl-ppcre #:cxml)
  :serial t
  :components ((:file "package")
               (:file "derp")
               (:file "commands")
               (:file "queues")))

