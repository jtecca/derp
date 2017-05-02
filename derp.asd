(asdf:defsystem #:derp
  :description "derp"
  :author "dptd <dptdescribe@gmail.com>"
  :license "MIT"
  :depends-on (#:jasaw #:jasb #:cl-json #:cl-ppcre #:cxml #:cl-async #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "derp")
               (:file "commands")
               (:file "queues")
               (:file "requests")
               (:file "derponarium")
               (:file "reminders")))

