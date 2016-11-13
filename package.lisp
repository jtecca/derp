(defpackage #:derp
  (:use #:cl #:cl-json #:cl-ppcre)
  (:export direct-message make-derp-config reject spawn-derp start-derping))

(defpackage #:derp.cmds
  (:use #:cl #:jasa.chat #:cxml)
  (:export cat dog help ping rand-user random-number review remove-last-message joke yesno))

(defpackage #:derp.queues
  (:use #:cl #:jasa.chat)
  (:export add-queue lock queue-status status-all unlock queues))

(defpackage #:derp.requests
  (:use #:cl #:jasa.chat)
  (:export requests request-command))
