(defpackage #:derp
  (:use #:cl #:cl-json #:cl-ppcre)
  (:export make-derp-config spawn-derp start-derping))

(defpackage #:derp.cmds
  (:use #:cl #:jasa.chat #:cxml)
  (:export cat dog help other ping rand-user random-number review remove-last-message joke yesno))

(defpackage #:derp.queues
  (:use #:cl #:jasa.chat)
  (:export queues add-queue status-all))
