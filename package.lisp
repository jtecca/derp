(defpackage #:derp
  (:use #:cl #:cl-json #:cl-ppcre)
  (:export make-derp-config spawn-derp start-derping))

(defpackage #:derp.cmds
  (:use #:cl #:jasa.chat #:cxml)
  (:export cat dog help other ping rand-user review remove-last-message joke))
