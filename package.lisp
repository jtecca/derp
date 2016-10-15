(defpackage #:derp
  (:use #:cl #:cl-json #:cl-ppcre)
  (:export make-derp-config spawn-derp))

(defpackage #:derp.cmds
  (:use #:cl #:jasa.chat #:cxml)
  (:export cat help other ping review))
