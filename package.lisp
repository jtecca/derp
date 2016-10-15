(defpackage #:derp
  (:use #:cl #:cl-json #:cl-ppcre)
  (:export make-derp-config spawn-derp))

(defpackage #:derp.cmds
  (:use #:cl)
  (:export ping help review other))
