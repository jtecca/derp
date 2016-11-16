(in-package #:derponarium)

(defclass derponarium ()
  ((derps :initarg :token
          :accessor token
          :initform nil
          :documentation "Active derps.")))
