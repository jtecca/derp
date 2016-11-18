(in-package #:derponarium)

;; hashtable: derp name + derp channel -> derp instance, thread and running

(defstruct derp-thread
  (derp-instance nil)
  (thread nil)
  (running nil))

(setf *derps* nil)

(defun prehash (derp)
  "Returns string based on derp name and channel."
  (concatenate 'string
               (slot-value derp 'derp::name)
               (slot-value derp 'derp::channel)))

(defun add-derp (derp)
  (setf *derps* (cons (cons
                       (prehash derp)
                       (derponarium::make-derp-thread :derp-instance derp
                                                      :thread nil
                                                      :running nil))
                      *derps*)))

(defun find-derp (derp)
  (if (stringp derp)
      (assoc derp *derps* :test 'string=)
      (error "Must be a string.")))

(defun get-derp (derp)
  (if (stringp derp)
      (derp-thread-derp-instance (cdr (find-derp derp)))
      (error "Must be a string.")))

;; (defun start-derp (derp)
;;   (if (stringp derp)
;;       (progn
;;         (bt:make-thread (derp:start-derping (get-derp derp)))
;;         (derp-thread-thread (find-derp derp))
;;       (error "Must be a string.")))

(defun stop-derp (derp)
  ())
