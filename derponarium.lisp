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
  (if (typep derp 'derp::derp)
      (setf *derps* (cons (cons
                           (prehash derp)
                           (derponarium::make-derp-thread :derp-instance derp
                                                          :thread nil
                                                          :running nil))
                          *derps*))
      (error "Parameter must be an instance of a derp class.")))

(defun find-derp (derp)
  (if (stringp derp)o
      (assoc derp *derps* :test 'string=)
      (error "Parameter must be a string.")))

(defun get-derp (derp)
  (if (stringp derp)
      (derp-thread-derp-instance (cdr (find-derp derp)))
      (error "Parameter must be a string.")))

(defun start-derp (derp)
  (if (stringp derp)
      (if (find-derp derp)
          (progn
            (setf (derp-thread-thread (cdr (find-derp derp))) (bt:make-thread (lambda ()
                                                                          (derp:start-derping (get-derp derp)))))
            (setf (derp-thread-running (cdr (find-derp derp))) t))
          (error (format nil "Cannot find derp named: ~A." derp)))
      (error "Must be a string.")))

(defun stop-derp (derp)
  ())
