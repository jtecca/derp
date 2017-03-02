(in-package #:derponarium)

;; hashtable: derp name + derp channel -> derp instance, thread and running

(defstruct derp-thread
  (derp-instance nil)
  (thread nil)
  (running nil))

(defvar *derps* nil)

(defun prehash (derp)
  "Returns string based on derp name and channel."
  (concatenate 'string
               (slot-value derp 'jasb::name)
               (slot-value derp 'jasb::channel)))

(defun add-derp (derp)
  (if (typep derp 'derp::derp)
      (setf *derps* (cons (cons
                           (prehash derp)
                           (derponarium::make-derp-thread :derp-instance derp
                                                          :thread nil
                                                          :running nil))
                          *derps*))
      (error "Parameter must be an instance of a derp class.")))

(defun remove-derp (derp)
  (if (running-p derp)
      (stop-derp derp))
  (setf *derps* (remove-if #'(lambda (x) (string= derp (car x))) *derps*)))

(defun find-derp (derp)
  (if (stringp derp)
      (assoc derp *derps* :test 'string=)
      (error "Parameter must be a string.")))

(defun get-derp (derp)
  (if (stringp derp)
      (derp-thread-derp-instance (cdr (find-derp derp)))
      (error "Parameter must be a string.")))

(defun running-p (derp)
  (if (stringp derp)
      (if (find-derp derp)
          (derp-thread-running (cdr (find-derp derp)))
          (error (format nil "Cannot find derp named: ~A." derp)))
      (error "Must be a string.")))

(defun start-derp (derp)
  (if (not (running-p derp))
      (progn
        (setf (derp-thread-thread (cdr (find-derp derp))) (bt:make-thread (lambda ()
                                                                            (derp:start-derping (get-derp derp)))))
        (setf (derp-thread-running (cdr (find-derp derp))) t))
      (error (format nil "Derp named: ~A is already running." derp))))

(defun stop-derp (derp)
  (if (running-p derp)
      (progn
        (bt:destroy-thread (derp-thread-thread (cdr (find-derp derp))))
        (setf (derp-thread-thread (cdr (find-derp derp))) nil)
        (setf (derp-thread-running (cdr (find-derp derp))) nil))
      (error (format nil "Derp named: ~A is not running." derp))))
