(in-package #:derp)

(defclass derp (jasb)
  ((commands :initarg :commands
             :accessor commands
             :initform (error "Derp needs to know some commands.")
             :documentation "Set of available commands.")
   (queues :initarg :queues
           :accessor queues
           :initform nil
           :documentation "Stored queues.")
   (requests :initarg :requests
             :accessor requests
             :initform nil
             :documentation "Stored requests.")))

(defvar *available-commands* '("add"
                               "cat"
                               "dog"
                               "help"
                               "joke"
                               "kick"
                               "lock"
                               "ping"
                               "randuser"
                               "randnumber"
                               "remove"
                               "rename"
                               "review"
                               "request"
                               "requests"
                               "status"
                               "features"
                               "unlock"
                               "yes?"
                               "queues"))

(defun prepare-commands (commands)
  "Takes list of commands and returns list of the only available ones."
  (intersection commands *available-commands* :test #'string=))

(defstruct derp-config
  "Structure used by spawn-derp function."
  (name nil :type string)
  (icon nil :type string)
  (token nil :type string)
  (id nil :type string)
  (channel nil :type string)
  (commands nil :type list)
  (tasks nil :type list)
  (queues nil :type list)
  (requests nil :type list))

(defun spawn-derp (config)
  "Creates one derp."
  (if (derp-config-p config)
      (make-instance 'derp
                     :channel (derp-config-channel config)
                     :token (derp-config-token config)
                     :id (derp-config-id config)
                     :commands (prepare-commands (derp-config-commands config))
                     :name (derp-config-name config)
                     :icon (derp-config-icon config)
                     :tasks (derp-config-tasks config)
                     :queues (mapcar #'list (derp-config-queues config))
                     :requests (derp-config-requests config))
      (error "spawn-derp requires derp-config as parameter")))

(defmethod run-tasks ((bot derp))
  "Running all tasks from the queue."
  (mapcar #'(lambda (x) (run-task bot x)) (slot-value bot 'jasb::tasks))
  (setf (slot-value bot 'jasb::tasks) nil))

(defmethod run-task ((bot derp) command)
  (with-slots (commands) bot
    (let* ((user (car command))
           (cmd (cadr command))
           (args (cddr command)))
      (if (member cmd commands :test #'string=)
          (cond
            ((string= cmd "add") (derp.queues:add-queue bot args))
            ((string= cmd "cat") (derp.cmds:cat bot))
            ((string= cmd "dog") (derp.cmds:dog bot))
            ((string= cmd "features") (derp.cmds:features bot))
            ((string= cmd "help") (derp.cmds:help bot))
            ((string= cmd "joke") (derp.cmds:joke bot))
            ((string= cmd "kick") (derp.queues:kick bot args))
            ((string= cmd "lock") (derp.queues:lock bot user args))
            ((string= cmd "ping") (derp.cmds:ping bot))
            ((string= cmd "randuser") (derp.cmds:rand-user bot))
            ((string= cmd "randnumber") (ignore-errors (derp.cmds:random-number bot args)))
            ((string= cmd "rename") (derp.queues:rename bot args))
            ((string= cmd "request") (derp.requests:request-command bot user args))
            ((string= cmd "requests") (derp.requests:requests bot))
            ((string= cmd "status") (derp.queues:status-all bot))
            ((string= cmd "remove") (derp.cmds:remove-last-message bot))
            ((string= cmd "review") (derp.cmds:review bot))
            ((string= cmd "unlock") (derp.queues:unlock bot user args))
            ((string= cmd "yes?") (derp.cmds:yesno bot))
            ((string= cmd "queues") (derp.queues:queues bot)))
          (jasb:reject bot (format nil "I do not know this command."))))))

(defmethod start-derping ((bot derp))
  (loop
    (progn
      (jasb:get-commands-and-users bot)
      (run-tasks bot)
      (sleep 1))))
