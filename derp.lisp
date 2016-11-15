(in-package #:derp)

(defclass derp ()
  ((token :initarg :token
          :accessor token
          :initform (error "Derp cannot work without Slack API token.")
          :documentation "Slack API token.")
   (id :initarg :id
       :accessor id
       :initform (error "Derp cannot work without ID.")
       :documentation "Slack bot ID.")
   (channel :initarg :channel
            :accessor channel
            :initform (error "Derp needs to know the channel name.")
            :documentation "Channel where derp should be spawned.")
   (commands :initarg :commands
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
             :documentation "Stored requests.")
   (tasks :initarg :tasks
          :accessor tasks
          :initform nil
          :documentation "List of tasks to do.")
   (name :initarg :name
         :accessor name
         :initform (error "Your derp has to have a name.")
         :documentation "Derp's name.")
   (icon :initarg :icon
         :accessor icon
         :initform nil
         :documentation "Derp's icon.")
   (users :initarg :users
          :accessor users
          :initform nil
          :documentation "Cached users from the channel.")
   (ts :initarg :ts
       :accessor ts
       :initform (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0))
       :documentation "Latest timestamp.")))

(defvar *available-commands* '("add"
                               "cat"
                               "dog"
                               "help"
                               "joke"
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

(defmethod fetch-history ((bot derp))
  (with-slots (channel token ts) bot
    (cond ((char= #\C (char channel 0))
           (ignore-errors (jasa.channels:history :token token
                                                 :channel channel
                                                 :oldest ts)))
          ((char= #\G (char channel 0))
           (ignore-errors (jasa.groups:history :token token
                                               :channel channel
                                               :oldest ts)))
          (t (error "Derp channel field is invalid. Should be starting with C or G character.")))))

(defmethod run-tasks ((bot derp))
  "Running all tasks from the queue."
  (with-accessors ((tasks tasks)) bot
      (mapcar #'(lambda (x) (run-task bot x)) tasks)
      (setf tasks nil)))

(defmethod reject ((bot derp) reason)
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :attachments (jasa.chat:prepare-attachments :text reason
                                                                      :mrkdwn_in '("text")
                                                                      :color "danger")
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

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
          (reject bot (format nil "I do not know this command."))))))

(defmethod fetch-messages ((bot derp))
  (cdaddr (fetch-history bot)))

(defmethod update-timestamp ((bot derp) messages)
  (with-slots (ts) bot
    (if messages
        (setf ts (cdr (assoc :ts (car messages)))))))

(defun extract-text (msg)
  (cdr (assoc :text msg)))

(defmethod direct-message-p ((bot derp) msg)
  (with-slots (id) bot
    (search (format nil "<@~A>" id) (cdr (assoc :text msg)))))

(defmethod get-name ((bot derp) id)
  (with-slots (token users) bot
    (let ((name (assoc id users :test #'string=)))
      (if name (cdr name)
            (let ((new-name (jasa.utils:get-user-name-from-id :token token :user-id id)))
              (if new-name
                  (progn
                    (push (cons id new-name) users)
                    new-name)))))))

(defmethod convert-msg-to-command ((bot derp) msg)
  (let ((command (cl-ppcre:split "\\s+" (extract-text msg)))
        (userid (cdr (assoc :user msg))))
    (cons
     (get-name bot userid)
     (cdr command))))

(defmethod store-command ((bot derp) msg)
  (with-slots (tasks) bot
    (push (convert-msg-to-command bot msg) tasks)))

(defmethod get-commands-and-users ((bot derp))
  (with-slots (token channel ts) bot
    (let ((messages (fetch-messages bot)))
        (update-timestamp bot messages)
        (dolist (msg messages)
          (if (direct-message-p bot msg) (store-command bot msg))))))

(defun direct-message (username text)
  (format nil "<@~A> ~A" username text))

(defmethod start-derping ((bot derp))
  (loop
     (progn
       (derp::get-commands-and-users bot)
       (derp::run-tasks bot)
       (sleep 1))))
