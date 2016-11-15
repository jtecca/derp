(in-package #:derp.queues)

(defmethod save-queues ((bot derp::derp))
  (with-open-file (out (format nil "~~/tmp/~A-~A-queues.db" (slot-value bot 'derp::name) (slot-value bot 'derp::channel))
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print (slot-value bot 'derp::queues) out))))

(defmethod load-queues ((bot derp::derp))
  (with-open-file (in (format nil "~~/tmp/~A-~A-queues.db" (slot-value bot 'derp::name) (slot-value bot 'derp::channel))
                      :external-format :utf-8)
    (with-standard-io-syntax
      (setf (slot-value bot 'derp::queues) (read in)))))

(defun available-queues (queues)
  (if queues
      (concatenate 'string (format nil "∙ *~A*~%" (caar queues)) (available-queues (cdr queues)))))

(defmethod queues ((bot derp::derp))
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text "Available queues:"
                          :attachments (jasa.chat:prepare-attachments :fallback ""
                                                                      :text (available-queues (slot-value bot 'derp::queues))
                                                                      :mrkdwn_in '("text")
                                                                      :color "good")
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

(defmethod queue-exists-p ((bot derp::derp) queue)
  (if (assoc queue (slot-value bot 'derp::queues) :test #'string=)
      t
      (progn
        (derp:reject bot (format nil "Queue \"*~A*\" doesn't exist. Use `queues` command to see all available queues." queue))
        nil)))

(defmethod queue-status ((bot derp::derp) q)
  (let ((queue (cdr (assoc q (slot-value bot 'derp::queues) :test #'string=))))
  (cond ((= 0 (length queue)) (format nil "*[~A]* Queue is empty!" q))
        ((< 1 (length queue)) (format nil "*[~A]* TAIL → ~{*~A* → ~}HEAD" q queue))
        ((format nil "*[~A]* TAIL → *~A* -> HEAD" q (car queue))))))

(defmethod status ((bot derp::derp) queue)
  (if (queue-exists-p bot queue)
      (jasa.chat:post-message :token (slot-value bot 'derp::token)
                              :channel (slot-value bot 'derp::channel)
                              :text (queue-status bot queue)
                              :username (slot-value bot 'derp::name)
                              :icon_emoji (slot-value bot 'derp::icon))))

(defmethod queues-status ((bot derp::derp) queues)
  (if queues
      (concatenate 'string (format nil "~A~%" (queue-status bot (caar queues))) (queues-status bot (cdr queues)))))

(defmethod status-all ((bot derp::derp))
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                              :channel (slot-value bot 'derp::channel)
                              :text "Status of all queues:"
                              :attachments (jasa.chat:prepare-attachments
                                            :text (queues-status bot (slot-value bot 'derp::queues))
                                            :mrkdwn_in '("text")
                                            :color "good")
                              :username (slot-value bot 'derp::name)
                              :icon_emoji (slot-value bot 'derp::icon)))

(defmethod add-queue ((bot derp::derp) args)
  (let ((queue (car args))
          (msg nil))
    (if (not queue)
        (derp:reject bot (format nil "What name do you need? `add <queue_name>`")))
    (if (assoc queue (slot-value bot 'derp::queues) :test #'string=)
        (setf msg "This queue already exists.")
        (progn
          (push (list queue) (slot-value bot 'derp::queues))
          (save-queues bot)
          (setf msg (format nil "Queue *~A* added." queue))))
    (jasa.chat:post-message :token (slot-value bot 'derp::token)
                            :channel (slot-value bot 'derp::channel)
                            :text msg
                            :username (slot-value bot 'derp::name)
                            :icon_emoji (slot-value bot 'derp::icon))))

(defmethod present-in-the-queue-p ((bot derp::derp) user queue)
  (member user (cdr (assoc queue (slot-value bot 'derp::queues) :test #'string=)) :test #'string=))

;;;; adding to the queue
(defmethod add-to-the-queue ((bot derp::derp) user queue)
  (push user (cdr (assoc queue (slot-value bot 'derp::queues) :test #'string=))))

(defmethod add-if-possible ((bot derp::derp) user queue)
  (if (queue-exists-p bot queue)
      (if (not (present-in-the-queue-p bot user queue))
          (progn
            (add-to-the-queue bot user queue)
            (save-queues bot))
          (progn
            (derp:reject bot "I'm afraid you are already in this queue.")
            nil))))

(defmethod lock ((bot derp::derp) user args)
  (let ((queue (car args)))
    (if (not queue)
        (derp:reject bot (format nil "Which queue you want to lock? `lock <queue_name>`")))
    (if (add-if-possible bot user queue)
        (jasa.chat:post-message :token (slot-value bot 'derp::token)
                                :channel (slot-value bot 'derp::channel)
                                :text (queue-status bot queue)
                                :username (slot-value bot 'derp::name)
                                :icon_emoji (slot-value bot 'derp::icon)))))


;;;; removing from the queue
(defmethod current-user-p ((bot derp::derp) user queue)
  "Checks if user is in the head of the queue."
  (string= user (car (reverse (cdr (assoc queue (slot-value bot 'derp::queues) :test #'string=))))))

(defmethod remove-from-the-queue ((bot derp::derp) user queue)
  "Removes user from the queue."
  (setf
   (cdr (assoc queue (slot-value bot 'derp::queues) :test #'string=))
   (delete-if #'(lambda (x) (string= user x)) (cdr (assoc queue (slot-value bot 'derp::queues) :test #'string=))))
  t)

(defmethod remove-if-possible ((bot derp::derp) user queue)
  (if (queue-exists-p bot queue)
      (if (not (present-in-the-queue-p bot user queue))
          (progn
            (derp:reject bot "I'm afraid you are not even in this queue.")
            nil)
          (progn
            (remove-from-the-queue bot user queue)
            (save-queues)))))

(defmethod unlock ((bot derp::derp) user args)
  (let ((queue (car args))
        (head (current-user-p bot user queue)))
    (if (not queue)
        (derp:reject bot (format nil "Which queue you want to unlock? `unlock <queue_name>`")))
    (if (remove-if-possible bot user queue)
        (progn
          (jasa.chat:post-message :token (slot-value bot 'derp::token)
                                  :channel (slot-value bot 'derp::channel)
                                  :text (queue-status bot queue)
                                  :username (slot-value bot 'derp::name)
                                  :icon_emoji (slot-value bot 'derp::icon))
          (if head
              (ping-next-user bot queue))))))

(defmethod ping-next-user ((bot derp::derp) q)
  (let ((queue (cdr (assoc q (slot-value bot 'derp::queues) :test #'string=))))
    (if (not (= 0 (length queue)))
        (jasa.chat:post-message :token (slot-value bot 'derp::token)
                                :channel (slot-value bot 'derp::channel)
                                :text (derp:direct-message (car (last queue)) (format nil "it's your turn in the queue *[~A]*" q))
                                :username (slot-value bot 'derp::name)
                                :icon_emoji (slot-value bot 'derp::icon)))))

(defmethod rename ((bot derp::derp) args)
  "Renames queue. Takes current name and new name."
  (if (and (car args) (cadr args))
      (let ((old (car args))
            (new (cadr args)))
        (if (rename-possible-p bot old new)
            (progn
              (setf (car (assoc old (slot-value bot 'derp::queues) :test #'string=)) new)
              (save-queues)
              (jasa.chat:post-message :token (slot-value bot 'derp::token)
                                      :channel (slot-value bot 'derp::channel)
                                      :text (format nil "Queue *~A* was renamed to *~A*." old new)
                                      :username (slot-value bot 'derp::name)
                                      :icon_emoji (slot-value bot 'derp::icon)))))
      (derp:reject bot (format nil "I need two arguments.~%`rename <old_name> <new_name>`"))))

(defmethod rename-possible-p ((bot derp::derp) old new)
  (if (assoc old (slot-value bot 'derp::queues) :test #'string=)
      (if (not (assoc new (slot-value bot 'derp::queues) :test #'string=))
          t
          (progn
            (derp:reject bot (format nil "Queue *~A* already exists.~%`rename <old_name> <new_name>`" new))
            nil))
      (progn
        (derp:reject bot (format nil "Queue *~A* doesn't exists.~%`rename <old_name> <new_name>`" old))
        nil)))
