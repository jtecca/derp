(in-package #:derp.queues)

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

(defmethod add-queue ((bot derp::derp) queue)
  (let ((msg nil))
    (if (assoc queue (slot-value bot 'derp::queues) :test #'string=)
        (setf msg "This queue already exists.")
        (progn
          (push (list queue) (slot-value bot 'derp::queues))
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
          (add-to-the-queue bot user queue)
          (progn
            (derp:reject bot "I'm afraid you are already in this queue.")
            nil))))

(defmethod lock ((bot derp::derp) user queue)
  (if (add-if-possible bot user queue)
      (jasa.chat:post-message :token (slot-value bot 'derp::token)
                              :channel (slot-value bot 'derp::channel)
                              :text (queue-status bot queue)
                              :username (slot-value bot 'derp::name)
                              :icon_emoji (slot-value bot 'derp::icon))))


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
          (remove-from-the-queue bot user queue))))

(defmethod unlock ((bot derp::derp) user queue)
  (let ((head (current-user-p bot user queue)))
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

;;;; todo, rename queue and remove queue
