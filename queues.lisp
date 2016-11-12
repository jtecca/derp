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
                                                                      :color "#6984c9")
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

(defmethod queue-exists-p ((bot derp::derp) queue)
  (if (find queue (slot-value bot 'derp::queues) :test #'string=)
      t
      (jasa.chat:post-message :token (slot-value bot 'derp::token)
                              :channel (slot-value bot 'derp::channel)
                              :text (format nil "Queue \"*~A*\" doesn't exist. Use `queues` command to see all available queues." queue)
                              :username (slot-value bot 'derp::name)
                              :icon_emoji (slot-value bot 'derp::icon))))

(defmethod queue-status ((bot derp::derp) q)
  (let ((queue (caddr (assoc q (slot-value bot 'derp::queues) :test #'string=))))
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
                                            :color "#6984c9")
                              :username (slot-value bot 'derp::name)
                              :icon_emoji (slot-value bot 'derp::icon)))

(defmethod add-queue ((bot derp::derp) queue)
  (let ((msg nil))
    (if (assoc queue (slot-value bot 'derp::queues) :test #'string=)
        (setf msg "This queue already exists.")
        (progn
          (push (cons queue ''nil) (slot-value bot 'derp::queues))
          (setf msg (format nil "Queue *~A* added." queue))))
    (jasa.chat:post-message :token (slot-value bot 'derp::token)
                            :channel (slot-value bot 'derp::channel)
                            :text msg
                            :username (slot-value bot 'derp::name)
                            :icon_emoji (slot-value bot 'derp::icon))))
