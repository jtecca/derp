(in-package #:derp.queues)

(defun available-queues (queues)
  (if queues
      (concatenate 'string (format nil "• *~A*\\n" (car queues)) (available-queues (cdr queues)))))

(defmethod queues ((bot derp::derp))
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text "Available queues:"
                          :attachments (jasa.chat:prepare-attachments :fallback ""
                                                                      :text (available-queues (slot-value bot 'derp::queues))
                                                                      :mrkdwn_in "[\"text\"]"
                                                                      :color "good")
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))
