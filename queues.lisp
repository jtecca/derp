(in-package #:derp.queues)

(defun available-queues (queues)
  (if queues
      (concatenate 'string (format nil "∙ *~A*~%" (car queues)) (available-queues (cdr queues)))))

(defmethod queues ((bot derp::derp))
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text "Available queues:"
                          :attachments (jasa.chat:prepare-attachments :fallback ""
                                                                      :text (available-queues (slot-value bot 'derp::queues))
                                                                      :color "good")
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
