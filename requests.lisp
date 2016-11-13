(in-package #:derp.requests)

(defmethod get-requests (requests)
  (if requests
      (concatenate 'string (format nil "∙ `~A` - ~A (requested by ~A)~%" (caar requests) (cadar requests) (caddar requests)) (get-requests (cdr requests)))))


(defmethod requests ((bot derp::derp))
  (let ((number-of-requests (length (slot-value bot 'derp::requests))))
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :attachments (jasa.chat:prepare-attachments :title (format nil "Requested commands (~A):" number-of-requests)
                                                                      :text (if (< 0 number-of-requests)
                                                                                (get-requests (slot-value bot 'derp::requests))
                                                                                "Currently there are no requests.")
                                                                      :mrkdwn_in '("text"))
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon))))

(defun extract-description (args)
  (if args
      (concatenate 'string (format nil "~A " (car args)) (extract-description (cdr args)))))

(defmethod request-command ((bot derp::derp) user args)
  (if (and user (car args) (cdr args))
      (progn
        (push (list (car args) (extract-description (cdr args)) user) (slot-value bot 'derp::requests))
        (jasa.chat:post-message :token (slot-value bot 'derp::token)
                                :channel (slot-value bot 'derp::channel)
                                :text "Stored, thanks!"
                                :username (slot-value bot 'derp::name)
                                :icon_emoji (slot-value bot 'derp::icon)))
  (derp:reject bot (format nil "I need two arguments.~%`request <command_name> <command_description>`"))))
