(in-package #:derp.requests)

(defmethod save-requests ((bot derp::derp))
  (with-open-file (out (format nil "~~/tmp/~A-~A-requests.db" (slot-value bot 'jasb::name) (slot-value bot 'jasb::channel))
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print (slot-value bot 'derp::requests) out))))

(defmethod load-requests ((bot derp::derp))
  (with-open-file (in (format nil "~~/tmp/~A-~A-requests.db" (slot-value bot 'jasb::name) (slot-value bot 'jasb::channel))
                      :external-format :utf-8)
    (with-standard-io-syntax
      (setf (slot-value bot 'derp::requests) (read in)))))

(defmethod get-requests (requests)
  (if requests
      (concatenate 'string (format nil "∙ `~A` - ~A (requested by ~A)~%" (caar requests) (cadar requests) (caddar requests)) (get-requests (cdr requests)))))

(defmethod requests ((bot derp::derp))
  (let ((number-of-requests (length (slot-value bot 'derp::requests))))
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                          :channel (slot-value bot 'jasb::channel)
                          :attachments (jasaw.chat:prepare-attachments :title (format nil "Requested commands (~A):" number-of-requests)
                                                                      :text (if (< 0 number-of-requests)
                                                                                (get-requests (slot-value bot 'derp::requests))
                                                                                "Currently there are no requests.")
                                                                      :mrkdwn_in '("text"))
                          :username (slot-value bot 'jasb::name)
                          :icon_emoji (slot-value bot 'jasb::icon))))

(defun extract-description (args)
  (if args
      (concatenate 'string (format nil "~A " (car args)) (extract-description (cdr args)))))

(defmethod request-command ((bot derp::derp) user args)
  (if (and user (car args) (cdr args))
      (progn
        (push (list (car args) (extract-description (cdr args)) user) (slot-value bot 'derp::requests))
        (save-requests bot)
        (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                                :channel (slot-value bot 'jasb::channel)
                                :text "Stored, thanks!"
                                :username (slot-value bot 'jasb::name)
                                :icon_emoji (slot-value bot 'jasb::icon)))
  (jasb:reject bot (format nil "I need two arguments.~%`request <command_name> <command_description>`"))))
