(in-package #:derp.cmds)

(setf *random-state* (make-random-state t))

(defmethod ping ((bot derp::derp))
  "Mostly used to check if derp is working."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text "pong"
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

(defmethod help ((bot derp::derp))
  "Displays known commands with description."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text "Working on it!"
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

(defmethod other ((bot derp::derp))
  "Informs that given command is not supported."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text "I do not know this command."
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

(defmethod fetch-users ((bot derp::derp))
  (jasa.channels:info :token (slot-value bot 'derp::token)
                      :channel (slot-value bot 'derp::channel)))

(defmethod get-users ((bot derp::derp))
  (cdr (delete (slot-value bot 'derp::id) (assoc :members (cdadr (fetch-users bot))) :test #'string=)))

(defmethod random-user ((bot derp::derp))
  (let* ((users (get-users bot))
         (winner (random (length users))))
    (derp::get-name bot (nth winner users))))

(defmethod review ((bot derp::derp) user) ;; todo, check if there are at least 2 people in the channel
  "Picks two, random people from the channel."
  (let ((first-person (random-user bot))
        (second-person (random-user bot)))
    (loop while (string= first-person second-person)
       do (setf second-person (random-user bot)))
    (jasa.chat:post-message :token (slot-value bot 'derp::token)
                            :channel (slot-value bot 'derp::channel)
                            :attachments (jasa.chat:prepare-attachments :fallback ""
                                                                        :title "I'm so sorry..."
                                                                        :text (format nil "<@~A> and <@~A>" first-person second-person)
                                                                        :color "good")
                            :username (slot-value bot 'derp::name)
                            :icon_emoji (slot-value bot 'derp::icon))))

(defun get-cat-xml ()
  (cxml:parse
   (dex:get "http://thecatapi.com/api/images/get?format=xml&type=gif")
   (cxml-dom:make-dom-builder)))

(defun get-cat-url ()
  (dom:data
   (dom:first-child
    (dom:item
     (dom:get-elements-by-tag-name (dom:document-element (get-cat-xml)) "url") 0))))

;; (defmethod cat ((bot derp::derp))
;;   (jasa.chat:post-message :token (slot-value bot 'derp::token)
;;                           :channel (slot-value bot 'derp::channel)
;;                           :text (get-cat-url)
;;                           :username (slot-value bot 'derp::name)
;;                           :icon_emoji (slot-value bot 'derp::icon)))

(defmethod get-user-id ((bot derp::derp) user)
  (car (rassoc user (slot-value bot 'derp::users) :test #'string=)))

(defmethod get-im-channel ((bot derp::derp) user)
  (let ((user-id (car (rassoc user (slot-value bot 'derp::users) :test #'string=)))
        (ims (cdadr (jasa.im:list-im :token (slot-value bot 'derp::token)))))
    (car (delete nil (mapcar #'(lambda (x) (if (string= (cdr (assoc :user x)) user-id) (cdr (assoc :id x)) nil)) ims)))))

(defmethod cat ((bot derp::derp) user)
  (let ((token (slot-value bot 'derp::token)))
    (jasa.im:open-im :token token :user (get-user-id bot user))
    (jasa.chat:post-message :token token
                            :channel (get-im-channel bot user)
                            :text (get-cat-url)
                            :username (slot-value bot 'derp::name)
                            :icon_emoji (slot-value bot 'derp::icon))))
