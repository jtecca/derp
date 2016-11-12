(in-package #:derp.cmds)

(setf *random-state* (make-random-state t))

;;;; ping
(defmethod ping ((bot derp::derp))
  "Mostly used to check if derp is working."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text "pong"
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))
;;;; help
(defmethod help ((bot derp::derp))
  "Displays known commands with description."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text "Working on it!"
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

;;;; unknown command
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

;;;; randuser
(defmethod rand-user ((bot derp::derp))
  "Picks random user from the channel."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text (format nil "And the winner is... *~A*! :tada:" (random-user bot))
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

;;;; review
(defmethod review ((bot derp::derp) user) ;; todo, check if there are at least 2 people in the channel
  "Picks two, random people from the channel."
  (let ((first-person (random-user bot))
        (second-person (random-user bot)))
    (loop while (string= first-person second-person)
       do (setf second-person (random-user bot)))
    (jasa.chat:post-message :token (slot-value bot 'derp::token)
                            :channel (slot-value bot 'derp::channel)
                            :text "*I'm so sorry...*"
                            :attachments (jasa.chat:prepare-attachments :text (format nil "<@~A> and <@~A>" first-person second-person)
                                                                        :color "good")
                            :username (slot-value bot 'derp::name)
                            :icon_emoji (slot-value bot 'derp::icon))))

;;;; direct channels
(defmethod get-user-id ((bot derp::derp) user)
  (car (rassoc user (slot-value bot 'derp::users) :test #'string=)))

(defmethod get-im-channel ((bot derp::derp) user)
  (let ((user-id (car (rassoc user (slot-value bot 'derp::users) :test #'string=)))
        (ims (cdadr (jasa.im:list-im :token (slot-value bot 'derp::token)))))
    (car (delete nil (mapcar #'(lambda (x) (if (string= (cdr (assoc :user x)) user-id) (cdr (assoc :id x)) nil)) ims)))))

;;;; cat
(defun get-cat-xml ()
  (cxml:parse
   (dex:get "http://thecatapi.com/api/images/get?format=xml&type=gif")
   (cxml-dom:make-dom-builder)))

(defun get-cat-url ()
  (dom:data
   (dom:first-child
    (dom:item
     (dom:get-elements-by-tag-name (dom:document-element (get-cat-xml)) "url") 0))))

(defmethod cat ((bot derp::derp))
  "Posts random cat gif."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text (get-cat-url)
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

(defmethod cat-private ((bot derp::derp) user)
  (let ((token (slot-value bot 'derp::token)))
    (jasa.im:open-im :token token :user (get-user-id bot user))
    (jasa.chat:post-message :token token
                            :channel (get-im-channel bot user)
                            :text (get-cat-url)
                            :username (slot-value bot 'derp::name)
                            :icon_emoji (slot-value bot 'derp::icon))))

;;;; dog
(defun get-dog-json ()
  (cl-json:decode-json-from-string
   (dex:get (format nil "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=funny+dogs"))))

(defun get-dog-url ()
  (cdr (assoc :image--original--url (cdar (get-dog-json)) :test #'string=)))

(defmethod dog ((bot derp::derp))
  "Posts random dog gif."
  (let ((token (slot-value bot 'derp::token))
        (channel (slot-value bot 'derp::channel)))
    (jasa.chat:post-message :token token
                            :channel channel
                            :text (get-dog-url)
                            :username (slot-value bot 'derp::name)
                            :icon_emoji (slot-value bot 'derp::icon))))

;;;; remove
(defmethod find-latest-message ((bot derp::derp))
  "Returns ts of the latest derp message."
  (mapcar #'(lambda (x) (if
                         (string= (slot-value bot 'derp::name) (cdadr x))
                         (return-from find-latest-message (cdr (assoc :ts x)))))
          (cdadr (jasa.channels:history :token (slot-value bot 'derp::token)
                                        :channel (slot-value bot 'derp::channel)))))

(defmethod remove-last-message ((bot derp::derp))
  "Removes last derp message."
  (jasa.chat:delete-message :token (slot-value bot 'derp::token)
                            :channel (slot-value bot 'derp::channel)
                            :ts (find-latest-message bot)))

;;;; joke
(defun fetch-chuck-joke ()
  (cl-json:decode-json-from-string
   (dex:get (format nil "http://api.icndb.com/jokes/random"))))

(defun get-joke-text ()
  (cdr (assoc :joke (cdr (car (cdr (fetch-chuck-joke)))))))

(defmethod joke ((bot derp::derp))
  "Posts random Chuck Norris joke."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text (format nil "\"~A\"" (get-joke-text))
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))

;;;; yes-no
(defmethod yesno ((bot derp::derp))
  "Posts random yes, no or maybe gif."
  (jasa.chat:post-message :token (slot-value bot 'derp::token)
                          :channel (slot-value bot 'derp::channel)
                          :text (cdr (assoc :image (cl-json:decode-json-from-string (dex:get "https://yesno.wtf/api/"))))
                          :username (slot-value bot 'derp::name)
                          :icon_emoji (slot-value bot 'derp::icon)))
