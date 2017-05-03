(in-package #:derp.cmds)

(setf *random-state* (make-random-state t))

;;;; ping
(defmethod ping ((bot derp::derp))
  "Mostly used to check if derp is working."
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                           :channel (slot-value bot 'jasb::channel)
                           :text "pong"
                           :username (slot-value bot 'jasb::name)
                           :icon_emoji (slot-value bot 'jasb::icon)))
;;;; help
;;;; use "fields" here, https://api.slack.com/docs/message-attachments
(defmethod help ((bot derp::derp))
  "Displays known commands with description."
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                           :channel (slot-value bot 'jasb::channel)
                           :text "Working on it!"
                           :username (slot-value bot 'jasb::name)
                           :icon_emoji (slot-value bot 'jasb::icon)))

(defmethod fetch-users ((bot derp::derp))
  (cond ((char= #\C (char (slot-value bot 'jasb::channel) 0))
         (jasaw.channels:info :token (slot-value bot 'jasb::token)
                              :channel (slot-value bot 'jasb::channel)))
        ((char= #\G (char (slot-value bot 'jasb::channel) 0))
         (jasaw.groups:info :token (slot-value bot 'jasb::token)
                            :channel (slot-value bot 'jasb::channel)))
        (t (error "Derp channel field is invalid. Should be starting with C or G character."))))

(defmethod get-users ((bot derp::derp))
  (cdr (delete (slot-value bot 'jasb::id) (assoc :members (cdadr (fetch-users bot))) :test #'string=)))

(defmethod random-user ((bot derp::derp))
  (let* ((users (get-users bot))
         (winner (random (length users))))
    (derp::get-name bot (nth winner users))))

;;;; randuser
(defmethod rand-user ((bot derp::derp))
  "Picks random user from the channel."
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                           :channel (slot-value bot 'jasb::channel)
                           :text (format nil "And the winner is... *~A*! :tada:" (random-user bot))
                           :username (slot-value bot 'jasb::name)
                           :icon_emoji (slot-value bot 'jasb::icon)))

;;;; review
(defmethod review ((bot derp::derp)) ;; todo, check if there are at least 2 people in the channel
  "Picks two, random people from the channel."
  (let ((first-person (random-user bot))
        (second-person (random-user bot)))
    (loop while (string= first-person second-person)
       do (setf second-person (random-user bot)))
    (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                             :channel (slot-value bot 'jasb::channel)
                             :text "*I'm so sorry...*"
                             :attachments (jasaw.chat:prepare-attachments :text (format nil "<@~A> and <@~A>" first-person second-person)
                                                                          :color "good")
                             :username (slot-value bot 'jasb::name)
                             :icon_emoji (slot-value bot 'jasb::icon))))

;;;; direct channels
(defmethod get-user-id ((bot derp::derp) user)
  (car (rassoc user (slot-value bot 'jasb::users) :test #'string=)))

(defmethod get-im-channel ((bot derp::derp) user)
  (let ((user-id (car (rassoc user (slot-value bot 'jasb::users) :test #'string=)))
        (ims (cdadr (jasaw.im:list-im :token (slot-value bot 'jasb::token)))))
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
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                           :channel (slot-value bot 'jasb::channel)
                           :text (get-cat-url)
                           :username (slot-value bot 'jasb::name)
                           :icon_emoji (slot-value bot 'jasb::icon)))

(defmethod cat-private ((bot derp::derp) user)
  (let ((token (slot-value bot 'jasb::token)))
    (jasaw.im:open-im :token token :user (get-user-id bot user))
    (jasaw.chat:post-message :token token
                             :channel (get-im-channel bot user)
                             :text (get-cat-url)
                             :username (slot-value bot 'jasb::name)
                             :icon_emoji (slot-value bot 'jasb::icon))))

;;;; dog
(defun get-dog-json ()
  (cl-json:decode-json-from-string
   (dex:get (format nil "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=funny+dogs"))))

(defun get-dog-url ()
  (cdr (assoc :image--original--url (cdar (get-dog-json)) :test #'string=)))

(defmethod dog ((bot derp::derp))
  "Posts random dog gif."
  (let ((token (slot-value bot 'jasb::token))
        (channel (slot-value bot 'jasb::channel)))
    (jasaw.chat:post-message :token token
                             :channel channel
                             :text (get-dog-url)
                             :username (slot-value bot 'jasb::name)
                             :icon_emoji (slot-value bot 'jasb::icon))))

;;;; remove
(defmethod find-latest-message ((bot derp::derp))
  "Returns ts of the latest derp message."
  (mapcar #'(lambda (x) (if
                         (string= (slot-value bot 'jasb::name) (cdadr x))
                         (return-from find-latest-message (cdr (assoc :ts x)))))
          (cdadr (jasaw.channels:history :token (slot-value bot 'jasb::token)
                                         :channel (slot-value bot 'jasb::channel)))))

(defmethod remove-last-message ((bot derp::derp))
  "Removes last derp message."
  (jasaw.chat:delete-message :token (slot-value bot 'jasb::token)
                             :channel (slot-value bot 'jasb::channel)
                             :ts (find-latest-message bot)))

;;;; joke
(defun fetch-chuck-joke ()
  (cl-json:decode-json-from-string
   (dex:get (format nil "http://api.icndb.com/jokes/random"))))

(defun get-joke-text ()
  (cdr (assoc :joke (cdr (car (cdr (fetch-chuck-joke)))))))

(defmethod joke ((bot derp::derp))
  "Posts random Chuck Norris joke."
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                           :channel (slot-value bot 'jasb::channel)
                           :text (format nil "\"~A\"" (get-joke-text))
                           :username (slot-value bot 'jasb::name)
                           :icon_emoji (slot-value bot 'jasb::icon)))

;;;; yes-no
(defmethod yesno ((bot derp::derp))
  "Posts random yes, no or maybe gif."
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                           :channel (slot-value bot 'jasb::channel)
                           :text (cdr (assoc :image (cl-json:decode-json-from-string (dex:get "https://yesno.wtf/api/"))))
                           :username (slot-value bot 'jasb::name)
                           :icon_emoji (slot-value bot 'jasb::icon)))
;;;; random-number
(defmethod random-number ((bot derp::derp) args)
  "Returns random number."
  (let ((max (car args)))
    (if (not max)
        (jasb:reject bot (format nil "What is the maximum number? `randnumber <max_number>`")))
    (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                             :channel (slot-value bot 'jasb::channel)
                             :text (format nil ":game_die: ~A :game_die:" (random (parse-integer max)))
                             :username (slot-value bot 'jasb::name)
                             :icon_emoji (slot-value bot 'jasb::icon))))

;;;; features
(defun format-features (features)
  (if features
      (concatenate 'string (format nil "∙ `~A`~%" (car features)) (format-features (cdr features)))))

(defmethod features ((bot derp::derp))
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                           :channel (slot-value bot 'jasb::channel)
                           :attachments (jasaw.chat:prepare-attachments :title "Available features:"
                                                                        :text (format-features (slot-value bot 'derp::commands))
                                                                        :mrkdwn_in '("text")
                                                                        :color "good")
                           :username (slot-value bot 'jasb::name)
                           :icon_emoji (slot-value bot 'jasb::icon)))
;;;; dawid
(defmethod dawid ((bot derp::derp) args)
  (jasaw.chat:post-message :token (slot-value bot 'jasb::token)
                           :channel (slot-value bot 'jasb::channel)
                           :text (format nil "4.10.2016, 9:24AM \`@derp rand -1\` #neverforget")
                           :username (slot-value bot 'jasb::name)
                           :icon_emoji (slot-value bot 'jasb::icon))
  (jasaw.reactions::add-reaction :token (slot-value bot 'jasb::token)
                                 :channel (slot-value bot 'jasb::channel)
                                 :name "cs"
                                 :timestamp (car (last args))))
