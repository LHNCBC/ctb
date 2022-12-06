(ns ctb.sessions
  (:import (java.lang System)))

(defn wrap-expire-sessions
  [hdlr & {:keys [inactive-timeout
                  hard-timeout]
            :or {inactive-timeout (* 1000 60 15)
                 hard-timeout (* 1000 60 60 2)}}]
  (fn [req]
    (let [now (System/currentTimeMillis)
          session (:session req)
          session-key (:session/key req)]
      (if session-key ;; there is a session
        (let [{:keys [last-activity session-created]} session]
          (if (and last-activity
                   (< (- now last-activity) inactive-timeout)
                   session-created
                   (< (- now session-created) hard-timeout))
            (let [resp (hdlr req)]
              (if (:session resp)
                (-> resp
                    (assoc-in [:session :last-activity] now)
                    (assoc-in [:session :session-created] session-created))
                resp))
            ;; expired session
            ;; block request and delete session
            {:body "Your session has expired."
             :status 401
             :headers {}
             :session nil}))
        ;; no session, just call the handler
        ;; assume friend or other system will handle it
        (hdlr req)))))
