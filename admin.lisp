#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(define-implement-hook admin
  (admin:define-panel cache purplish (:access (purplish admin cache) :icon "fa-database" :tooltip "Recache parts." :lquery (template "admin-cache.ctml"))
    (with-actions (error info)
        ((:recache
          (dolist (cache (post/get "type[]"))
            (cond ((string= cache "front")
                   (recache-frontpage)
                   (push "Frontpage" info))
                  ((string= cache "board")
                   (recache-board (parse-integer (post/get "board"))
                                  :cascade (string= "cascade" "true"))
                   (push "Board" info))
                  ((string= cache "thread")
                   (recache-thread (parse-integer (post/get "thread"))
                                   :cascade (string= "cascade" "true")
                                   :propagate (string= "propagate" "true"))
                   (push "Thread" info))
                  ((string= cache "post")
                   (recache-post (parse-integer (post/get "post"))
                                 :propagate (string= "propagate" "true"))
                   (push "Post" info))
                  ((string= cache "atom")
                   (recache-atom :board (let ((board (parse-integer (post/get "atom"))))
                                          (unless (= board -1) board)))
                   (push "Atom" info))))))
      (r-clip:process
       T
       :error error :info (when info (format NIL "Recached ~{~a~^, ~}" info))
       :boards (boards)))))
