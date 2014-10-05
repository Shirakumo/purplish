#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun serve-or-err (file error-message)
  (setf (content-type *response*) "application/xhtml+xml")
  (or (with-open-file (stream file :if-does-not-exist NIL)
        (when stream
          (let ((doc (plump:parse stream))
                (user (auth:current)))
            (when user
              (lquery:$ doc "#replybox .author" (val (user:username user))))
            (lquery:$
              doc ".post"
              (each #'(lambda (node)
                        (unless (and user
                                     (or (string-equal (lquery:$ node "a[rel=author]" (text) (node)) (user:username user))
                                         (user:check user '(pruplish post change))))
                          (lquery:$ node "nav.edit" (remove))))))
            (when (get-var "error")
              (lquery:$ doc "body>header" (after (format NIL "<div class=\"error\">~a</div>" (get-var "error")))))
            (with-output-to-string (stream)
              (plump:serialize doc stream)))))
      (error 'request-not-found :message error-message)))

(define-page frontpage #@"chan/" ()
  (serve-or-err (front-cache) "Frontpage not found."))

(define-page board #@"chan/board/([0-9a-zA-Z\\-]+)" (:uri-groups (board))
  (serve-or-err (board-cache board) "Board not found."))

(define-page thread #@"chan/thread/([0-9]+)" (:uri-groups (thread))
  (serve-or-err (thread-cache thread) "Thread not found."))

(define-page post #@"chan/post/([0-9]+)" (:uri-groups (post))
  (serve-or-err (post-cache post) "Post not found."))

(define-page history #@"chan/history/([0-9]+)" (:uri-groups (post))
  "Poo.")

(define-page search #@"chan/search" ()
  "Poo.")

(define-page user #@"chan/user/([^/]+)" (:uri-groups (user))
  "Poo.")
