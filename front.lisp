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
          (plump::slurp-stream stream)))
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
  )

(define-page search #@"chan/search" ()
  )
