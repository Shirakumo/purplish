#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun show-error (doc &optional (error (get-var "error")))
  (when error
    (lquery:$ doc "body>header" (after (format NIL "<div class=\"error\">~a</div>" error)))))

(defun remove-inaccessible-options (doc &optional (user (auth:current)))
  (lquery:$
    doc ".post"
    (each #'(lambda (node)
              (if (and user
                       (or (string-equal (lquery:$ node "a[rel=author]" (text) (node)) (user:username user))
                           (user:check user (perm purplish post change))))
                  (progn (unless (user:check user (perm purplish post move))
                           (lquery:$ node "nav.edit .move-button" (remove)))
                         (unless (user:check user (perm purplish post purge))
                           (lquery:$ node "nav.edit .purge-button" (remove))))
                  (lquery:$ node "nav.edit" (remove)))))))

;;;;
;; Static
(defun serve-or-err (file error-message &optional (content-type "application/xhtml+xml"))
  (setf (content-type *response*) content-type)
  (or (with-open-file (stream file :if-does-not-exist NIL)
        (when stream
          (let ((doc (plump:parse stream))
                (user (auth:current)))
            (when user
              (lquery:$ doc "#replybox .author" (val (user:username user)))
              (if (implementation :admin)
                  (lquery:$ doc "#user-panel-link" (attr :href (uri-to-url (resource 'domain 'admin) :representation :external)) (text "UCP"))
                  (lquery:$ doc "#user-panel-link" (remove))))
            (remove-inaccessible-options doc user)
            (show-error doc)
            (with-output-to-string (stream)
              (plump:serialize doc stream)))))
      (error 'request-not-found :message error-message)))

(define-page frontpage #@"chan/" ()
  (serve-or-err (front-cache) "Frontpage not found."))

(define-page board #@"chan/board/([0-9a-zA-Z\\-]+)" (:uri-groups (board))
  (serve-or-err (board-cache board) "Board not found."))

(define-page thread #@"chan/thread/([0-9]+)" (:uri-groups (thread))
  (serve-or-err (thread-cache thread) "Thread not found."))

;;;;
;; Redirects
(define-page post #@"chan/post/([0-9]+)" (:uri-groups (post))
  (let ((post (ensure-post post)))
    (redirect (external-pattern "chan/thread/{0}#post-{1}"
                                (if (= -1 (dm:field post "parent"))
                                    (dm:id post)
                                    (dm:field post "parent"))
                                (dm:id post)))))

(define-page user #@"chan/user/([^/]+)" (:uri-groups (user))
  (redirect (external-pattern "<profile:user;{0}>" user)))

;;;;
;; Dynamics
(defmacro with-dynamic-env ((doc template) &body body)
  (let ((stream (gensym "STREAM")))
    `(let ((,doc (plump:parse (template ,template)))
           (*package* ,(find-package "RAD-USER")))
       (setf (content-type *response*) "application/xhtml+xml")
       ,@body
       (with-output-to-string (,stream)
         (plump:serialize ,doc ,stream)))))

(define-page edit #@"chan/edit/([0-9]+)" (:uri-groups (post))
  (let ((post (ensure-post post)))
    (unless post
      (error 'request-not-found :message "No such post."))
    (with-dynamic-env (doc "edit.ctml")
      (clip:process
       doc
       :title "Post History"
       :post post
       :revision (or (last-revision post) post))
      (show-error doc))))

(define-page history #@"chan/history/([0-9]+)" (:uri-groups (post))
  (let ((post (ensure-post post)))
    (unless post
      (error 'request-not-found :message "No such post."))
    (with-dynamic-env (doc "history.ctml")
      (clip:process
       doc
       :title "Post History"
       :post post
       :revisions (dm:get 'purplish-posts (db:query (:and (:= 'parent (dm:id post))
                                                          (:> 'revision 0)))))
      (lquery:$ doc ".revisions nav.edit" (remove))
      (remove-inaccessible-options doc))))

(rate:define-rate search (time-left :timeout 10)
  (with-dynamic-env (doc "search.ctml")
    (clip:process
     doc
     :title "Search Results")
    (show-error doc (format NIL "Please wait ~a seconds before searching again." time-left))))

(define-page search #@"chan/search" ()
  (rate:with-rate-limitation (search)
    (with-dynamic-env (doc "search.ctml")
      (clip:process
       doc
       :title "Search Results"
       :posts (search-posts (post/get "s") :thread (or* (post/get "thread")) :board (or* (post/get "board"))))
      (remove-inaccessible-options doc))))
