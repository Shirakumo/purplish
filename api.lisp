#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defmacro with-board ((board id) &body body)
  (let ((bid (gensym "ID")))
    `(let* ((,bid (or (parse-integer ,id :junk-allowed T) ,id))
            (,board (dm:get-one 'purplish-boards (etypecase ,bid
                                                   (fixnum (db:query (:= '_id ,bid)))
                                                   (string (db:query (:= 'title ,bid)))))))
       (unless ,board
         (error 'api-argument-invalid :argument ',id :message "No such board found."))
       ,@body)))

(defmacro with-post ((post id) &body body)
  (let ((pid (gensym "ID")))
    `(let* ((,pid ,id)
            (,post (dm:get-one 'purplish-posts (db:query (:= '_id (or (parse-integer ,pid :junk-allowed T) "-1"))))))
       (unless ,post
         (error 'api-argument-invalid :argument ',id :message "No such post found."))
       ,@body)))

(defmacro with-api-error (&body body)
  `(handler-case (progn ,@body)
     (radiance-error (err)
       (error 'api-error :message (message err)))
     (error (err)
       (error 'api-error :message (princ-to-string err)))))

(defun post-accessible-p (post)
  (let ((user (auth:current)))
    (and user
         (or (user:check user '(purplish post change))
             (and (= (dm:field post "registered") 1)
                  (string-equal (dm:field post "author")
                                (user:username user)))))))

(defmacro with-post-accessible ((post-dm) &body body)
  `(progn
     (unless (post-accessible-p ,post-dm)
       (error 'api-auth-error :message "You do not have permission to change this post."))
     ,@body))

(rate:define-rate post (time-left :timeout 10)
  (error 'api-error :message (format NIL "Please wait ~a seconds before posting again." time-left)))

(rate:define-rate api-search (time-left :timeout 10)
  (error 'api-error :message (format NIL "Please wait ~a seconds between searches." time-left)))

(define-api purplish/boards () ()
  (api-output (db:select 'purplish-boards (db:query (:= 'visible 1)))))

(define-api purplish/search (query) ()
  (api-output (search-posts query :func 'db:select)))

(define-api purplish/board/threads (board &optional skip amount) ()
  (let ((skip (or (parse-integer skip :junk-allowed T) 0))
        (amount (or (parse-integer amount :junk-allowed T) 20))
        (board (dm:get-one 'purplish-boards (db:query (:= 'name board)))))
    (unless board
      (error 'api-argument-invalid :argument 'board :message "No such board found."))
    (api-output (db:select 'purplish-posts (db:query (:and (:= 'board (dm:field board '_id))
                                                           (:= 'parent -1)
                                                           (:= 'revision 0)))
                           :amount amount :skip skip :sort '((time :DESC))))))

(define-api purplish/board/search (board query) ()
  (let ((board (dm:get-one 'purplish-boards (db:query (:= 'name board)))))
    (unless board
      (error 'api-argument-invalid :argument 'board :message "No such board found."))
    (api-output (search-posts query :board board :func 'db:select))))

(define-api purplish/thread/posts (thread &optional skip amount) ()
  (let ((skip (or (parse-integer skip :junk-allowed T) 0))
        (amount (or (parse-integer amount :junk-allowed T) 20))
        (thread (dm:get-one 'purplish-posts (db:query (:and (:= '_id (or (parse-integer thread :junk-allowed T) "-1"))
                                                            (:= 'parent -1))))))
    (unless thread
      (error 'api-argument-invalid :argument 'thread :message "No such thread found."))
    (api-output (db:select 'purplish-posts (db:query (:and (:= 'parent (dm:field thread '_id))
                                                           (:= 'revision 0)))
                           :amount amount :skip skip :sort '((time :DESC))))))

(define-api purplish/thread/search (thread query) ()
  (rate:with-rate-limitation (api-search)
    (let ((thread (dm:get-one 'purplish-posts (db:query (:and (:= '_id (or (parse-integer thread :junk-allowed T) "-1"))
                                                              (:= 'parent -1))))))
      (unless thread
        (error 'api-argument-invalid :argument 'thread :message "No such thread found."))
      (api-output (search-posts query :thread thread :func 'db:select)))))

(define-api purplish/post (post) ()
  (let ((post (first (db:select 'purplish-posts (db:query (:and (:= '_id (or (parse-integer post :junk-allowed T) "-1"))
                                                                (:= 'revision 0)))))))
    (unless post
      (error 'api-argument-invalid :argument 'post :message "No such post found."))
    (let ((revision (last-revision post))
          (files (db:select 'purplish-files (db:query (:= 'post (gethash "_id" post))))))
      (setf (gethash "revision" post) (dm:field revision "revision")
            (gethash "editor" post) (dm:field revision "author")
            (gethash "edit-time" post) (dm:field revision "time")
            (gethash "files" post) files)
      (api-output post))))

(define-api purplish/post/revisions (post) ()
  (let ((posts (db:select 'purplish-posts (db:query (:and (:= 'parent (or (parse-integer post :junk-allowed T) "-1"))
                                                          (:> 'revision 0)))
                          :sort '((time :DESC)))))
    (unless posts
      (error 'api-argument-invalid :argument 'post :message "No such post found."))
    (api-output posts)))

(define-api purplish/post/files (post) ()
  (with-post (post post)
    (api-output (db:select 'purplish-files (db:query (:= 'post (dm:field post '_id)))))))

(define-api purplish/board/create (name &optional description visible) (:access (purplish board create))
  (when (dm:get-one 'purplish-boards (db:query (:= 'title name)))
    (error 'api-argument-invalid :argument 'name :message "A board with that name already exists."))
  (with-api-error (create-board name description (string= visible "true")))
  (if (string= (post/get "browser") "true")
      (redirect (format NIL "/board/~a" name))
      (api-output "Board created.")))

(define-api purplish/board/delete (board) (:access (purplish board delete))
  (with-board (board board)
    (with-api-error (delete-board board))
    (if (string= (post/get "browser") "true")
        (redirect "/")
        (api-output "Board deleted."))))

(define-api purplish/thread/create (board title text files[] &optional author username) ()
  (unless (or (not username) (string= username ""))
    (error 'api-argument-invalid :argument 'username :message "Hi, spambot."))
  (rate:with-rate-limitation (post)
    (with-board (board board)
      (let ((thread (with-api-error (create-post (dm:id board) -1 title text files[] author
                                                 (if (and author (auth:current) (string-equal (user:username (auth:current)) author)) 1 0)))))
        (if (string= (post/get "browser") "true")
            (redirect (format NIL "/thread/~a" (dm:id thread)))
            (api-output "Thread created."))))))

(define-api purplish/thread/delete (thread) ()
  (with-post (thread thread)
    (unless (= (dm:field thread "parent") -1)
      (error 'api-argument-invalid :argument 'thread :message "This isn't a thread."))
    (unless (user:check (auth:current) '(purplish thread delete))
      (error 'api-auth-error :message "You do not have permission to delete threads."))
    (with-api-error (delete-post thread :purge T))
    (if (string= (post/get "browser") "true")
        (redirect (format NIL "/board/~a" (dm:field thread "board")))
        (api-output "Thread purged."))))

(define-api purplish/post/create (thread &optional author title text files[] username) ()
  (unless (or (not username) (string= username ""))
    (error 'api-argument-invalid :argument 'username :message "Hi, spambot."))
  (unless (or* title text files[])
    (error 'api-argument-missing :argument 'text :message "Title, text or file required."))
  (rate:with-rate-limitation (post)
    (with-post (thread thread)
      (unless (= (dm:field thread "parent") -1)
        (error 'api-argument-invalid :argument 'thread :message "This isn't a thread."))
      (let ((post (with-api-error
                    (create-post (dm:field thread "board") (dm:id thread) title text files[] author
                                 (if (and author (auth:current) (string-equal (user:username (auth:current)) author)) 1 0)))))        
        (if (string= (post/get "browser") "true")
            (redirect (format NIL "/post/~a" (dm:id post)))
            (api-output "Post created."))))))

(define-api purplish/post/edit (post &optional title text) ()
  (with-post (post post)
    (with-post-accessible (post)
      (with-api-error (edit-post post (user:username (auth:current)) title text))
      (if (string= (post/get "browser") "true")
          (redirect (format NIL "/post/~a" (dm:id post)))
          (api-output "Board created.")))))

(define-api purplish/post/delete (post &optional purge) ()
  (with-post (post post)
    (with-post-accessible (post)
      (cond
        ((and (string= purge "true")
              (user:check (auth:current) '(purplish post purge)))
         (with-api-error (delete-post post :purge T))
         (if (string= (post/get "browser") "true")
             (redirect (format NIL "/thread/~a" (dm:field post "parent")))
             (api-output "Post purged.")))
        ((string= purge "true")
         (error 'api-auth-error :message "You do not have permission to purge posts."))
        (T
         (with-api-error (delete-post post))
         (if (string= (post/get "browser") "true")
             (redirect (format NIL "/post/~a" (dm:id post)))
             (api-output "Post deleted.")))))))

(define-api purplish/header () ()
  (serve-file (random-header)))
