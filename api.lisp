#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(define-api purplish/boards () ()
  (api-output (db:select 'purplish-boards (db:query (:= 'visibility "p")))))

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

(define-api purplish/thread/posts (thread &optional skip amount) ()
  (let ((skip (or (parse-integer skip :junk-allowed T) 0))
        (amount (or (parse-integer amount :junk-allowed T) 20))
        (thread (dm:get-one 'purplish-posts (db:query (:= '_id (or (parse-integer thread :junk-allowed T) "-1"))))))
    (unless thread
      (error 'api-argument-invalid :argument 'thread :message "No such thread found."))
    (api-output (db:select 'purplish-posts (db:query (:and (:= 'parent (dm:field thread '_id))
                                                           (:= 'revision 0)))
                           :amount amount :skip skip :sort '((time :DESC))))))

(define-api purplish/post (post) ()
  (let ((post (first (db:select 'purplish-posts (db:query (:and (:= '_id (or (parse-integer post :junk-allowed T) "-1"))
                                                                (:= 'revision 0)))))))
    (unless post
      (error 'api-argument-invalid :argument 'post :message "No such post found."))
    (let ((revision (dm:get-one 'purplish-posts (db:query (:= 'parent (gethash "_id" post))) :sort '((revision :DESC))))
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
  (let ((post (dm:get-one 'purplish-posts (db:query (:= '_id (or (parse-integer post :junk-allowed T) "-1"))))))
    (unless post
      (error 'api-argument-invalid :argument 'post :message "No such post found."))
    (api-output (db:select 'purplish-files (db:query (:= 'post (dm:field post '_id)))))))

(define-api purplish/board/create (title &optional description visibility) (:access (purplish admin boards))
  )

(define-api purplish/thread/create (title text files &optional author) ()
  )

(define-api purplish/thread/delete (thread) ()
  )

(define-api purplish/post/create (thread &optional author title text files) ()
  )

(define-api purplish/post/edit (post &optional title text) ()
  )

(define-api purplish/post/delete (post) ()
  )
