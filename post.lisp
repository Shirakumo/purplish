#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defvar *cache* (asdf:system-relative-pathname :purplish "cache/"))

(defmacro with-cache-file ((stream path descriptor) &body body)
  `(let ((,path (merge-pathnames ,descriptor *cache*)))
     (ensure-directories-exist ,path)
     (with-open-file (,stream ,path :direction :output)
       ,@body)))

(defun ensure-post (post)
  (etypecase post
    (dm:data-model post)
    (fixnum (dm:get-one 'purplish-posts (db:query (:= '_id post))))
    (string (dm:get-one 'purplish-posts (db:query (:= '_id (parse-integer post)))))))

(defun ensure-board (board)
  (etypecase board
    (dm:data-model board)
    (fixnum (dm:get-one 'purplish-boards (db:query (:= '_id board))))
    (string (dm:get-one 'purplish-boards (db:query (:= 'name board))))))

(defun recache-post (post)
  (let* ((post (ensure-post post))
         (revision (dm:get-one 'purplish-posts (db:query (:= 'parent (dm:field 'post "_id")))
                               :sort '((revision :DESC)))))
    (with-cache-file (stream path (format NIL "post/~a.html" (dm:field 'post "_id")))
      (plump:serialize
       (clip:process
        (plump:parse (template "post.ctml"))
        :post post :revision revision)
       stream)
      path)))

(defun recache-thread (thread &key cascade full)
  (let* ((thread (ensure-post thread))
         (posts (dm:get 'purplish-posts (db:query (:and (:= 'parent (dm:field 'thread "_id"))
                                                        (:= 'revision 0))))))
    (when cascade
      (dolist (post posts)
        (recache-post post)))

    (when full
      (with-cache-file (stream path (format NIL "thread/~a.html" (dm:field 'thread "_id")))
        (plump:serialize
         (clip:process
          (plump:parse (template "thread.ctml"))
          :thread thread :posts posts)
         stream)
        path))

    (with-cache-file (stream path (format NIL "thread-min/~a.html" (dm:field 'thread "_id")))
      (plump:serialize
       (clip:process
        (plump:parse (template "thread-min.ctml"))
        :thread thread :posts posts)
       stream)
      path)))

(defun recache-board (board &key cascade)
  (let ((board (ensure-board board))
        (threads (dm:get 'purplish-posts (db:query (:and (:= 'board board)
                                                         (:= 'parent -1))))))
    (when cascade
      (dolist (thread threads)
        (recache-thread thread)))

    (with-cache-file (stream path (format NIL "board/~a.html" (dm:field 'board "name")))
      (plump:serialize
       (clip:process
        (plump:parse (template "board.ctml"))
        :board board :threads threads)
       stream)
      path)))

(defun create-post (board parent title text files &optional (author (auth:current)))
  )

(defun delete-post (post &key purge)
  )

(defun edit-post (post title text)
  )
