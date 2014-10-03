#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defvar *cache* (asdf:system-relative-pathname :purplish "cache/"))

(defmacro with-cache-file ((stream path descriptor) &body body)
  `(let ((,path ,descriptor))
     (ensure-directories-exist ,path)
     (with-open-file (,stream ,path :direction :output)
       ,@body)))

(defun front-cache ()
  (merge-pathnames "frontpage.html" *cache*))

(defun board-cache (board)
  (let ((board (ensure-board board)))
    (merge-pathnames (format NIL "board/~a.html" (dm:field board "name"))
                     *cache*)))

(defun thread-cache (thread)
  (let ((thread (ensure-post thread)))
    (merge-pathnames (format NIL "~a/thread/~a.html" (dm:field thread "board") (dm:field thread "_id"))
                     *cache*)))

(defun thread-min-cache (thread)
  (let ((thread (ensure-post thread)))
    (merge-pathnames (format NIL "~a/thread-min/~a.html" (dm:field thread "board") (dm:field thread "_id"))
                     *cache*)))

(defun post-cache (post)
  (let ((post (ensure-post post)))
    (merge-pathnames (format NIL "~a/post/~a.html" (dm:field post "board") (dm:field post "_id"))
                     *cache*)))

(defun recache-frontpage ()
  (with-cache-file (stream path (front-cache))
    (plump:serialize
     (clip:process
      (plump:parse (template "frontpage.ctml"))
      :posts (dm:get 'purplish-posts (db:query :all) :amount 20 :sort '((time :DESC))))
     stream)))

(defun recache-post (post &key (propagate T))
  (let* ((post (ensure-post post))
         (revision (last-revision post)))
    (with-cache-file (stream path (post-cache post))
      (plump:serialize
       (clip:process
        (plump:parse (template "post.ctml"))
        :post post :revision revision)
       stream)
      path)
    (when propagate
      (recache-frontpage)
      (recache-thread (dm:field post "parent")))))

(defun recache-thread (thread &key cascade (propagate T) (full T))
  (let* ((thread (ensure-post thread))
         (posts (dm:get 'purplish-posts (db:query (:and (:= 'parent (dm:field thread "_id"))
                                                        (:= 'revision 0))))))
    (when cascade
      (dolist (post posts)
        (recache-post post :propagate NIL)))

    (when full
      (with-cache-file (stream path (thread-cache thread))
        (plump:serialize
         (clip:process
          (plump:parse (template "thread.ctml"))
          :thread thread :posts posts)
         stream)
        path))

    (with-cache-file (stream path (thread-min-cache thread))
      (plump:serialize
       (clip:process
        (plump:parse (template "thread-min.ctml"))
        :thread thread :posts posts)
       stream)
      path)

    (when propagate
      (recache-board (dm:field thread "board")))))

(defun recache-board (board &key cascade)
  (let ((board (ensure-board board))
        (threads (dm:get 'purplish-posts (db:query (:and (:= 'board board)
                                                         (:= 'parent -1))))))
    (when cascade
      (dolist (thread threads)
        (recache-thread thread :cascade T :propagate NIL)))

    (with-cache-file (stream path (board-cache board))
      (plump:serialize
       (clip:process
        (plump:parse (template "board.ctml"))
        :board board :threads threads)
       stream)
      path)))
