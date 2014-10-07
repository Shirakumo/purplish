#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defvar *cache* (asdf:system-relative-pathname :purplish "cache/"))

(defun boards ()
  (dm:get 'purplish-boards (db:query (:= 'visible 1)) :sort '((name :ASC))))

(defun themes ()
  (mapcar #'pathname-name (uiop:directory-files (static-file "theme/*.css") #p"")))

(defmacro with-cache-file ((stream path descriptor) &body body)
  `(let ((,path ,descriptor)
         (*package* (find-package "RAD-USER")))
     (ensure-directories-exist ,path)
     (with-open-file (,stream ,path :direction :output :if-exists :supersede)
       ,@body)))

(defun front-cache ()
  (merge-pathnames "frontpage.html" *cache*))

(defun board-cache (board)
  (let ((board (ensure-board board)))
    (merge-pathnames (format NIL "board/~a.html" (dm:id board))
                     *cache*)))

(defun thread-cache (thread)
  (let ((thread (ensure-post thread)))
    (merge-pathnames (format NIL "~a/thread/~a.html" (dm:field thread "board") (dm:id thread))
                     *cache*)))

(defun thread-min-cache (thread)
  (let ((thread (ensure-post thread)))
    (merge-pathnames (format NIL "~a/thread-min/~a.html" (dm:field thread "board") (dm:id thread))
                     *cache*)))

(defun post-cache (post)
  (let ((post (ensure-post post)))
    (merge-pathnames (format NIL "~a/post/~a.html" (dm:field post "board") (dm:id post))
                     *cache*)))

(defun recache-post (post &key (propagate T))
  (let* ((post (ensure-post post))
         (revision (last-revision post)))
    (v:debug :purplish-cache "Recaching Post ~a" (dm:id post))
    (with-cache-file (stream path (post-cache post))
      (plump:serialize
       (clip:process
        (plump:parse (template "post.ctml"))
        :post post
        :files (dm:get 'purplish-files (db:query (:= 'parent (dm:id post))))
        :revision revision)
       stream)
      path)
    (when propagate
      (recache-frontpage)
      (if (= -1 (dm:field post "parent"))
          (recache-thread post)
          (recache-thread (dm:field post "parent"))))))

(defun recache-thread (thread &key cascade (propagate T) (full T))
  (let* ((thread (ensure-post thread))
         (posts (dm:get 'purplish-posts (db:query (:and (:= 'parent (dm:id thread))
                                                        (:= 'revision 0)))
                        :sort '((time :ASC)))))
    (v:debug :purplish-cache "Recaching Thread ~a" (dm:id thread))
    (when cascade
      (recache-post thread :propagate NIL)
      (dolist (post posts)
        (recache-post post :propagate NIL)))

    (when full
      (with-cache-file (stream path (thread-cache thread))
        (plump:serialize
         (clip:process
          (plump:parse (template "thread.ctml"))
          :title (dm:field thread "title")
          :thread thread :posts posts)
         stream)
        path))

    (with-cache-file (stream path (thread-min-cache thread))
      (plump:serialize
       (clip:process
        (plump:parse (template "thread-min.ctml"))
        :thread thread :posts (loop for minposts = posts
                                    then (cdr minposts)
                                    repeat (- (length posts) 3)
                                    finally (return minposts)))
       stream)
      path)

    (when propagate
      (recache-board (dm:field thread "board")))))

(defun recache-board (board &key cascade)
  (let* ((board (ensure-board board))
         (threads (dm:get 'purplish-posts (db:query (:and (:= 'board (dm:id board))
                                                          (:= 'parent -1)))
                          :sort '((updated :DESC)))))
    (v:debug :purplish-cache "Recaching Board ~a" (dm:id board))
    (when cascade
      (dolist (thread threads)
        (recache-thread thread :cascade T :propagate NIL)))

    (with-cache-file (stream path (board-cache board))
      (plump:serialize
       (clip:process
        (plump:parse (template "board.ctml"))
        :title (dm:field board "name")
        :board board :threads threads)
       stream)
      path)))

(defun recache-frontpage ()
  (v:debug :purplish-cache "Recaching Frontpage")
  (with-cache-file (stream path (front-cache))
    (plump:serialize
     (clip:process
      (plump:parse (template "frontpage.ctml"))
      :title (config-tree :purplish :title)
      :posts (dm:get 'purplish-posts (db:query (:= 'revision 0))
                     :amount 20 :sort '((time :DESC))))
     stream)))
