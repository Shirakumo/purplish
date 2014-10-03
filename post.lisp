#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun create-board (name description &optional (visible T))
  (db:insert 'purplish-boards `((name . ,name)
                                (description . ,(or description ""))
                                (visible . ,(if visible 1 0)))))

(defun delete-board (board)
  (let ((id (dm:field board "_id")))
    (uiop:delete-directory-tree (merge-pathnames (format NIL "~a/" id) *files*) :validate (constantly T))
    (uiop:delete-directory-tree (merge-pathnames (format NIL "~a/" id) *cache*) :validate (constantly T))
    (delete-file (merge-pathnames (format NIL "board/~a.html" id)))
    (db:remove 'purplish-posts (db:query (:= 'board id)))
    (db:remove 'purplish-files (db:query (:= 'board id)))
    (dm:delete board)))

(defun create-post (board parent title text files &optional (author (auth:current)) (revision 0))
  (with-model post ('purplish-posts NIL)
    (setf (dm:field post "board") board
          (dm:field post "parent") parent
          (dm:field post "revision") revision
          (dm:field post "author") author
          (dm:field post "registered") registered
          (dm:field post "title") title
          (dm:field post "text") text)
    (dm:insert post)
    
    (dolist (file files)
      (create-file post file))

    (if (= parent -1)
        (recache-thread post)
        (recache-post post))))

(defun delete-post (post &key purge)
  (cond
    (purge
     (let ((id (dm:field post "_id")))
       (dm:delete post)
       (dolist (file (dm:get 'purplish-files (db:query (:= 'parent id))))
         (delete-file (merge-pathnames (format NIL "~a/~a.~a"
                                               (dm:field file "board")
                                               (dm:field file "_id")
                                               (dm:field file "type")) *files*)))
       (db:remove 'purplish-files (db:query (:= 'parent id)))
       (cond
         ((= (dm:field post "parent") -1)
          (dolist (post (dm:get 'purplish-posts (db:query (:= 'parent id))))
            (db:remove 'purplish-posts (db:query (:= 'parent (dm:field post "_id")))))
          (db:remove 'purplish-posts (db:query (:= 'parent id)))
          (recache-board (dm:field post "board")))
         (T
          (recache-thread id)))))
    (T
     (edit-post post "" "__deleted__"))))

(defun edit-post (post title text)
  (let ((revision (last-revision post)))
    (with-model edit ('purplish-posts NIL)
      (setf (dm:field edit "board") (dm:field revision "board")
            (dm:field edit "parent") (dm:field revision "parent")
            (dm:field edit "revision") (1+ (dm:field revision "revision"))
            (dm:field edit "author") (user:username (auth:current))
            (dm:field edit "registered") 1
            (dm:field edit "title") title
            (dm:field edit "text") text)
      (dm:insert edit)))
  (recache-post post))
