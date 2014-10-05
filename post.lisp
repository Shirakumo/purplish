#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun create-board (name description &optional (visible T))
  (with-model board ('purplish-boards NIL)
    (setf (dm:field board "name") name
          (dm:field board "description") (or description "")
          (dm:field board "visible") (if visible 1 0))
    (dm:insert board)
    ;; Update header and init board
    (dolist (board (dm:get 'purplish-boards (db:query :all)))
      (recache-board board :cascade T))
    (recache-frontpage)
    board))

(defun delete-board (board)
  (let ((board (ensure-board board)))
    (let ((id (dm:id board)))
      (ignore-errors
       (uiop:delete-directory-tree (merge-pathnames (format NIL "~a/" id) *files*) :validate (constantly T)))
      (ignore-errors
       (uiop:delete-directory-tree (merge-pathnames (format NIL "~a/" id) *cache*) :validate (constantly T)))
      (delete-file (board-cache board))
      (db:remove 'purplish-posts (db:query (:= 'board id)))
      (db:remove 'purplish-files (db:query (:= 'board id)))
      (dm:delete board)
      ;; Update header
      (dolist (board (dm:get 'purplish-boards (db:query :all)))
        (recache-board board :cascade T))
      (recache-frontpage))
    T))

(defun create-post (board parent title text files &optional author (registered 0) (revision 0))
  (with-model post ('purplish-posts NIL)
    (dolist (file files)
      (check-file file))    
    (setf (dm:field post "board") board
          (dm:field post "parent") parent
          (dm:field post "revision") revision
          (dm:field post "author") (or* author "Anonymous")
          (dm:field post "registered") (or* registered 0)
          (dm:field post "time") (get-universal-time)
          (dm:field post "title") title
          (dm:field post "text") text)
    (dm:insert post)
    ;; Shuffle files around
    (dolist (file files)
      (create-file post file))
    ;; Publicise!
    (if (= parent -1)
        (recache-thread post)
        (recache-post post))
    post))

(defun delete-post (post &key purge)
  (let ((post (ensure-post post)))
    (cond
      (purge
       (let ((id (dm:id post)))
         (dm:delete post)
         ;; Purge files
         (dolist (file (dm:get 'purplish-files (db:query (:= 'parent id))))
           (delete-file (merge-pathnames (format NIL "~a/~a.~a"
                                                 (dm:field file "board")
                                                 (dm:id file)
                                                 (dm:field file "type")) *files*)))
         (db:remove 'purplish-files (db:query (:= 'parent id)))
         (when (= (dm:field post "parent") -1)
           ;; Purge each post's revisions.
           (dolist (post (dm:get 'purplish-posts (db:query (:= 'parent id))))
             (db:remove 'purplish-posts (db:query (:= 'parent (dm:id post))))))
         ;; Purge main posts & revisions
         (db:remove 'purplish-posts (db:query (:= 'parent id)))
         ;; Publicise!
         (cond
           ((= (dm:field post "parent") -1)
            (recache-board (dm:field post "board"))
            (recache-frontpage))
           (T
            (recache-thread (dm:field post "parent"))))))
      (T
       (edit-post post "" "_deleted_")))
    T))

(defun edit-post (post title text)
  (let ((post (ensure-post post)))
    ;; Create a new post with increased revision number.
    (let ((revision (or (last-revision post)
                        post)))
      (with-model edit ('purplish-posts NIL)
        (setf (dm:field edit "board") (dm:field revision "board")
              (dm:field edit "parent") (dm:id post)
              (dm:field edit "revision") (1+ (dm:field revision "revision"))
              (dm:field edit "author") (user:username (auth:current))
              (dm:field edit "registered") 1
              (dm:field edit "time") (get-universal-time)
              (dm:field edit "title") title
              (dm:field edit "text") text)
        (dm:insert edit)))
    ;; Publicise!
    (recache-post post)
    post))
