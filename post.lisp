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

(defun last-revision (post)
  (let ((id (etypecase post
              (dm:data-model (dm:field post "_id"))
              (fixnum post)
              (string (parse-integer post)))))
    (dm:get-one 'purplish-posts (db:query (:= 'parent id))
                :sort '((revision :DESC)))))

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

(defun date-machine (stamp)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '((:year 4) "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2)))))

(defun date-human (stamp)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '((:year 4) "." (:month 2) "." (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))))

(defun date-fancy (stamp)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '(:long-weekday ", " :ordinal-day " of " :long-month " " :year ", " :hour ":" :min ":" :sec))))

(lquery:define-lquery-function purplish-time (node time)
  (let ((stamp (local-time:universal-to-timestamp time)))
    (setf (plump:attribute node "datetime")
          (date-machine stamp))
    (setf (plump:attribute node "title")
          (date-fancy stamp))
    (setf (plump:children node) (plump:make-child-array))
    (plump:make-text-node node (date-human stamp))))

(defun recache-post (post &key (propagate T))
  (let* ((post (ensure-post post))
         (revision (last-revision post)))
    (with-cache-file (stream path (format NIL "~a/post/~a.html" (dm:field post "board") (dm:field post "_id")))
      (plump:serialize
       (clip:process
        (plump:parse (template "post.ctml"))
        :post post :revision revision)
       stream)
      path)
    (when propagate
      (recache-thread (dm:field post "parent")))))

(defun recache-thread (thread &key cascade (propagate T) (full T))
  (let* ((thread (ensure-post thread))
         (posts (dm:get 'purplish-posts (db:query (:and (:= 'parent (dm:field thread "_id"))
                                                        (:= 'revision 0))))))
    (when cascade
      (dolist (post posts)
        (recache-post post :propagate NIL)))

    (when full
      (with-cache-file (stream path (format NIL "~a/thread/~a.html" (dm:field thread "board") (dm:field thread "_id")))
        (plump:serialize
         (clip:process
          (plump:parse (template "thread.ctml"))
          :thread thread :posts posts)
         stream)
        path))

    (with-cache-file (stream path (format NIL "~a/thread-min/~a.html" (dm:field thread "board") (dm:field thread "_id")))
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

    (with-cache-file (stream path (format NIL "board/~a.html" (dm:field 'board "name")))
      (plump:serialize
       (clip:process
        (plump:parse (template "board.ctml"))
        :board board :threads threads)
       stream)
      path)))

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

(defun create-post (board parent title text files &optional (author (auth:current) (revision 0)))
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
