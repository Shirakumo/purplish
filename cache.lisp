#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defvar *cache* (asdf:system-relative-pathname :purplish "cache/"))
(defvar *themes* (@static "theme/"))

(defun boards ()
  (dm:get 'purplish-boards (db:query (:= 'visible 1)) :sort '((name :ASC))))

(defun themes ()
  (mapcar #'pathname-name (uiop:directory-files *themes* #p"*.css")))

(defmacro with-retry-restart ((name format &rest args) &body body)
  (let ((return (gensym "RETURN")))
    `(loop with ,return = NIL
           until (with-simple-restart (,name ,format ,@args)
                   (setf ,return
                         (progn ,@body))
                   T)
           finally (return ,return))))

(defmacro with-deleting-restart ((delete-func object &rest args) &body body)
  (let ((obj (gensym "OBJECT")))
    `(let ((,obj ,object))
       (restart-case
           (progn ,@body)
         (delete ()
           :report (lambda (s) (format s "Delete the offending ~s" ,obj))
           (l:warn :purplish-cache "Deleting cache-offending object ~s" ,obj)
           (,delete-func ,obj ,@args))))))

(defmacro with-cache-file ((stream path descriptor) &body body)
  (let ((temp (gensym "TEMP"))
        (err (gensym "ERROR")))
    `(let* ((,path ,descriptor)
            (,temp (make-pathname :type "tmp" :defaults ,path))
            (*package* (find-package "RAD-USER")))
       (handler-bind ((error #'(lambda (,err)
                                 (l:error :purplish-cache "[~a] during caching of ~s: ~a"
                                          (type-of ,err) ,path ,err))))
         (restart-case
             (with-retry-restart (retry "Retry caching ~a" ,path)
               (ensure-directories-exist ,temp)
               (with-open-file (,stream ,temp :direction :output :if-exists :supersede)
                 ,@body)
               (uiop:rename-file-overwriting-target ,temp ,path))
           (stub ()
             :report "Create a stub file instead."
             (with-open-file (,stream ,path :direction :output :if-exists :supersede)
               (format ,stream "<div class=\"error\">Error generating cache!<br />~a</div>" ,path)))
           (bail ()
             :report "Bail out, potentially leaving no cache file behind (dangerous)."))))))

(defun front-cache (&optional ensure-cached)
  (merge-pathnames "frontpage.html" *cache*))

(defun board-cache (board &optional ensure-cached)
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
    (l:debug :purplish-cache "Recaching Post ~a" (dm:id post))
    (with-deleting-restart (delete-post post :author "SYSTEM" :purge T)
      (with-cache-file (stream path (post-cache post))
        (plump:serialize
         (clip:process
          (plump:parse (@template "post.ctml"))
          :post post
          :files (dm:get 'purplish-files (db:query (:= 'parent (dm:id post))))
          :revision revision)
         stream)
        path))
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
    (l:debug :purplish-cache "Recaching Thread ~a" (dm:id thread))
    (when cascade
      (recache-post thread :propagate NIL)
      (dolist (post posts)
        (recache-post post :propagate NIL)))

    (with-deleting-restart (delete-post thread :author "SYSTEM" :purge T)
      (when full
        (with-cache-file (stream path (thread-cache thread))
          (plump:serialize
           (clip:process
            (plump:parse (@template "thread.ctml"))
            :title (let ((revision (last-revision thread)))
                     (if revision
                         (dm:field revision "title")
                         (dm:field thread "title")))
            :board `(:_id ,(dm:field thread "board")) :thread thread :posts posts)
           stream)
          path))

      (with-cache-file (stream path (thread-min-cache thread))
        (plump:serialize
         (clip:process
          (plump:parse (@template "thread-min.ctml"))
          :thread thread :posts (loop for minposts = posts
                                      then (cdr minposts)
                                      repeat (- (length posts) 3)
                                      finally (return minposts)))
         stream)
        path))

    (when propagate
      (recache-board (dm:field thread "board")))))

(defun recache-board (board &key cascade)
  (let* ((board (ensure-board board))
         (threads (dm:get 'purplish-posts (db:query (:and (:= 'board (dm:id board))
                                                          (:= 'parent -1)))
                          :sort '((updated :DESC)))))
    (l:debug :purplish-cache "Recaching Board ~a" (dm:id board))
    (when cascade
      (dolist (thread threads)
        (recache-thread thread :cascade T :propagate NIL)))
    (with-deleting-restart (delete-board board)
      (with-cache-file (stream path (board-cache board))
        (plump:serialize
         (clip:process
          (plump:parse (@template "board.ctml"))
          :title (dm:field board "name")
          :board board :threads threads)
         stream)
        path))))

(defun recache-frontpage ()
  (l:debug :purplish-cache "Recaching Frontpage")
  (with-cache-file (stream path (front-cache))
    (plump:serialize
     (clip:process
      (plump:parse (@template "frontpage.ctml"))
      :title (config :title)
      :posts (dm:get 'purplish-posts (db:query (:= 'revision 0))
                     :amount 20 :sort '((time :DESC))))
     stream)))

(defun prune-cache ()
  (l:warn :purplish-cache "Pruning cache")
  (when (probe-file *cache*)
    (uiop:delete-directory-tree *cache* :validate (constantly T)))
  (ensure-directories-exist *cache*))

(defun recache-all ()
  (prune-cache)
  (l:info :purplish-cache "Recaching all")
  (dolist (board (dm:get 'purplish-boards (db:query :all)))
    (recache-board board :cascade T)
    (recache-atom :board board))
  (recache-frontpage)
  (recache-atom))

;; Template helpers
(lquery:define-lquery-function purplish-cache (node type object)
  (let ((path (ecase type
                (:board (board-cache object))
                (:thread (thread-cache object))
                (:thread-min (thread-min-cache object))
                (:post (post-cache object))))
        (retry T))
    (loop 
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
        (cond (stream
               ;; We don't want to parse the cached file again just to serialise it anew
               ;; so we cheat by changing this to a fulltext element and abusing its direct
               ;; serialisation.
               (change-class node 'plump:fulltext-element
                             :children (plump:make-child-array))
               (plump:make-text-node node (plump::slurp-stream stream))
               (return))
              (retry
               (l:warn :purplish-cache "Attempting to recache ~s as a cache-fetch occurred but we could not find the file ~s!"
                       object path)
               (setf retry NIL)
               (handler-bind ((error #'(lambda (err)
                                         (declare (ignore err))
                                         (invoke-restart 'stub))))
                 (ecase type
                   (:board (recache-board object))
                   (:thread (recache-thread object :full T :propagate NIL))
                   (:thread-min (recache-thread object :full NIL :propagate NIL))
                   (:post (recache-post object :propagate NIL)))))
              (T
               (l:severe :purplish-cache "WTF! Still failed to find cache for ~s at ~s despite recaching (with stub) attempt!"
                       object path)
               (let ((div (plump:make-element node "div")))
                 (setf (plump:attribute div "class") "error")
                 (plump:make-text-node div (format NIL "Failed to fetch cache for ~a" path)))
               (return))))))
  node)

(lquery:define-lquery-function purplish-template (node object)
  (setf (plump:children node) (plump:make-child-array))
  (plump:parse (@template (format NIL "~(~a~).ctml" object)) :root node)
  node)
