(in-package #:org.tymoonnext.radiance.purplish)

(define-hook board-created (id))
(define-hook board-deleted (id))
(define-hook post-created (id))
(define-hook post-deleted (id))
(define-hook post-purged (id))
(define-hook post-edited (id edit-id))
(define-hook post-moved (id old-thread-id))
(define-hook thread-moved (id old-board-id))

(defun create-board (name description &optional (visible T))
  (when (dm:get-one 'boards (db:query (:= 'name name)))
    (error "A board with that name already exists!"))
  (dm:with-model board ('boards NIL)
    (db:with-transaction ()
      (setf (dm:field board "name") name
            (dm:field board "description") (or description "")
            (dm:field board "visible") (if visible 1 0))
      (dm:insert board)
      ;; Update header and init board
      (dolist (board (dm:get 'boards (db:query :all)))
        (recache-board board :cascade T))
      (recache-frontpage))
    (trigger 'board-created (dm:id board))
    board))

(defun delete-board (board)
  (let ((board (ensure-board board)))
    (let ((id (dm:id board)))
      (ignore-errors
       (uiop:delete-directory-tree (merge-pathnames (format NIL "~a/" id) *files*) :validate (constantly T)))
      (ignore-errors
       (uiop:delete-directory-tree (merge-pathnames (format NIL "~a/" id) *cache*) :validate (constantly T)))
      (delete-file (board-cache board))
      (db:remove 'posts (db:query (:= 'board id)))
      (db:remove 'files (db:query (:= 'board id)))
      (dm:delete board)
      ;; Update header
      (dolist (board (dm:get 'boards (db:query :all)))
        (recache-board board :cascade T))
      (recache-frontpage)
      (trigger 'board-deleted id))
    T))

(defun create-post (board parent title text files &optional author (registered 0) (revision 0))
  (dm:with-model post ('posts NIL)
    (dolist (file files)
      (check-file file))
    (db:with-transaction ()
      (setf (dm:field post "board") board
            (dm:field post "parent") parent
            (dm:field post "revision") revision
            (dm:field post "author") (or* author "Anonymous")
            (dm:field post "registered") (or* registered 0)
            (dm:field post "time") (get-universal-time)
            (dm:field post "updated") (get-universal-time)
            (dm:field post "title") (or title "")
            (dm:field post "text") (or text ""))
      (dm:insert post)

      ;; Shuffle files around
      (dolist (file files)
        (create-file post file))
      ;; Bump
      (unless (= parent -1)
        (db:update 'posts (db:query (:= '_id parent)) `(("updated" . ,(get-universal-time)))))
      ;; Publicise!
      (recache-post post))
    (trigger 'post-created (dm:id post))
    post))

(defun delete-post (post &key (author (user:username (auth:current))) purge)
  (let ((post (ensure-post post)))
    (cond
      (purge
       (let ((id (dm:id post)))
         (dm:delete post)
         ;; Purge files
         (dolist (file (dm:get 'files (db:query (:= 'parent id))))
           (delete-file (merge-pathnames (format NIL "~a/~a.~a"
                                                 (dm:field file "board")
                                                 (dm:id file)
                                                 (dm:field file "type")) *files*)))
         (db:remove 'files (db:query (:= 'parent id)))
         (when (= (dm:field post "parent") -1)
           ;; Purge each post's revisions.
           (dolist (post (dm:get 'posts (db:query (:= 'parent id))))
             (db:remove 'posts (db:query (:= 'parent (dm:id post))))))
         ;; Purge main posts & revisions
         (db:remove 'posts (db:query (:= 'parent id)))
         ;; Publicise!
         (cond
           ((= (dm:field post "parent") -1)
            (recache-board (dm:field post "board"))
            (recache-frontpage))
           (T
            (recache-thread (dm:field post "parent"))
            (recache-frontpage)))
         (trigger 'post-purged id)))
      (T
       (edit-post post (or author "Anonymous") "" "_deleted_" :delete T)
       (trigger 'post-deleted (dm:id post))))
    T))

(defun edit-post (post author title text &key delete)
  (let ((post (ensure-post post)))
    ;; Create a new post with increased revision number.
    (let ((revision (or (last-revision post)
                        post)))
      (dm:with-model edit ('posts NIL)
        (db:with-transaction ()
          (setf (dm:field edit "board") (dm:field revision "board")
                (dm:field edit "parent") (dm:id post)
                (dm:field edit "revision") (1+ (dm:field revision "revision"))
                (dm:field edit "author") author
                (dm:field edit "registered") 1
                (dm:field edit "deleted") (if delete 1 0)
                (dm:field edit "time") (get-universal-time)
                (dm:field edit "updated") (get-universal-time)
                (dm:field edit "title") (or title "")
                (dm:field edit "text") (or text ""))
          (dm:insert edit)
          ;; Publicise!
          (recache-post post))
        (trigger 'post-edited (dm:id post) (dm:id edit))))
    post))

(defun move-post (post new-thread)
  (let* ((post (ensure-post post))
         (old-thread (dm:field post "parent"))
         (new-thread (ensure-thread new-thread)))
    (when (= old-thread -1)
      (error "Post is a thread."))
    (db:with-transaction ()
      ;; Update revisions
      (db:update 'posts (db:query (:= 'parent (dm:id post)))
                 `((board . ,(dm:field new-thread "board"))))
      ;; Update post
      (setf (dm:field post "board") (dm:field new-thread "board")
            (dm:field post "parent") (dm:id new-thread))
      (dm:save post)
      ;; Publicise!
      (recache-post post)
      (recache-thread old-thread))
    (trigger 'post-moved (dm:id post) old-thread)
    post))

(defun move-thread (thread new-board)
  (let* ((thread (ensure-thread thread))
         (old-board (dm:field thread "board"))
         (new-board (ensure-board new-board)))
    (db:with-transaction ()
      ;; Update revisions
      (dolist (post (dm:get 'posts (db:query (:= 'parent (dm:id thread)))))
        (db:update 'posts (db:query (:= 'parent (dm:id post)))
                   `((board . ,(dm:id new-board)))))
      ;; Update posts
      (db:update 'posts (db:query (:= 'parent (dm:id thread)))
                 `((board . ,(dm:id new-board))))
      ;; Update thread
      (setf (dm:field thread "board") (dm:id new-board))
      (dm:save thread)
      ;; Publicise!
      (recache-thread thread :cascade T)
      (recache-board old-board))
    (trigger 'thread-moved (dm:id thread) old-board)
    thread))
