#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(define-implement-hook admin
  (admin:define-panel cache purplish (:access (perm purplish admin cache) :icon "fa-database" :tooltip "Recache parts." :lquery (template "admin-cache.ctml"))
    (with-actions (error info)
        ((:recache
          (dolist (cache (post/get "type[]"))
            (cond ((string= cache "front")
                   (recache-frontpage)
                   (push "Frontpage" info))
                  ((string= cache "board")
                   (recache-board (parse-integer (post/get "board"))
                                  :cascade (string= "cascade" "true"))
                   (push "Board" info))
                  ((string= cache "thread")
                   (recache-thread (parse-integer (post/get "thread"))
                                   :cascade (string= "cascade" "true")
                                   :propagate (string= "propagate" "true"))
                   (push "Thread" info))
                  ((string= cache "post")
                   (recache-post (parse-integer (post/get "post"))
                                 :propagate (string= "propagate" "true"))
                   (push "Post" info))
                  ((string= cache "atom")
                   (recache-atom :board (let ((board (parse-integer (post/get "atom"))))
                                          (unless (= board -1) board)))
                   (push "Atom" info))))))
      (r-clip:process
       T
       :error error :info (when info (format NIL "Recached ~{~a~^, ~}" info))
       :boards (boards))))

  (admin:define-panel settings purplish (:access (perm purplish admin config) :icon "fa-gears" :tooltip "Change the purplish configuration" :lquery (template "admin-config.ctml"))
    (with-actions (error info)
        ((:save
          (setf (config :title) (post-var "title")
                (config :description) (post-var "description")
                (config :news) (post-var "news")
                (config :thumb :width) (post-var "thumb-width")
                (config :thumb :height) (post-var "thumb-height")
                (config :thumb :gif) (post-var "thumb-gif")
                (config :file :size-limit) (post-var "file-size-limit"))))
      (r-clip:process
       T :error error :info info)))

  (defun header-id (header)
    (format NIL "~a.~a" (pathname-name header) (pathname-type header)))

  (defun header-src (header)
    (format NIL "/static/purplish/headers/~a" (header-id header)))

  (admin:define-panel headers purplish (:access (perm purplish admin headers) :icon "fa-image" :tooltip "Add or remove header images." :lquery (template "admin-headers.ctml"))
    (with-actions (error info)
        ((:delete
          (dolist (file (or (post-var "selected[]") (list (post-var "file"))))
            (delete-file (merge-pathnames file *headers*)))
          (setf info "Header deleted."))
         (:upload
          (let ((target))
            (loop do (setf target (merge-pathnames (format NIL "~a.~a"
                                                           (make-random-string 16)
                                                           (mimes:mime-file-type (mimes:mime-lookup (second (post-var "file")))))
                                                   *headers*))
                  while (probe-file target))
            (uiop:copy-file (first (post-var "file")) target))
          (setf info "Header uploaded.")))
      (r-clip:process
       T :error error :info info :headers (uiop:directory-files *headers*))))

  (admin:define-panel themes purplish (:access (perm purplish admin themes) :icon "fa-paint-brush" :tooltip "Manage available chan themes." :lquery (template "admin-themes.ctml"))
    (with-actions (error info)
        ((:delete
          (dolist (name (or (post-var "selected[]") (list (post-var "name"))))
            (ignore-errors (delete-file (make-pathname :name name :type "css" :defaults *themes*)))
            (ignore-errors (delete-file (make-pathname :name name :type "js" :defaults *themes*))))
          (setf info "Themes deleted."))
         (:upload
          (let ((target (make-pathname :name (post-var "name") :defaults *themes*)))
            (uiop:copy-file (first (post-var "css")) (make-pathname :type "css" :defaults target))
            (when (post-var "js")
              (uiop:copy-file (first (post-var "js")) (make-pathname :type "js" :defaults target)))
            (setf info "Theme uploaded."))))
      (r-clip:process
       T :error error :info info :themes (themes))))

  (admin:define-panel boards purplish (:access (perm purplish admin boards) :icon "fa-newspaper-o" :tooltip "Create or remove boards." :lquery (template "admin-boards.ctml"))
    (with-actions (error info)
        ((:delete
          (dolist (name (or (post-var "selected[]") (list (post-var "name"))))
            (delete-board name))
          (setf info "Boards deleted."))
         (:create
          (create-board (post-var "name") (post-var "description") (when (string= (post-var "visible") "true") T))
          (setf info "Board created.")))
      (r-clip:process
       T :error error :info info :boards (dm:get 'purplish-boards (db:query :all))))))
