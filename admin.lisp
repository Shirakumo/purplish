#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(define-implement-hook admin
  (admin:define-panel cache purplish (:access (purplish admin cache) :icon "fa-database" :tooltip "Recache parts." :lquery (template "admin-cache.ctml"))
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

  (admin:define-panel settings purplish (:access (purplish admin conifg) :icon "fa-gears" :tooltip "Change the purplish configuration" :lquery (template "admin-config.ctml"))
    (with-actions (error info)
        ((:save
          (setf (config-tree :purplish :title) (post-var "title")
                (config-tree :purplish :description) (post-var "description")
                (config-tree :purplish :news) (post-var "news")
                (config-tree :purplish :thumb :width) (post-var "thumb-width")
                (config-tree :purplish :thumb :height) (post-var "thumb-height")
                (config-tree :purplish :thumb :gif) (post-var "thumb-gif")
                (config-tree :purplish :file :size-limit) (post-var "file-size-limit"))))
      (r-clip:process
       T :error error :info info)))

  (defun header-id (header)
    (format NIL "~a.~a" (pathname-name header) (pathname-type header)))

  (defun header-src (header)
    (format NIL "/static/purplish/headers/~a" (header-id header)))

  (admin:define-panel headers purplish (:access (purplish admin headers) :icon "fa-image" :tooltip "Add or remove header images." :lquery (template "admin-headers.ctml"))
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
       T :error error :info info :headers (uiop:directory-files *headers*)))))
