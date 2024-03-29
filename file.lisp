(in-package #:org.tymoonnext.radiance.purplish)

(defvar *headers* (ensure-directories-exist
                   (environment-module-pathname #.*package* :data "header/")))
(defvar *files* (ensure-directories-exist
                 (environment-module-pathname #.*package* :data "file/")))
(defvar *file-embedders* (make-hash-table :test 'equalp))
(defvar *allowed-types* ())

(defmacro define-file-embedder (type (file name) &body body)
  (destructuring-bind (mime &optional (ext (mimes:mime-file-type (string mime))))
      (if (listp type) type (list type))
    `(progn
       (setf (gethash ,(string ext) *file-embedders*)
             #'(lambda (,file ,name)
                 ,@body))
       (pushnew ,(string mime) *allowed-types* :test #'string-equal))))

(defmacro define-for-multiple (&body defs)
  `(progn
     ,@(loop for (types . body) in defs
             appending (loop for type in types
                             collect `(define-file-embedder ,type ,@body)))))

(define-for-multiple
  ((:image/jpeg :image/png :image/gif :image/x-ms-bmp :image/svg+xml :image/webp)
   (file name)
   (let ((thumb (make-pathname :name (format NIL "thumb-~a" (pathname-name file)) :defaults file)))
     (clip:process (@template "files/image.ctml") :file file :file-thumb thumb :name name)))

  ((:video/mp4 :video/webm :video/ogg)
   (file name) (clip:process (@template "files/video.ctml") :file file :name name))

  ((:audio/mpeg :audio/x-wav :audio/ogg)
   (file name) (clip:process (@template "files/audio.ctml") :file file :name name))

  ((:text/plain :text/html :text/css :text/javascript :application/x-javascript
    :application/pdf :application/epub+zip :application/x-mobipocket-ebook
    :message/rfc822 :application/xml)
   (file name) (clip:process (@template "files/general.ctml") :file file :name name)))

(defun file-path (file)
  (format NIL "/static/purplish/file/~a/~a.~a"
          (dm:field file "board")
          (dm:id file)
          (dm:field file "type")))

(defun embed-file (file)
  (let ((file (ensure-file file)))
    (funcall (gethash (dm:field file "type") *file-embedders*)
             (file-path file)
             (dm:field file "filename"))))

(defun check-file (file)
  (let ((mime (mimes:mime-lookup (second file))))
    (if mime
        (unless (find mime *allowed-types* :test #'string-equal)
          (error "Files of type ~s are not allowed." mime))
        (error "Unknown file format."))
    (when (and (integerp (config :file :size-limit))
               (<= (config :file :size-limit)
                   (/ (file-size (first file)) 1024 1024)))
      (error "File is too big. Must be below ~aMb" (config :file :size-limit)))))

(defun create-thumb (file mime)
  (when (find mime '(:image/jpeg :image/png :image/gif :image/x-ms-bmp :image/svg+xml :image/webp) :test #'string-equal)
    (thumbnail:create
     file NIL
     :width (or* (config :thumb :width) 150)
     :height (or* (config :thumb :height) 150)
     :preserve-gif (config :thumb :gif)
     :if-exists :warn)))

(defun create-file (post file)
  (let ((path (first file))
        (mime (mimes:mime-lookup (second file)))
        (name (if (< 128 (length (second file)))
                  (subseq (second file) (- (length (second file)) 128))
                  (second file))))
    (unless (find mime *allowed-types* :test #'string-equal)
      (error "Files of type ~s are not allowed." mime))
    (dm:with-model model ('files NIL)
      (setf (dm:field model "board") (dm:field post "board")
            (dm:field model "parent") (dm:id post)
            (dm:field model "type") (mimes:mime-file-type mime)
            (dm:field model "filename") name)
      (dm:insert model)
      (let ((new-file (merge-pathnames
                       (format NIL "~a/~a.~a"
                               (dm:field model "board")
                               (dm:id model)
                               (dm:field model "type"))
                       *files*)))
        (ensure-directories-exist new-file)
        ;; We can't use rename-file across devices, so just copy it.
        (uiop:copy-file path new-file)
        (create-thumb new-file mime)))))

(defun random-header ()
  (let ((headers (uiop:directory-files *headers*)))
    (nth (random (length headers))
         headers)))

(define-page static-files ("/static/purplish/([^/]+)/(.+)" 1001) (:uri-groups (type path))
  (cond ((string= type "file")
         (setf (header "Cache-Control") "public, max-age=31536000")
         (serve-file (merge-pathnames (parse-path-safely path) *files*)))
        ((string= type "headers")
         (setf (header "Cache-Control") "public, max-age=31536000")
         (serve-file (merge-pathnames (parse-path-safely path) *headers*)))
        ((string= type "theme")
         (serve-file (merge-pathnames (parse-path-safely path) *themes*)))
        (T
         (error 'request-not-found))))
