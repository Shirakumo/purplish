#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defvar *headers* (static-file "headers/"))
(defvar *files* (static-file "file/"))
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
  ((:image/jpeg :image/png :image/gif :image/x-ms-bmp :image/svg+xml)
   (file name)
   (let ((thumb (make-pathname :name (format NIL "thumb-~a" (pathname-name file)) :defaults file)))
     (clip:process (template "files/image.ctml") :file file :file-thumb thumb :name name)))

  ((:video/mp4 :video/webm :video/ogg)
   (file name) (clip:process (template "files/video.ctml") :file file :name name))

  ((:audio/mpeg :audio/x-wav :audio/ogg)
   (file name) (clip:process (template "files/audio.ctml") :file file :name name))

  ((:application/pdf)
   (file name) (clip:process (template "files/general.ctml") :file file :name name)))

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
    (when (and (integerp (config-tree :purplish :file :size-limit))
               (<= (config-tree :purplish :file :size-limit)
                   (/ (file-size (first file)) 1024 1024)))
      (error "File is too big. Must be below ~aMb" (config-tree :purplish :file :size-limit)))))

(defun create-thumb (file mime)
  (when (find mime '(:image/jpeg :image/png :image/gif :image/x-ms-bmp :image/svg+xml) :test #'string-equal)
    (thumbnail:create
     file NIL
     :width (or* (config-tree :purplish :thumb :width) 150)
     :height (or* (config-tree :purplish :thumb :height) 150)
     :preserve-gif (config-tree :purplish :thumb :gif)
     :if-exists :warn)))

(defun create-file (post file)
  (let ((path (first file))
        (mime (mimes:mime-lookup (second file)))
        (name (if (< 128 (length (second file)))
                  (subseq (second file) (- (length (second file)) 128))
                  (second file))))
    (unless (find mime *allowed-types* :test #'string-equal)
      (error "Files of type ~s are not allowed." mime))
    (with-model model ('purplish-files NIL)
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

(define-page static-files (#@"/static/purplish/([^/]+)/(.+)" 1001) (:uri-groups (type path))
  (when (or (string-equal type "file")
            (string-equal type "headers"))
    (setf (header "Cache-Control") "public, max-age=31536000"))
  (serve-file (static-file (format NIL "~a/~a" type path))))
