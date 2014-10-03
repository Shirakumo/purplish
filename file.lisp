#|
 This file is a part of Purplish
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defvar *files* (static-file "file/"))
(defvar *file-embedders* (make-hash-table :test 'equalp))
(defvar *allowed-types* ())

(defun ensure-file (file)
  (etypecase file
    (dm:data-model file)
    (fixnum (dm:get-one 'purplish-files (db:query (:= '_id file))))
    (string (dm:get-one 'purplish-files (db:query (:= '_id (parse-integer file)))))))

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
   (let ((thumb (merge-pathnames (make-pathname :name (format NIL "~a-thumb" (pathname-name file))) file)))
     (clip:process (template "files/image.ctml") :file file :file-thumb thumb :name name)))

  ((:video/mp4 :video/webm :video/ogg)
   (file name) (clip:process (template "files/video.ctml" :file file :name name)))

  ((:audio/mpeg :audio/x-wav :audio/ogg)
   (file name) (clip:process (template "files/audio.ctml" :file file :name name)))

  ((:application/pdf)
   (file name) (clip:process (template "files/general.ctml" :file file :name name))))

(defun file-path (file)
  (format NIL "/static/purplish/file/~a/~a.~a"
          (dm:field file "board")
          (dm:field file "_id")
          (dm:field file "type")))

(defun embed-file (file)
  (let ((file (ensure-file file)))
    (funcall (gethash (dm:field file "type") *file-embedders*)
             (file-path file)
             (dm:field file "filename"))))

(defun create-file (post file)
  (let ((path (first file))
        (mime (mimes:mime-probe (first file))))
    (unless (find mime *allowed-types :test #'string-equal)
      (error "Files of type ~s are not allowed." mime))
    (with-model model ('purplish-files NIL)
      (setf (dm:field model "board") (dm:field post "board")
            (dm:field model "parent") (dm:field post "_id")
            (dm:field model "type") (mimes:mime-file-type mime)
            (dm:field model "filename") (second file))
      (dm:insert model)
      (let ((new-file (merge-pathnames
                       (format NIL "~a/~a.~a"
                               (dm:field file "board")
                               (dm:field file "_id")
                               (dm:field file "type"))
                       *files*)))
        (ensure-directories-exist new-file)
        (rename-file path new-file)))))
