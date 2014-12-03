#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defvar *external-embedders* (make-hash-table :test 'equalp))

(defun external-embedder (name)
  (or (gethash name *external-embedders*)
      #'(lambda (address) (declare (ignore address)) NIL)))

(defun (setf external-embedder) (func name)
  (setf (gethash name *external-embedders*) func))

(defmacro define-external-embedder (name (address) &body body)
  `(setf (external-embedder ,(string name))
         #'(lambda (,address) (block NIL ,@body))))

(defun youtube-code (url)
  (let ((pieces (nth-value 1 (cl-ppcre:scan-to-strings "((http|https)://)?(www\\.)?(youtube\\.com|youtu\\.be)/(watch\\?v=)?([0-9a-zA-Z_\\-]{4,12})" url))))
    (when pieces (aref pieces 5))))

(define-external-embedder youtube (address)
  (format NIL "<iframe width=\"100%\" height=\"240\" frameborder=\"no\" allowfullscreen=\"yes\" src=\"//www.youtube.com/embed/~a\"></iframe>"
          (or (youtube-code address) (return))))

(defun vimeo-code (url)
  (let ((pieces (nth-value 1 (cl-ppcre:scan-to-strings "((http|https)://)?(www\\.)?vimeo\\.com/([0-9]+)" url))))
    (when pieces (aref pieces 3))))

(define-external-embedder vimeo (address)
  (format NIL "<iframe width=\"100%\" height=\"240\" frameborder=\"no\" allowfullscreen=\"yes\" src=\"//player.vimeo.com/video/~a\"></iframe>"
          (or (vimeo-code address) (return))))

(defun vine-code (url)
  (let ((pieces (nth-value 1 (cl-ppcre:scan-to-strings "((http|https)://)?(www\\.)?vine\\.co/v/([0-9a-zA-Z]+)" url))))
    (when pieces (aref pieces 3))))

(define-external-embedder vine (address)
  (format NIL "<iframe class=\"vine-embed\" src=\"https://vine.co/v/~a/embed/postcard\" width=\"320\" height=\"320\" frameborder=\"no\"></iframe><script async src=\"//platform.vine.co/static/scripts/embed.js\" charset=\"utf-8\"></script>"
          (or (vine-code address) (return))))

(define-external-embedder soundcloud (address)
  (format NIL "<iframe width=\"100%\" height=\"166\" frameborder=\"no\" src=\"//w.soundcloud.com/player/?url=~a\"></iframe>"
          address))

(defun bandcamp-code (url)
  (ignore-errors
   (when (cl-ppcre:scan "^((http|https)://)?(www\\.)?([0-9a-zA-Z_\\-]+)\\.bandcamp\\.com/(album|track)/([^/]+)$" url)
     (let* ((drakma:*text-content-types* (cons '("application" . "json") (cons '("text" . "json") drakma:*text-content-types*)))
            (resp (drakma:http-request "http://api.bandcamp.com/api/url/1/info" :parameters `(("key" . "vatnajokull")
                                                                                              ("url" . ,url)))))
       (let ((json (cl-json:decode-json-from-string resp)))
         (list (cdr (assoc :band--id json))
               (cdr (assoc :album--id json))
               (cdr (assoc :track--id json))))))))

(define-external-embedder bandcamp (address)
  (destructuring-bind (band album track) (or (bandcamp-code address) (return))
    (declare (ignorable band))
    (format NIL "<iframe width=\"100%\" height=\"42\" frameborder=\"no\" src=\"//bandcamp.com/EmbeddedPlayer/~@[album=~a/~]size=small/bgcol=ffffff/linkcol=0687f5/~@[track=~a/~]transparent=true/\" seamless></iframe>"
            album track)))

(defun embed-external (target start end match-start match-end reg-starts reg-ends)
  (declare (ignore start end))
  (let ((label (subseq target (aref reg-starts 0) (aref reg-ends 0)))
        (address (subseq target (aref reg-starts 1) (aref reg-ends 1))))
    (setf address (cl-ppcre:regex-replace-all "\\\"" address "%22"))
    (or (funcall (external-embedder label) address)
        (subseq target match-start match-end))))

(defun preparse (text)
  (setf text (cl-ppcre:regex-replace-all ">>([0-9]+)" text "<a href=\"/post/\\1\" class=\"post-reference\">&gt;&gt;\\1</a>"))
  (setf text (cl-ppcre:regex-replace-all "\\!?\\[([a-zA-Z]+)\\]\\(([^)]+)\\)" text #'embed-external))
  (setf text (cl-ppcre:regex-replace-all "\\|\\?(.*?)\\?\\|" text "<span class=\"spoiler\">\\1</span>"))
  ;; temporary hack fix to circumvent 3bmd crashing, ugh.
  (setf text (cl-ppcre:regex-replace-all "(> *){5,}" text ">>>>>")))

(defun sanitize (node)
  (lquery:$ node "script,link,frame,frameset,embed,object,applet" (remove))
  (lquery:$ node "[onclick],[onfocus],[onblur],[onmouseover],[onmouseout],[ondoubleclick],[onload],[onunload]"
            (each #'(lambda (node) (loop for attr being the hash-keys of (plump:attributes node)
                                         when (and (< 2 (length attr)) (string-equal attr "on" :end1 2))
                                         do (remhash attr (plump:attributes node))))))
  (lquery:$ node "[href^=javascript],[href^=jscript]" (remove-attr :href))
  (lquery:$ node "[src^=javascript],[src^=jscript]" (remove-attr :src))
  node)

(defun parse (text)
  (let ((3bmd:*smart-quotes* T)
        (3bmd-code-blocks:*code-blocks* T))
    (let ((doc (plump:parse
                (with-output-to-string (stream)
                  (3bmd:parse-string-and-print-to-stream
                   (preparse text) stream)))))
      (sanitize doc))))
