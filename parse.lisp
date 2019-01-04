#|
 This file is a part of Purplish
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.purplish)

(defun youtube-code (url)
  (let ((pieces (nth-value 1 (cl-ppcre:scan-to-strings "((http|https)://)?(www\\.)?(youtube\\.com|youtu\\.be)/(watch\\?v=)?([0-9a-zA-Z_\\-]{4,12})" url))))
    (when pieces (aref pieces 5))))

(defclass cl-markless-components::youtube (cl-markless-components:video)
  ())

(defmethod cl-markless:output-component ((component cl-markless-components::youtube) (target plump-dom:nesting-node) (format cl-markless-plump:plump))
  (let ((element (plump-dom:make-element target "iframe")))
    (setf (plump-dom:attribute element "width") "100%")
    (setf (plump-dom:attribute element "height") "240")
    (setf (plump-dom:attribute element "frameborder") "no")
    (setf (plump-dom:attribute element "allowfullscreen") "yes")
    (setf (plump-dom:attribute element "src")
          (format NIL "//www.youtube.com/embed/~a?" (youtube-code (cl-markless-components:target component))))
    (loop for option in (cl-markless-components:options component)
          do (typecase option
               (cl-markless-components:autoplay-option
                (setf (plump-dom:attribute element "src")
                      (format NIL "~aautoplay=1&" (plump-dom:attribute element "src"))))
               (cl-markless-components:loop-option
                (setf (plump-dom:attribute element "src")
                      (format NIL "~aloop=1&" (plump-dom:attribute element "src"))))
               (cl-markless-components:width-option
                (setf (plump-dom:attribute element "width")
                      (format NIL "~d~(~a~)"
                              (cl-markless-components:size option)
                              (cl-markless-components:unit option))))
               (cl-markless-components:height-option
                (setf (plump-dom:attribute element "height")
                      (format NIL "~d~(~a~)"
                              (cl-markless-components:size option)
                              (cl-markless-components:unit option))))
               (cl-markless-components:float-option
                (setf (plump-dom:attribute element "style")
                      (format NIL "float:~(~a~)" (cl-markless-components:direction option))))))))

(defclass quote-line (cl-markless-components:block-component cl-markless-components:parent-component)
  ())

(defmethod cl-markless:output-component ((component quote-line) (target plump-dom:nesting-node) (format cl-markless-plump:plump))
  (let ((element (plump-dom:make-element target "div")))
    (setf (plump-dom:attribute element "class") "quote")
    (loop for child across (cl-markless-components:children component)
          do (cl-markless:output-component child element format))))

(defclass post-reference (cl-markless-components:inline-component cl-markless-components:unit-component)
  ((post-id :initarg :post-id :initform (error "POST-ID required") :accessor post-id)))

(defmethod cl-markless:output-component ((component post-reference) (target plump-dom:nesting-node) (format cl-markless-plump:plump))
  (let ((element (plump-dom:make-element target "a")))
    (setf (plump-dom:attribute element "class") "post-reference")
    (setf (plump-dom:attribute element "href") (uri-to-url (format NIL "chan/post/~a" (post-id component))
                                                           :representation :external))
    (plump-dom:make-text-node element (format NIL ">>~a" (post-id component)))))

(defclass board-reference (cl-markless-components:inline-component cl-markless-components:unit-component)
  ((board-id :initarg :board-id :initform (error "BOARD-ID required") :accessor board-id)))

(defmethod cl-markless:output-component ((component board-reference) (target plump-dom:nesting-node) (format cl-markless-plump:plump))
  (let ((element (plump-dom:make-element target "a")))
    (setf (plump-dom:attribute element "class") "board-reference")
    (setf (plump-dom:attribute element "href") (uri-to-url (format NIL "chan/board/~a" (board-id component))
                                                           :representation :external))
    (plump-dom:make-text-node element (format NIL ">>/~a/" (board-id component)))))


(defclass greentext-block (cl-markless:singular-line-directive)
  ())

(defmethod cl-markless:prefix ((_ greentext-block))
  #(">" " "))

(defmethod cl-markless:begin ((_ greentext-block) parser line cursor)
  (cond ((or (<= (length line) (1+ cursor))
             (char= #\> (aref line (1+ cursor))))
         (cl-markless::delegate-paragraph parser line cursor))
        (T
         (cl-markless:commit _ (make-instance 'quote-line) parser)
         cursor)))

(defclass greentext-ref (cl-markless:inline-directive)
  ())

(defmethod cl-markless:prefix ((_ greentext-ref))
  #(">" ">"))

(defmethod cl-markless:begin ((_ greentext-ref) parser line cursor)
  (let* ((stack-top (cl-markless:stack-top (cl-markless:stack parser)))
         (children (cl-markless-components:children (cl-markless:stack-entry-component stack-top)))
         (end cursor))
    
    (loop while (and (< end (length line))
                     (char= #\> (aref line end)))
          do (incf end))
    (let ((next (if (< end (length line)) (aref line end) #\Nul)))
      (cond ((<= (char-code #\0) (char-code next) (char-code #\9))
             (let ((mid end))
               (loop while (and (< end (length line))
                                (<= (char-code #\0) (char-code (aref line end)) (char-code #\9)))
                     do (incf end))
               (vector-push-extend (make-instance 'post-reference :post-id (subseq line mid end)) children)))
            ((char= next #\/)
             (let ((mid end))
               (loop do (incf end)
                     while (and (< end (length line))
                                (char/= #\/ (aref line end))))
               (let ((next (if (< end (length line)) (aref line end) #\Nul)))
                 (cond ((and (/= mid end) (char= #\/ next))
                        (vector-push-extend (make-instance 'board-reference :board-id (subseq line (1+ mid) end)) children)
                        (incf end))
                       (T
                        (vector-push-extend (subseq line cursor end) children))))))
            (T
             (vector-push-extend (subseq line cursor end) children))))
    end))

(defparameter *markless-directives*
  '(cl-markless:paragraph
    cl-markless:blockquote-header
    cl-markless:blockquote
    cl-markless:unordered-list
    cl-markless:ordered-list
    cl-markless:header
    cl-markless:horizontal-rule
    cl-markless:code-block
    cl-markless:embed
    cl-markless:bold
    cl-markless:italic
    cl-markless:underline
    cl-markless:strikethrough
    cl-markless:code
    cl-markless:dash
    cl-markless:supertext
    cl-markless:subtext
    cl-markless:compound
    cl-markless:url
    cl-markless:newline
    greentext-block
    greentext-ref))

(defun parse (text)
  (let ((parser (make-instance 'cl-markless:parser :directives *markless-directives*
                                                   :stack-size-limit 128))
        (node (plump-dom:make-root))
        (text (cl-ppcre:regex-replace-all "\\r\\n" text (string #\Linefeed))))
    (cl-markless:output (cl-markless:parse text parser)
                        :target node :format 'cl-markless-plump:plump)
    node))
