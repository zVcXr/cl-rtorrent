(in-package #:bencode)

(define-condition unexpected-octet (error)
  ((the-octet :initarg :the-octet :reader the-octet)
   (expected-octets :initarg :expected-octets :reader expected-octets)))

(defvar *read-stream* nil)

(defun read-ascii-char (char)
  (let ((it (read-byte *read-stream*)))
    (unless (= it (ascii-char->octet char))
      (error 'unexpected-octet
	     :the-octet it
	     :expected-octets (->octets (ascii-char->octet char))))))

(defun read-until-ascii-char (char)
  (with-octets-builder octets
    (let-while it (read-byte *read-stream*) (/= it (ascii-char->octet char))
	       (put-value octets it))))

(defun read-string-length ()
  (parse-integer
   (octets-to-string
    (read-until-ascii-char #\:)
    :encoding :ascii)))

(defun read-string ()
  (let* ((length (read-string-length))
	 (octets (with-octets-builder octets
		   (dotimes (i length)
		     (put-value octets (read-byte *read-stream*))))))
    (handler-case (octets-to-string octets :encoding :utf-8)
      (character-decoding-error () octets))))

(defun read-integer ()
  (read-ascii-char #\i)
  (parse-integer
   (octets-to-string
    (read-until-ascii-char #\e)
    :encoding :ascii)))

(defmacro peek-byte (&rest args)
  `(let ((the-byte (funcall #'read-byte *read-stream* ,@args)))
     (unread-byte the-byte *read-stream*)
     the-byte))

(defun read-list ()
  (read-ascii-char #\l)
  (let ((the-list (with-list-builder list
		    (let-while it (peek-byte nil) (and it (/= it (ascii-char->octet #\e)))
			       (put-value list (read-value))))))
    (read-ascii-char #\e)
    the-list))

(defun read-dict () 
  (read-ascii-char #\d)
  (let ((the-dict (with-dict-builder dict
		    (let-while it (peek-byte nil) (and it (/= it (ascii-char->octet #\e)))
			       (put-value dict (read-string) (read-value))))))
    (read-ascii-char #\e)
    the-dict))

(defun read-value ()
  (let ((it (peek-byte)))
    (setq it (octet->ascii-char it))
    (case it
      (#\i
       (read-integer))
      (#\l
       (read-list))
      (#\d
       (read-dict))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (read-string))
      (t
       (error 'unexpected-octet
	      :the-octet it
	      :expected-octets
	      (string-to-octets
	       "ild01234567890"
	       :encoding :ascii))))))

(defgeneric decode (stream-or-string &key style)
  (:documentation "Convert bencoded string or stream to CL data structures."))

(defmethod decode ((string string) &key (style *style*))
  (decode (make-in-memory-input-stream
	   (string-to-octets string :encoding :utf-8))
	  :style style))

(defmethod decode ((stream stream) &key (style *style*))
  (decode (make-flexi-stream stream)
	  :style style))

(defmethod decode ((stream flexi-stream) &key (style *style*))
  (let ((*style* style)
	(*read-stream* stream))
    (read-value)))
