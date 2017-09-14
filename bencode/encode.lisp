(in-package #:bencode)

(define-condition unsupported-value (error)
  ((the-value :initarg :the-value :reader the-value)))

(defvar *write-stream* nil)

(defun write-integer (value)
  (write-sequence (string-to-octets (write-to-string value) :encoding :ascii) *write-stream*))

(defun write-ascii-char (value)
  (write-byte (ascii-char->octet value) *write-stream*))

(defun write-value-in-style (value)
  (write-value value *style*))

(defgeneric write-value (value style))

(defmethod write-value ((value integer) style)
  (write-ascii-char #\i)
  (write-integer value)
  (write-ascii-char #\e))

(defmethod write-value ((value string) style)
  (let ((octets (string-to-octets value :encoding :utf-8)))
    (write-value octets style)))

(defmethod write-value ((octets #.(class-of (make-array 0 :element-type 'octet))) style)
  (write-integer (length octets))
  (write-ascii-char #\:)
  (write-sequence octets *write-stream*))

(defun ->string (object &key (style *style*))
  (octets-to-string (with-output-to-sequence (stream)
		      (encode object stream :style style))
		    :encoding :utf-8 :errorp nil))

(defgeneric encode (object stream &key style)
  (:documentation "Convert CL data structures to bencoded stream."))

(defmethod encode (object (stream stream) &key (style *style*))
  (encode object (make-flexi-stream stream) :style style))

(defmethod encode (object (stream flexi-stream) &key (style *style*))
  (let ((*style* style)
	(*write-stream* stream))
    (write-value-in-style object)))
