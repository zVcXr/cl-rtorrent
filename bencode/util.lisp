(in-package #:bencode)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar #'(lambda (name) `(,name (gensym))) names)
     ,@body))

(defmacro let-while (var step-form test-form &body body)
  `(do ((,var ,step-form ,step-form))
       ((not ,test-form))
     ,@body))

(defun ->singleton-list (value)
  (if (null value)
      nil
      (list value)))

(defgeneric put-value (builder &rest args)
  (:documentation "Adds element(s) to the builder.

Depending on specific builder the arguments can be treated differently."))

;; Octet

(deftype octet ()
  '(unsigned-byte 8))

(defun ->octets (&rest contents)
  (make-array (length contents)
	      :element-type 'octet
	      :initial-contents contents))

(defclass cl-octets-builder ()
  ((backing-vector :initarg :backing-vector :reader backing-vector)))

(defmethod put-value ((octets cl-octets-builder) &rest args)
  (destructuring-bind (value) args
    (with-slots (backing-vector) octets
      (vector-push-extend value backing-vector))))

(defmacro with-octets-builder (octets &body body)
  `(let ((,octets
	   (make-instance
	    'cl-octets-builder
	    :backing-vector
	    (make-array 0
			:element-type 'octet
			:fill-pointer 0
			:adjustable t))))
     ,@body
     (with-slots (backing-vector) ,octets
       (make-array (length backing-vector) :element-type 'octet :initial-contents backing-vector))))

;; ASCII

(define-condition unsupported-ascii-octet (error)
  ((the-octet :initarg :the-octet :reader the-octet)))

(define-condition unsupported-ascii-char (error)
  ((the-char :initarg :the-char :reader the-char)))

(defun ascii-char->octet (char)
  (let ((octets (string-to-octets (string char) :encoding :ascii)))
    (when (> (length octets) 1)
      (error 'unsupported-ascii-char :the-char char))
    (elt octets 0)))

(defun octet->ascii-char (octet)
  (let ((chars (octets-to-string (->octets octet))))
    (when (> (length chars) 1)
      (error 'unsupported-ascii-octet :the-octet octet))
    (char chars 0)))
