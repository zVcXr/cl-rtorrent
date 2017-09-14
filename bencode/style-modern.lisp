(in-package #:bencode)

;; Decode (vector + hash table)

(defclass modern-list-builder ()
  ((backing-vector :initarg :backing-vector :reader backing-vector)))

(defmethod put-value ((list modern-list-builder) &rest args)
  (destructuring-bind (value) args
    (list-put value list)))

(defmethod make-bencode-list ((style (eql :modern)))
  (make-instance 'modern-list-builder
		 :backing-vector (make-array 0 :fill-pointer 0 :adjustable t)))

(defmethod list-put (value (list modern-list-builder))
  (with-slots (backing-vector) list
    (vector-push-extend value backing-vector)))

(defmethod list-close ((list modern-list-builder))
  (with-slots (backing-vector) list
    (make-array (length backing-vector)
		:initial-contents backing-vector)))


(defclass modern-dict-builder ()
  ((backing-table :initarg :backing-table :reader backing-table)))

(defmethod put-value ((dict modern-dict-builder) &rest args)
  (destructuring-bind (key value) args
    (dict-put key value dict)))

(defmethod make-bencode-dict ((style (eql :modern)))
  (make-instance 'modern-dict-builder
		 :backing-table (make-hash-table :test 'equal)))

(defmethod dict-put (key value (dict modern-dict-builder))
  (with-slots (backing-table) dict
    (setf (gethash key backing-table) value)))

(defmethod dict-close ((dict modern-dict-builder))
  (with-slots (backing-table) dict
    backing-table))

;; Traverse

(defmethod traverse-key ((key integer)
			 (list #.(class-of (make-array 0)))
			 (style (eql :modern)))
  (elt list key))

(defmethod traverse-key ((key string)
			 (dict #.(class-of (make-hash-table :test 'equal)))
			 (style (eql :modern)))
  (gethash key dict))

(defmethod traverse-key ((key (eql 'list*))
			 (list #.(class-of (make-array 0)))
			 (style (eql :modern)))
  list)

(defmethod traverse-key ((key (eql 'dict*))
			 (dict #.(class-of (make-hash-table :test 'equal)))
			 (style (eql :modern)))
  (let ((values nil))
    (maphash #'(lambda (k v)
		 (push v values))
	     dict)
    values))

;; Encode

(defmethod write-value ((list #.(class-of (make-array 0)))
			(style (eql :modern)))
  (write-ascii-char #\l)
  (map 'vector #'write-value-in-style list)
  (write-ascii-char #\e))

(defmethod write-value ((dict #.(class-of (make-hash-table :test 'equal)))
			(style (eql :modern)))
  (write-ascii-char #\d)
  (maphash #'(lambda (key value)
	       (write-value-in-style key)
	       (write-value-in-style value))
	   dict)
  (write-ascii-char #\e))
