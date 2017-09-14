(in-package #:bencode)

;; Decode (list + alist)

(defclass sexp-list-builder ()
  ((backing-list :initarg :backing-list :reader backing-list)))

(defmethod put-value ((list sexp-list-builder) &rest args)
  (destructuring-bind (value) args
    (list-put value list)))

(defmethod make-bencode-list ((style (eql :sexp)))
  (make-instance 'sexp-list-builder :backing-list nil))

(defmethod list-put (value (list sexp-list-builder))
  (with-slots (backing-list) list
    (setf backing-list (push value backing-list))))

(defmethod list-close ((list sexp-list-builder))
  (with-slots (backing-list) list
    (nreverse backing-list)))

(defclass sexp-dict-builder ()
  ((backing-list :initarg :backing-list :reader backing-list)))

(defmethod put-value ((dict sexp-dict-builder) &rest args)
  (destructuring-bind (key value) args
    (dict-put key value dict)))

(defmethod make-bencode-dict ((style (eql :sexp)))
  (make-instance 'sexp-dict-builder :backing-list nil))

(defmethod dict-put (key value (dict sexp-dict-builder))
  (with-slots (backing-list) dict
    (setf backing-list (acons key value backing-list))))

(defmethod dict-close ((dict sexp-dict-builder))
  (with-slots (backing-list) dict
    (nreverse backing-list)))

;; Traverse

(defmethod traverse-key ((key integer) list (style (eql :sexp)))
  (let ((value (nth key list)))
    (->singleton-list value)))

(defmethod traverse-key ((key string) dict (style (eql :sexp)))
  (let ((value (rest (assoc key dict :test #'string=))))
    (->singleton-list value)))

(defmethod traverse-key ((key (eql 'list*)) list (style (eql :sexp)))
  `(,@list))

(defmethod traverse-key ((key (eql 'dict*)) dict (style (eql :sexp)))
  (mapcar #'cdr dict))
