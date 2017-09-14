(in-package #:bencode)

;; Decode (list + alist)

(defclass tagged-sexp-list-builder (sexp-list-builder) ())

(defmethod make-bencode-list ((style (eql :tagged-sexp)))
  (make-instance 'tagged-sexp-list-builder :backing-list nil))

(defmethod list-close ((list tagged-sexp-list-builder))
  (with-slots (backing-list) list
    `(:list ,@(nreverse backing-list))))

(defclass tagged-sexp-dict-builder (sexp-dict-builder) ())

(defmethod make-bencode-dict ((style (eql :tagged-sexp)))
  (make-instance 'tagged-sexp-dict-builder :backing-list nil))

(defmethod dict-close ((dict tagged-sexp-dict-builder))
  (with-slots (backing-list) dict
    `(:dict ,@(nreverse backing-list))))

;; Traverse

(define-condition expected-dict (error)
  ((the-object :initarg :the-object :reader the-object)))

(define-condition expected-list (error)
  ((the-object :initarg :the-object :reader the-object)))

(defmacro validate-tagged-sexp-symbol (expected-tag object)
  `(when (not (eq (first ,object) ,expected-tag))
     (error (find-symbol ,(concatenate 'string "EXPECTED-" (symbol-name expected-tag)))
	    :the-object ,object)))

(defmethod traverse-key ((key integer) list (style (eql :tagged-sexp)))
  (validate-tagged-sexp-symbol :list list)
  (traverse-level key (rest list) :sexp))

(defmethod traverse-key ((key string) dict (style (eql :tagged-sexp)))
  (validate-tagged-sexp-symbol :dict dict)
  (traverse-level key (rest dict) :sexp))

(defmethod traverse-key ((key (eql 'list*)) list (style (eql :tagged-sexp)))
  (validate-tagged-sexp-symbol :list list)
  (traverse-level key (rest list) :sexp))

(defmethod traverse-key ((key (eql 'dict*)) dict (style (eql :tagged-sexp)))
  (validate-tagged-sexp-symbol :dict dict)
  (traverse-level key (rest dict) :sexp))

;; Encode

(defmethod write-value ((value cons) (style (eql :tagged-sexp)))
  (case (first value)	 ; tag
    (:list (write-list (rest value)))
    (:dict (write-dict (rest value)))
    (t (error 'unsupported-value
     	      :the-value value))))

(defun write-list (list)
  (write-ascii-char #\l)
  (dolist (value list)
    (write-value-in-style value))
  (write-ascii-char #\e))

(defun write-dict (dict)
  (write-ascii-char #\d)
  (stable-sort dict #'string-lessp :key #'first)
  (dolist (it dict)
    (destructuring-bind (key . value) it
      (write-value-in-style key)
      (write-value-in-style value)))
  (write-ascii-char #\e))
