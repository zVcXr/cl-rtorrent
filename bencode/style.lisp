(in-package #:bencode)

(defvar *style* :tagged-sexp)

;; Decode

(defgeneric make-bencode-list (style))

(defgeneric list-put (value list))

(defgeneric list-close (list))

(defmacro with-list-builder (list &body body)
  `(let ((,list (make-bencode-list *style*)))
     ,@body
     (list-close ,list)))

(defgeneric make-bencode-dict (style))

(defgeneric dict-put (key value dict))

(defgeneric dict-close (dict))

(defmacro with-dict-builder (dict &body body)
  `(let ((,dict (make-bencode-dict *style*)))
     ,@body
     (dict-close ,dict)))

;; Traverse

(defun traverse (path object &key (style *style*))
  (let ((list (->singleton-list object)))
    (dolist (key path)
      (setf list
	    (let ((*style* :sexp))
	      (with-list-builder new-list :sexp
		(dolist (obj list)
		  (dolist (new-obj (traverse-level key obj style))
		    (put-value new-list new-obj)))))))
    list))

(defgeneric traverse-key (key object style)
  (:documentation "Traverse single level."))
