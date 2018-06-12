(defpackage #:bencode
  (:use #:cl)
  (:import-from #:flexi-streams
   :flexi-stream
   :make-flexi-stream
   :make-in-memory-input-stream
   :with-output-to-sequence)
  (:import-from #:babel
   :octets-to-string
   :string-to-octets
   :character-decoding-error)
  (:export
   encode
   decode
   ->string))

(in-package #:bencode)

(defmacro let-while (var step-form test-form &body body)
  `(do ((,var ,step-form ,step-form))
       ((not ,test-form))
     ,@body))

(defun singleton-list (value)
  (if (null value)
      nil
      (list value)))

(defgeneric make-collector (type))

(defmethod make-collector (type)
  (make-instance type))

(defmacro register-collector (collector-type type &body body)
  `(defmethod make-collector ((type (eql ,type)))
     (make-instance ,collector-type ,@body)))

(defgeneric collect (collector &rest arguments)
  (:documentation "Adds element(s) to the collector.

Depending on specific collector the arguments can be treated differently."))

(defgeneric finalize-collector (collector))

(defmethod finalize-collector (collector) collector)

(defun make-keyword (name)
  (values (intern (string-upcase (string name)) "KEYWORD")))

(defmacro define-collect-with-arguments (method-name (collector-name collector-type) (&rest arguments) &body body)
  (flet ((generate-check-types (arguments)
	   (mapcar #'(lambda (thing)
		       (when (and (consp thing) (>= (length thing) 2))
			 (destructuring-bind (name type) thing
			   `(check-type ,name ,type))))
		   arguments)))
    (let ((argument-names (mapcar #'alexandria:ensure-car arguments)))
      `(progn
	 (defgeneric ,method-name (,collector-name ,@argument-names))
	 ,@(when body
	     (list `(defmethod ,method-name ((,collector-name ,collector-type) ,@argument-names)
		      ,@body)))
	 (defmethod collect ((,collector-name ,collector-type) &rest arguments)
	   (destructuring-bind (,@argument-names) arguments
	     ,@(generate-check-types arguments)
	     (,method-name ,collector-name ,@argument-names)))))))

(defmacro define-collector (type (collector-name collector-type (&rest superclasses)) (&rest slots) &body body)
  (let ((slot-names (mapcar #'first slots)))
    (flet ((expand-slot-description (slot)
	     (destructuring-bind (name &rest slot-options &key &allow-other-keys) slot
	       `(,name :initarg ,(getf slot-options :initarg (make-keyword name))
		       :reader ,(getf slot-options :reader name)
		       ,@(alexandria:remove-from-plist slot-options :initarg :reader))))
	   (expand-collect-statement (statement) 
	     (destructuring-bind (method-name (&rest method-arguments) &rest method-body) statement 
	       `(define-collect-with-arguments ,method-name (,collector-name ,collector-type) (,@method-arguments)
		  (with-slots (,@slot-names) ,collector-name
		    ,@method-body)))))
      (multiple-value-bind (collect-statements statements)
	  (serapeum:partition #'(lambda (thing) (and (consp thing) (eq :collect (first thing)))) body)
	`(progn
	   (defclass ,collector-type (,@superclasses)
	     (,@(mapcar #'expand-slot-description slots)))
	   ,@(mapcar #'expand-collect-statement collect-statements) 
	   ,@(if (null statements)
		 nil
		 (list `(defmethod finalize-collector ((,collector-name ,collector-type))
			  (with-slots (,@slot-names) ,collector-name
			    ,@statements)))))))))

(define-collector 'octets (collector octets-collector ())
    ((octets :initform (make-array 0 :element-type 'octet :fill-pointer 0 :adjustable t)))
  (:collect ((octet octet)) (vector-push-extend octet octets))
  (make-array (length octets) :element-type 'octet :initial-contents octets))

(defclass octets-collector ()
  ((octets :initarg :octets :reader octets
	   :initform (make-array 0 :element-type 'octet :fill-pointer 0 :adjustable t))))

(register-collector 'octets-collector 'octets)

(define-collect-with-arguments octets-collect (collector octets-collector) ((octet octet))
  (with-slots (octets) collector
    (vector-push-extend octet octets)))

(defmethod finalize-collector ((collector octets-collector))
  (with-slots (octets) collector
    (make-array (length octets) :element-type 'octet :initial-contents octets)))

(defmacro collecting-octets (octets &body body)
  `(collecting ,octets 'octets-collector
     ,@body))

(defmethod finalize-collector (collector)
  collector)

(defmacro collecting (var type &body body)
  `(let ((,var (make-collector ,type)))
     ,@body
     (finalize-collector ,var)))

;; Octet

(deftype octet ()
  '(unsigned-byte 8))

(deftype octets ()
  '(array octet *))

(defun arguments-to-octets (&rest contents)
  (make-array (length contents)
	      :element-type 'octet
	      :initial-contents contents))

(defclass octets-collector ()
  ((octets :initarg :octets :reader octets
	   :initform (make-array 0 :element-type 'octet :fill-pointer 0 :adjustable t))))

(register-collector 'octets-collector 'octets)

(define-collect-with-arguments octets-collect (collector octets-collector) ((octet octet))
  (with-slots (octets) collector
    (vector-push-extend octet octets)))

(defmethod finalize-collector ((collector octets-collector))
  (with-slots (octets) collector
    (make-array (length octets) :element-type 'octet :initial-contents octets)))

(defmacro collecting-octets (octets &body body)
  `(collecting ,octets 'octets-collector
     ,@body))

;; Object model

(defclass bclass () ())

(defclass bint (bclass)
  ((octets :initarg :octets :reader octets)))

(defclass bint-collector (octets-collector) ())

(defmethod finalize-collector ((collector bint-collector))
  (with-slots (octets) collector
    (make-instance 'bint :octets octets)))

(defclass bstr (bclass)
  ((octets :initarg :octets :reader octets)))

(defclass bstr-collector (octets-collector) ())

(defmethod finalize-collector ((collector bstr-collector))
  (with-slots (octets) collector
    (make-instance 'bstr :octets octets)))

(defclass finalizable-once-collector ()
  ((finalized-p :initform nil)))

(defmethod collect :around ((collector finalizable-once-collector) &rest args)
  (declare (ignore args))
  (with-slots (finalized-p) collector
    ;; TODO signal error?
    (unless finalized-p
      (call-next-method))))

(defmethod finalize-collector :around ((collector finalizable-once-collector))
  (with-slots (finalized-p) collector
    ;; TODO signal error?
    (unless finalized-p
      (let ((result (call-next-method)))
	(setf finalized-p t)
	result))))

(defclass blist (bclass) ())

(defclass blist-collector () ())

(define-collect-with-arguments blist-collect (collector blist-collector) ((element bclass)))

(defclass blist-list (blist blist-collector finalizable-once-collector)
  ((elements :initarg :elements :reader elements :initform nil)))

(defmethod blist-collect ((collector blist-list) element)
  (with-slots (elements) collector
    (push element elements)))

(defmethod finalize-collector ((collector blist-list))
  (with-slots (elements) collector
    (setf elements (nreverse elements)))
  collector)

(defclass blist-vector (blist)
  ((elements :initarg :elements :reader elements)))

(defclass blist-vector-collector (blist-collector)
  ((elements :initarg :elements :reader elements
	     :initform (make-array 0 :element-type 'bclass :fill-pointer 0 :adjustable t))))

(defmethod blist-collect ((collector blist-vector-collector) element)
  (with-slots (elements) collector
    (vector-push-extend element elements)))

(defmethod finalize-collector ((collector blist-vector-collector))
  (with-slots (elements) collector
    (make-instance 'blist-vector :elements elements)))

(defclass bdict (bclass) ())

(defclass bdict-collector () ())

(define-collect-with-arguments bdict-collect (collector bdict-collector) ((key bstr) (value bclass)))

(defclass bdict-alist (bdict bdict-collector finalizable-once-collector)
  ((elements :initarg :elements :reader elements :initform nil)))

(defmethod bdict-collect ((collector bdict-alist) key value)
  (with-slots (elements) collector
    (setf elements (acons key value elements))))

(defmethod finalize-collector ((collector bdict-alist))
  (with-slots (elements) collector
    (setf elements (nreverse elements)))
  collector)

(defclass bdict-hashmap (bdict)
  ((elements :initarg :elements :reader elements
	     :initform (make-hash-table ))))

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
  (let ((chars (octets-to-string (arguments-to-octets octet))))
    (when (> (length chars) 1)
      (error 'unsupported-ascii-octet :the-octet octet))
    (char chars 0)))

;; Style

(defvar *style* :tagged-sexp)

;; Decode methods

(defgeneric make-blist (style))

(defgeneric blist-collect (value list))

(defgeneric blist-finalize (list))

(defmacro collecting-blist (list &body body)
  `(let ((,list (make-blist *style*)))
     ,@body
     (blist-finalize ,list)))

(defgeneric make-bdict (style))

(defgeneric bdict-collect (key value dict))

(defgeneric bdict-finalize (dict))

(defmacro collecting-bdict (dict &body body)
  `(let ((,dict (make-bdict *style*)))
     ,@body
     (bdict-finalize ,dict)))

;; Traverse methods

(defun traverse (path object &key (style *style*))
  (let ((list (singleton-list object)))
    (dolist (key path)
      (setf list
	    (let ((*style* :sexp))
	      (collecting-blist new-list
		(dolist (obj list)
		  (dolist (new-obj (traverse-key key obj style))
		    (collect new-list new-obj)))))))
    list))

(defgeneric traverse-key (key object style)
  (:documentation "Traverse single level."))

;; Decode

(define-condition unexpected-octet (error)
  ((the-octet :initarg :the-octet :reader the-octet)
   (expected-octets :initarg :expected-octets :reader expected-octets)))

(defvar *read-stream* nil "Readable FLEXI-STREAM")

(defun read-ascii-char (char)
  (let ((it (read-byte *read-stream*)))
    (unless (= it (ascii-char->octet char))
      (error 'unexpected-octet
	     :the-octet it
	     :expected-octets (arguments-to-octets (ascii-char->octet char))))))

(defun read-until-ascii-char (char) 
  (collecting-octets octets
    (let-while it (read-byte *read-stream*) (/= it (ascii-char->octet char)) 
      (collect octets it))))

(defun read-string-length ()
  (parse-integer
   (octets-to-string
    (read-until-ascii-char #\:)
    :encoding :ascii)))

(defun read-string ()
  (let* ((length (read-string-length))
	 (octets
	   (collecting-octets octets
	     (dotimes (i length)
	       (collect octets (read-byte *read-stream*))))))
    ;; TODO: this is not the right place for string decoding
    (handler-case (octets-to-string octets :encoding :utf-8)
      (character-decoding-error () octets))))

(defun read-integer ()
  (read-ascii-char #\i)
  (parse-integer
   (octets-to-string
    (read-until-ascii-char #\e)
    :encoding :ascii)))

(defmacro peek-byte (&rest args)
  `(flexi-streams:peek-byte *read-stream* ,@args))

(defun read-list ()
  (read-ascii-char #\l)
  (let ((the-list
	  (collecting-blist list
	    (let-while it (peek-byte) (and it (/= it (ascii-char->octet #\e)))
	      (collect list (read-value))))))
    (read-ascii-char #\e)
    the-list))

(defun read-dict () 
  (read-ascii-char #\d)
  (let ((the-dict
	  (collecting-bdict dict
	    (let-while it (peek-byte) (and it (/= it (ascii-char->octet #\e)))
	      (collect dict (read-string) (read-value))))))
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

;; Encode

(define-condition unsupported-value (error)
  ((the-value :initarg :the-value :reader the-value)))

(defvar *write-stream* nil "Writable FLEXI-STREAM")

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

(defun encode-to-string (object &key (style *style*))
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

;; Style: SEXP (decode only)

;; Decode (list + alist)

(defclass sexp-blist-collector ()
  ((backing-list :initarg :backing-list :reader backing-list)))

(defmethod collect ((list sexp-blist-collector) &rest args)
  (destructuring-bind (value) args
    (blist-collect value list)))

(defmethod make-blist ((style (eql :sexp)))
  (make-instance 'sexp-blist-collector :backing-list nil))

(defmethod blist-collect (value (list sexp-blist-collector))
  (with-slots (backing-list) list
    (setf backing-list (push value backing-list))))

(defmethod blist-finalize ((list sexp-blist-collector))
  (with-slots (backing-list) list
    (nreverse backing-list)))

(defclass sexp-bdict-collector ()
  ((backing-list :initarg :backing-list :reader backing-list)))

(defmethod collect ((dict sexp-bdict-collector) &rest args)
  (destructuring-bind (key value) args
    (bdict-collect key value dict)))

(defmethod make-bdict ((style (eql :sexp)))
  (make-instance 'sexp-bdict-collector :backing-list nil))

(defmethod bdict-collect (key value (dict sexp-bdict-collector))
  (with-slots (backing-list) dict
    (setf backing-list (acons key value backing-list))))

(defmethod bdict-finalize ((dict sexp-bdict-collector))
  (with-slots (backing-list) dict
    (nreverse backing-list)))

;; Traverse

(defmethod traverse-key ((key integer) list (style (eql :sexp)))
  (let ((value (nth key list)))
    (singleton-list value)))

(defmethod traverse-key ((key string) dict (style (eql :sexp)))
  (let ((value (rest (assoc key dict :test #'string=))))
    (singleton-list value)))

(defmethod traverse-key ((key (eql 'list*)) list (style (eql :sexp)))
  `(,@list))

(defmethod traverse-key ((key (eql 'dict*)) dict (style (eql :sexp)))
  (mapcar #'cdr dict))

;; Style: Tagged SEXP

;; Decode (list + alist)

(defclass tagged-sexp-blist-collector (sexp-blist-collector) ())

(defmethod make-blist ((style (eql :tagged-sexp)))
  (make-instance 'tagged-sexp-blist-collector :backing-list nil))

(defmethod blist-finalize ((list tagged-sexp-blist-collector))
  (with-slots (backing-list) list
    `(:list ,@(nreverse backing-list))))

(defclass tagged-sexp-bdict-collector (sexp-bdict-collector) ())

(defmethod make-bdict ((style (eql :tagged-sexp)))
  (make-instance 'tagged-sexp-bdict-collector :backing-list nil))

(defmethod bdict-finalize ((dict tagged-sexp-bdict-collector))
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
  (traverse-key key (rest list) :sexp))

(defmethod traverse-key ((key string) dict (style (eql :tagged-sexp)))
  (validate-tagged-sexp-symbol :dict dict)
  (traverse-key key (rest dict) :sexp))

(defmethod traverse-key ((key (eql 'list*)) list (style (eql :tagged-sexp)))
  (validate-tagged-sexp-symbol :list list)
  (traverse-key key (rest list) :sexp))

(defmethod traverse-key ((key (eql 'dict*)) dict (style (eql :tagged-sexp)))
  (validate-tagged-sexp-symbol :dict dict)
  (traverse-key key (rest dict) :sexp))

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

;; Style: MODERN

;; Decode (vector + hash table)

(defclass modern-blist-collector ()
  ((backing-vector :initarg :backing-vector :reader backing-vector)))

(defmethod collect ((list modern-blist-collector) &rest args)
  (destructuring-bind (value) args
    (blist-collect value list)))

(defmethod make-blist ((style (eql :modern)))
  (make-instance 'modern-blist-collector
		 :backing-vector (make-array 0 :fill-pointer 0 :adjustable t)))

(defmethod blist-collect (value (list modern-blist-collector))
  (with-slots (backing-vector) list
    (vector-push-extend value backing-vector)))

(defmethod blist-finalize ((list modern-blist-collector))
  (with-slots (backing-vector) list
    (make-array (length backing-vector)
		:initial-contents backing-vector)))


(defclass modern-bdict-collector ()
  ((backing-table :initarg :backing-table :reader backing-table)))

(defmethod collect ((dict modern-bdict-collector) &rest args)
  (destructuring-bind (key value) args
    (bdict-collect key value dict)))

(defmethod make-bdict ((style (eql :modern)))
  (make-instance 'modern-bdict-collector
		 :backing-table (make-hash-table :test 'equal)))

(defmethod bdict-collect (key value (dict modern-bdict-collector))
  (with-slots (backing-table) dict
    (setf (gethash key backing-table) value)))

(defmethod bdict-finalize ((dict modern-bdict-collector))
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
		 (declare (ignore k))
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
  ;; TODO sort!
  (maphash #'(lambda (key value)
	       (write-value-in-style key)
	       (write-value-in-style value))
	   dict)
  (write-ascii-char #\e))
