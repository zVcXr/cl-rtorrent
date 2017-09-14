;;;; package.lisp

(defpackage #:bencode
  (:use #:cl)
  (:import-from #:flexi-streams
   :flexi-stream
   :make-flexi-stream
   :make-in-memory-input-stream
   :with-output-to-sequence
   :unread-byte)
  (:import-from #:babel
   :octets-to-string
   :string-to-octets
   :character-decoding-error)
  (:export
   encode
   decode
   ->string))
