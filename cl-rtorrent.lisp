;;;; cl-rtorrent.lisp

(in-package #:cl-rtorrent)

;;; "cl-rtorrent" goes here. Hacks and glory await!

(defun string-ends-with (str substr)
  (let ((len (length str))
	(sublen (length substr)))
    (string= substr (subseq str (- len sublen)))))

;; (defun file->alist (file)
;;   (bencode::dictionary->alist
;;    (with-open-file (in file
;; 		       :element-type '(unsigned-byte 8))
;;      (bencode:decode in))))

;; (defun list-torrents ()
;;   (let ((files ()))
;;     (fad:walk-directory
;;      "~/session/"
;;      (lambda (file)
;;        (push (file->alist file) files))
;;      :test (lambda (file)
;; 	     (string-ends-with (file-namestring file) ".rtorrent")))
;;     (print files)))
