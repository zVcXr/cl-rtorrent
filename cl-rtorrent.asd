;;;; cl-rtorrent.asd

(asdf:defsystem #:cl-rtorrent
  :description "Describe cl-rtorrent here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-fad #:flexi-streams #:babel)
  :components
  ((:module #:bencode
    :components
    ((:file "package")
     (:file "util")
     (:file "style")
     (:file "decode")
     (:file "encode")
     (:file "style-sexp")
     (:file "style-tagged-sexp")
     (:file "style-modern")))
   (:file "package")
   (:file "cl-rtorrent")))

