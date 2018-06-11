;;;; cl-rtorrent.asd

(asdf:defsystem #:cl-rtorrent
  :description "Describe cl-rtorrent here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-fad #:flexi-streams #:babel #:alexandria #:serapeum)
  :components
  ((:module #:bencode
    :components
    ((:file "bencode")))
   (:file "package")
   (:file "cl-rtorrent")))

