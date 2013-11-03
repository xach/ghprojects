;;;; ghprojects.asd

(asdf:defsystem #:ghprojects
  :serial t
  :description "Scrape GitHub and write out a feed for new Common Lisp
  projects."
  :depends-on (#:drakma
               #:yason
               #:flexi-streams
               #:html-template
               #:cl-ppcre)
  :components ((:file "package")
               (:file "ghprojects")))


(defpackage #:ghprojects-config
  (:export #:*base*))

(defparameter ghprojects-config:*base*
  (make-pathname :type nil :name nil
                 :defaults *load-truename*))

