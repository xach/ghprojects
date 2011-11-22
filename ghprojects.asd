;;;; ghprojects.asd

(asdf:defsystem #:ghprojects
  :serial t
  :description "Scrape GitHub and write out a feed for new Common Lisp
  projects."
  :depends-on (#:webscraper
               #:html-template
               #:cl-ppcre
               #:closure-html)
  :components ((:file "package")
               (:file "ghprojects")))


(defpackage #:ghprojects-config
  (:export #:*base*))

(defparameter ghprojects-config:*base*
  (make-pathname :type nil :name nil
                 :defaults *load-truename*))

