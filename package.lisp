;;;; package.lisp

(defpackage #:ghprojects
  (:use #:cl)
  (:import-from #:local-time
                #:now
                #:timestamp+
                #:format-timestring)
  (:export #:main))

