;;;; ghprojects.lisp

(in-package #:ghprojects)

;;; "ghprojects" goes here. Hacks and glory await!

(defun days-per-month (month year)
  (assert (plusp month))
  (let ((days #(() 31 28 31 30 31 30 31 31 30 31 30 31)))
    (if (and (= month 2) (= 0 (mod year 4)))
        29
        (aref days month))))

(defun github-api-date (&optional (days-ago 0))
  "A date formatted for the github api created field.
e.g. 2013-11-03"
  (multiple-value-bind (_ __ ___ day month year) (decode-universal-time
                                                  (get-universal-time)
                                                  0)
    (declare (ignore  _ __ ___))
    (setq day (- day days-ago))
    (loop until (plusp day) do
          (setq month (if (plusp (- month 1))
                          (- month 1)
                          (progn (setq year (- year 1))
                                 12)))
          (setq day (+ (days-per-month month year) day)))
    (format nil "~A-~2,'0D-~2,'0D" year month day)))

(defun url (&key (days-ago 0))
  "The API url for lisp projects created DAYS-AGO."
  (concatenate
   'string
   "https://api.github.com/search/repositories?q=language:lisp+created:"
   (github-api-date days-ago)))

(defun get-repo-list (url)
  (gethash "items"
           (yason:parse
            (flexi-streams:octets-to-string
             (drakma:http-request url :user-agent :safari
                                  ;; github doesn't recognize uri encoded #\+
                                  :preserve-uri t)))))

(defun iso8601-universal-time (date-string)
  "Return an UNIVERSAL-TIME integer for ISO 8601-style DATE-STRING
Assumes UTC."
  (ppcre:register-groups-bind ((#'parse-integer year month day hour minute))
      ("(\\d\\d\\d\\d?)-(..)-(..)T(..):(..)" date-string)
    (encode-universal-time 0 minute hour day month year 0)))

(defun attribute-list (names repo-list)
  (unless (listp names)
    (setq names (list names)))
  (loop for repo in repo-list
        collect (loop for name in names
                      for attribute = (gethash name repo)
                      then (gethash name attribute)
                      finally (return attribute))))

(defun template-values (repo-list)
  "Return template values suitable for filling in atom-template.xml
for REPO-LIST with HTML-TEMPLATE."
  (setq repo-list (sort repo-list #'> :key (lambda (repo)
                                             (iso8601-universal-time
                                              (gethash "created_at" repo)))))
  (let ((titles (attribute-list "name" repo-list))
        (owners (attribute-list '("owner" "login") repo-list))
        (descs (attribute-list "description" repo-list))
        (dates (attribute-list "created_at" repo-list))
        (paths (attribute-list "html_url" repo-list)))
    (list :max-date (car dates)
          :entries
          (mapcar (lambda (title owner desc path date)
                    (list :title title
                          :owner owner
                          :desc desc
                          :path path
                          :date date))
                  titles owners descs paths dates))))

(defparameter *template*
  (html-template:create-template-printer
   (merge-pathnames "atom-template.xml"
                    ghprojects-config:*base*)))

(defun write-feed (file)
  (let ((repo-list (append (get-repo-list (url))
                           (get-repo-list (url :days-ago 1)))))
    (with-open-file (stream file :direction :output
                            :if-exists :rename-and-delete)
      (html-template:fill-and-print-template *template*
                                             (template-values repo-list)
                                             :stream stream))
    (probe-file file)))

(defun main (argv)
  (sb-ext:disable-debugger)
  (setf *print-pretty* nil)
  (destructuring-bind (file)
      (rest argv)
    (write-feed file)))
