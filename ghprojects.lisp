;;;; ghprojects.lisp

(in-package #:ghprojects)

;;; "ghprojects" goes here. Hacks and glory await!

(defun github-api-date (&optional (days-ago 0))
  "A date formatted for the github api created field.
e.g. 2013-11-03"
  (let ((timestamp (timestamp+ (now) (- days-ago) :day)))
    (format-timestring nil timestamp
                       :format
                       '(:year "-" (:month 2 #\0) "-" (:day 2 #\0)))))

(defun url (&key (days-ago 0))
  "The API url for lisp projects created DAYS-AGO."
  (concatenate
   'string
   "https://api.github.com/search/repositories?q=language:lisp+created:"
   (github-api-date days-ago)))

(defun get-repos (url)
  "Return a list of git repo information tables from URL."
  (gethash "items"
           (yason:parse
            (flexi-streams:octets-to-string
             (drakma:http-request url :user-agent :safari
                                  ;; github doesn't recognize uri encoded #\+
                                  :preserve-uri t)))))

(defun lookup (key-path table)
  "Given a list of keys KEY-PATH, return the result of recursively
looking up each key in TABLE, e.g. (lookup '(a b c) table) is equivalent to (gethash 'c (gethash 'b (gethash 'a table)))"
  (if (listp key-path)
      (dolist (key key-path table)
        (setf table (gethash key table)))
      (gethash key-path table)))

(defun =lookup (key-path)
  (lambda (table)
    (lookup key-path table)))

(defun attribute-list (key-path repos)
  "Collect the result of looking up (via LOOKUP) KEY-PATH in each repo
of REPOS."
  (mapcar (=lookup key-path) repos))

(defun template-values (repos)
  "Return template values suitable for filling in atom-template.xml
for REPOS with HTML-TEMPLATE."
  (setf repos (sort repos #'string>
                    :key (lambda (repo)
                           (gethash "created_at" repo))))
  (let ((titles (attribute-list "name" repos))
        (owners (attribute-list '("owner" "login") repos))
        (descs (attribute-list "description" repos))
        (dates (attribute-list "created_at" repos))
        (paths (attribute-list "html_url" repos)))
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
  (let ((repos (append (get-repos (url))
                       (get-repos (url :days-ago 1))
                       (get-repos (url :days-ago 2)))))
    (with-open-file (stream file :direction :output
                            :if-exists :rename-and-delete)
      (html-template:fill-and-print-template *template*
                                             (template-values repos)
                                             :stream stream))
    (probe-file file)))

(defun main (argv)
  (sb-ext:disable-debugger)
  (setf *print-pretty* nil)
  (destructuring-bind (file)
      (rest argv)
    (write-feed file)))
