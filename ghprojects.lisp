;;;; ghprojects.lisp

(in-package #:ghprojects)

;;; "ghprojects" goes here. Hacks and glory await!

(defvar *url* "https://github.com/languages/Common%20Lisp/created"
  "The URL to scrape for projects.")

(defvar *months*
  (let ((table (make-hash-table :test 'equalp)))
    (loop for i from 1
          for month in '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                         "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
          do (setf (gethash month table) i))
    table)
  "A hash table mapping a three-letter English month abbreviation to
  an integer from 1 to 12.")

(defun current-year ()
  (nth-value 5 (get-decoded-time)))

(defun parse-date-string (date-string)
  "Parse a date string in the format \"DD Mon HH:MM\" to a
  universal-time. Assumes the current year."
  (ppcre:register-groups-bind ((#'parse-integer day)
                               month
                               (#'parse-integer hour minute))
      ("(\\d\\d?) (...) (..):(..)" date-string)
    (let ((month-number (gethash month *months*)))
      (unless month-number
        (error "Invalid month -- ~S" month))
      (encode-universal-time 0 minute hour day month-number (current-year)))))

(defun iso8601 (universal-time)
  "Return an ISO 8601-style time string for UNIVERSAL-TIME."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun matching-text (fun doc)
  "Return a list of string values for all nodes in DOC matching FUN."
  (let ((nodes (stp:filter-recursively fun doc)))
    (mapcar 'stp:string-value nodes)))

(defun trim (string)
  "Remove whitespace from the beginning and end of STRING."
  (string-trim '(#\Space #\Return #\Newline) string))

(defun tds (class doc)
  "Return the string contents of any <td> elements with the given
CLASS in DOC."
  (matching-text (=and (=name-is "td")
                       (=attribute-is "class" class))
                 doc))

(defun gravatars (doc)
  "Return the SRC attribute of any gravatar <img> elements in DOC."
  (let ((imgs (stp:filter-recursively
               (=and (=name-is "img")
                    (=attribute-matches "src" "gravatar"))
               doc)))
    (mapcar (lambda (img)
              (stp:attribute-value img "src"))
            imgs)))

(defun template-values (doc)
  "Return template values suitable for filling in atom-template.xml
for DOC with HTML-TEMPLATE."
  (let ((titles (mapcar 'trim (tds "title" doc)))
        (owners (mapcar 'trim (tds "owner" doc)))
        (descs (mapcar 'trim (tds "desc" doc)))
        (dates (mapcar 'parse-date-string (tds "date" doc)))
        (gravatars (gravatars doc)))
    (let ((max-date (reduce #'max dates)))
      (list :max-date (iso8601 max-date)
            :entries
            (mapcar (lambda (title owner desc date gravatar)
                      (list :title title
                            :owner owner
                            :desc desc
                            :path (format nil "/~A/~A" owner title)
                            :universal-time date
                            :date (iso8601 date)
                            :gravatar gravatar))
                    titles owners descs dates gravatars)))))

(defparameter *template*
  (html-template:create-template-printer
   (merge-pathnames "atom-template.xml"
                    ghprojects-config:*base*)))

(defun write-feed (file)
  (let* ((html (url-content *url*))
         (doc (chtml:parse html (stp:make-builder))))
    (with-open-file (stream file :direction :output
                            :if-exists :rename-and-delete)
      (html-template:fill-and-print-template *template*
                                             (template-values doc)
                                             :stream stream))
    (probe-file file)))

(defun main (argv)
  (sb-ext:disable-debugger)
  (setf *print-pretty* nil)
  (destructuring-bind (file)
      (rest argv)
    (write-feed file)))
