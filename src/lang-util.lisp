;; lang-util
;; Basic utilities for language models in Common Lisp
;;
;; Copyright 2022 Jörgen Brandt <joergen@cuneiform-lang.org>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(in-package :lang-util)

(defgeneric find-duplicate (l &key test)
  (:documentation "
(find-duplicate L)
(find-duplicate L :test TEST)

Find duplicates in the list L, returning the first duplicate found.
Returns nil, if no duplicates are present. Compares elements using
the function TEST. Uses #'equal by default.

Examples:

  (find-duplicate '(a a) :test #'cl:eq) ==> 'a
  (find-duplicate '(1 2 3) :test #'=)   ==> nil
  (find-duplicate '(1 2 1))             ==> 1
"))

(defmethod find-duplicate ((l null) &key &allow-other-keys)
  nil)

(defmethod find-duplicate ((l cons) &key (test #'equal))
  (let ((x    (car l))
	(rest (cdr l)))
    (if (member x rest :test test)
	x
	(find-duplicate rest :test test))))


(defgeneric line-pad (s prefix &key unless-starts-with)
  (:documentation "
(line-pad S PREFIX)

Left-pad every line in S with PREFIX. Optionally, the function
checks if the line starts with a special character given as a
:unless-starts-with keyword argument and skips padding, if the
character is detected.

Examples:

  (line-pad \"this is a comment\" \"// \") ==> \"// this is a comment\"
  (line-pad \"indented\nlines\" \"  \")    ==> \"  indented\n  lines\"
  (line-pad \"#include <don't pad this>\nbut pad everything else\" \"  \" :unless-starts-with #\#)
"))

(defmethod line-pad ((s string) (prefix string) &key unless-starts-with)
  (let* ((line-list          (uiop:split-string s :separator (format nil "~%")))
	 (indented-line-list (loop for line in line-list
				   collect (if (and
						unless-starts-with
						(> (length line) 0)
						(char= unless-starts-with (char line 0)))
					       line
					       (format nil "~a~a" prefix line)))))
    (format nil "~{~a~^~%~}" indented-line-list)))

    
  

