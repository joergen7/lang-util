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

(in-package :lang-util/test)
(in-suite lang-util-suite)


(test lang-util-find-duplicate
  (is (equal 'a (find-duplicate '(a a) :test #'eq)))
  (is-false (find-duplicate '(1 2 3) :test #'=))
  (is (equal 1 (find-duplicate '(1 2 1)))))

(test lang-util-line-pad
  (is (equal "  blub" (line-pad "blub" "  ")))
  (is (equal (format nil "  bla~%  blub") (line-pad (format nil "bla~%blub") "  ")))
  (is (equal "#include <iostream>" (line-pad "#include <iostream>" "  " :unless-starts-with #\#))))

(test lang-util-line-adjust
  (is (string-equal "" (line-adjust "" #\x 0)))
  (is (string-equal "" (line-adjust "blub" #\x 0)))
  (is (string-equal "xxxx" (line-adjust "" #\x 4)))
  (is (string-equal "blub" (line-adjust "blub" #\x 4)))
  (is (string-equal "blub" (line-adjust "bluba" #\x 4)))
  (is (string-equal (format nil "blax~%blub") (line-adjust (format nil "bla~%blub") #\x 4)))
  (is (string-equal (format nil "blak~%blub") (line-adjust (format nil "blak~%blub") #\x 4)))
  (is (string-equal (format nil "blak~%blub") (line-adjust (format nil "blakk~%blub") #\x 4))))

(test lang-util-pos->line
  (is (= 1 (pos->line "" 0)))
  (is (= 1 (pos->line (format nil "~%") 0)))
  (is (= 2 (pos->line (format nil "~%") 1)))
  (is (= 1 (pos->line (format nil "a~%") 0)))
  (is (= 1 (pos->line (format nil "a~%") 1)))
  (is (= 2 (pos->line (format nil "a~%") 2)))
  (is (= 1 (pos->line (format nil "a~%b") 0)))
  (is (= 1 (pos->line (format nil "a~%b") 1)))
  (is (= 2 (pos->line (format nil "a~%b") 2)))
  (is (= 2 (pos->line (format nil "a~%b") 3))))

