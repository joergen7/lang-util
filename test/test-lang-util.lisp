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

(test lang-util-line-pad-test
  (is (equal "  blub" (line-pad "blub" "  ")))
  (is (equal (format nil "  bla~%  blub") (line-pad (format nil "bla~%blub") "  ")))
  (is (equal "#include <iostream>" (line-pad "#include <iostream>" "  " :unless-starts-with #\#))))

