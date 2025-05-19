;;; plru-test.el --- tests for plru.el

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'plru)

(defmacro plru-with-repository (var arglist &rest body)
  (declare (indent 2) (debug t))
  `(let ((,var (apply plru-repository ',arglist)))
     (unwind-protect
         (progn
           ,@body)
       (plru-destroy-repository ,(car arglist)))))

(ert-deftest plru-create-repo ()
  (plru-with-repository repo ("plru-test/tmp")
    (should (object-of-class-p repo 'plru-repository))))

(ert-deftest plru-double-destroy ()
  (plru-with-repository repo ("plru-test/tmp")
    (plru-destroy-repository "plru-test/tmp")))

(ert-deftest plru-put-get ()
  (plru-with-repository repo ("plru-test/tmp")
    (plru-put repo 'foo 42)
    (should (eq 42 (plru-get repo 'foo)))))

(ert-deftest plru-validate-simple ()
  (plru-with-repository repo ("plru-test/tmp")
    (plru-put repo 'foo 42)
    (should (plru-validate-repo repo))))

(ert-deftest plru-get-invalidated ()
  (plru-with-repository repo ("plru-test/tmp")
    (plru-put repo 'foo 42)
    (should (eq 42 (plru-get repo 'foo)))
    (plru-invalidate repo 'foo)
    (should (null (plru-get repo 'foo)))))

(ert-deftest plru-put-reload-get ()
  (plru-with-repository repo ("plru-test/tmp1")
    (plru-put repo 'foo 44)
    (plru-save repo t)
    (with-current-buffer (find-file-noselect
                          (concat plru-directory "plru-test/tmp1"))
      (goto-char (point-min))
      (while (search-forward "tmp1" nil t)
        (replace-match "tmp2"))
      (write-file (concat plru-directory "plru-test/tmp2"))))
  (plru-with-repository repo ("plru-test/tmp2")
    (should (eq 44 (plru-get repo 'foo)))))

(ert-deftest plru-keys-most-to-least ()
  (plru-with-repository repo ("plru-test/tmp")
    (plru-put repo 'foo "foo")
    (plru-put repo 'bar "bar")
    (plru-put repo 'baz "baz")
    (should
     (equal '(baz bar foo) (plru-entry-keys-most-to-least-recent repo)))))

(ert-deftest plru-keys-least-to-most ()
  (plru-with-repository repo ("plru-test/tmp")
    (plru-put repo 'foo "foo")
    (plru-put repo 'bar "bar")
    (plru-put repo 'baz "baz")
    (should
     (equal '(foo bar baz) (plru-entry-keys-least-to-most-recent repo)))))

(ert-deftest plru-updates-recency ()
  (plru-with-repository repo ("plru-test/tmp")
    (plru-put repo 'foo "foo")
    (plru-put repo 'bar "bar")
    (plru-put repo 'baz "baz")
    (plru-put repo 'bar "bar")
    (should
     (equal '(bar baz foo) (plru-entry-keys-most-to-least-recent repo)))))

(ert-deftest plru-maintains-max-size ()
  (plru-with-repository repo ("plru-test/tmp" :max-size 3)
    (plru-put repo 'foo1 1)
    (plru-put repo 'foo2 2)
    (plru-put repo 'foo3 3)
    (plru-put repo 'foo4 4)
    (should
     (and
      (= 3 (length (hash-table-keys (oref repo entries))))
      (= 3 (length (plru-entry-keys-least-to-most-recent repo)))))))

(ert-deftest plru-removes-oldest ()
  (plru-with-repository repo ("plru-test/tmp" :max-size 3)
    (plru-put repo 'foo1 1)
    (plru-put repo 'foo2 2)
    (plru-put repo 'foo3 3)
    (plru-put repo 'foo4 4)
    (should
     (and (equal '(foo4 foo3 foo2) (plru-entry-keys-most-to-least-recent repo))
          (plru-validate-repo repo)))))

(provide 'plru-test)
;;; plru-test.el ends here
