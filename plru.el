;;; plru.el --- Persistent LRU cache -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: May 17, 2025
;; Modified: May 17, 2025
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/cashweaver/plru
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Persistent LRU cache
;;
;;  Inspired by plru
;;
;;; Code:

(require 'cl-lib)
(require 'cl-extra)
(require 'eieio)
(require 'eieio-base)

(defvar plru-directory (concat user-emacs-directory "plru/"))

(defvar *plru-repositories* (make-hash-table :test 'equal))

(defvar plru-avoid-recursion nil)

(defconst plru-default-save-delay 300)

(defconst plru-internal-version-constant "0.0")

(defconst plru-version-constant
  (format "%s/%s" emacs-version plru-internal-version-constant))

(defclass
  plru-repository (eieio-persistent eieio-named)
  ((version :initarg :version :initform nil)
   (version-constant :allocation :class)
   (entry-key-list :initarg :entry-key-list :initform '())
   (entries :initarg :entries :initform (make-hash-table))
   (entry-cls :initarg :entry-cls :initform plru-entry)
   (timestamp :initarg :timestamp :initform (float-time (current-time)))
   (max-size :initarg :max-size :initform 200)
   (save-delay :initarg :save-delay)))

(oset-default 'plru-repository :save-delay plru-default-save-delay)
(oset-default 'plru-repository version-constant plru-version-constant)

(defvar *plru-repository-name* nil)

(cl-defmethod make-instance ((cls (subclass plru-repository)) &rest args)
  (let* ((newname
          (or (and (stringp (car args)) (car args))
              (plist-get args :object-name)
              *plru-repository-name*
              (symbol-name cls)))
         (e (gethash newname *plru-repositories*))
         (path (concat plru-directory newname)))
    (setq args (append args (list :object-name newname)))
    (or e
        (and (not plru-avoid-recursion)
             (file-exists-p path)
             (condition-case nil
                 (let* ((plru-avoid-recursion t)
                        (*plru-repository-name* newname)
                        (obj (eieio-persistent-read path 'plru-repository t)))
                   (and (or (plru-validate-repo obj) (error "wrong version"))
                        (puthash newname obj *plru-repositories*)
                        obj))
               (error nil)))
        (let ((obj (cl-call-next-method))
              (dir (file-name-directory path)))
          (unless (file-exists-p dir)
            (make-directory dir t))
          (oset obj :file path)
          (oset obj :version (oref-default obj version-constant))
          (puthash newname obj *plru-repositories*)
          obj))))

(defun plru-hash-table-values (h)
  (let (values)
    (maphash (lambda (k v) (push v values)) h)
    values))

(cl-defmethod plru-entry-keys-most-to-least-recent ((cache plru-repository))
  "Return entries from CACHE in DESCENDING order."
  (oref cache entry-key-list))

(cl-defmethod plru-entry-keys-least-to-most-recent ((cache plru-repository))
  "Return entries from CACHE in DESCENDING order."
  (nreverse (oref cache entry-key-list)))

;; force custom implementation.
(cl-defmethod plru-validate-repo ((cache t))
  nil)

(cl-defmethod plru-validate-repo ((cache plru-repository))
  (and
   (equal
    (oref cache version)
    (oref-default (eieio-object-class cache) version-constant))
   (listp (oref cache entry-key-list)) (hash-table-p (oref cache entries))
   (and
    ;; Assert entries in hash and list match
    (equal
     (length (oref cache entry-key-list))
     (length (hash-table-keys (oref cache entries))))
    (cl-every
     (function
      (lambda (key)
        (plru-has cache key)))
     (plru-entry-keys-most-to-least-recent cache)))
   (cl-every
    ;; Assert every entry in hash is of expected type
    (function
     (lambda (entry)
       (and (object-of-class-p entry (oref cache entry-cls))
            (plru-validate-entry entry))))
    (plru-hash-table-values (oref cache entries)))))

(defclass
  plru-entry ()
  ((value :initarg :value :initform nil)
   (value-cls :initarg :value-cls :initform nil)))

;; force custom implementation.
(cl-defmethod plru-validate-entry ((entry t))
  nil)

(cl-defmethod plru-validate-entry ((entry plru-entry))
  (or (null (oref entry value-cls))
      (object-of-class-p (oref entry value) (oref entry value-cls))))

(cl-defmethod plru-entry-valid-p ((entry plru-entry))
  t)

(cl-defmethod plru-get ((cache plru-repository) key &optional default)
  (let* ((entries (oref cache entries))
         (entry (gethash key entries)))
    (if entry
        (oref entry value)
      default)))

(cl-defmethod plru-has ((cache plru-repository) key)
  "Return non-nil if CACHE holds KEY."
  (let* ((default (make-symbol ":nil"))
         (table (oref cache entries))
         (entry (gethash key table default)))
    (if (or (eq entry default)
            (not (plru-entry-valid-p entry)))
        nil
      entry)))

(cl-defmethod plru-full-p ((cache plru-repository))
  "Return non-nil if CACHE is full."
  (>= (length (hash-table-keys (oref cache entries)))
      (oref cache max-size)))

(cl-defmethod plru--remove-oldest ((cache plru-repository))
  "Remove the oldest entry from CACHE."
  (let* ((table (oref cache entries))
         (oldest-key (car (last (oref cache entry-key-list)))))
    (oset cache :entry-key-list (butlast (oref cache entry-key-list)))
    (remhash oldest-key table)
    (plru-save cache)))

(cl-defmethod plru-put ((cache plru-repository) key value)
  (let ((entry
         (or (and (eieio-object-p value)
                  (object-of-class-p value 'plru-entry)
                  value)
             (make-instance
              (oref cache entry-cls)
              :value value
              :value-cls
              (and (eieio-object-p value) (eieio-object-class value))))))
    (when (plru-full-p cache)
      (plru--remove-oldest cache))
    (if (plru-has cache key)
        (oset cache :entry-key-list (remove key (oref cache entry-key-list)))
      (puthash key entry (oref cache entries)))
    (push key (oref cache entry-key-list))
    (plru-save cache)
    entry))

(cl-defmethod plru-invalidate ((cache plru-repository) key)
  (let ((table (oref cache entries)))
    (oset cache :entry-key-list (remove key (oref cache entry-key-list)))
    (remhash key table)
    (plru-save cache)))

(cl-defmethod plru-clear ((cache plru-repository))
  (let* ((entries (oref cache entries))
         (test (hash-table-test entries))
         (resize (hash-table-rehash-size entries))
         (threshold (hash-table-rehash-threshold entries))
         (weakness (hash-table-weakness entries)))
    (oset cache :entry-key-list '())
    (oset
     cache
     :entries
     (make-hash-table
      :test test
      :rehash-size resize
      :rehash-threshold threshold
      :weakness weakness)))
  (plru-save cache))

(cl-defmethod plru-purge-invalid ((cache plru-repository))
  (let ((table (oref cache entries)))
    (maphash
     (lambda (k e)
       (unless (plru-entry-valid-p e)
         (remhash k table)))
     table)
    (plru-save cache)))

(cl-defmethod plru-save ((cache plru-repository) &optional force)
  (let ((timestamp (oref cache timestamp))
        (delay (oref cache save-delay))
        (time (float-time (current-time))))
    (when (or force (> time (+ timestamp delay)))
      (oset cache :timestamp time)
      ;; make sure version is saved to file
      (oset
       cache
       :version (oref-default (eieio-object-class cache) version-constant))
      (eieio-persistent-save cache))))

(cl-defmethod plru-map ((cache plru-repository) func)
  (let ((table (oref cache entries)))
    (maphash func table)))

(defun plru-kill-emacs-hook ()
  (maphash
   (lambda (k v)
     (condition-case nil
         (plru-purge-invalid v)
       (error nil))
     (condition-case nil
         (plru-save v t)
       (error nil)))
   *plru-repositories*))

(defun plru-destroy-repository (name)
  (remhash name *plru-repositories*)
  (let ((fname (concat plru-directory name)))
    (when (file-exists-p fname)
      (delete-file fname))))

(add-hook 'kill-emacs-hook #'plru-kill-emacs-hook)

;; in case we reload in place, clean all repositories with invalid version
(let (to-clean)
  (maphash
   (lambda (k v)
     (condition-case nil
         (unless (eql (oref v version) plru-version-constant)
           (signal 'error nil))
       (error (setq to-clean (cons k to-clean)))))
   *plru-repositories*)
  (dolist (k to-clean)
    (remhash k *plru-repositories*)))

(provide 'plru)
;;; plru.el ends here
