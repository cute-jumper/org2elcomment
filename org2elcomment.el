;;; org2elcomment.el --- Convert Org file to Elisp comments  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Package-Requires: ((org "8.3.4"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;                             _______________

;;                              ORG2ELCOMMENT

;;                               Junpeng Qiu
;;                             _______________


;; Table of Contents
;; _________________

;; 1 Overview
;; 2 Usage
;; 3 Customization


;; Convert `org-mode' file to Elisp comments.


;; 1 Overview
;; ==========

;;   This simple package is mainly used for Elisp package writers. After
;;   you've written the `README.org' for your package, you can use
;;   `org2elcomment' to convert the org file to Elisp comments in the
;;   corresponding source code file.


;; 2 Usage
;; =======

;;   Make sure your source code file has `;;; Commentary:' and `;;; Code:'
;;   lines. The generated comments will be put between these two lines. If
;;   you use `auto-insert', it will take care of generating a standard file
;;   header that contains these two lines in your source code.

;;   In your Org file, invoke `org2elcomment', select the source code file,
;;   and done! Now take a look at your source code file, you can see your
;;   Org file has been converted to the comments in your source code file.


;; 3 Customization
;; ===============

;;   Behind the scenes, this package uses `org-export-as' function and the
;;   default backend is `ascii'. You can change to whatever backend that
;;   your org-mode export engine supports, such as `md' (for markdown):
;;   ,----
;;   | (setq org2elcomment-backend 'md)
;;   `----

;;; Code:

(require 'org)
(require 'pulse nil t)

(defvar org2elcomment-backend 'ascii)

(defvar org2elcomment-last-source nil)
(make-variable-buffer-local 'org2elcomment-last-source)

(defvar org2elcomment-anywhere-last-source nil)

(defvar org2elcomment-anywhere-readme nil)
(make-variable-buffer-local 'org2elcomment-anywhere-readme)

(defvar org2elcomment-readme-exporter 'org2elcomment--export-readme)

(defun org2elcomment--find-bounds (buffer)
  (let (beg end)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^;;;[[:blank:]]+Commentary:[[:blank:]]*$" nil t)
          (setq beg (line-beginning-position 2))
          (when (re-search-forward "^;;;[[:blank:]]+Code:[[:blank:]]*$")
            (setq end (line-beginning-position))
            (if (and beg end)
                (cons beg end)
              (message "org2elcomment: No \";;; Commentary:\" or \";;; Code:\" found.")
              nil)))))))

;;;###autoload
(defun org2elcomment-anywhere (el-file &optional readme-file)
  (interactive
   (list
    (let ((prompt
           (if org2elcomment-anywhere-last-source
               (format "Elisp file (default \"%s\"): "
                       org2elcomment-anywhere-last-source)
             "Elisp file: ")))
      (setq org2elcomment-anywhere-last-source
            (read-file-name prompt
                            nil
                            org2elcomment-anywhere-last-source
                            t)))))
  (let* ((el-file-dir (file-name-directory el-file)))
    (with-temp-buffer
      (insert-file-contents el-file)
      (emacs-lisp-mode)

      ;; Load local-variable `org2elcomment-anywhere-readme' from current buffer
      (hack-local-variables)

      (goto-char (point-min))
      (setq readme-file
            (or (if (and readme-file (file-exists-p readme-file))
                    readme-file)
                (org2elcomment--get-readme-setting (current-buffer) el-file-dir)
                (read-file-name "README.org file: " nil nil t)))

      (unless (org2elcomment--get-readme-setting (current-buffer) el-file-dir)
        (org2elcomment--save-readme-setting
         (current-buffer) (file-relative-name readme-file el-file-dir)))

      (let ((bounds (org2elcomment--find-bounds (current-buffer)))
            (output (funcall org2elcomment-readme-exporter readme-file))
            beg)
        (when (and bounds output)
          (kill-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          (insert "\n")
          (setq beg (point))
          (insert output)
          (comment-region beg (point))
          (insert "\n")
          (setq output (buffer-string))
          (if (file-locked-p el-file)
              (message "org2elcomment: File %S has been locked. Operation denied. " el-file)
            (let ((buffer (get-file-buffer el-file)))
              (if buffer
                  (with-current-buffer buffer
                    (let ((point (point)))
                      (erase-buffer)
                      (insert output)
                      (goto-char point))
                    (message "org2elcomment: The commentary in buffer %S has been updated! "
                             (buffer-name buffer)))
                (write-region (point-min)
                              (point-max) el-file)
                (message "org2elcomment: The commentary of file %S have been updated! " el-file)))))))))

(defun org2elcomment--export-readme (readme-file)
  (let (output)
    (with-temp-buffer
      (insert-file-contents readme-file)
      (setq output
            (org-export-as org2elcomment-backend)))
    output))

(defun org2elcomment--save-readme-setting (buffer value)
  "Set current file's local variable `org2elcomment-readme'."
  (when (yes-or-no-p "Save README.org setting to elisp file? ")
    (with-current-buffer buffer
      (add-file-local-variable
       'org2elcomment-anywhere-readme value))))

(defun org2elcomment--get-readme-setting (buffer el-file-dir)
  "Get readme setting from current buffer's context.

`org2elcomment-anywhere-readme' only save README.org's file name
we need to return its full path based directory EL-FILE-DIR."
  (with-current-buffer buffer
    (let ((readme-file
           (when org2elcomment-anywhere-readme
             (concat (file-name-as-directory el-file-dir)
                     org2elcomment-anywhere-readme))))
      (when (and readme-file
                 (file-exists-p readme-file))
        readme-file))))

;;;###autoload
(defun org2elcomment (file-name)
  (interactive
   (list
    (let ((prompt
           (if org2elcomment-last-source
               (format "Source file (default \"%s\"): "
                       org2elcomment-last-source)
             "Source file: ")))
      (setq org2elcomment-last-source (read-file-name prompt
                                                      nil
                                                      org2elcomment-last-source
                                                      t)))))
  (let* ((src-buf (find-file-noselect file-name))
         (bounds (org2elcomment--find-bounds src-buf))
         (output (org-export-as org2elcomment-backend))
         beg end)
    (when bounds
      (progn
        (with-current-buffer src-buf
          (kill-region (car bounds) (cdr bounds))
          (save-excursion
            (goto-char (car bounds))
            (insert "\n")
            (setq beg (point))
            (insert output)
            (comment-region beg (point))
            (insert "\n")
            (setq end (point))))
        (switch-to-buffer src-buf)
        (push-mark)
        (goto-char (car bounds))
        (recenter 0)
        (when (featurep 'pulse)
          (pulse-momentary-highlight-region (car bounds) end))))))

(provide 'org2elcomment)
;;; org2elcomment.el ends here

;; Local Variables:
;; org2elcomment-anywhere-readme: "README.org"
;; End:
