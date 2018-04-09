;;; code-archive.el ---  Archive and reference source code.   -*- lexical-binding: t -*-

;; Copyright (C) 2018 Michael Schuldt

;; Author: Michael Schuldt <mbschuldt@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/mschuldt/code-archive

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Archive and reference source code snippets.

;;; Code:

(require 'cl)

(defvar code-archive-dir "~/code-archive-TEST"
  "Directory in which to archive source files.")

(defvar code-archive-src-map '((lisp-interaction-mode . "emacs-lisp")
                               (makefile-automake-mode . "Makefile")
                               (GNUmakefile . "Makefile")
                               (fundamental-mode . "text")
                               (sh-mode . "bash")
                               (mhtml-mode . "html")
                               ))

(defvar code-archive--save-stack nil)

(defvar code-archive--link-file
  (concat (file-name-as-directory code-archive-dir) "_code-links.el"))

(defvar code-archive--id-file
  (concat (file-name-as-directory code-archive-dir) "_next-id.el"))

(defvar code-archive--codeblocks (make-hash-table))

(defvar code-archive-initialized nil
  "Non-nil when the code archive has been initialized.")

(defvar code-archive--codeblocks-loaded nil
  "Non-nil when codeblocks are loaded.")

(defstruct code-archive--entry codeblock src-type string)
(defstruct code-archive--codeblock id file archived-file line archived-git-commit archived-md5)

(defun code-archive-init ()
  "Initialize the code archive."
  (unless (or code-archive-initialized
              (file-exists-p (concat (file-name-as-directory code-archive-dir) ".git")))
    (unless (file-exists-p code-archive-dir)
      (mkdir code-archive-dir))
    (with-temp-buffer
      (write-file code-archive--link-file)
      (insert "0")
      (write-file code-archive--next-id-file))
    (shell-command
     (format "cd %s; git init; git add *; git commit -m \"initial\""
             code-archive-dir)))
  (setq code-archive-initialized t))

;;;###autoload
(defun code-archive-save-code ()
  "Archive the current buffer and save the region to the code archive kill stack."
  (interactive)
  (code-archive-init)
  (let* ((src-type (and major-mode (cdr (assoc major-mode code-archive-src-map))))
         (file (buffer-file-name))
         (line (and file (code-archive--current-line (and (region-active-p) (region-beginning)))))
         (region-string (and (region-active-p) (buffer-substring (region-beginning) (region-end))))
         (codeblock (code-archive--save-buffer-file)))
    (unless src-type
      (setq src-type (and major-mode (car (split-string (symbol-name major-mode) "-mode")))))
    (setf (code-archive--codeblock-file codeblock) file)
    (setf (code-archive--codeblock-line codeblock) line)
    (push (make-code-archive--entry :codeblock codeblock
                                    :src-type src-type
                                    :string region-string)
          code-archive--save-stack)))

;;;###autoload
(defun code-archive-yank-code ()
  "Insert an ‘org-mode’ styled code block sourced from the code archive kill stack."
  (interactive)
  (if code-archive--save-stack
      (let* ((entry (pop code-archive--save-stack))
             (codeblock (code-archive--entry-codeblock entry))
             (src-type (code-archive--entry-src-type entry))
             (string (code-archive--entry-string entry))
             (id (code-archive--next-id)))
        (setf (code-archive--codeblock-id codeblock) id)
        (code-archive--add-codeblock codeblock)
        (save-excursion (insert (format  "\n#+BEGIN_SRC %s :var _id=%s
%s\n#+END_SRC
" src-type id string))))
    ;;else:
    (message "org code ring is empty")))

;;;###autoload
(defun code-archive-goto-src ()
  "Open the origin source file of the codeblock at point.
The point must be on the first line."
  (interactive)
  (let (bound id info)
    (save-excursion
      (end-of-line)
      (setq bound (point))
      (beginning-of-line)
      (if (looking-at "[ \t]*#\\+BEGIN_SRC")
          (if (re-search-forward "_id=\\([0-9]+\\)" bound)
              (setq id (string-to-number (match-string 1)))
            (message "Error: could not find block id"))
        (message "Error: not on a source block header")))
    (when id
      (setq info (code-archive--get-block-info id))
      (if info
          (progn (find-file-other-window (code-archive--codeblock-file info))
                 (goto-char 1)
                 (forward-line (1- (code-archive--codeblock-line info))))
        (message "Error: no link info for codeblock id: %s" id)))))

(defun code-archive--next-id ()
  "Return the next source block id."
  (assert (not (zerop (or (nth 7 (file-attributes code-archive--id-file)) 0))))
  (let ((n (with-temp-buffer
             (insert-file-contents code-archive--id-file)
             (read (current-buffer)))))
    (with-temp-file code-archive--id-file
      (insert (number-to-string (1+ n))))
    n))

(defun code-archive--save-buffer-file ()
  "Archive the current buffer in `code-archive-dir'.
Return the archive data in a code-archive--codeblock struct."
  (save-buffer)
  (let* ((str (buffer-string))
         (checksum (md5 str))
         (path (or (buffer-file-name) ""))
         (name (replace-regexp-in-string "[/*]" "_"
                                         (or (file-name-nondirectory path)
                                             (buffer-name))))
         (filename (format "%s_%s" (md5 (or path (buffer-name))) name))
         (archive-path (concat (file-name-as-directory code-archive-dir)
                               filename))
         commit curr-md5 git-output commit-hash)

    (if (file-exists-p archive-path)
        ;; check if file has changed
        (progn (setq curr-md5
                     (code-archive--strip-end (shell-command-to-string
                                               (format "md5sum %s" archive-path))
                                              "\n"))
               (setq curr-md5 (car (split-string curr-md5)))
               (unless (string= checksum curr-md5)
                 (copy-file path archive-path :overwrite)
                 (setq commit "changed")))
      (progn (if path
                 ;; copy file to archive directory
                 (copy-file path archive-path)
               (with-temp-buffer
                 ;; file does not exist on disk, save the buffer contents to file
                 (insert str)
                 (write-file archive-path)))
             (setq commit "added")))

    (when commit
      (setq git-output
            (shell-command-to-string
             (format "cd %s && git add %s && git commit -m \"%s\""
                     code-archive-dir filename
                     (format "%s: %s" commit path))))
      (message  "git: %s" git-output))

    (setq commit-hash (code-archive--strip-end
                       (shell-command-to-string
                        (format "cd %s && git rev-parse HEAD" code-archive-dir))
                       "\n"))
    (make-code-archive--codeblock :file path
                                  :archived-file filename
                                  :archived-git-commit commit-hash
                                  :archived-md5 checksum)
    ))

(defun code-archive--current-line (&optional point)
  "Get the current line at the current point or at POINT."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (or point (point)))))))

(defun code-archive--char-split-string (string)
  "Split a STRING into its charaters."
  (cdr (butlast (split-string string ""))))

(defun code-archive--strip-end (string &optional char)
  "If CHAR occurs at the end of STRING, remove it."
  (let ((split (code-archive--char-split-string string))
        (char (or char " ")))
    (while (string= char (car (last split)))
      (setq split (butlast split)))
    (mapconcat 'identity split "")))

(defun code-archive--git-commit (msg)
  (message "git: %s" (shell-command-to-string
                      (format "cd %s && git add * && git commit -m \"%s\""
                              code-archive-dir
                              msg))))

(defun code-archive--add-codeblock (codeblock)
  "Add a new CODEBLOCK link to the archive."
  (code-archive--load-codeblocks)
  (with-temp-buffer
    (let ((print-level nil)
          (print-length nil))
      (print codeblock (current-buffer)))
    (append-to-file (point-min)
                    (point-max)
                    code-archive--link-file))
  (puthash (code-archive--codeblock-id codeblock)
           codeblock
           code-archive--codeblocks)
  (code-archive--git-commit "added: code block link"))

(defun code-archive--get-block-info (id)
  "Return the source information for codeblock with given ID."
  (code-archive--load-codeblocks)
  (gethash id code-archive--codeblocks))

(defun code-archive--load-codeblocks ()
  "Load code archive codeblocks links."
  (unless code-archive--codeblocks-loaded
    (let ((c 0)
          (codeblocks (make-hash-table))
          blocks)
      (with-temp-buffer
        (condition-case nil
            (progn
              (insert-file-contents-literally code-archive--link-file)
              (goto-char (point-max))
              (insert ")")
              (goto-char 1)
              (insert "(")
              (goto-char 1)
              (setq blocks (read (current-buffer))))
          (error
           (message "Error reading kb codeblock file '%s'" code-archive--link-file))))
      (dolist (x blocks)
        (if (gethash (code-archive--codeblock-id x) codeblocks)
            (error  "Duplicate codeblock link for id: %s" (code-archive--codeblock-id x))
          (puthash (code-archive--codeblock-id x) x codeblocks)
          (setq c (1+ c))))
      (setq code-archive--codeblocks codeblocks)
      (message (format "loaded %s codeblock links" c)))
    (setq code-archive--codeblocks-loaded t)))

(provide 'code-archive)

;;; code-archive.el ends here
