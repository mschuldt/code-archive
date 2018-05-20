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

(defvar code-archive-dir "~/code-archive"
  "Directory in which to archive source files.")

(defvar code-archive-src-map '((lisp-interaction-mode . "emacs-lisp")
                               (makefile-automake-mode . "Makefile")
                               (GNUmakefile . "Makefile")
                               (fundamental-mode . "text")
                               (sh-mode . "bash")
                               (mhtml-mode . "html")
                               ))

(defvar code-archive-git-executable "git"
  "The Git executable used by code-archive.")

(defvar code-archive--save-stack nil)

(defvar code-archive--codeblocks (make-hash-table))

(defvar code-archive-initialized nil
  "Non-nil when the code archive has been initialized.")

(defvar code-archive--codeblocks-loaded nil
  "Non-nil when codeblocks are loaded.")

(defstruct code-archive--entry
  codeblock src-type string)

(defstruct code-archive--codeblock
  id file archived-file line archived-git-commit archived-md5)

(defun code-archive--link-file ()
  "Return the archive link-file."
  (concat (file-name-as-directory code-archive-dir) "_code-links.el"))

(defun code-archive--id-file ()
  "Return the archive id-file."
  (concat (file-name-as-directory code-archive-dir) "_next-id.el"))

(defun code-archive--run-git (&rest command-args)
  "Execute Git with COMMAND-ARGS, display any output."
  (let (s)
    (with-temp-buffer
      (cd code-archive-dir)
      (dolist (args command-args)
        (erase-buffer)
        (apply 'call-process code-archive-git-executable nil t nil args)
        (setq s (buffer-string))
        (when (> (length s) 0)
          (message s))))))

(defun code-archive-init ()
  "Initialize the code archive."
  (unless (or code-archive-initialized
              (file-exists-p (concat (file-name-as-directory code-archive-dir)
                                     ".git")))
    (unless (file-exists-p code-archive-dir)
      (mkdir code-archive-dir))
    (with-temp-buffer
      (write-file (code-archive--link-file))
      (insert "0")
      (write-file (code-archive--id-file)))
    (code-archive--run-git '("init")
                           '("add" "*")
                           '("commit" "-m" "initial")))
  (setq code-archive-initialized t))

(defun code-archive--source-type ()
  "Return the source type of the current buffer."
  (when major-mode
    (or (cdr (assoc major-mode code-archive-src-map))
        (car (split-string (symbol-name major-mode) "-mode")))))

;;;###autoload
(defun code-archive-save-code ()
  "Archive the current buffer and save the region to the code archive stack."
  (interactive)
  (code-archive-init)
  (let* ((file (buffer-file-name))
         (line (and file
                    (line-number-at-pos (and (region-active-p)
                                             (region-beginning))
                                        t)))
         (region-string (and (region-active-p)
                             (buffer-substring (region-beginning)
                                               (region-end))))
         (codeblock (code-archive--save-buffer-file)))
    (setf (code-archive--codeblock-file codeblock) file)
    (setf (code-archive--codeblock-line codeblock) line)
    (push (make-code-archive--entry :codeblock codeblock
                                    :src-type (code-archive--source-type)
                                    :string region-string)
          code-archive--save-stack)))

(defun code-archive--format-org-block ()
  "Format an `org-mode' styled code block sourced from the code archive stack.
This consumes an entry from ‘code-archive--save-stack’."
  (let* ((entry (pop code-archive--save-stack))
         (codeblock (code-archive--entry-codeblock entry))
         (src-type (code-archive--entry-src-type entry))
         (string (code-archive--entry-string entry))
         (id (code-archive--next-id)))
    (setf (code-archive--codeblock-id codeblock) id)
    (code-archive--add-codeblock codeblock)
    (format  "\n#+BEGIN_SRC %s :var _id=%s
%s\n#+END_SRC
" src-type id string)))

;;;###autoload
(defun code-archive-insert-org-block ()
  "Insert an `org-mode' styled code block sourced from the code archive stack.
This consumes an entry from ‘code-archive--save-stack’."
  (interactive)
  (if code-archive--save-stack
      (save-excursion
        (insert (code-archive--format-org-block)))
    (message "org code ring is empty")))

;;;###autoload
(defun code-archive-do-org-capture (filename)
  "For use in an org capture template, insert an org code block.
FILENAME is the name of the file visited by buffer when org capture was called.
Usage in capture template: (code-archive-do-org-capture \"%f\")"
  (with-current-buffer (find-buffer-visiting filename)
    (code-archive-save-code))
  (code-archive--format-org-block))

;;;###autoload
(defun code-archive-org-src-tag (filename)
  "For use in an org capture template, insert an org code block.
FILENAME is the name of the file visited by buffer when org capture was called.
Usage in capture template: (code-archive-org-src-tag \"%f\")"
  (let (src-type)
    (with-current-buffer (find-buffer-visiting filename)
      (setq src-type (code-archive--source-type)))
    (if src-type
        (format ":%s:" (replace-regexp-in-string"-" "_" src-type))
      "")))

;;;###autoload
(defun code-archive-goto-src ()
  "Open the original source file of the codeblock at point.
The point must be on the first line." ;;TODO: jump from anywhere in the source block
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
          (let* ((source-file (code-archive--codeblock-file info))
                 (file source-file)
                 (archive-md5 (code-archive--codeblock-archived-md5 info))
                 (line (1- (code-archive--codeblock-line info)))
                 (file-exists (file-exists-p file))
                 changed)
            (unless (and file-exists
                         (string= (code-archive--file-md5 file)
                                  archive-md5))
              (setq file (concat (file-name-as-directory code-archive-dir)
                                 (code-archive--codeblock-archived-file info))
                    changed t))
            (find-file-other-window file)
            (goto-char 1)
            (forward-line line)
            (when changed
              (read-only-mode 1)
              (if file-exists
                  (message "Visiting archived version. Press 'o' to visit original changed file")
                (message "Visiting archived version. Original file deleted."))

              (local-set-key (kbd "o")
                             (lambda ()
                               (interactive)
                               (if file-exists
                                   (progn
                                     (find-file-other-window source-file)
                                     (goto-char 1)
                                     (forward-line line))
                                 (message "Original file does not exist"))
                               ))
              (local-set-key (kbd "q")
                             (lambda ()
                               (interactive)
                               (kill-buffer)
                               ))))
        (message "Error: no link info for codeblock id: %s" id)))))

(defun code-archive--next-id ()
  "Return the next source block id."
  (assert (not (zerop (or (nth 7 (file-attributes (code-archive--id-file))) 0))))
  (let ((n (with-temp-buffer
             (insert-file-contents (code-archive--id-file))
             (read (current-buffer)))))
    (with-temp-file (code-archive--id-file)
      (insert (number-to-string (1+ n))))
    n))

(defun code-archive--file-md5 (filename)
  "Calculate the md5 digest of the file FILENAME."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (md5 (buffer-string))))

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
         commit curr-md5 commit-hash)

    (if (file-exists-p archive-path)
        ;; check if file has changed
        (progn (setq curr-md5 (code-archive--file-md5 archive-path))
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
      (code-archive--run-git (list "add" filename)
                             (list "commit" "-m" (format "%s: %s"
                                                         commit path))))

    (setq commit-hash (code-archive--strip-end
                       (shell-command-to-string
                        (format "cd %s && git rev-parse HEAD" code-archive-dir))
                       "\n"))
    (make-code-archive--codeblock :file path
                                  :archived-file filename
                                  :archived-git-commit commit-hash
                                  :archived-md5 checksum)
    ))

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

(defun code-archive--record-to-vector (record)
  "Convert RECORD type to a vector."
  (let* ((len (1- (length record)))
         (v (make-vector len nil)))
    (dotimes (i len)
      (aset v i (aref record (1+ i))))
    v))

(defun code-archive--codeblock-to-vector (codeblock)
  "Convert CODEBLOCK type to a vector."
  (cond ((vectorp codeblock)
         codeblock)
        ((recordp codeblock)
         (code-archive--record-to-vector codeblock))
        (t (error "Unhanded type: %s" (type-of codeblock)))))

(defun code-archive--array-to-codeblock (a)
  "Create a codeblock struct from the array A."
  (make-code-archive--codeblock :id (aref a 0)
                                :file (aref a 1)
                                :archived-file (aref a 2)
                                :line (aref a 3)
                                :archived-git-commit (aref a 4)
                                :archived-md5 (aref a 5)))

(defun code-archive--add-codeblock (codeblock)
  "Add a new CODEBLOCK link to the archive."
  (code-archive--load-codeblocks)
  (with-temp-buffer
    (let ((print-level nil)
          (print-length nil))
      (print (code-archive--codeblock-to-vector codeblock) (current-buffer)))
    (append-to-file (point-min)
                    (point-max)
                    (code-archive--link-file)))
  (puthash (code-archive--codeblock-id codeblock)
           codeblock
           code-archive--codeblocks)
  (code-archive--run-git '("add" "*")
                         (list "commit" "-m"
                               (format "added: code block link %s"
                                       (code-archive--codeblock-id codeblock)))))

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
        (condition-case err
            (progn
              (insert-file-contents-literally (code-archive--link-file))
              (goto-char (point-max))
              (insert ")")
              (goto-char 1)
              (insert "(")
              (goto-char 1)
              (setq blocks (mapcar 'code-archive--array-to-codeblock
                                   (read (current-buffer)))))
          (error (message "Error reading kb codeblock file '%s': %s"
                          (code-archive--link-file) err))))
      (dolist (x blocks)
        (if (gethash (code-archive--codeblock-id x) codeblocks)
            (error  "Duplicate codeblock link for id: %s"
                    (code-archive--codeblock-id x))
          (puthash (code-archive--codeblock-id x) x codeblocks)
          (setq c (1+ c))))
      (setq code-archive--codeblocks codeblocks)
      (message (format "loaded %s codeblock links" c)))
    (setq code-archive--codeblocks-loaded t)))

(provide 'code-archive)

;;; code-archive.el ends here
