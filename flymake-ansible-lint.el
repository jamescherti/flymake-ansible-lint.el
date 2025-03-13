;;; flymake-ansible-lint.el --- A Flymake backend for ansible-lint -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.2
;; URL: https://github.com/jamescherti/flymake-ansible-lint.el
;; Keywords: tools
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The flymake-ansible-lint package provides a Flymake backend for ansible-lint,
;; enabling real-time syntax and style checking for Ansible playbooks and roles
;; within Emacs.
;;
;; (This package can also work with Flycheck: simply use the `flymake-flycheck`
;; package, which allows any Emacs Flymake backend to function as a Flycheck
;; checker.)
;;
;; Installation from MELPA:
;; ------------------------
;; (use-package flymake-ansible-lint
;;   :ensure t
;;   :commands flymake-ansible-lint-setup
;;   :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
;;          ((yaml-ts-mode yaml-mode) . flymake-mode)))
;;
;; Customizations:
;; ---------------
;; You can configure ansible-lint parameters using the flymake-ansible-lint-args
;; variable:
;;   (setq flymake-ansible-lint-args '("--offline"
;;                                     "-x" "run-once[play],no-free-form"))
;;
;; Links:
;; ------
;; - ansible-lint.el @GitHub:
;;   https://github.com/jamescherti/flymake-ansible-lint.el

;;; Code:


(require 'flymake)

(defgroup flymake-ansible-lint nil
  "Non-nil if flymake-ansible-lint mode mode is enabled."
  :group 'flymake-ansible-lint
  :prefix "flymake-ansible-lint-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/flymake-ansible-lint.el"))

(defcustom flymake-ansible-lint-args nil
  "List of additional command-line arguments for the `ansible-lint` tool.

This variable specifies a list of command-line arguments to pass to the
`ansible-lint` command. You can customize this list to include any extra options
or settings you need for `ansible-lint`.

Example usage:
  (setq flymake-ansible-lint-args (list \"--project-dir\" \"/path/to/dir\"))

Useful `ansible-lint` command-line arguments:
  --offline
  Disable installation of requirements.yml and schema refreshing.

  -c CONFIG_FILE, --config-file CONFIG_FILE
  Specify the configuration file to use. By default, `ansible-lint` looks for
  `.ansible-lint` or `.config/ansible-lint.yml`.

  --project-dir PROJECT_DIR
  Define the location of the project/repository. `ansible-lint` will autodetect
  based on the configuration file's location.

  -x SKIP_LIST, --skip-list SKIP_LIST
  Only check rules whose IDs/tags do not match these values (e.g.,
  `--skip-list=name,run-once`).

  -w WARN_LIST, --warn-list
  Only warn about rules in this list, unless overridden in the config file.
  The current version defaults to: `experimental`, `jinja[spacing]`.

  -r RULESDIR, --rules-dir RULESDIR
  Specify custom rule directories. Use `-R` to retain embedded rules from
  /usr/lib/python3/dist-packages/ansiblelint/rules

  -R
  Retain default rules when using `-r`.

  --profile {min,basic,moderate,safety,shared,production}
  Specify which rules profile to be used.

  -t TAGS, --tags TAGS
  Only check rules whose IDs/tags match these values.

  --enable-list ENABLE_LIST
  Activate optional rules based on their tag names."
  :type '(list string)
  :group 'flymake-ansible-lint)

(defcustom flymake-ansible-lint-executable "ansible-lint"
  "Path to the ansible-lint executable.
If not specified with a full path (e.g., ansible-lint), the
`flymake-ansible-lint-backend' function will search for the executable in the
directories listed in the $PATH environment variable."
  :type 'string
  :group 'flymake-ansible-lint)

(defvar flymake-ansible-lint-tmp-files-prefix "flymake_"
  "Prefix used for temporary files created by Flymake Ansible.
This prefix is used to identify temporary files created by Flymake Ansible
in the same directory as the original file being linted with ansible-lint.")

(defvar flymake-ansible-lint-tmp-files-enabled t
  "Control whether Flymake Ansible Lint creates a temporary file.
When non-nil, `flymake-ansible-lint' will create a temporary file in the same
directory as the original file, allowing for syntax checking to occur even when
`flymake-no-changes-timeout' is active and the file has not been saved.
Default value is t, enabling temporary file creation.")

(defvar-local flymake-ansible-lint--quickdef-procs nil
  "Internal variable used by `flymake-ansible-lint--quickdef-backend'.
Do not edit its value.  This variable holds a plist used to store
handles to running processes for Flymake backends.  Entries are
keyed by the symbol name of the appropriate backend function and
values are running processes.")

(defmacro flymake-ansible-lint--quickdef-backend (name &optional docstring &rest defs)
  "Custom version of quickdef that supports cleanup.
Quickly define a backend for use with Flymake. This macro produces a new
function, NAME, which is suitable for use with the variable
`flymake-diagnostic-functions'. If a string DOCSTRING is provided, it will be
the documentation for the function. The body of the function is generated based
on values provided to the macro in DEFS, described below."
  (declare (indent defun) (doc-string 2))
  ;; Do some initial sanity checks on the provided arguments
  ;; Ex: If there isn't a docstring, join the first argument into def-plist
  (unless lexical-binding
    (error "Need lexical-binding for flymake-quickdef-backend (%s)" name))
  (let* ((def-docstring (when (stringp docstring) docstring))
         (def-plist (if (stringp docstring) defs (cons docstring defs)))
         (write-type (or (eval (plist-get def-plist :write-type)) 'pipe))
         (temp-dir-symb (make-symbol "fmqd-temp-dir"))
         (fmqd-err-symb (make-symbol "fmqd-err"))
         (diags-symb (make-symbol "diags"))
         (cleanup-form (when (eq write-type 'file)
                         (list (list 'delete-directory temp-dir-symb t))))
         (custom-cleanup
          (let ((custom-cleanup-value (plist-get def-plist :cleanup)))
            (when custom-cleanup-value
              custom-cleanup-value))))
    (dolist (elem '(:proc-form :search-regexp :prep-diagnostic))
      (unless (plist-get def-plist elem)
        (error "Missing flymake backend definition `%s'" elem)))
    (unless (memq write-type '(file pipe nil))
      (error "Invalid `:write-type' value `%s'" (plist-get def-plist :write-type)))
    ;; Start of actual generated function definition
    `(defun ,name (report-fn &rest _args)
       ,def-docstring
       (let* ((fmqd-source (current-buffer))
              ;; If storing to a file, create the temporary directory
              ,@(when (eq write-type 'file)
                  `((,temp-dir-symb (make-temp-file "flymake-" t))
                    (fmqd-temp-file
                     (concat
                      (file-name-as-directory ,temp-dir-symb)
                      (file-name-nondirectory (or (buffer-file-name) (buffer-name)))))))
              ;; Next we do the :pre-let phase
              ,@(plist-get def-plist :pre-let))
         ;; With vars defined, do :pre-check
         (condition-case ,fmqd-err-symb
             (progn
               ,(plist-get def-plist :pre-check))
           (error ,@cleanup-form
                  ,custom-cleanup
                  (signal (car ,fmqd-err-symb) (cdr ,fmqd-err-symb))))
         ;; No errors so far, kill any running (obsolete) running processes
         (let ((proc (plist-get flymake-ansible-lint--quickdef-procs ',name)))
           (when (process-live-p proc)
             (kill-process proc)))
         (save-restriction
           (widen)
           ;; If writing to a file, send the data to the temp file
           ,@(when (eq write-type 'file)
               '((write-region nil nil fmqd-temp-file nil 'silent)))
           ;; Launch the new external process
           (setq flymake-ansible-lint--quickdef-procs
                 (plist-put flymake-ansible-lint--quickdef-procs ',name
                            (make-process
                             :name ,(concat (symbol-name name) "-flymake")
                             :noquery t
                             :connection-type 'pipe
                             :buffer (generate-new-buffer ,(concat " *" (symbol-name name) "-flymake*"))
                             :command ,(plist-get def-plist :proc-form)
                             :sentinel
                             (lambda (proc _event)
                               ;; If the process is actually done we can continue
                               (unless (process-live-p proc)
                                 (unwind-protect
                                     (if (eq proc (plist-get (buffer-local-value 'flymake-ansible-lint--quickdef-procs fmqd-source) ',name))
                                         ;; If case: this is the current process
                                         ;; Widen the code buffer so we can compute line numbers, etc.
                                         (with-current-buffer fmqd-source
                                           (save-restriction
                                             (widen)
                                             ;; Scan the process output for errors
                                             (with-current-buffer (process-buffer proc)
                                               (goto-char (point-min))
                                               (save-match-data
                                                 (let ((,diags-symb nil))
                                                   (while (search-forward-regexp
                                                           ,(plist-get def-plist :search-regexp)
                                                           nil t)
                                                     ;; Save match data to work around a bug in `flymake-diag-region'
                                                     ;; That function seems to alter match data and is commonly called here
                                                     (save-match-data
                                                       (save-excursion
                                                         (let* ((diag-vals ,(plist-get def-plist :prep-diagnostic))
                                                                (diag-beg (nth 1 diag-vals))
                                                                (diag-end (nth 2 diag-vals))
                                                                (diag-type (nth 3 diag-vals)))
                                                           ;; Remove diagnostics with invalid values
                                                           ;; for either beg or end. This guards against
                                                           ;; overlay errors inside Flymake. Log such
                                                           ;; errors with Flymake.
                                                           (if (and (integer-or-marker-p diag-beg)
                                                                    (integer-or-marker-p diag-end))
                                                               ;; Skip any diagnostics with a type of nil
                                                               ;; This makes it easier to filter some out
                                                               (when diag-type
                                                                 (push (apply #'flymake-make-diagnostic diag-vals) ,diags-symb))
                                                             (with-current-buffer fmqd-source
                                                               (flymake-log :error "Got invalid buffer position %s or %s in %s"
                                                                            diag-beg diag-end proc)))))))
                                                   (funcall report-fn (nreverse ,diags-symb)))))))
                                       ;; Else case: this process is obsolete
                                       (flymake-log :warning "Canceling obsolete check %s" proc))
                                   ;; Unwind-protect cleanup forms
                                   ,@cleanup-form
                                   ,custom-cleanup
                                   (kill-buffer (process-buffer proc))))))))
           ;; If piping, send data to process
           ,@(when (eq write-type 'pipe)
               `((let ((proc (plist-get flymake-ansible-lint--quickdef-procs ',name)))
                   (process-send-region proc (point-min) (point-max))
                   (process-send-eof proc)))))))))

(defun flymake-ansible-lint--create-temp-file-same-dir (file-path)
  "Create a temporary file in the same directory as FILE-PATH.
Write the contents of the current buffer to the temporary file.
Returns the path of the newly created temporary file."
  (when file-path
    (let* ((file-path (expand-file-name file-path))
           (directory (file-name-directory file-path))
           (filename (file-name-nondirectory file-path))
           (basename (file-name-sans-extension filename))
           (extension (file-name-extension filename))
           (counter 0)
           (temp-file-path))
      (while (progn
               (setq temp-file-path
                     (expand-file-name (concat
                                        flymake-ansible-lint-tmp-files-prefix
                                        (if (> counter 0)
                                            (format "%d_" counter))
                                        basename
                                        (when extension
                                          ".")
                                        extension)
                                       directory))
               (or (> counter 10000)
                   (file-exists-p temp-file-path)))
        (setq counter (1+ counter)))
      (unless (file-exists-p temp-file-path)
        (save-restriction
          (widen)
          (write-region (point-min) (point-max) temp-file-path nil 'quiet))
        temp-file-path))))

(flymake-ansible-lint--quickdef-backend flymake-ansible-lint-backend
  :cleanup
  (progn
    (when (and tmp-path (not (string= buffer-path tmp-path)))
      (delete-file tmp-path nil)))

  :pre-let ((buffer-path (buffer-file-name (buffer-base-buffer)))
            (buffer-modified-p (current-buffer))
            (tmp-path (when (and buffer-path
                                 flymake-ansible-lint-tmp-files-enabled
                                 buffer-modified-p)
                        (flymake-ansible-lint--create-temp-file-same-dir
                         buffer-path)))
            (ansible-lint-exec (executable-find
                                flymake-ansible-lint-executable))
            (file-path (or tmp-path buffer-path)))

  :pre-check
  (unless ansible-lint-exec
    (error "The '%s' executable was not found" ansible-lint-exec))

  :write-type nil
  :proc-form (append (list ansible-lint-exec
                           "--nocolor"
                           "--parseable")
                     (if flymake-ansible-lint-args
                         (append flymake-ansible-lint-args
                                 (list file-path))
                       (list file-path)))
  :search-regexp
  (rx bol
      ;; file.yaml:57:7: syntax-check[specific]: message
      ;; file.yaml:1: internal-error: Unexpected error code 1
      (seq (zero-or-more any)
           (literal (file-name-nondirectory file-path)) ":" ; File name
           ;; Line/Column
           (group (one-or-more digit)) ":" ; Line number
           (optional (group (one-or-more digit) ":")) ; Optional column
           ;; Code
           (one-or-more space)
           (group (one-or-more (not ":")))  ":" ; Code
           ;; Message
           (one-or-more space)
           (group (one-or-more any))) ; Msg
      eol)

  :prep-diagnostic
  (progn
    (let* ((lnum (string-to-number (match-string 1)))
           (col (let ((col-string (match-string 2)))
                  (if col-string
                      (string-to-number col-string)
                    nil)))
           (code (match-string 3))
           (text (match-string 4))
           (pos (flymake-diag-region fmqd-source lnum col))
           (beg (car pos))
           (end (cdr pos))
           (type :error)
           (msg (format "%s: %s" code text)))

      (list fmqd-source beg end type msg))))

;;;###autoload
(defun flymake-ansible-lint-setup ()
  "Set up Flymake for `ansible-lint` linting in the current buffer.
This function adds `flymake-ansible-lint-backend' to the list of Flymake
diagnostic functions, enabling Ansible-Lint style checks locally for the current
buffer."
  (add-hook 'flymake-diagnostic-functions #'flymake-ansible-lint-backend nil t))

(provide 'flymake-ansible-lint)
;;; flymake-ansible-lint.el ends here
