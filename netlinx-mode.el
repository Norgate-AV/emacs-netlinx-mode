;;; netlinx-mode.el --- Major mode for NetLinx files -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Norgate AV

;; Author: Norgate AV
;; Maintainer: Norgate AV
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, netlinx, amx, harman
;; URL: https://github.com/Norgate-AV/emacs-netlinx-mode
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package provides a major mode for editing NetLinx source files (.axs, .axi)
;; with tree-sitter support for syntax highlighting and code navigation.

;;; Code:

;; Load tree-sitter support for syntax highlighting
(require 'treesit)

;; Declare functions from treesit.c to silence byte-compiler warnings
;; These are C functions built into Emacs. We declare them here so the byte-compiler
;; knows they exist and won't complain about undefined functions when we use them later.
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-language "treesit.c")
(declare-function treesit-node-type "treesit.c")

;; Define the customization group for NetLinx mode
;; This creates a section in Emacs' customization interface (M-x customize-group RET netlinx)
;; where users can modify NetLinx-related settings. All defcustom variables use :group 'netlinx
;; to appear in this section.
(defgroup netlinx nil
  "Major mode for NetLinx using tree-sitter."
  :group 'languages
  :prefix "netlinx-")

;; Load font-lock (syntax highlighting) settings
(require 'netlinx-mode-font-lock)

;; Load indentation settings
(require 'netlinx-mode-indent)

;; Configuration for tree-sitter grammar
(defcustom netlinx-mode-grammar-location
  "https://github.com/Norgate-AV/tree-sitter-netlinx"
  "Repository URL for the tree-sitter NetLinx grammar."
  :type 'string
  :group 'netlinx)

;; Indentation configuration
(defcustom netlinx-mode-indent-offset 4
  "Number of spaces for each indentation step in `netlinx-mode'."
  :type 'integer
  :safe 'integerp
  :group 'netlinx)

;; Specify the version/tag of the grammar to use
(defcustom netlinx-mode-grammar-version
  "v1.0.4"
  "Version/tag of the tree-sitter NetLinx grammar to use."
  :type 'string
  :group 'netlinx)

;; Path to NetLinx Keyword Help file
(defcustom netlinx-mode-help-file
  (when (eq system-type 'windows-nt)
    "C:/Program Files (x86)/AMX Control Disk/NetLinx Studio/NetLinxKeywords.chm")
  "Path to the NetLinx Keyword Help CHM file.
If set, enables quick access to NetLinx documentation via \\[netlinx-open-help]."
  :type '(choice (const :tag "Not configured" nil)
                 (file :tag "Path to CHM file"))
  :group 'netlinx)

;; Tell Emacs where to download the NetLinx grammar from
(add-to-list 'treesit-language-source-alist
             `(netlinx ,netlinx-mode-grammar-location ,netlinx-mode-grammar-version))

;; Auto-install grammar if not present (runs once when mode file is loaded)
(defun netlinx-mode--ensure-grammar ()
  "Ensure NetLinx grammar is installed, installing if necessary."
  (unless (treesit-language-available-p 'netlinx)
    (message "NetLinx: Installing tree-sitter grammar...")
    (treesit-install-language-grammar 'netlinx)
    (message "NetLinx: Grammar installation complete")))

;;; Commands

(defun netlinx-open-help ()
  "Open the NetLinx Keyword Help CHM file.
The file path is configured via `netlinx-mode-help-file'."
  (interactive)
  (if (and netlinx-mode-help-file
           (file-exists-p netlinx-mode-help-file))
      (start-process "netlinx-help" nil "open" netlinx-mode-help-file)
    (message "NetLinx help file not configured or not found. Set `netlinx-mode-help-file' to the CHM file path.")))

;; Create a new major mode for NetLinx files called netlinx-mode
;; Use prog-mode as the parent mode (for programming languages)
;; Set the mode name to "NetLinx" (this appears in the mode line)
;;;###autoload
(define-derived-mode netlinx-mode prog-mode "NetLinx"
  "Major mode for NetLinx."
  :group 'netlinx

  ;; Ensure grammar is installed
  (netlinx-mode--ensure-grammar)

  ;; Keybindings
  (define-key netlinx-mode-map (kbd "C-c C-d") #'netlinx-open-help)

  ;; Check if the NetLinx grammar is installed (t forces fresh check after installation)
  (if (treesit-ready-p 'netlinx t)
      (progn
        ;; Create parser
        (treesit-parser-create 'netlinx)

        ;; Comments
        (setq-local comment-start "// ")
        (setq-local comment-end "")
        (setq-local comment-start-skip "//+\\s-*")
        (setq-local comment-multi-line t)

        ;; Indentation
        (setq-local treesit-simple-indent-rules netlinx-mode--indent-rules)
        (setq-local indent-line-function #'treesit-indent)
        (setq-local electric-indent-chars
                    (append "{}():;," electric-indent-chars))

        ;; Setup font-lock
        (setq-local treesit-font-lock-settings netlinx-mode--font-lock-settings)
        (setq-local treesit-font-lock-level 4)
        (setq-local treesit-font-lock-feature-list
                    '((comment)
                      (keyword string preprocessor)
                      (constant number type boolean)
                      (function variable property operator bracket delimiter error)))

        ;; Improve incremental parsing
        (setq-local treesit-font-lock-recompute-features t)

        ;; Enable tree-sitter
        (treesit-major-mode-setup))
    (message "NetLinx: tree-sitter grammar not available. Check *Messages* for errors.")))

;; Associate .axs and .axi file extensions with netlinx-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axs\\'" . netlinx-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axi\\'" . netlinx-mode))

(provide 'netlinx-mode)

;;; netlinx-mode.el ends here
