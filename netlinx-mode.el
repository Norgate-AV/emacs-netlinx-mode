;;; netlinx-mode.el --- Major mode for NetLinx files -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Norgate AV

;; Author: Norgate AV
;; Maintainer: Norgate AV
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (yasnippet "0.14.0") (company "0.9.13"))
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

;; Declare yasnippet and company variables/functions to silence byte-compiler warnings
;; These are loaded at runtime by netlinx-mode--ensure-yasnippet and netlinx-mode--ensure-company
(defvar yas-snippet-dirs)
(defvar company-backends)
(declare-function yas-minor-mode "yasnippet")
(declare-function yas-reload-all "yasnippet")
(declare-function company-mode "company")

;; Declare electric-pair-mode functions
(declare-function electric-pair-default-inhibit "elec-pair")

;; Define the customization group for NetLinx mode
;; This creates a section in Emacs' customization interface (M-x customize-group RET netlinx)
;; where users can modify NetLinx-related settings. All defcustom variables use :group 'netlinx
;; to appear in this section.
(defgroup netlinx nil
  "Major mode for NetLinx using tree-sitter."
  :group 'languages
  :prefix "netlinx-")

;; Load helper functions
(require 'netlinx-mode-helpers)

;; Load font-lock (syntax highlighting) settings
(require 'netlinx-mode-font-lock)

;; Load indentation settings
(require 'netlinx-mode-indent)

;; Load navigation and imenu settings
(require 'netlinx-mode-navigation)

;; Configuration for tree-sitter grammar
(defcustom netlinx-mode-grammar-location
  "https://github.com/Norgate-AV/tree-sitter-netlinx"
  "Repository URL for the tree-sitter NetLinx grammar."
  :type 'string
  :group 'netlinx)

;; Specify the version/tag of the grammar to use
(defcustom netlinx-mode-grammar-version
  "v1.0.4"
  "Version/tag of the tree-sitter NetLinx grammar to use."
  :type 'string
  :group 'netlinx)

;; Path to NetLinx Keyword Help file
;; "C:\Program Files (x86)\AMX Control Disc\NetLinx Studio 4\NetLinx-Keywords.chm"
(defcustom netlinx-mode-keyword-help-file
  (when (eq system-type 'windows-nt)
    "C:/Program Files (x86)/AMX Control Disc/NetLinx Studio 4/NetLinx-Keywords.chm")
  "Path to the NetLinx Keyword Help CHM file.
If set, enables quick access to NetLinx keyword documentation via \\[netlinx-open-keyword-help]."
  :type '(choice (const :tag "Not configured" nil)
                 (file :tag "Path to CHM file"))
  :group 'netlinx)

;; Path to Standard NetLinx API (SNAPI) Help file
;; "C:\Program Files (x86)\AMX Control Disc\NetLinx Studio 4\Standard_NetLinx_API.chm"
(defcustom netlinx-mode-snapi-help-file
  (when (eq system-type 'windows-nt)
    "C:/Program Files (x86)/AMX Control Disc/NetLinx Studio 4/Standard_NetLinx_API.chm")
  "Path to the NetLinx SNAPI Help CHM file.
If set, enables quick access to NetLinx SNAPI documentation via \\[netlinx-open-snapi-help]."
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

;; Auto-install yasnippet if not present
(defun netlinx-mode--ensure-yasnippet ()
  "Ensure yasnippet is installed, installing if necessary."
  (unless (require 'yasnippet nil t)
    (message "NetLinx: Installing yasnippet...")
    (package-refresh-contents)
    (package-install 'yasnippet)
    (require 'yasnippet)))

;; Auto-install company if not present
(defun netlinx-mode--ensure-company ()
  "Ensure company is installed, installing if necessary."
  (unless (require 'company nil t)
    (message "NetLinx: Installing company...")
    (package-refresh-contents)
    (package-install 'company)
    (require 'company)))

;; Yasnippet integration
(defun netlinx-mode--setup-snippets ()
  "Setup yasnippet snippets for netlinx-mode."
  (netlinx-mode--ensure-yasnippet)
  (netlinx-mode--ensure-company)
  ;; Enable yasnippet minor mode first
  (yas-minor-mode 1)
  ;; Then load the snippets - always relative to where netlinx-mode.el is
  (let ((snippets-dir (expand-file-name
                       "snippets"
                       (file-name-directory
                        (locate-library "netlinx-mode")))))
    (when (file-directory-p snippets-dir)
      (add-to-list 'yas-snippet-dirs snippets-dir t)
      (yas-reload-all)))
  ;; Setup company-mode with yasnippet backend
  (setq-local company-backends '((company-yasnippet company-capf company-dabbrev-code)))
  (company-mode 1))

;;; Commands

(defun netlinx-open-keyword-help ()
  "Open the NetLinx Keyword Help CHM file.
The file path is configured via `netlinx-mode-keyword-help-file'."
  (interactive)
  (if (and netlinx-mode-keyword-help-file
           (file-exists-p netlinx-mode-keyword-help-file))
      (start-process "netlinx-keyword-help" nil "open" netlinx-mode-keyword-help-file)
    (message "NetLinx keyword help file not configured or not found. Set `netlinx-mode-keyword-help-file' to the CHM file path.")))

(defun netlinx-open-snapi-help ()
  "Open the NetLinx SNAPI Help CHM file.
The file path is configured via `netlinx-mode-snapi-help-file'."
  (interactive)
  (if (and netlinx-mode-snapi-help-file
           (file-exists-p netlinx-mode-snapi-help-file))
      (start-process "netlinx-snapi-help" nil "open" netlinx-mode-snapi-help-file)
    (message "NetLinx SNAPI help file not configured or not found. Set `netlinx-mode-snapi-help-file' to the CHM file path.")))

;; Create a new major mode for NetLinx files called netlinx-mode
;; Use prog-mode as the parent mode (for programming languages)
;; Set the mode name to "NetLinx" (this appears in the mode line)
;;;###autoload
(define-derived-mode netlinx-mode prog-mode "NetLinx"
  "Major mode for NetLinx."
  :group 'netlinx

  ;; Ensure grammar is installed
  (netlinx-mode--ensure-grammar)

  ;; Setup yasnippet integration
  (netlinx-mode--setup-snippets)

  ;; Keybindings
  (define-key netlinx-mode-map (kbd "C-c C-d") #'netlinx-open-keyword-help)
  (define-key netlinx-mode-map (kbd "C-c C-s") #'netlinx-open-snapi-help)

  ;; Configure electric pair mode for auto-pairing
  (setq-local electric-pair-pairs
              '((?\( . ?\))
                (?\[ . ?\])
                (?{ . ?})
                (?\\" . ?\\")
                (?\' . ?\')))
  ;; Inhibit single quote pairing only after word characters (to avoid issues with contractions)
  (setq-local electric-pair-inhibit-predicate
              (lambda (c)
                (if (char-equal c ?\')
                    (eq (char-syntax (char-before (1- (point)))) ?w)
                  (electric-pair-default-inhibit c))))
  (electric-pair-local-mode 1)

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

        ;; Navigation
        (setq-local treesit-defun-type-regexp netlinx-mode--defun-type-regexp)
        (setq-local treesit-defun-name-function #'netlinx-mode--defun-name)

        ;; Imenu
        (setq-local treesit-simple-imenu-settings netlinx-mode--imenu-settings)

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
