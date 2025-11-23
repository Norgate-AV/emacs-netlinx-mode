;;; netlinx-mode.el --- Major mode for NetLinx using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Norgate AV

;; Author: Norgate AV
;; Maintainer: Norgate AV
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, netlinx, amx, harman
;; URL: https://github.com/Norgate-AV/emacs-netlinx-mode
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Major mode for editing NetLinx files with built-in tree-sitter support.
;; This mode provides syntax highlighting for NetLinx programming language.

;;; Code:

(require 'treesit)

;; Ensure font-lock file can be loaded
(eval-and-compile
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (add-to-list 'load-path dir)))

(require 'netlinx-mode-font-lock)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defgroup netlinx nil
  "Major mode for NetLinx using tree-sitter."
  :group 'languages
  :prefix "netlinx-")

(defcustom netlinx-mode-grammar-location
  "https://github.com/Norgate-AV/tree-sitter-netlinx"
  "Repository URL for the tree-sitter NetLinx grammar."
  :type 'string
  :group 'netlinx)

;; Add grammar source on load
(add-to-list 'treesit-language-source-alist
             '(netlinx "https://github.com/Norgate-AV/tree-sitter-netlinx"))

(defun netlinx-mode--ensure-grammar ()
  "Ensure NetLinx grammar is installed, installing if necessary."
  (unless (treesit-language-available-p 'netlinx)
    (message "NetLinx: Installing tree-sitter grammar (this may take a minute)...")
    (treesit-install-language-grammar 'netlinx)
    (message "NetLinx: Grammar installation complete")))

;;;###autoload
(define-derived-mode netlinx-mode prog-mode "NetLinx"
  "Major mode for editing NetLinx files with tree-sitter support."
  :group 'netlinx

  ;; Install grammar if needed
  (netlinx-mode--ensure-grammar)

  (if (treesit-ready-p 'netlinx t)
      (progn
        ;; Create parser
        (treesit-parser-create 'netlinx)

        ;; Font-lock
        (setq-local treesit-font-lock-settings netlinx-mode--font-lock-settings)
        (setq-local treesit-font-lock-feature-list
                    '((comment)
                      (keyword string preprocessor)
                      (constant number type boolean)
                      (function variable operator bracket delimiter)))

        ;; Enable tree-sitter
        (treesit-major-mode-setup))
    (message "NetLinx: tree-sitter grammar not available. Check *Messages* for errors.")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axs\\'" . netlinx-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axi\\'" . netlinx-mode))

(provide 'netlinx-mode)

;;; netlinx-mode.el ends here
