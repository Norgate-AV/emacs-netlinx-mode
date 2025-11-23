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
;;
;; The tree-sitter grammar will be automatically installed when you first
;; open a NetLinx file (.axs or .axi extension).

;;; Code:

(require 'treesit)
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

(defun netlinx-mode--install-grammar ()
  "Install the tree-sitter grammar for NetLinx if not already installed."
  (interactive)
  (unless (treesit-language-available-p 'netlinx)
    (message "Installing tree-sitter grammar for NetLinx...")
    (add-to-list 'treesit-language-source-alist
                 `(netlinx ,netlinx-mode-grammar-location))
    (treesit-install-language-grammar 'netlinx)))

;;;###autoload
(define-derived-mode netlinx-mode prog-mode "NetLinx"
  "Major mode for editing NetLinx files with tree-sitter support."
  :group 'netlinx

  ;; Ensure grammar is installed
  (unless (treesit-ready-p 'netlinx)
    (netlinx-mode--install-grammar))

  ;; Check again after installation attempt
  (unless (treesit-ready-p 'netlinx)
    (error "Tree-sitter grammar for NetLinx is not available. Please install it manually with M-x netlinx-mode--install-grammar"))

  ;; Create parser
  (treesit-parser-create 'netlinx)

  ;; Font-lock
  (setq-local treesit-font-lock-settings netlinx-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword string preprocessor)
                (constant number type boolean)
                (function variable operator bracket delimiter)))

  ;; Enable tree-sitter font-lock
  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axs\\'" . netlinx-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axi\\'" . netlinx-mode))

(provide 'netlinx-mode)

;;; netlinx-mode.el ends here
