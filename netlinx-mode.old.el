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

;;;###autoload
(define-derived-mode netlinx-mode prog-mode "NetLinx"
  "Major mode for editing NetLinx files with tree-sitter support."
  :group 'netlinx

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

;;; netlinx-mode.el ends here
