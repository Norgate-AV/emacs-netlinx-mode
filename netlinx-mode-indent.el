;;; netlinx-mode-indent.el --- Indent settings for NetLinx mode -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2024 Norgate AV

;; Author: Norgate AV
;; Maintainer: Norgate AV

;;; Commentary:

;; This file provides indent settings for NetLinx mode.
;; This is an internal library for netlinx-mode.el and is not a standalone package.

;;; Code:

;; Indentation rules for tree-sitter
(defvar netlinx-mode--indent-rules
  `((netlinx
     ;; Closing delimiters align with their opening construct
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)

     ;; Control flow keywords align with their parent
     ((node-is "else_clause") parent-bol 0)
     ((node-is "case_statement") parent-bol netlinx-mode-indent-offset)
     ((node-is "default_label") parent-bol netlinx-mode-indent-offset)

     ;; Statement blocks are indented
     ((parent-is "if_statement") parent-bol netlinx-mode-indent-offset)
     ((parent-is "for_statement") parent-bol netlinx-mode-indent-offset)
     ((parent-is "while_statement") parent-bol netlinx-mode-indent-offset)
     ((parent-is "switch_statement") parent-bol netlinx-mode-indent-offset)
     ((parent-is "case_statement") parent-bol netlinx-mode-indent-offset)
     ((parent-is "else_clause") parent-bol netlinx-mode-indent-offset)
     ((parent-is "select_statement") parent-bol netlinx-mode-indent-offset)
     ((parent-is "active_statement") parent-bol netlinx-mode-indent-offset)

     ;; Function and event definitions
     ((parent-is "define_function") parent-bol netlinx-mode-indent-offset)
     ((parent-is "function_definition") parent-bol netlinx-mode-indent-offset)
     ((parent-is "event_handler") parent-bol netlinx-mode-indent-offset)

     ;; Compound statements
     ((parent-is "compound_statement") parent-bol netlinx-mode-indent-offset)

     ;; Array and struct initializers
     ((parent-is "initializer_list") parent-bol netlinx-mode-indent-offset)

     ;; Parameter lists
     ((parent-is "parameter_list") parent-bol netlinx-mode-indent-offset)
     ((parent-is "argument_list") parent-bol netlinx-mode-indent-offset)

     ;; Default: maintain current indentation for other nodes
     (no-node parent-bol 0)))
  "Tree-sitter indentation rules for `netlinx-mode'.")

(provide 'netlinx-mode-indent)

;;; netlinx-mode-indent.el ends here
