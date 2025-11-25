;;; netlinx-mode-indent.el --- Indent settings for NetLinx mode -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2024 Norgate AV

;; Author: Norgate AV
;; Maintainer: Norgate AV

;;; Commentary:

;; This file provides indent settings for NetLinx mode.
;; This is an internal library for netlinx-mode.el and is not a standalone package.

;;; Code:

(defgroup netlinx nil
  "Major mode for NetLinx using tree-sitter."
  :group 'languages
  :prefix "netlinx-")

;; Indentation configuration
(defcustom netlinx-mode-indent-offset 4
  "Number of spaces for each indentation step in `netlinx-mode'."
  :type 'integer
  :safe 'integerp
  :group 'netlinx)

(defcustom netlinx-mode-align-argument-list-to-first-sibling nil
  "Align argument lists to the first sibling.

If set to t, the following indentation is used:

    send_command device, \"'PROPERTY-Name,Value1,Value2,',
                          'Value3'\"

Otherwise, the indentation is:

    send_command device, \"'PROPERTY-Name,Value1,Value2,',
        'Value3'\""
  :type 'boolean
  :safe 'booleanp
  :group 'netlinx)

(defcustom netlinx-mode-align-device-array-to-first-sibling nil
  "Align device array elements to the first sibling.

If set to t, the following indentation is used:

    dvDevices = { dvPanel1,
                  dvPanel2,
                  dvPanel3 }

Otherwise, the indentation is:

    dvDevices = { dvPanel1,
        dvPanel2,
        dvPanel3 }"
  :type 'boolean
  :safe 'booleanp
  :group 'netlinx)

;; Helper function for conditional indentation
(defun netlinx-mode--indent-argument-list (node parent &rest _)
  "Indent argument list NODE relative to PARENT.
Uses `netlinx-mode-align-argument-list-to-first-sibling' to determine alignment."
  (if netlinx-mode-align-argument-list-to-first-sibling
      (treesit-node-start (treesit-node-child parent 0))
    `(parent-bol ,netlinx-mode-indent-offset)))

(defun netlinx-mode--indent-initializer-list (node parent &rest _)
  "Indent initializer list NODE relative to PARENT.
Uses `netlinx-mode-align-device-array-to-first-sibling' to determine alignment."
  (if netlinx-mode-align-device-array-to-first-sibling
      (treesit-node-start (treesit-node-child parent 0))
    `(parent-bol ,netlinx-mode-indent-offset)))

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
     ((parent-is "initializer_list") netlinx-mode--indent-initializer-list)

     ;; Parameter lists
     ((parent-is "parameter_list") parent-bol netlinx-mode-indent-offset)
     ((parent-is "argument_list") netlinx-mode--indent-argument-list)

     ;; Default: maintain current indentation for other nodes
     (no-node parent-bol 0)))
  "Tree-sitter indentation rules for `netlinx-mode'.")

(provide 'netlinx-mode-indent)

;;; netlinx-mode-indent.el ends here
