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
;; (require 'netlinx-mode-font-lock)

;; Define custom face for device variables (dv/vdv)
(defface netlinx-device-variable-face
  '((t :inherit font-lock-variable-name-face :slant italic))
  "Face for NetLinx device variables (identifiers starting with dv or vdv)."
  :group 'netlinx-mode)

;; Define custom face for user-defined types
(defface netlinx-user-type-face
  '((t :inherit font-lock-type-face :slant italic))
  "Face for NetLinx user-defined types (type_identifier)."
  :group 'netlinx-mode)

;; Define custom face for string quotes
(defface netlinx-string-quote-face
  '((t :inherit font-lock-operator-face :weight bold))
  "Face for NetLinx string quote characters."
  :group 'netlinx-mode)

;; Tell Emacs where to download the NetLinx grammar from
(add-to-list 'treesit-language-source-alist
             '(netlinx "https://github.com/Norgate-AV/tree-sitter-netlinx"))

;; Auto-install grammar if not present
(unless (treesit-language-available-p 'netlinx)
  (message "Installing NetLinx tree-sitter grammar...")
  (treesit-install-language-grammar 'netlinx))

;; Define syntax highlighting rules
(defvar netlinx-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Comments
   :language 'netlinx
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ;; Keywords
   :language 'netlinx
   :feature 'keyword
   '([(break_keyword)
      (case_keyword)
      (continue_keyword)
      (default_keyword)
      (else_keyword)
      (for_keyword)
      (if_keyword)
      (return_keyword)
      (switch_keyword)
      (while_keyword)
      (select_keyword)
      (active_keyword)
      (struct_keyword)
      (structure_keyword)
      (program_name_keyword)
      (module_name_keyword)
      (define_device_keyword)
      (define_constant_keyword)
      (define_type_keyword)
      (define_variable_keyword)
      (define_system_variable_keyword)
      (define_start_keyword)
      (define_event_keyword)
      (define_mutually_exclusive_keyword)
      (define_function_keyword)
      (define_library_function_keyword)
      (define_combine_keyword)
      (define_connect_level_keyword)
      (define_latching_keyword)
      (define_toggling_keyword)
      (define_program_keyword)
      (define_call_keyword)
      (define_module_keyword)
      (push_keyword)
      (release_keyword)
      (hold_keyword)
      (repeat_keyword)
      (on_keyword)
      (off_keyword)
      (online_keyword)
      (offline_keyword)
      (onerror_keyword)
      (string_keyword)
      (command_keyword)
      (awake_keyword)
      (standby_keyword)
      (send_level_keyword)
      (send_string_keyword)
      (send_command_keyword)
      (clear_buffer_keyword)
      (create_buffer_keyword)
      (create_multi_buffer_keyword)
      (call_keyword)
      (system_call_keyword)
      (devchan_on_keyword)
      (devchan_off_keyword)
      (devchan_to_keyword)
      (devchan_min_to_keyword)
      (devchan_total_off_keyword)
      (devchan_pulse_keyword)
      (wait_keyword)
      (wait_until_keyword)
      (cancel_wait_keyword)
      (cancel_wait_until_keyword)
      (cancel_all_wait_keyword)
      (cancel_all_wait_until_keyword)
      (button_event_keyword)
      (channel_event_keyword)
      (data_event_keyword)
      (level_event_keyword)
      (timeline_event_keyword)
      (custom_event_keyword)
      (band)
      (bor)
      (bxor)
      (bnot)
      (lshift)
      (rshift)
      (and)
      (or)
      (not)
      (xor)] @font-lock-keyword-face)

   ;; Qualifiers
   :language 'netlinx
   :feature 'keyword
   '([(constant_keyword)
      (volatile_keyword)
      (non_volatile_keyword)
      (persistent_keyword)] @font-lock-keyword-face)

   ;; Storage classes
   :language 'netlinx
   :feature 'keyword
   '([(local_var_keyword)
      (stack_var_keyword)] @font-lock-keyword-face)

   ;; Preprocessor
   :language 'netlinx
   :feature 'preprocessor
   '([(preproc_include_keyword)
      (preproc_define_keyword)
      (preproc_warn_keyword)
      (preproc_disable_warning_keyword)
      (preproc_if_defined_keyword)
      (preproc_if_not_defined_keyword)
      (preproc_else_keyword)
      (preproc_end_if_keyword)] @font-lock-preprocessor-face)

   ;; Preproc arguments
   :language 'netlinx
   :feature 'preprocessor
   '((preproc_arg) @font-lock-constant-face)

   ;; Operators
   :language 'netlinx
   :feature 'operator
   '(["=" "+" "-" "*" "/" "%" ">" "<" "&" "|" "^" "~"
      "&&" "||" "==" "!=" "<=" ">=" "<<" ">>" "++" "--" "<>"
      (range_operator)] @font-lock-operator-face)

   ;; Negation operators
   :language 'netlinx
   :feature 'operator
   '(["!" (not)] @font-lock-negation-char-face)

   ;; Punctuation brackets
   :language 'netlinx
   :feature 'bracket
   '(["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face)

   ;; Punctuation delimiters
   :language 'netlinx
   :feature 'delimiter
   '(["." ";" "," ":"] @font-lock-delimiter-face)

   ;; Literals - strings
   :language 'netlinx
   :feature 'string
   '(((string_literal) @font-lock-builtin-face
      (:match "^'/.*/'$" @font-lock-builtin-face))
     (string_literal) @font-lock-string-face
     (escape_sequence) @font-lock-escape-face
     "\"" @netlinx-string-quote-face)

   ;; Literals - numbers
   :language 'netlinx
   :feature 'number
   '((number_literal) @font-lock-number-face
     (device_literal) @font-lock-number-face)

   ;; Literals - booleans
   :language 'netlinx
   :feature 'boolean
   '([(true) (false)] @font-lock-constant-face)

   ;; Types
   :language 'netlinx
   :feature 'type
   '((type_identifier) @netlinx-user-type-face
     (primitive_type) @font-lock-type-face
     (structured_type) @font-lock-type-face
     (system_type) @font-lock-type-face)

   ;; Properties
   :language 'netlinx
   :feature 'property
   '((field_identifier) @font-lock-property-use-face)

   ;; Functions - builtin
   :language 'netlinx
   :feature 'function
   '((call_expression
      function: (system_function) @font-lock-builtin-face))

   ;; Functions - calls
   :language 'netlinx
   :feature 'function
   '((call_expression
      function: (identifier) @font-lock-function-call-face))

   ;; Functions - definitions
   :language 'netlinx
   :feature 'function
   '((function_definition
      name: (identifier) @font-lock-function-name-face))

   ;; Functions - declarations
   :language 'netlinx
   :feature 'function
   '((function_declaration
      name: (identifier) @font-lock-function-name-face))

   ;; Variables
   :language 'netlinx
   :feature 'variable
   '((system_variable) @font-lock-variable-name-face
     (compiler_variable) @font-lock-builtin-face)

   ;; Constants - system
   :language 'netlinx
   :feature 'constant
   '((system_constant) @font-lock-constant-face)

   ;; Device variables (dv/vdv prefix) - must be before ALL_CAPS and catch-all
   :language 'netlinx
   :feature 'variable
   '(((identifier) @netlinx-device-variable-face
      (:match "^v?dv[a-zA-Z_][a-zA-Z\\d_]*$" @netlinx-device-variable-face)))

   ;; Constants - ALL_CAPS identifiers
   :language 'netlinx
   :feature 'constant
   '(((identifier) @font-lock-constant-face
      (:match "^[A-Z][A-Z\\d_]*$" @font-lock-constant-face)))

   ;; Error nodes
   :language 'netlinx
   :feature 'error
   '((ERROR) @font-lock-warning-face
     (MISSING) @font-lock-warning-face)

   ;; Identifiers (catch-all for regular variables, must be last)
   :language 'netlinx
   :feature 'variable
   '((identifier) @font-lock-variable-name-face)))

;; Create a new major mode for NetLinx files called netlinx-mode
;; Use prog-mode as the parent mode (for programming languages)
;; Set the mode name to "NetLinx" (this appears in the mode line)
;;;###autoload
(define-derived-mode netlinx-mode prog-mode "NetLinx"
  "Major mode for NetLinx."
  ;; Check if the NetLinx grammar is installed
  (when (treesit-ready-p 'netlinx)
    ;; Create parser
    (treesit-parser-create 'netlinx)

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
    (treesit-major-mode-setup)))

;; Associate .axs and .axi file extensions with netlinx-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axs\\'" . netlinx-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axi\\'" . netlinx-mode))

(provide 'netlinx-mode)

;;; netlinx-mode.el ends here
