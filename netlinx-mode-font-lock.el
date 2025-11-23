;;; netlinx-mode-font-lock.el --- Font-lock settings for netlinx-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Norgate AV

;; Author: Norgate AV
;; Maintainer: Norgate AV
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, netlinx, amx, harman
;; URL: https://github.com/Norgate-AV/emacs-netlinx-mode
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tree-sitter font-lock configuration for NetLinx mode.

;;; Code:

(require 'treesit)

(defvar netlinx-mode--font-lock-settings
  "Return tree-sitter font-lock settings for NetLinx."
  (treesit-font-lock-rules
   :language 'netlinx
   :feature 'comment
   '((comment) @font-lock-comment-face)

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
      (rebuild_event_keyword)
      (band)
      (bor)
      (bxor)
      (bnot)
      (lshift)
      (rshift)
      (and)
      (or)
      (not)
      (xor)
      (constant_keyword)
      (volatile_keyword)
      (non_volatile_keyword)
      (persistent_keyword)
      (local_var_keyword)
      (stack_var_keyword)] @font-lock-keyword-face)

   :language 'netlinx
   :feature 'preprocessor
   '([(preproc_include_keyword)
      (preproc_define_keyword)
      (preproc_warn_keyword)
      (preproc_disable_warning_keyword)
      (preproc_if_defined_keyword)
      (preproc_if_not_defined_keyword)
      (preproc_else_keyword)
      (preproc_end_if_keyword)] @font-lock-preprocessor-face
     (preproc_arg) @font-lock-constant-face)

   :language 'netlinx
   :feature 'string
   '((string_literal) @font-lock-string-face)

   :language 'netlinx
   :feature 'number
   '((number_literal) @font-lock-number-face
     (device_literal) @font-lock-number-face)

   :language 'netlinx
   :feature 'boolean
   '([(true) (false)] @font-lock-constant-face)

   :language 'netlinx
   :feature 'constant
   '((system_constant) @font-lock-constant-face
     ((identifier) @font-lock-constant-face
      (:match "^[A-Z][A-Z\\d_]*$" @font-lock-constant-face)))

   :language 'netlinx
   :feature 'type
   '((type_identifier) @font-lock-type-face
     (primitive_type) @font-lock-type-face
     (structured_type) @font-lock-type-face)

   :language 'netlinx
   :feature 'function
   '((function_definition
      name: (identifier) @font-lock-function-name-face)
     (function_declaration
      name: (identifier) @font-lock-function-name-face)
     (call_expression
      function: (system_function) @font-lock-builtin-face)
     (call_expression
      function: (identifier) @font-lock-function-call-face))

   :language 'netlinx
   :feature 'variable
   '((compiler_variable) @font-lock-builtin-face
     (field_identifier) @font-lock-property-use-face)

   :language 'netlinx
   :feature 'operator
   '(["=" "+" "-" "*" "/" "%" ">" "<" "&" "|" "^" "!" "~"
      "&&" "||" "==" "!=" "<=" ">=" "<<" ">>" "++" "--" "<>"
      (range_operator)] @font-lock-operator-face)

   :language 'netlinx
   :feature 'bracket
   '(["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face)

   :language 'netlinx
   :feature 'delimiter
   '(["." ";" "," ":"] @font-lock-delimiter-face)))

(provide 'netlinx-mode-font-lock)

;;; netlinx-mode-font-lock.el ends here
