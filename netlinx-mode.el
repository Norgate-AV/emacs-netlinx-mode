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

    :language 'netlinx
    :feature 'string
    '((string_literal) @font-lock-string-face)))

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
    (setq-local treesit-font-lock-feature-list '((keyword string)))

    ;; Enable tree-sitter
    (treesit-major-mode-setup)))

;; Associate .axs and .axi file extensions with netlinx-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axs\\'" . netlinx-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.axi\\'" . netlinx-mode))

(provide 'netlinx-mode)

;;; netlinx-mode.el ends here
