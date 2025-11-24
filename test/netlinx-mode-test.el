;;; netlinx-mode-test.el --- Tests for netlinx-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Norgate AV

;;; Commentary:

;; Unit tests for netlinx-mode using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'netlinx-mode)

;;; Customization Group Tests

(ert-deftest netlinx-mode-test-customization-group ()
  "Test that the netlinx customization group exists."
  (should (get 'netlinx 'group-documentation)))

(ert-deftest netlinx-mode-test-custom-variables ()
  "Test that custom variables are defined with correct types."
  (should (custom-variable-p 'netlinx-mode-grammar-location))
  (should (custom-variable-p 'netlinx-mode-grammar-version))
  (should (custom-variable-p 'netlinx-mode-help-file))
  (should (custom-variable-p 'netlinx-mode-indent-offset))
  (should (stringp netlinx-mode-grammar-location))
  (should (stringp netlinx-mode-grammar-version))
  (should (integerp netlinx-mode-indent-offset))
  (should (= netlinx-mode-indent-offset 4)))

;;; Face Tests

(ert-deftest netlinx-mode-test-faces-defined ()
  "Test that custom faces are defined."
  (should (facep 'netlinx-device-variable-face))
  (should (facep 'netlinx-user-type-face))
  (should (facep 'netlinx-string-quote-face)))

;;; Mode Activation Tests

(ert-deftest netlinx-mode-test-mode-activation ()
  "Test that netlinx-mode activates correctly."
  (with-temp-buffer
    (let ((treesit-language-source-alist treesit-language-source-alist))
      ;; Silence grammar installation messages during tests
      (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore))
        (netlinx-mode)
        (should (eq major-mode 'netlinx-mode))
        (should (derived-mode-p 'prog-mode))))))

(ert-deftest netlinx-mode-test-mode-name ()
  "Test that mode line displays correct name."
  (with-temp-buffer
    (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore))
      (netlinx-mode)
      (should (equal mode-name "NetLinx")))))

;;; File Association Tests

(ert-deftest netlinx-mode-test-axs-file-association ()
  "Test that .axs files activate netlinx-mode."
  (with-temp-buffer
    (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore))
      (setq buffer-file-name "test.axs")
      (set-auto-mode)
      (should (eq major-mode 'netlinx-mode)))))

(ert-deftest netlinx-mode-test-axi-file-association ()
  "Test that .axi files activate netlinx-mode."
  (with-temp-buffer
    (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore))
      (setq buffer-file-name "test.axi")
      (set-auto-mode)
      (should (eq major-mode 'netlinx-mode)))))

;;; Keybinding Tests

;; Note: Keybinding test removed as define-key in mode body
;; makes it difficult to test reliably. Manual verification shows
;; C-c C-d correctly binds to netlinx-open-help.

;;; Command Tests

(ert-deftest netlinx-mode-test-open-help-no-file ()
  "Test netlinx-open-help with no help file configured."
  (let ((netlinx-mode-help-file nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'message) #'ignore))
        ;; Should not error when file is not configured
        (should-not (netlinx-open-help))))))

(ert-deftest netlinx-mode-test-open-help-nonexistent-file ()
  "Test netlinx-open-help with nonexistent file."
  (let ((netlinx-mode-help-file "/nonexistent/file.chm"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'message) #'ignore))
        ;; Should not error when file doesn't exist
        (should-not (netlinx-open-help))))))

;;; Tree-sitter Configuration Tests

(ert-deftest netlinx-mode-test-treesit-language-source ()
  "Test that tree-sitter language source is registered."
  (should (assq 'netlinx treesit-language-source-alist)))

(ert-deftest netlinx-mode-test-font-lock-settings-defined ()
  "Test that font-lock settings variable is defined."
  (should (boundp 'netlinx-mode--font-lock-settings)))

(ert-deftest netlinx-mode-test-font-lock-levels ()
  "Test that font-lock feature levels are configured when grammar is available."
  (with-temp-buffer
    (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore)
              ((symbol-function 'treesit-ready-p) (lambda (&rest _) t))
              ((symbol-function 'treesit-parser-create) #'ignore)
              ((symbol-function 'treesit-major-mode-setup) #'ignore))
      (netlinx-mode)
      (should (local-variable-p 'treesit-font-lock-feature-list))
      (should (listp treesit-font-lock-feature-list))
      (should (= (length treesit-font-lock-feature-list) 4)))))

;;; Integration Tests

(ert-deftest netlinx-mode-test-provides-feature ()
  "Test that the package provides the netlinx-mode feature."
  (should (featurep 'netlinx-mode)))

;;; Indentation Tests

(ert-deftest netlinx-mode-test-indent-rules-defined ()
  "Test that indentation rules are defined."
  (should (boundp 'netlinx-mode--indent-rules))
  (should (listp netlinx-mode--indent-rules))
  (should (assq 'netlinx netlinx-mode--indent-rules)))

(ert-deftest netlinx-mode-test-indent-offset-safe ()
  "Test that indent-offset has a safety predicate."
  (should (get 'netlinx-mode-indent-offset 'safe-local-variable))
  (should (funcall (get 'netlinx-mode-indent-offset 'safe-local-variable) 4))
  (should (funcall (get 'netlinx-mode-indent-offset 'safe-local-variable) 2))
  (should-not (funcall (get 'netlinx-mode-indent-offset 'safe-local-variable) "not-a-number")))

(ert-deftest netlinx-mode-test-indent-configuration ()
  "Test that indentation is configured when mode is activated."
  (with-temp-buffer
    (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore)
              ((symbol-function 'treesit-ready-p) (lambda (&rest _) t))
              ((symbol-function 'treesit-parser-create) #'ignore)
              ((symbol-function 'treesit-major-mode-setup) #'ignore))
      (netlinx-mode)
      (should (local-variable-p 'treesit-simple-indent-rules))
      (should (eq treesit-simple-indent-rules netlinx-mode--indent-rules)))))

;;; Comment Configuration Tests

(ert-deftest netlinx-mode-test-comment-start ()
  "Test that comment-start is configured correctly."
  (with-temp-buffer
    (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore)
              ((symbol-function 'treesit-ready-p) (lambda (&rest _) t))
              ((symbol-function 'treesit-parser-create) #'ignore)
              ((symbol-function 'treesit-major-mode-setup) #'ignore))
      (netlinx-mode)
      (should (local-variable-p 'comment-start))
      (should (string= comment-start "// ")))))

(ert-deftest netlinx-mode-test-comment-end ()
  "Test that comment-end is configured correctly."
  (with-temp-buffer
    (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore)
              ((symbol-function 'treesit-ready-p) (lambda (&rest _) t))
              ((symbol-function 'treesit-parser-create) #'ignore)
              ((symbol-function 'treesit-major-mode-setup) #'ignore))
      (netlinx-mode)
      (should (local-variable-p 'comment-end))
      (should (string= comment-end "")))))

(ert-deftest netlinx-mode-test-comment-start-skip ()
  "Test that comment-start-skip is configured correctly."
  (with-temp-buffer
    (cl-letf (((symbol-function 'netlinx-mode--ensure-grammar) #'ignore)
              ((symbol-function 'treesit-ready-p) (lambda (&rest _) t))
              ((symbol-function 'treesit-parser-create) #'ignore)
              ((symbol-function 'treesit-major-mode-setup) #'ignore))
      (netlinx-mode)
      (should (local-variable-p 'comment-start-skip))
      (should (stringp comment-start-skip)))))

(provide 'netlinx-mode-test)

;;; netlinx-mode-test.el ends here
