;;; netlinx-mode-navigation.el --- Navigation support for netlinx-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Norgate AV

;;; Commentary:

;; This file provides navigation and imenu support for netlinx-mode using
;; tree-sitter. It defines which syntax nodes are considered "defuns" for
;; navigation purposes and how to extract names for imenu.

;;; Code:

(require 'treesit)

(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-filter-child "treesit.c")

(defun netlinx-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ;; Function definitions: define_function
    ("define_function"
     (when-let* ((func-def (netlinx-mode--child-of-type node "function_definition"))
                 (name-node (treesit-node-child-by-field-name func-def "name")))
       (treesit-node-text name-node t)))

    ;; Module definitions: define_module
    ("define_module"
     (when-let* ((module-def (netlinx-mode--child-of-type node "module_definition"))
                 (name-node (treesit-node-child-by-field-name module-def "module_name")))
       (treesit-node-text name-node t)))

    ;; Event declarators
    ((or "button_event_declarator"
         "channel_event_declarator"
         "data_event_declarator"
         "level_event_declarator"
         "custom_event_declarator"
         "timeline_event_declarator")
     ;; For events, use the first child as the name (the event target/device)
     (when-let* ((first-child (treesit-node-child node 0)))
       (treesit-node-text first-child t)))

    ;; Sections (DEFINE_DEVICE, DEFINE_VARIABLE, etc.)
    ("section"
     (treesit-node-text node t))

    ;; Struct definitions
    ("struct_specifier"
     (when-let* ((name-node (treesit-node-child-by-field-name node "name")))
       (treesit-node-text name-node t)))))

(defun netlinx-mode--child-of-type (node type)
  "Return first child of NODE that has TYPE."
  (car (treesit-filter-child
        node
        (lambda (child)
          (equal (treesit-node-type child) type)))))

;; Define which node types are considered "defuns" for navigation
(defvar netlinx-mode--defun-type-regexp
  (rx (or "define_function"
          "define_module"
          "button_event_declarator"
          "channel_event_declarator"
          "data_event_declarator"
          "level_event_declarator"
          "custom_event_declarator"
          "timeline_event_declarator"
          "section"
          "struct_specifier"))
  "Regexp matching node types that are considered defuns in NetLinx.")

;; Define imenu categories and their corresponding node types
(defvar netlinx-mode--imenu-settings
  '(("Function" "\\`define_function\\'" nil nil)
    ("Module" "\\`define_module\\'" nil nil)
    ("Button Event" "\\`button_event_declarator\\'" nil nil)
    ("Channel Event" "\\`channel_event_declarator\\'" nil nil)
    ("Data Event" "\\`data_event_declarator\\'" nil nil)
    ("Level Event" "\\`level_event_declarator\\'" nil nil)
    ("Custom Event" "\\`custom_event_declarator\\'" nil nil)
    ("Timeline Event" "\\`timeline_event_declarator\\'" nil nil)
    ("Section" "\\`section\\'" nil nil)
    ("Struct" "\\`struct_specifier\\'" nil nil))
  "Imenu settings for netlinx-mode.
Each entry is (CATEGORY REGEXP PRED NAME-FN) as expected by
`treesit-simple-imenu-settings'.")

(provide 'netlinx-mode-navigation)

;;; netlinx-mode-navigation.el ends here
