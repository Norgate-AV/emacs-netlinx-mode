;;; netlinx-mode-helpers.el --- Helper functions for NetLinx mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Norgate AV

;; Author: Norgate AV
;; Maintainer: Norgate AV

;;; Commentary:

;; This file provides helper functions for tree-sitter indentation rules
;; in NetLinx mode. These utilities enable context-aware indentation based
;; on code layout and tree-sitter node relationships.

;;; Code:

(defun netlinx-mode--same-line? (point-1 point-2)
  "Return t if POINT-1 and POINT-2 are on the same line."
  (equal (netlinx-mode--line-beginning-position-of-point point-1)
         (netlinx-mode--line-beginning-position-of-point point-2)))

(defun netlinx-mode--line-beginning-position-of-point (point)
  "Return the position of the beginning of the line of POINT."
  (save-mark-and-excursion
    (goto-char point)
    (line-beginning-position)))

(defun netlinx-mode--parent-is-and-sibling-on-same-line (parent-type sibling-index)
  "Check the type of the node's parent and if a sibling is on the same line.

Return t if the node's parent type matches PARENT-TYPE and if the sibling with
index SIBLING-INDEX is on the same line of the current node's parent."
  (lambda (_node parent &rest _)
    (and (string-match-p parent-type (treesit-node-type parent))
         (netlinx-mode--same-line? (treesit-node-start parent)
                                   (treesit-node-start (treesit-node-child parent sibling-index))))))

(defun netlinx-mode--parent-is-and-sibling-not-on-same-line (parent-type sibling-index)
  "Check the type of the node's parent and if a sibling is not on the same line.

Return t if the node's parent type matches PARENT-TYPE and if the sibling with
index SIBLING-INDEX is not on the same line of the current node's parent."
  (lambda (_node parent &rest _)
    (and (string-match-p parent-type (treesit-node-type parent))
         (not (netlinx-mode--same-line? (treesit-node-start parent)
                                        (treesit-node-start (treesit-node-child parent sibling-index)))))))

(defun netlinx-mode--ancestor-node (node regexp)
  "Return the ancestor NODE that matches REGEXP, if it exists."
  (treesit-parent-until node
                        (lambda (node)
                          (string-match-p regexp (treesit-node-type node)))))

(defun netlinx-mode--ancestor-is (regexp)
  "Return the ancestor of NODE that matches REGEXP, if it exists."
  (lambda (node &rest _)
    (netlinx-mode--ancestor-node node regexp)))

(defun netlinx-mode--ancestor-bol (regexp)
  "Return the BOL of the current node's ancestor that matches REGEXP."
  (lambda (node &rest _)
    (treesit-node-start (netlinx-mode--ancestor-node node regexp))))

(defun netlinx-mode--ancestor-is-and-sibling-on-same-line (ancestor-type sibling-index)
  "Check the type of the node's ancestor and if a sibling is on the same line.

Return t if the node's ancestor type matches ANCESTOR-TYPE and if the sibling
with index SIBLING-INDEX is on the same line of the ancestor."
  (lambda (node &rest _)
    (let ((ancestor (netlinx-mode--ancestor-node node ancestor-type)))
      (and ancestor
           (netlinx-mode--same-line? (treesit-node-start ancestor)
                                     (treesit-node-start (treesit-node-child ancestor sibling-index)))))))

(defun netlinx-mode--ancestor-is-and-sibling-not-on-same-line (ancestor-type sibling-index)
  "Check the type of the node's ancestor and if a sibling is not on the same line.

Return t if the node's ancestor type matches ANCESTOR-TYPE and if the sibling
with index SIBLING-INDEX is not on the same line of the ancestor."
  (lambda (node &rest _)
    (let ((ancestor (netlinx-mode--ancestor-node node ancestor-type)))
      (and ancestor
           (not (netlinx-mode--same-line? (treesit-node-start ancestor)
                                          (treesit-node-start (treesit-node-child ancestor sibling-index))))))))

(defun netlinx-mode--grand-parent-bol (_node parent &rest _)
  "Return the beginning of the line (non-space char) where the PARENT's parent is.
Argument _NODE is unused but required for tree-sitter matcher signature."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))
    (back-to-indentation)
    (point)))

(defun netlinx-mode--grand-parent-first-sibling (_node parent &rest _)
  "Return the start of the first child of the parent of PARENT.
Argument _NODE is unused but required for tree-sitter matcher signature."
  (treesit-node-start (treesit-node-child (treesit-node-parent parent) 0)))

(provide 'netlinx-mode-helpers)

;;; netlinx-mode-helpers.el ends here
