;;; emacs-netlinx-mode.el --- Major mode for NetLinx -*- lexical-binding: t; -*-

;; Author: Norgate AV
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (tree-sitter "0.15.0") (tree-sitter-langs "0.17.0"))
;; Keywords: languages, netlinx
;; URL: https://github.com/Norgate-AV/emacs-netlinx-mode

;;; Commentary:

;; Major mode for NetLinx with Tree-sitter support.

;;; Code:

(require 'tree-sitter)
(require 'tree-sitter-langs)

(defvar emacs-netlinx-mode--grammar-repo
  "https://github.com/Norgate-AV/tree-sitter-netlinx"
  "Git repository URL for the Tree-sitter NetLinx grammar.")

(defvar emacs-netlinx-mode--grammar-dir
  (expand-file-name "tree-sitter-netlinx" user-emacs-directory)
  "Directory where the Tree-sitter NetLinx grammar is cloned.")

(defun emacs-netlinx-mode--ensure-grammar ()
  "Ensure the NetLinx Tree-sitter grammar is installed."
  (unless (file-directory-p emacs-netlinx-mode--grammar-dir)
    (make-directory emacs-netlinx-mode--grammar-dir t)
    (let ((default-directory user-emacs-directory))
      (shell-command
       (format "git clone %s %s"
               emacs-netlinx-mode--grammar-repo
               emacs-netlinx-mode--grammar-dir))))
  ;; Compile grammar
  (let ((default-directory emacs-netlinx-mode--grammar-dir))
    (unless (file-exists-p "build/wasm/tree_sitter_netlinx.wasm")
      (shell-command "tree-sitter generate")
      (shell-command "mkdir -p build")
      (shell-command "gcc -shared -fPIC src/parser.c -o build/tree_sitter_netlinx.so"))))

;;;###autoload
(define-derived-mode emacs-netlinx-mode prog-mode "NetLinx"
  "Major mode for editing NetLinx files."

  ;; Enable Tree-sitter
  (tree-sitter-mode)
  (tree-sitter-hl-mode)

  ;; Load grammar
  (emacs-netlinx-mode--ensure-grammar)
  (tree-sitter-load 'netlinx emacs-netlinx-mode--grammar-dir)

  ;; Optional: set file extensions
  (add-to-list 'auto-mode-alist '("\\.axi?\\'" . emacs-netlinx-mode))
  (add-to-list 'auto-mode-alist '("\\.axs?\\'" . emacs-netlinx-mode)))

(provide 'emacs-netlinx-mode)

;;; emacs-netlinx-mode.el ends here
