#!/bin/bash

set -e

# Get the script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Files to lint
FILES=("netlinx-mode.el")

echo "Project root: $PROJECT_ROOT"
echo "Files to lint: ${FILES[*]}"
echo ""

echo "Installing package-lint..."
emacs -Q --batch \
    --eval "(require 'package)" \
    --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
    --eval "(package-initialize)" \
    --eval "(package-refresh-contents)" \
    --eval "(package-install 'package-lint)" || {
    echo "Failed to install package-lint"
    exit 1
}

echo ""
echo "Running package-lint..."
(cd "$PROJECT_ROOT" && emacs -Q --batch \
    --eval "(require 'package)" \
    --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
    --eval "(package-initialize)" \
    --eval "(require 'package-lint)" \
    -f package-lint-batch-and-exit "${FILES[@]}") || {
    echo "Package lint failed"
    exit 1
}

echo "Package lint succeeded for files: ${FILES[*]}"
