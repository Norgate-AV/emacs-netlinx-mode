#!/bin/bash

set -e

# Get the script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Files to compile (in dependency order)
FILES=("netlinx-mode-font-lock.el" "netlinx-mode.el")

# Build absolute paths
ABSOLUTE_FILE_PATHS=()
for file in "${FILES[@]}"; do
    ABSOLUTE_FILE_PATHS+=("$PROJECT_ROOT/$file")
done

echo "Project root: $PROJECT_ROOT"
echo "Files to compile: ${FILES[*]}"
echo ""

# Check if all files exist before attempting to compile
for file in "${ABSOLUTE_FILE_PATHS[@]}"; do
    if [ ! -f "$file" ]; then
        echo "Error: $file not found"
        exit 1
    fi
done

# Run byte compilation
emacs -Q --batch \
    --eval "(add-to-list 'load-path \"$PROJECT_ROOT\")" \
    --eval "(setq byte-compile-error-on-warn t)" \
    -f batch-byte-compile "${ABSOLUTE_FILE_PATHS[@]}" || {
    echo "Byte compilation failed"
    exit 1
}

echo "Byte compilation succeeded for files: ${FILES[*]}"
