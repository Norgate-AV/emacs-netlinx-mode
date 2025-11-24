#!/bin/bash

set -e

# Get the script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "Project root: $PROJECT_ROOT"
echo ""

echo "Running ERT tests..."
(cd "$PROJECT_ROOT" && emacs -Q --batch \
    --eval "(add-to-list 'load-path \".\")" \
    --eval "(add-to-list 'load-path \"./test\")" \
    --eval "(require 'ert)" \
    --eval "(require 'netlinx-mode-test)" \
    --eval "(ert-run-tests-batch-and-exit)") || {
    echo "Tests failed"
    exit 1
}

echo ""
echo "All tests passed!"
