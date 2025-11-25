#!/bin/bash

set -e

if [ "$CI" != "true" ]; then
    echo "Not running in CI, skipping version bump"
    exit 0
fi

TAG="${1}"
if [ -z "$TAG" ]; then
    echo "Usage: $0 <tag>"
    echo "Example: $0 v0.6.0"
    exit 1
fi

# Strip 'v' prefix to get version
VERSION="${TAG#v}"

if [ -z "$VERSION" ]; then
    echo "Failed to extract version from tag: $TAG"
    exit 1
fi

# Files to update
FILES=("netlinx-mode.el")

# Check if all files exist before attempting to modify them
for file in "${FILES[@]}"; do
    if [ ! -f "$file" ]; then
        echo "Error: $file not found"
        exit 1
    fi
done

# Update version in each file
if ! sed -i "s/^;; Version: .*$/;; Version: ${VERSION}/" netlinx-mode.el; then
    echo "Error: sed command failed"
    exit 1
fi

# Verify the changes were made
if ! grep -q "^;; Version: ${VERSION}$" netlinx-mode.el; then
    echo "Error: Failed to update version in netlinx-mode.el"
    exit 1
fi

echo "Version bumped to $VERSION in ${FILES[*]}"
