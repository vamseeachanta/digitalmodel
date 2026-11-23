#!/bin/bash
# File: poetry_env.sh
# Usage: ./poetry_env.sh
# Description: Initialize poetry project and install dependencies from requirements.txt

set -e

# Define top-level repo path (assumed one level up)
TOPLEVEL="$(cd "$(dirname "$0")/.." && pwd)"
REQ="$TOPLEVEL/dev_tools/requirements.txt"
PYPROJECT="$TOPLEVEL/pyproject.toml"

# Move to top-level directory
cd "$TOPLEVEL"

if [ -f "$PYPROJECT" ]; then
    echo "[INFO] Found pyproject.toml in the repo. Validating..."
    if grep -q '\[tool.poetry\]' "$PYPROJECT"; then
        echo "[INFO] pyproject.toml is Poetry-compatible. Proceeding."
    else
        echo "[ERROR] pyproject.toml exists but is not Poetry-compatible. Please convert it manually."
        exit 1
    fi
else
    echo "[INFO] No pyproject.toml found. Initializing poetry project..."
    poetry init
fi

if [ -f "$REQ" ]; then
    echo "[INFO] creating environment via requirements.txt..."
    poetry run pip install -r "$REQ"
    echo "[INFO] environment created successfully."
else
    echo "[WARN] requirements.txt not found at $REQ. Skipping installation."
fi
