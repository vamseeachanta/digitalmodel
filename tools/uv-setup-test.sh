#!/bin/bash
# Setup and test project using uv (Linux/macOS)
if [ ! -d ".venv" ]; then
  python3 -m venv .venv
fi
source .venv/bin/activate
pip install uv
uv pip install -e .
uv pip install pytest
uv pip run pytest
