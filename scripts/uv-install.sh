#!/bin/bash
# Install all dependencies using uv (for bash environments)
uv pip install -r <(uv pip compile pyproject.toml)
