#!/bin/bash
# Update all dependencies using uv (for bash environments)
uv pip install --upgrade -r <(uv pip compile pyproject.toml)
