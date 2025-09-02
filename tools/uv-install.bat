@echo off
REM Install all dependencies using uv
uv pip install -r <(uv pip compile pyproject.toml)
