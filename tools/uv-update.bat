@echo off
REM Update all dependencies using uv
uv pip install --upgrade -r <(uv pip compile pyproject.toml)
