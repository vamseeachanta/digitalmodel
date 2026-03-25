@echo off
REM Setup and test project using uv (Windows)
REM Create venv if not exists
if not exist .venv (
    python -m venv .venv
)
call .venv\Scripts\activate
pip install uv
uv pip install -e .
uv pip install pytest
uv pip run pytest
