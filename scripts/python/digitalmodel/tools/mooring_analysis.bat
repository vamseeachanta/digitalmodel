@echo off
:: Convenience batch script for running mooring comparative analysis on Windows

:: Check if python is available
python --version >nul 2>&1
if errorlevel 1 (
    echo Error: Python is not installed or not in PATH
    exit /b 1
)

:: Run the analysis module
python -m digitalmodel.orcaflex.analysis %*