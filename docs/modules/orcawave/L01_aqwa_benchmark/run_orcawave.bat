@echo off
REM ========================================
REM OrcaWave Benchmark Analysis Launcher
REM ========================================

echo.
echo ========================================
echo OrcaWave Benchmark Analysis
echo ========================================
echo.

REM Find Python
set PYTHON_EXE=python
if exist "..\..\..\..\..\.venv\Scripts\python.exe" (
    set PYTHON_EXE=..\..\..\..\..\.venv\Scripts\python.exe
    echo Using venv Python: %PYTHON_EXE%
) else (
    echo Using system Python
)

REM Run with arguments
echo.
echo Running with 4 threads...
echo.

%PYTHON_EXE% run_orcawave_benchmark.py --threads 4 %*

echo.
echo ========================================
echo.

pause
