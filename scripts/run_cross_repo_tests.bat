@echo off
REM ABOUTME: Windows batch script to run cross-repository tests
REM Quick launcher for cross-repository test runner

echo ========================================
echo Cross-Repository Test Runner
echo ========================================
echo.

cd /d "%~dp0.."

if "%1"=="--help" (
    echo Usage: run_cross_repo_tests.bat [OPTIONS]
    echo.
    echo Options:
    echo   --sequential    Run tests sequentially instead of parallel
    echo   --workers N     Set number of parallel workers (default: 4)
    echo   --timeout N     Set timeout per repo in seconds (default: 300)
    echo   --help          Show this help message
    echo.
    goto :eof
)

echo Starting cross-repository tests...
echo This will test all repositories in D:\workspace-hub
echo.

uv run python scripts/cross_repo_test_runner.py %*

if %ERRORLEVEL% EQU 0 (
    echo.
    echo ========================================
    echo Tests completed successfully!
    echo Check reports/cross_repo_tests/ for results
    echo ========================================
) else (
    echo.
    echo ========================================
    echo Tests failed with errors
    echo ========================================
)
