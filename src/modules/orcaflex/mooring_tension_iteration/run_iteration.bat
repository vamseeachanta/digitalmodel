@echo off
REM Batch script to run mooring tension iteration
REM Usage: run_iteration.bat [options]

echo ============================================
echo Mooring Tension Iteration System
echo ============================================
echo.

REM Check if Python is available
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python is not installed or not in PATH
    exit /b 1
)

REM Check if config file exists
if not exist "config.yaml" (
    echo ERROR: config.yaml not found in current directory
    echo Please ensure config.yaml exists before running
    exit /b 1
)

REM Default command
set COMMAND=run

REM Parse arguments
if "%1"=="status" (
    set COMMAND=status
    shift
)
if "%1"=="validate" (
    set COMMAND=validate
    shift
)
if "%1"=="help" (
    goto :show_help
)

REM Run the appropriate command
if "%COMMAND%"=="run" (
    echo Starting iteration process...
    echo.
    python mooring_iteration_cli.py run --config config.yaml %*
    if errorlevel 1 (
        echo.
        echo ERROR: Iteration process failed. Check logs for details.
        exit /b 1
    ) else (
        echo.
        echo Iteration process completed successfully!
    )
) else if "%COMMAND%"=="status" (
    python mooring_iteration_cli.py status --output-dir iteration_output %*
) else if "%COMMAND%"=="validate" (
    python mooring_iteration_cli.py validate --config config.yaml %*
)

goto :end

:show_help
echo Usage: run_iteration.bat [command] [options]
echo.
echo Commands:
echo   run       Run the iteration process (default)
echo   status    Check status of previous run
echo   validate  Validate configuration file
echo   help      Show this help message
echo.
echo Examples:
echo   run_iteration.bat                    - Run with default settings
echo   run_iteration.bat --max-iterations 20  - Run with 20 iterations max
echo   run_iteration.bat status             - Check last run status
echo   run_iteration.bat validate           - Validate config file
echo.

:end
pause