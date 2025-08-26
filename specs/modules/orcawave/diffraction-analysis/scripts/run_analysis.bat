@echo off
REM Batch file to run Sea Cypress OrcaWave analysis
REM Usage: run_analysis.bat [--dry-run]

echo ========================================
echo Sea Cypress OrcaWave Diffraction Analysis
echo ========================================
echo.

REM Check Python is available
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python is not installed or not in PATH
    pause
    exit /b 1
)

REM Set working directory to script location
cd /d "%~dp0"

REM Create necessary directories
if not exist ..\outputs mkdir ..\outputs
if not exist ..\logs mkdir ..\logs

REM Run the analysis
if "%1"=="--dry-run" (
    echo Running in DRY RUN mode...
    python run_sea_cypress_analysis.py --dry-run
) else (
    echo Starting OrcaWave analysis...
    echo This may take several hours depending on mesh size.
    echo.
    python run_sea_cypress_analysis.py
)

REM Check exit code
if errorlevel 1 (
    echo.
    echo ERROR: Analysis failed. Check logs for details.
    pause
    exit /b 1
) else (
    echo.
    echo SUCCESS: Analysis completed successfully.
    echo Check outputs folder for results.
)

pause