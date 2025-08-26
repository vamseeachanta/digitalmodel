@echo off
REM Run OrcaWave Analysis for Sea Cypress
REM This script executes the OrcaWave diffraction analysis

echo ========================================
echo OrcaWave Sea Cypress Diffraction Analysis
echo ========================================
echo.

REM Set OrcaWave path
set ORCAWAVE_PATH=C:\Program Files (x86)\Orcina\OrcaFlex\11.5
set ORCAWAVE_EXE="%ORCAWAVE_PATH%\OrcaWave.exe"

REM Check if OrcaWave exists
if not exist %ORCAWAVE_EXE% (
    echo ERROR: OrcaWave not found at %ORCAWAVE_EXE%
    echo Please verify OrcaWave installation path
    pause
    exit /b 1
)

echo Found OrcaWave at: %ORCAWAVE_EXE%
echo.

REM Set working directory
cd /d "%~dp0"
cd ..

REM Create output directories
if not exist outputs mkdir outputs
if not exist logs mkdir logs

REM Set paths
set CONFIG_FILE=configs\sea_cypress_diffraction.yml
set GDF_FILE=inputs\geometry\sea_cypress_trimesh.gdf
set OUTPUT_DIR=outputs
set LOG_FILE=logs\orcawave_run_%date:~-4,4%%date:~-10,2%%date:~-7,2%_%time:~0,2%%time:~3,2%%time:~6,2%.log

REM Remove spaces from log file name
set LOG_FILE=%LOG_FILE: =0%

echo Configuration: %CONFIG_FILE%
echo Geometry: %GDF_FILE%
echo Output: %OUTPUT_DIR%
echo Log: %LOG_FILE%
echo.

REM Check if files exist
if not exist %CONFIG_FILE% (
    echo ERROR: Configuration file not found: %CONFIG_FILE%
    pause
    exit /b 1
)

if not exist %GDF_FILE% (
    echo ERROR: GDF file not found: %GDF_FILE%
    pause
    exit /b 1
)

echo Starting OrcaWave analysis...
echo This may take 2-3 hours for 162 calculations (18 frequencies x 9 headings)
echo.
echo Press Ctrl+C to cancel or any other key to continue...
pause > nul

REM Run OrcaWave
echo.
echo Executing OrcaWave...
echo Command: %ORCAWAVE_EXE% -batch %CONFIG_FILE%
echo.

REM Execute OrcaWave in batch mode
%ORCAWAVE_EXE% -batch %CONFIG_FILE% > %LOG_FILE% 2>&1

REM Check result
if %ERRORLEVEL% EQU 0 (
    echo.
    echo SUCCESS: OrcaWave analysis completed successfully!
    echo Check output files in: %OUTPUT_DIR%
    echo Log saved to: %LOG_FILE%
    
    REM Process results
    echo.
    echo Processing results...
    cd scripts
    python process_orcawave_results.py --output-dir ../outputs
    
) else (
    echo.
    echo ERROR: OrcaWave analysis failed with error code %ERRORLEVEL%
    echo Check log file: %LOG_FILE%
    echo.
    echo Common issues:
    echo - No license available
    echo - Invalid configuration
    echo - Memory issues
    echo - File permissions
)

echo.
echo ========================================
echo Analysis Complete
echo ========================================
pause