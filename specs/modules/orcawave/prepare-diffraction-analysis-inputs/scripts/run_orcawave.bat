@echo off
REM ============================================================================
REM OrcaWave Execution Script
REM ============================================================================
REM This script runs OrcaWave analysis with the generated input files
REM 
REM Usage: run_orcawave.bat [input_file.yml] [options]
REM 
REM Options:
REM   /GUI     - Launch OrcaWave in GUI mode
REM   /BATCH   - Run in batch mode (default)
REM   /LOG     - Enable detailed logging
REM ============================================================================

setlocal enabledelayedexpansion

REM Set default values
set MODE=BATCH
set INPUT_FILE=
set LOG_ENABLED=0
set ORCAWAVE_EXE="C:\Program Files\Orcina\OrcaWave\11.5\OrcaWave.exe"

REM Parse command line arguments
:parse_args
if "%~1"=="" goto :check_input
if /i "%~1"=="/GUI" (
    set MODE=GUI
    shift
    goto :parse_args
)
if /i "%~1"=="/BATCH" (
    set MODE=BATCH
    shift
    goto :parse_args
)
if /i "%~1"=="/LOG" (
    set LOG_ENABLED=1
    shift
    goto :parse_args
)
REM Assume it's the input file
set INPUT_FILE=%~1
shift
goto :parse_args

:check_input
if "%INPUT_FILE%"=="" (
    echo ERROR: No input file specified
    echo.
    echo Usage: run_orcawave.bat [input_file.yml] [options]
    echo.
    echo Options:
    echo   /GUI     - Launch OrcaWave in GUI mode
    echo   /BATCH   - Run in batch mode ^(default^)
    echo   /LOG     - Enable detailed logging
    exit /b 1
)

REM Check if input file exists
if not exist "%INPUT_FILE%" (
    echo ERROR: Input file not found: %INPUT_FILE%
    exit /b 1
)

REM Check if OrcaWave is installed
if not exist %ORCAWAVE_EXE% (
    echo ERROR: OrcaWave not found at: %ORCAWAVE_EXE%
    echo Please update the ORCAWAVE_EXE path in this script
    exit /b 1
)

REM Create output directory
set OUTPUT_DIR=%~dp1output
if not exist "%OUTPUT_DIR%" mkdir "%OUTPUT_DIR%"

REM Set log file if logging enabled
set LOG_FILE=
if %LOG_ENABLED%==1 (
    set LOG_FILE=%OUTPUT_DIR%\orcawave_%date:~-4,4%%date:~-10,2%%date:~-7,2%_%time:~0,2%%time:~3,2%.log
    set LOG_FILE=!LOG_FILE: =0!
    echo Logging to: !LOG_FILE!
)

echo ============================================================================
echo OrcaWave Analysis Runner
echo ============================================================================
echo Input File: %INPUT_FILE%
echo Mode: %MODE%
echo Output Dir: %OUTPUT_DIR%
if %LOG_ENABLED%==1 echo Log File: !LOG_FILE!
echo ============================================================================

REM Run OrcaWave based on mode
if /i "%MODE%"=="GUI" (
    echo Launching OrcaWave in GUI mode...
    start "" %ORCAWAVE_EXE% "%INPUT_FILE%"
    echo.
    echo OrcaWave GUI launched. Please run the analysis manually.
) else (
    echo Running OrcaWave in batch mode...
    
    if %LOG_ENABLED%==1 (
        %ORCAWAVE_EXE% /calculate "%INPUT_FILE%" > "!LOG_FILE!" 2>&1
    ) else (
        %ORCAWAVE_EXE% /calculate "%INPUT_FILE%"
    )
    
    if !errorlevel! neq 0 (
        echo ERROR: OrcaWave analysis failed with error code !errorlevel!
        if %LOG_ENABLED%==1 echo Check log file for details: !LOG_FILE!
        exit /b !errorlevel!
    )
    
    echo.
    echo ✅ OrcaWave analysis completed successfully
)

REM Check for output files
echo.
echo Checking for output files...
set OUTPUT_COUNT=0
for %%f in ("%OUTPUT_DIR%\*.csv" "%OUTPUT_DIR%\*.txt" "%OUTPUT_DIR%\*.dat") do (
    set /a OUTPUT_COUNT+=1
)

if %OUTPUT_COUNT% gtr 0 (
    echo Found %OUTPUT_COUNT% output files in: %OUTPUT_DIR%
) else (
    echo WARNING: No output files found in: %OUTPUT_DIR%
)

echo ============================================================================
echo Analysis complete
echo ============================================================================

endlocal
exit /b 0