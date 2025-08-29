@echo off
REM ================================================================
REM OrcaWave GUI Launcher Script
REM ================================================================
REM Description: Launches OrcaWave GUI with pre-loaded configurations
REM Author: OrcaWave Automation Module
REM Created: 2025-08-26
REM ================================================================

setlocal enabledelayedexpansion

REM Configuration
set "ORCAWAVE_PATH=C:\Program Files\Orcina\OrcaWave\11.5"
set "ORCAWAVE_EXE=%ORCAWAVE_PATH%\OrcaWave.exe"
set "CONFIG_DIR=%~dp0..\outputs\orcawave_configs\merged"

REM Check if OrcaWave is installed
if not exist "%ORCAWAVE_EXE%" (
    echo ERROR: OrcaWave not found at: %ORCAWAVE_EXE%
    echo Please update ORCAWAVE_PATH in this script
    pause
    exit /b 1
)

REM Display header
cls
echo ================================================================
echo                   OrcaWave GUI Launcher
echo ================================================================
echo.

REM Check for command line argument
if not "%~1"=="" (
    set "SELECTED_CONFIG=%~1"
    goto :launch
)

REM List available configurations
echo Available configurations:
echo.

set /a count=0
for %%F in ("%CONFIG_DIR%\*.yml") do (
    set /a count+=1
    set "config_!count!=%%~nxF"
    echo   !count!. %%~nxF
)

echo.
echo   0. Launch OrcaWave without loading a configuration
echo.
echo ----------------------------------------------------------------

REM Get user selection
set /p selection="Select configuration number (0-%count%): "

REM Validate selection
if "%selection%"=="0" (
    set "SELECTED_CONFIG="
    goto :launch
)

if %selection% lss 1 (
    echo ERROR: Invalid selection
    pause
    exit /b 1
)

if %selection% gtr %count% (
    echo ERROR: Invalid selection
    pause
    exit /b 1
)

REM Get selected config file
set "SELECTED_CONFIG=!config_%selection%!"

:launch
REM Launch OrcaWave
if "%SELECTED_CONFIG%"=="" (
    echo.
    echo Launching OrcaWave...
    start "" "%ORCAWAVE_EXE%"
    echo.
    echo OrcaWave launched successfully.
) else (
    set "CONFIG_PATH=%CONFIG_DIR%\%SELECTED_CONFIG%"
    
    if not exist "!CONFIG_PATH!" (
        echo ERROR: Configuration file not found: !CONFIG_PATH!
        pause
        exit /b 1
    )
    
    echo.
    echo Launching OrcaWave with configuration: %SELECTED_CONFIG%
    echo Full path: !CONFIG_PATH!
    start "" "%ORCAWAVE_EXE%" "!CONFIG_PATH!"
    echo.
    echo OrcaWave launched successfully with pre-loaded configuration.
    
    REM Display usage tips
    echo.
    echo ================================================================
    echo                         Usage Tips
    echo ================================================================
    echo.
    echo 1. The configuration has been loaded into OrcaWave
    echo 2. Review the model in the Bodies tab
    echo 3. Check mesh visualization in the Drawing tab
    echo 4. Verify analysis settings in Calculation tab
    echo 5. Click "Calculate" to run the analysis
    echo 6. View results in the Results tab after completion
    echo.
    echo Note: Mass is in tonnes (Te), inertia in Te.m²
    echo.
)

echo ----------------------------------------------------------------
echo Press any key to close this window...
pause >nul

endlocal
exit /b 0