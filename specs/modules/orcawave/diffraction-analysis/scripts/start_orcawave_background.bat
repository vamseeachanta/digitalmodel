@echo off
REM Start OrcaWave Analysis in Background
REM This launches the analysis and returns immediately

echo ========================================
echo Starting OrcaWave Analysis in Background
echo ========================================
echo.

cd /d "%~dp0"

REM Create a timestamp for unique log file
set TIMESTAMP=%date:~-4,4%%date:~-10,2%%date:~-7,2%_%time:~0,2%%time:~3,2%%time:~6,2%
set TIMESTAMP=%TIMESTAMP: =0%

REM Start Python script in background
echo Launching OrcaWave analysis...
echo This will run in the background for approximately 2-3 hours.
echo.
echo Log file: ..\logs\orcawave_%TIMESTAMP%.log
echo.

start /B cmd /c python execute_orcawave.py > ..\logs\orcawave_%TIMESTAMP%.log 2>&1

echo OrcaWave analysis started in background!
echo.
echo To check progress:
echo   - View log file in: ..\logs\
echo   - Check Task Manager for OrcaWave.exe process
echo   - Results will appear in: ..\outputs\
echo.
echo You can continue working while the analysis runs.
echo.
pause