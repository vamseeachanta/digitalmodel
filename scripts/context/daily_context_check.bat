@echo off
REM daily_context_check.bat - Windows wrapper for daily context check
REM Called by Windows Task Scheduler

if not defined WORKSPACE_ROOT (
    echo Set WORKSPACE_ROOT to the workspace-hub checkout.
    exit /b 1
)
set PATH=%PATH%;C:\Program Files\Git\bin;C:\Program Files\Git\usr\bin

cd /d %WORKSPACE_ROOT%

REM Run the bash script using Git Bash
"C:\Program Files\Git\bin\bash.exe" "%WORKSPACE_ROOT%\scripts\context\daily_context_check.sh"

REM Log completion
echo [%date% %time%] Daily context check completed >> "%WORKSPACE_ROOT%\.claude\reports\scheduler.log"
