@echo off
REM daily_context_check.bat - Windows wrapper for daily context check
REM Called by Windows Task Scheduler

set WORKSPACE_ROOT=D:\workspace-hub
set PATH=%PATH%;C:\Program Files\Git\bin;C:\Program Files\Git\usr\bin

cd /d %WORKSPACE_ROOT%

REM Run the bash script using Git Bash
"C:\Program Files\Git\bin\bash.exe" "%WORKSPACE_ROOT%\scripts\context\daily_context_check.sh"

REM Log completion
echo [%date% %time%] Daily context check completed >> "%WORKSPACE_ROOT%\.claude\reports\scheduler.log"
