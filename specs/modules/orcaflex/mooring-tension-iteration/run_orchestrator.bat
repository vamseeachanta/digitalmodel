@echo off
REM Mooring Tension Iteration Orchestrator - Windows Batch Wrapper
REM Runs the orchestrator from the go-by directory

echo ============================================================
echo MOORING TENSION ITERATION ORCHESTRATOR
echo ============================================================
echo.

REM Navigate to repository root for uv
cd /d "D:\github\digitalmodel"

REM Run orchestrator using uv environment
uv run python specs\modules\orcaflex\mooring-tension-iteration\orchestrator.py --working-dir specs\modules\orcaflex\mooring-tension-iteration\go-by %*

REM Check exit code
if %ERRORLEVEL% EQU 0 (
    echo.
    echo ============================================================
    echo ORCHESTRATOR COMPLETED SUCCESSFULLY
    echo ============================================================
) else (
    echo.
    echo ============================================================
    echo ORCHESTRATOR FAILED OR DID NOT CONVERGE
    echo ============================================================
)

pause