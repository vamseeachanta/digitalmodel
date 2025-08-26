@echo off
REM Run OrcaWave workflow with parallel testing
REM Tests configuration before execution

echo ========================================
echo  OrcaWave Parallel Testing Mode
echo ========================================
echo.

REM Activate UV environment
echo Activating UV environment...
cd /d D:\github\digitalmodel
call uv sync
echo.

REM Run in test mode first
echo Running parallel validation tests...
uv run python specs\modules\orcawave\diffraction-analysis\scripts\execute_orcawave_parallel.py ^
    --config specs\modules\orcawave\diffraction-analysis\inputs\orcawave\go-by\orcawave_001_ship_raos_rev2.yml ^
    --dry-run

echo.
echo ========================================
echo  Validation Complete
echo ========================================
echo.
echo Check validation results in:
echo specs\modules\orcawave\diffraction-analysis\validation\
echo.
echo If validation passed, run without --dry-run flag
echo.
pause