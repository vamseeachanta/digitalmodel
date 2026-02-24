@echo off
REM Production Rainflow Analysis Batch Script
REM Version 1.1.0
REM Date: 2025-01-24

echo ============================================================
echo PRODUCTION RAINFLOW ANALYSIS
echo ============================================================
echo.

REM Check if virtual environment exists
if exist "D:\github\digitalmodel\.venv\Scripts\python.exe" (
    set PYTHON_EXE=D:\github\digitalmodel\.venv\Scripts\python.exe
    echo Using virtual environment Python
) else (
    set PYTHON_EXE=python
    echo Using system Python
)

REM Navigate to the module directory
cd /d D:\github\digitalmodel\specs\modules\signal-analysis\rainflow_with_visualization

REM Run the production analysis
echo Starting production analysis...
echo Input: D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\scaled_tension
echo Output: D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output
echo.

%PYTHON_EXE% run_rainflow_analysis.py input\rainflow_analysis_config_production.yml

if %ERRORLEVEL% EQU 0 (
    echo.
    echo ============================================================
    echo PRODUCTION ANALYSIS COMPLETED SUCCESSFULLY
    echo ============================================================
    echo Check output folder for results:
    echo - Rainflow CSV files: D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\rainflow
    echo - FFT CSV files: D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\rainflow
    echo - Visualizations: D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\output\visualization
) else (
    echo.
    echo ============================================================
    echo ERROR: Analysis failed with error code %ERRORLEVEL%
    echo ============================================================
)

echo.
pause