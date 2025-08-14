@echo off
echo ========================================
echo OrcaFlex Browser Backend Test
echo ========================================
echo.

REM Check if Rich is installed
python -c "import rich" 2>nul
if errorlevel 1 (
    echo Installing required packages...
    pip install -r requirements.txt
    echo.
)

echo Choose test mode:
echo 1. Quick Test (run_test.py)
echo 2. Interactive CLI Test (cli/test_interface.py)
echo 3. View Latest Verification
echo.

set /p choice="Enter choice (1-3): "

if "%choice%"=="1" (
    python run_test.py
) else if "%choice%"=="2" (
    python cli/test_interface.py
) else if "%choice%"=="3" (
    python cli/test_interface.py --verify-latest
) else (
    echo Invalid choice!
)

echo.
pause