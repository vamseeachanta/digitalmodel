@echo off
echo ============================================================
echo  ORCAFLEX BROWSER INTERFACE - MANUAL TESTING
echo  Task 1.7: Interactive Validation
echo ============================================================
echo.
echo This will launch the interactive manual test interface.
echo Please follow the prompts to complete Task 1.7 validation.
echo.
echo Press any key to start...
pause > nul

cd /d "%~dp0"
python manual_test_interface.py

echo.
echo Testing complete. Check the generated report file.
pause