@echo off
REM ============================================================================
REM Batch Script: Run Manual Steps for Mooring Tension Iteration
REM ============================================================================
REM This script walks through each manual step of the mooring tension iteration
REM process, allowing you to run them one by one or all at once.
REM ============================================================================

setlocal enabledelayedexpansion

REM Set colors for better visibility
set RED=[91m
set GREEN=[92m
set YELLOW=[93m
set BLUE=[94m
set MAGENTA=[95m
set CYAN=[96m
set WHITE=[97m
set RESET=[0m

echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%         MOORING TENSION ITERATION - MANUAL STEPS RUNNER%RESET%
echo %CYAN%============================================================================%RESET%
echo.

REM Check Python availability
python --version >nul 2>&1
if errorlevel 1 (
    echo %RED%ERROR: Python is not installed or not in PATH%RESET%
    echo Please install Python 3.7+ and add it to your PATH
    pause
    exit /b 1
)

REM Check for required files
if not exist "config.yaml" (
    echo %RED%ERROR: config.yaml not found!%RESET%
    echo Please ensure config.yaml exists in the current directory
    pause
    exit /b 1
)

if not exist "manual_steps_automation.py" (
    echo %RED%ERROR: manual_steps_automation.py not found!%RESET%
    echo Please ensure all required files are present
    pause
    exit /b 1
)

:MAIN_MENU
cls
echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%                    MOORING TENSION ITERATION%RESET%
echo %CYAN%                    Manual Process Automation%RESET%
echo %CYAN%============================================================================%RESET%
echo.
echo %YELLOW%This script automates the manual mooring tension iteration process.%RESET%
echo %YELLOW%You can run steps individually or execute the complete workflow.%RESET%
echo.
echo %GREEN%Available Options:%RESET%
echo.
echo   %WHITE%1.%RESET% Run Complete Workflow (All Steps Automatically)
echo   %WHITE%2.%RESET% Interactive Step-by-Step Mode
echo   %WHITE%3.%RESET% Run Individual Steps
echo   %WHITE%4.%RESET% View Process Comparison (Manual vs Automated)
echo   %WHITE%5.%RESET% Check Configuration
echo   %WHITE%6.%RESET% View Previous Results
echo   %WHITE%7.%RESET% Export Manual Process Guide
echo   %WHITE%Q.%RESET% Quit
echo.
set /p choice=%MAGENTA%Select option (1-7, Q): %RESET%

if /i "%choice%"=="1" goto RUN_COMPLETE
if /i "%choice%"=="2" goto RUN_INTERACTIVE
if /i "%choice%"=="3" goto RUN_INDIVIDUAL
if /i "%choice%"=="4" goto VIEW_COMPARISON
if /i "%choice%"=="5" goto CHECK_CONFIG
if /i "%choice%"=="6" goto VIEW_RESULTS
if /i "%choice%"=="7" goto EXPORT_GUIDE
if /i "%choice%"=="Q" goto EXIT
goto INVALID_CHOICE

:RUN_COMPLETE
cls
echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%                    RUNNING COMPLETE WORKFLOW%RESET%
echo %CYAN%============================================================================%RESET%
echo.
echo %YELLOW%This will run all steps automatically until convergence or max iterations.%RESET%
echo.
set /p confirm=%MAGENTA%Proceed? (Y/N): %RESET%
if /i not "%confirm%"=="Y" goto MAIN_MENU

echo.
echo %GREEN%Starting automated workflow...%RESET%
echo.

REM Run the complete workflow
python -c "from manual_steps_automation import ManualStepsAutomation; automation = ManualStepsAutomation(); automation.run_complete_workflow()"

if errorlevel 1 (
    echo.
    echo %RED%ERROR: Workflow failed. Check logs for details.%RESET%
) else (
    echo.
    echo %GREEN%✓ Workflow completed successfully!%RESET%
)

pause
goto MAIN_MENU

:RUN_INTERACTIVE
cls
echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%                    INTERACTIVE STEP-BY-STEP MODE%RESET%
echo %CYAN%============================================================================%RESET%
echo.
echo %YELLOW%This will guide you through each step interactively.%RESET%
echo.

python step_by_step_runner.py

if errorlevel 1 (
    echo.
    echo %RED%ERROR: Interactive mode failed.%RESET%
)

pause
goto MAIN_MENU

:RUN_INDIVIDUAL
cls
echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%                    RUN INDIVIDUAL STEPS%RESET%
echo %CYAN%============================================================================%RESET%
echo.
echo %GREEN%Select step to run:%RESET%
echo.
echo   %WHITE%1.%RESET% Step 1: Input Preparation (Load CSV targets)
echo   %WHITE%2.%RESET% Step 2: Baseline Analysis (Run OrcaFlex)
echo   %WHITE%3.%RESET% Step 3: Result Extraction (Get tensions)
echo   %WHITE%4.%RESET% Step 4: Length Calculation (Calculate adjustments)
echo   %WHITE%5.%RESET% Step 5: Convergence Check (Check if done)
echo   %WHITE%B.%RESET% Back to Main Menu
echo.
set /p step=%MAGENTA%Select step (1-5, B): %RESET%

if /i "%step%"=="B" goto MAIN_MENU
if "%step%"=="1" goto RUN_STEP1
if "%step%"=="2" goto RUN_STEP2
if "%step%"=="3" goto RUN_STEP3
if "%step%"=="4" goto RUN_STEP4
if "%step%"=="5" goto RUN_STEP5
goto INVALID_STEP

:RUN_STEP1
echo.
echo %GREEN%Running Step 1: Input Preparation...%RESET%
python -c "from manual_steps_automation import ManualStepsAutomation; automation = ManualStepsAutomation(); automation.step1_input_preparation()"
pause
goto RUN_INDIVIDUAL

:RUN_STEP2
echo.
echo %GREEN%Running Step 2: Baseline Analysis...%RESET%
python -c "from manual_steps_automation import ManualStepsAutomation; automation = ManualStepsAutomation(); automation.step2_baseline_analysis()"
pause
goto RUN_INDIVIDUAL

:RUN_STEP3
echo.
echo %GREEN%Running Step 3: Result Extraction...%RESET%
python -c "from manual_steps_automation import ManualStepsAutomation; automation = ManualStepsAutomation(); automation.step3_result_extraction('iteration_00.sim')"
pause
goto RUN_INDIVIDUAL

:RUN_STEP4
echo.
echo %GREEN%Running Step 4: Length Calculation...%RESET%
python -c "from manual_steps_automation import ManualStepsAutomation; automation = ManualStepsAutomation(); automation.step4_length_calculation()"
pause
goto RUN_INDIVIDUAL

:RUN_STEP5
echo.
echo %GREEN%Running Step 5: Convergence Check...%RESET%
python -c "from manual_steps_automation import ManualStepsAutomation; automation = ManualStepsAutomation(); automation.step5_model_update_iteration({})"
pause
goto RUN_INDIVIDUAL

:VIEW_COMPARISON
cls
echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%                    PROCESS COMPARISON%RESET%
echo %CYAN%============================================================================%RESET%
echo.

python manual_vs_automated_comparison.py

pause
goto MAIN_MENU

:CHECK_CONFIG
cls
echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%                    CONFIGURATION CHECK%RESET%
echo %CYAN%============================================================================%RESET%
echo.

echo %GREEN%Current Configuration (config.yaml):%RESET%
echo.
type config.yaml | more

pause
goto MAIN_MENU

:VIEW_RESULTS
cls
echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%                    PREVIOUS RESULTS%RESET%
echo %CYAN%============================================================================%RESET%
echo.

if exist "iteration_output\iteration_history.json" (
    echo %GREEN%Found iteration history:%RESET%
    echo.
    python -c "import json; data=json.load(open('iteration_output/iteration_history.json')); [print(f'Iteration {e[\"iteration\"]}: Max error = {e[\"max_error\"]*100:.1f}%%, Converged = {e[\"converged\"]}') for e in data]"
) else (
    echo %YELLOW%No previous results found.%RESET%
    echo Run the iteration process to generate results.
)

pause
goto MAIN_MENU

:EXPORT_GUIDE
cls
echo.
echo %CYAN%============================================================================%RESET%
echo %CYAN%                    EXPORT MANUAL PROCESS GUIDE%RESET%
echo %CYAN%============================================================================%RESET%
echo.

echo %GREEN%Generating manual process guide...%RESET%
echo.

python -c "from step_by_step_runner import InteractiveRunner; runner = InteractiveRunner(); runner.export_manual_guide()"

echo.
echo %GREEN%✓ Guide exported to manual_process_guide.md%RESET%

pause
goto MAIN_MENU

:INVALID_CHOICE
echo.
echo %RED%Invalid option. Please try again.%RESET%
pause
goto MAIN_MENU

:INVALID_STEP
echo.
echo %RED%Invalid step selection. Please try again.%RESET%
pause
goto RUN_INDIVIDUAL

:EXIT
echo.
echo %GREEN%Thank you for using the Mooring Tension Iteration tool!%RESET%
echo.
pause
exit /b 0