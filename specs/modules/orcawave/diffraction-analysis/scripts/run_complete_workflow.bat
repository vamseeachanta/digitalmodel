@echo off
REM Complete OrcaWave Workflow Execution
REM Runs the entire pipeline from GMsh mesh to OrcaFlex data

echo ========================================
echo  OrcaWave Complete Workflow
echo ========================================
echo.

REM Set paths
set MESH_FILE=..\inputs\geometry\Sea Cypress_0.25 Mesh_Ascii.msh
set OUTPUT_DIR=..\inputs\orcawave
set RESULTS_DIR=..\outputs

REM Step 1: Generate OrcaWave input from GMsh mesh
echo [Step 1] Generating OrcaWave input files...
echo.
python generate_orcawave_input.py --mesh-file "%MESH_FILE%" --output-dir "%OUTPUT_DIR%"
if errorlevel 1 (
    echo Error in input generation!
    pause
    exit /b 1
)
echo.

REM Step 2: Validate and execute OrcaWave analysis
echo [Step 2] Validating OrcaWave configuration...
echo.
set CONFIG_FILE=%OUTPUT_DIR%\sea_cypress_from_gmsh_*.yml
for %%f in (%CONFIG_FILE%) do set CONFIG_FILE=%%f
python execute_orcawave_parallel.py --config "%CONFIG_FILE%" --dry-run
echo.

echo Do you want to proceed with execution? (Y/N)
set /p choice=
if /i "%choice%" neq "Y" (
    echo Execution cancelled.
    pause
    exit /b 0
)

echo [Step 3] Executing OrcaWave analysis...
python execute_orcawave_parallel.py --config "%CONFIG_FILE%"
if errorlevel 1 (
    echo Warning: OrcaWave execution encountered issues.
    echo Check validation report for details.
)
echo.

REM Step 4: Post-process results
echo [Step 4] Post-processing OrcaWave results...
echo.
python postprocess_orcawave_parallel.py --results-dir "%RESULTS_DIR%"
if errorlevel 1 (
    echo Error in post-processing!
    pause
    exit /b 1
)
echo.

echo ========================================
echo  Workflow Complete!
echo ========================================
echo.
echo Output files generated:
echo - OrcaWave input: %OUTPUT_DIR%
echo - Processed results: %RESULTS_DIR%\processed
echo - OrcaFlex data: %RESULTS_DIR%\processed\sea_cypress_orcaflex.yml
echo.
pause