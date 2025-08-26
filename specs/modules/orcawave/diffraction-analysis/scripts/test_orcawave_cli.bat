@echo off
REM Test OrcaWave geometry import via command line
REM This batch file tests different approaches to load geometry in OrcaWave

echo ========================================
echo OrcaWave Geometry Testing Script
echo ========================================
echo.

set ORCAWAVE="C:\Program Files (x86)\Orcina\OrcaFlex\11.5\OrcaWave.exe"
set CONFIGS=..\configs
set GEOMETRY=..\inputs\geometry

echo Checking OrcaWave installation...
if not exist %ORCAWAVE% (
    echo ERROR: OrcaWave not found at %ORCAWAVE%
    exit /b 1
)
echo OrcaWave found.
echo.

echo Available test options:
echo 1. Open simple box test configuration
echo 2. Open Sea Cypress configuration  
echo 3. Open OrcaWave GUI only
echo 4. Validate simple box YAML
echo 5. Validate Sea Cypress YAML
echo.

REM Option 1: Try opening with simple box config
echo Option 1: Testing simple box configuration...
echo Command: %ORCAWAVE% "%CONFIGS%\test_simple_box.yml"
echo.
echo Press any key to launch OrcaWave with simple box...
pause >nul
start "" %ORCAWAVE% "%CD%\%CONFIGS%\test_simple_box.yml"

echo.
echo ========================================
echo Manual Testing Steps:
echo ========================================
echo.
echo 1. Check if geometry loaded successfully
echo 2. If not, try File -^> Import -^> Wamit gdf
echo 3. Navigate to: %GEOMETRY%
echo 4. Select: simple_box_test.gdf
echo 5. If simple box works, try sea_cypress_orcawave.gdf
echo.
echo Alternative formats to test:
echo - sea_cypress_gmsh_optimized.dat (AQWA format)
echo - sea_cypress_trimesh.gdf (original attempt)
echo.
echo ========================================
pause