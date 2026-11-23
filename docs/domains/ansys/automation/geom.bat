@echo off

REM Define a variable to store the current path
set "currentPath=%~dp0"
set scriptpath=%currentPath%step2.py
echo The current path is: %currentPath%
echo The path to the script is: %scriptpath%

REM Run SpaceClaim for geometry building
REM echo "Run Geometry Building in SpaceClaim"
set SPCCLM_EXE=%AWP_ROOT231%/scdm/spaceclaim.exe
echo Running Geometry Building in SpaceClaim using %SPCCLM_EXE% ...

"%SPCCLM_EXE%" /RunScript="%scriptpath%" /Headless=True /Splash=False /Welcome=False /ExitAfterScript=True
echo "Geometry Created Succesfully!"