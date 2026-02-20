@echo off
cls
set "currentPath=%~dp0"
set Project_name=Aqwa_Cosimulation
set input_path=\dp0\AQW\AQW\AQ\Analysis\ANALYSIS.plt
set input_path=\dp0\AQW-2\AQW\AQ\Analysis\TIMERESPONSE.plt
REM set input_path=\dp0\AQW\AQW\AQ\Analysis\
set directory=%currentPath%%Project_name%_files%input_path%

set scriptpath=%currentPath%step2.py

echo currentPath=%currentPath%
echo Project_name=%Project_name%
echo input_path=%input_path%
echo directory=%directory%
REM cd %directory%

set output_path=%currentPath%
REM set output_path=D:\Cosimulation\automation
echo output=%output_path%


set WBDIR="C:\Program Files\ANSYS Inc\v231\aisol\workbench.bat"
set AQWrdrDIR="C:\Program Files\ANSYS Inc\v231\aisol\bin\winx64\AqwaReader.exe"
REM echo %options%
REM echo =========================================================================================================
REM echo %WBDIR% -cmd %AQWrdrDIR% %options%
REM "C:\Program Files\ANSYS Inc\v231\aisol\workbench.bat" -cmd "C:\Program Files\ANSYS Inc\v231\aisol\bin\winx64\AqwaReader.exe"
echo here!!!!
REM Heave:
set opt1=1
set opt2=1
set opt3=1
set opt4=3
set options= --PLT1 %opt1% --PLT2 %opt2% --PLT3 %opt3% --PLT4 %opt4% --Format "csv" --Type "Graphical" --InFile "%directory%" --OutFile "%output_path%Heave"
start cmd.exe /K "%WBDIR% -cmd %AQWrdrDIR% %options%"
REM VelocityX:
set opt3=2
set opt4=1 
REM X direciton
set options= --PLT1 %opt1% --PLT2 %opt2% --PLT3 %opt3% --PLT4 %opt4% --Format "csv" --Type "Graphical" --InFile "%directory%" --OutFile "%output_path%velocityX"
start cmd.exe /K "%WBDIR% -cmd %AQWrdrDIR% %options%"
REM AccelerationY:
set opt3=3
set opt4=2 
REM Y direciton
set options= --PLT1 %opt1% --PLT2 %opt2% --PLT3 %opt3% --PLT4 %opt4% --Format "csv" --Type "Graphical" --InFile "%directory%" --OutFile "%output_path%AccY"
start cmd.exe /K "%WBDIR% -cmd %AQWrdrDIR% %options%"
REM Mooring tension:
set opt3=51
set opt4=4 
REM Tension
set options= --PLT1 %opt1% --PLT2 %opt2% --PLT3 %opt3% --PLT4 %opt4% --Format "csv" --Type "Graphical" --InFile "%directory%" --OutFile "%output_path%Moortens1"
start cmd.exe /K "%WBDIR% -cmd %AQWrdrDIR% %options%"