REM Helper script for starting using USER FORCE with socket and python

for /f "tokens=2 delims==" %%p in ('"set AWP_ROOT | sort /R"') do (set last_install=%%p
goto :Step2)
:Step2

for /f %%v in ('"dir /b "%last_install%"\commonfiles\CPython | sort /R"') do (set python_version=%%v
goto :Step3)
:Step3

start "python %python_version%" "%last_install%"\commonfiles\CPython\%python_version%\winx64\Release\python\python.exe %1

ping localhost -n 3 > nul
