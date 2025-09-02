@echo off
REM Run file_preparation OrcaFlex analysis

echo Running file_preparation analysis...
cd /d "%~dp0\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/file_preparation_config.yml ^
    --parallel 4

echo Analysis complete!
pause
