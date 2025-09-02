@echo off
REM Run orcaflex_file_preparation OrcaFlex analysis

echo Running orcaflex_file_preparation analysis...
cd /d "%~dp0\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/orcaflex_file_preparation_config.yml ^
    --parallel 4

echo Analysis complete!
pause
