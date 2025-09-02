@echo off
REM Run universal OrcaFlex analysis

echo Running universal analysis...
cd /d "%~dp0\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/universal_config.yml ^
    --parallel 4

echo Analysis complete!
pause
