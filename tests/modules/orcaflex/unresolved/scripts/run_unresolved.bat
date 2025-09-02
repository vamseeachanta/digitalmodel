@echo off
REM Run unresolved OrcaFlex analysis

echo Running unresolved analysis...
cd /d "%~dp0\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/unresolved_config.yml ^
    --parallel 4

echo Analysis complete!
pause
