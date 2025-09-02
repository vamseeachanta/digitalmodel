@echo off
REM Run analysis OrcaFlex analysis

echo Running analysis analysis...
cd /d "%~dp0\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/analysis_config.yml ^
    --parallel 4

echo Analysis complete!
pause
