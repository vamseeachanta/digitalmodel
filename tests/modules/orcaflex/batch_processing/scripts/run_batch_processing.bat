@echo off
REM Run batch_processing OrcaFlex analysis

echo Running batch_processing analysis...
cd /d "%~dp0\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/batch_processing_config.yml ^
    --parallel 4

echo Analysis complete!
pause
