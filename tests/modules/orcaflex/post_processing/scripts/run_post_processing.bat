@echo off
REM Run post_processing OrcaFlex analysis

echo Running post_processing analysis...
cd /d "%~dp0\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/post_processing_config.yml ^
    --parallel 4

echo Analysis complete!
pause
