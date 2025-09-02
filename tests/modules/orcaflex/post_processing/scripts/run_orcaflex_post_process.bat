@echo off
REM Run orcaflex_post_process OrcaFlex analysis

echo Running orcaflex_post_process analysis...
cd /d "%~dp0\.."

REM Use the universal runner
python -m digitalmodel.modules.orcaflex.universal ^
    --input-directory . ^
    --config scripts/orcaflex_post_process_config.yml ^
    --parallel 4

echo Analysis complete!
pause
