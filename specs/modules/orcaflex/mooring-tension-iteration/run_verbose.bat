@echo off
REM Run orchestrator with verbose output for debugging

cd /d "D:\github\digitalmodel"
uv run python specs\modules\orcaflex\mooring-tension-iteration\orchestrator.py --working-dir specs\modules\orcaflex\mooring-tension-iteration\go-by --verbose %*
pause