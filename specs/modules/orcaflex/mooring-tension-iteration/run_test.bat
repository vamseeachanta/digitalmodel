@echo off
REM Test orchestrator with limited iterations

cd /d "D:\github\digitalmodel"
uv run python specs\modules\orcaflex\mooring-tension-iteration\orchestrator.py --working-dir specs\modules\orcaflex\mooring-tension-iteration\go-by --max-iterations 2 --verbose
pause