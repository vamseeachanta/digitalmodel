@echo off
echo ================================================================================
echo LAUNCHING ORCAFLEX BATCH PROCESSING IN BACKGROUND
echo ================================================================================
echo Configuration: batch_all_fsts_vessel_statics_6dof.yml
echo Max Workers: 30
echo Total Models: 24
echo ================================================================================

REM Start the batch processing in a new window
start "OrcaFlex Batch Processing" /MIN cmd /c python run_batch_background.py --config batch_all_fsts_vessel_statics_6dof.yml

echo.
echo Batch processing started in background window.
echo Check the minimized window for progress.
echo.
pause