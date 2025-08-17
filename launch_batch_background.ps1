# PowerShell script to launch OrcaFlex batch processing in background

Write-Host "================================================================================" -ForegroundColor Cyan
Write-Host "LAUNCHING ORCAFLEX BATCH PROCESSING IN BACKGROUND" -ForegroundColor Yellow
Write-Host "================================================================================" -ForegroundColor Cyan
Write-Host "Configuration: batch_all_fsts_vessel_statics_6dof.yml"
Write-Host "Max Workers: 30"
Write-Host "Total Models: 24"
Write-Host "================================================================================" -ForegroundColor Cyan

# Start the process in background
$process = Start-Process python -ArgumentList "run_batch_background.py", "--config", "batch_all_fsts_vessel_statics_6dof.yml" -PassThru -WindowStyle Minimized

Write-Host "`nBatch processing started in background (Process ID: $($process.Id))" -ForegroundColor Green
Write-Host "The process is running in a minimized window.`n"

# Option to monitor
$monitor = Read-Host "Do you want to monitor the progress? (Y/N)"
if ($monitor -eq 'Y' -or $monitor -eq 'y') {
    # Find the log file
    $logDir = "D:\1522\ctr7\orcaflex\rev_a08\base_files\fsts_lngc_pretension\output_batch_all_6dof"
    $logFile = Get-ChildItem -Path $logDir -Filter "background_batch_*.log" | Sort-Object LastWriteTime -Descending | Select-Object -First 1
    
    if ($logFile) {
        Write-Host "Monitoring log file: $($logFile.FullName)" -ForegroundColor Yellow
        Get-Content $logFile.FullName -Wait
    } else {
        Write-Host "Log file not found yet. Please check $logDir" -ForegroundColor Red
    }
} else {
    Write-Host "Process running in background. Check the minimized window for progress." -ForegroundColor Green
}