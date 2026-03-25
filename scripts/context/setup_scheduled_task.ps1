# setup_scheduled_task.ps1 - Create Windows scheduled task for daily context check
# Run as Administrator: powershell -ExecutionPolicy Bypass -File setup_scheduled_task.ps1

$TaskName = "ContextManagementDaily"
$TaskPath = "\Claude\"
$Description = "Daily context file health check and improvement suggestions"
$WorkspaceRoot = "D:\workspace-hub"
$ScriptPath = "$WorkspaceRoot\scripts\context\daily_context_check.bat"

# Check if running as admin
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "Please run this script as Administrator" -ForegroundColor Red
    exit 1
}

Write-Host "=========================================="
Write-Host "Setting up Windows Scheduled Task"
Write-Host "=========================================="
Write-Host ""

# Remove existing task if present
$existingTask = Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
if ($existingTask) {
    Write-Host "Removing existing task..."
    Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
}

# Create the action
$Action = New-ScheduledTaskAction -Execute $ScriptPath -WorkingDirectory $WorkspaceRoot

# Create the trigger (daily at 6:00 AM)
$Trigger = New-ScheduledTaskTrigger -Daily -At 6:00AM

# Create settings
$Settings = New-ScheduledTaskSettingsSet `
    -AllowStartIfOnBatteries `
    -DontStopIfGoingOnBatteries `
    -StartWhenAvailable `
    -RunOnlyIfNetworkAvailable:$false `
    -ExecutionTimeLimit (New-TimeSpan -Minutes 30)

# Create the principal (run whether user is logged on or not)
$Principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType S4U -RunLevel Limited

# Register the task
try {
    Register-ScheduledTask `
        -TaskName $TaskName `
        -TaskPath $TaskPath `
        -Action $Action `
        -Trigger $Trigger `
        -Settings $Settings `
        -Principal $Principal `
        -Description $Description

    Write-Host ""
    Write-Host "Task created successfully!" -ForegroundColor Green
    Write-Host ""
    Write-Host "Task Details:"
    Write-Host "  Name: $TaskName"
    Write-Host "  Path: $TaskPath"
    Write-Host "  Schedule: Daily at 6:00 AM"
    Write-Host "  Script: $ScriptPath"
    Write-Host ""
    Write-Host "To run manually:"
    Write-Host "  schtasks /run /tn `"\Claude\$TaskName`""
    Write-Host ""
    Write-Host "To view task:"
    Write-Host "  Get-ScheduledTask -TaskName $TaskName"
    Write-Host ""
    Write-Host "To remove task:"
    Write-Host "  Unregister-ScheduledTask -TaskName $TaskName"
}
catch {
    Write-Host "Error creating task: $_" -ForegroundColor Red
    exit 1
}

# Verify task was created
$task = Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
if ($task) {
    Write-Host "Verification: Task exists and is ready." -ForegroundColor Green
}
else {
    Write-Host "Verification failed: Task not found." -ForegroundColor Red
}
