# https://stackoverflow.com/questions/17683368/running-a-command-on-each-directory-in-a-list-using-powershell

$dir = dir C:\github | ?{$_.PSISContainer}

foreach ($d in $dir){
    echo "Running git push pull in folder"
    echo $d.FullName
    Set-Location -Path $d.FullName
    # $daily_routine_batch = Join-Path $d.FullName -ChildPath ("\dev_tools\daily_routine.bat")
    $daily_routine_batch = Join-Path $d.FullName -ChildPath ("\dev_tools\daily_routine.bat")
    
    & $daily_routine_batch
}

Set-Location -Path $dir
 