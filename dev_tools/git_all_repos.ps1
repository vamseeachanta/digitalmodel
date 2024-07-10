# https://stackoverflow.com/questions/17683368/running-a-command-on-each-directory-in-a-list-using-powershell

$dir = dir C:\github | ?{$_.PSISContainer}

foreach ($d in $dir){
    $name = Join-Path -Path $d.FullName -ChildPath ("\dev_tools\daily_routine.bat")
    
    & $name
}