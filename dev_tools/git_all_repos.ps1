# https://stackoverflow.com/questions/17683368/running-a-command-on-each-directory-in-a-list-using-powershell

$dir = dir K:\python | ? { $_.PSISContainer }

foreach ($d in $dir) {
    echo "Checking for changes in folder ... START"
    echo $d.FullName
    Set-Location -Path $d.FullName
    $changes = git status --porcelain # checks if there are changes in the working directory otherwise skips
    
    if ($changes) {
        echo "Changes detected. Running git push pull in folder ... START "**
        $daily_routine_batch = Join-Path $d.FullName -ChildPath ("dev_tools\daily_routine.bat")
        & $daily_routine_batch
        & echo "Running git push pull in folder ... DONE"
    }
    else {
        echo "No changes detected. Skipping git operations."
    }
}

Set-Location -Path $dir

cd K:\python\acma-projects\dev_tools
