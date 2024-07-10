https://stackoverflow.com/questions/17683368/running-a-command-on-each-directory-in-a-list-using-powershell

for /d %i in (C:\github\*) do @echo %~nxi
