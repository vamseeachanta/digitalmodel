For 12 cores:
<code>
reg add HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Orcina\OrcaFlex /v DefaultThreadCount /t REG_DWORD /d 12 /f
</code>

For 60 cores:
<code>
reg add HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Orcina\OrcaFlex /v DefaultThreadCount /t REG_DWORD /d 60 /f
</code>

or in UI:
[HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Orcina\OrcaFlex]
"DefaultThreadCount"=dword:0000000c
