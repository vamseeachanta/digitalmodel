# Requires admin privileges
# Run with: PowerShell -ExecutionPolicy Bypass -File .\add_orcaflex_registry.ps1

# dummy entry for firefox. works when i run this as an admin in powershell. 

# reg add HKEY_LOCAL_MACHINE\SOFTWARE\Mozilla\Firefox\TaskBarID /v siva-dummy /t REG_MULTI_SZ  /d dobbey

# hex values
# 10 - 0x0000000a
# 11 - 0x0000000b
# 12 - 0000000c

reg add HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Orcina\OrcaFlex /v DefaultThreadCount /t REG_DWORD /d 0000000a /f
