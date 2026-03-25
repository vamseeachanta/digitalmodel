#! /bin/bash 

# shell script to perform daily git operations
repo_root=$(git rev-parse --show-toplevel)
# get to repo root
cd "$repo_root"

repo_name=$(basename $(git rev-parse --show-toplevel))
bash_tools_home="dev_tools/bash_tools"
today=$(date '+%Y%m%d')


# Source common utilities
source "${bash_tools_home}/common.sh"

log_message "normal" "lets try something" 

# track and run registry management commands 
registry_key_path=HKEY_LOCAL_MACHINE\\SOFTWARE\\Mozilla\\Firefox\\TaskBarIDs
registry_key_name=siva-dummy
registry_key_data=nee-yenkamma

reg add HKEY_LOCAL_MACHINE\SOFTWARE\Mozilla\Firefox\TaskBarID /v siva-dummy /t REG_MULTI_SZ  /d "nee-yenkamma"


log_message "normal" "will execute \n\treg add \"$registry_key_path\" /v \"$registry_key_name\" /t REG_MULTI_SZ /d \"$registry_key_data\"\n"
reg add "$registry_key_path" /v "$registry_key_name" /t REG_MULTI_SZ /d "$registry_key_data"

exit 1 

# track and run registry management commands 
registry_key_path="HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Orcina\OrcaFlex"
registry_key_name="DefaultThreadCount"
registry_key_data="dword:0000000c"

#reg add "$registry_key_path" /v "$registry_key_name" /t REG_DWORD /d "$registry_key_data" /f 
reg add "$registry_key_path" /v "$registry_key_name" /t REG_DWORD /d "$registry_key_data"