#! /bin/bash
# common shell script utils 

# - [ ] #todo #13_siva_AceEngineer #ae_au have to use this all over the place

# Print colored output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# freq-relative paths 
test_module_home="test/modules"
test_common_home="test/common"

# Print a message with timestamp
log_message() {
    echo -e "[$(date +'%Y-%m-%d %H:%M:%S')] $1"
}

log_message2() {
    local color=$1
    local msg=$2
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    case $color in
        "red") echo -e "\033[31m$msg\033[0m" ;;
        "green") echo -e "\033[32m$msg\033[0m" ;;
        "yellow") echo -e "\033[33m$msg\033[0m" ;;
    esac
    echo "[${timestamp}] ${msg}" # >> "${LOG_FILE}"
}

pause_for_user() {
    read -n 1 -s -r -p "Pausing - press any key to continue... "
    echo -e "\n"
}

go2ProjectRoot() {
    project_root=$(git rev-parse --show-toplevel)
    log_message "inside common.sh: project_root: $project_root"
    cd "$project_root"
}

# use following if any script wants to go to project home
# go2ProjectHome