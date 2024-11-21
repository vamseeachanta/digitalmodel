#! /bin/bash

# Directory containing GitHub repositories
# ensure this path is correct, manually. 
github_dir="/k/github"

# rel path top bash_tools dir, daily_routine_script
bash_tools_home="dev_tools/bash_tools"
daily_routine_script_rel_path="${bash_tools_home}/daily_routine.sh"

# having to hard code this as opposed to sourcing common.sh. 
# since i have no guarantee on common.sh
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

log_message "Starting repository check process..."

# Iterate through all directories in the GitHub folder
for dir in "$github_dir"/*/ ; do
    if [ -d "$dir" ]; then
        log_message2 "green" "Checking for changes in folder: $dir"
        cd "$dir"

        # Check if there are any changes
        if [ -n "$(git status --porcelain)" ]; then
            log_message2 "yellow" "Changes detected in $(basename "$dir")"

            # Execute daily routine script if it exists
            # daily_routine_script="${dir}dev_tools/daily_routine.sh"
            daily_routine_script="${dir}/${daily_routine_script_rel_path}"
            if [ -f "$daily_routine_script" ]; then
                log_message2 "green" "Running daily routine script..."
                bash "$daily_routine_script"
                log_message2 "green" "Daily routine completed"
            else
                log_message2 "red" "Daily routine script not found: $daily_routine_script"
            fi
        else
            log_message2 "green" "No changes detected in $(basename "$dir")"
        fi
    fi
done

# Return to original directory
cd "$script_dir"
log_message2 "green" "Completed daily_routine for all repositories"
