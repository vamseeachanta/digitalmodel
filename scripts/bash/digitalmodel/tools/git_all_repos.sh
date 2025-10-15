#! /bin/bash

# Directory containing GitHub repositories
# ensure this path is correct, manually. 
github_dir="/k/github"

# rel path top bash_tools dir, daily_routine_script
bash_tools_home="dev_tools/bash_tools"
daily_routine_script_rel_path="${bash_tools_home}/daily_routine.sh"

# Define ANSI color codes as static variables
readonly RED="\033[31m"
readonly GREEN="\033[32m"
readonly YELLOW="\033[33m"
readonly NORMAL="\033[37m"  # Normal/White Color
readonly RESET="\033[0m"

# Print a message with timestamp
log_message() {
    local color=$1
    local msg=$2
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    case $color in
        "red") echo -e "${RED}$msg${RESET}" ;;
        "green") echo -e "${GREEN}$msg${RESET}" ;;
        "yellow") echo -e "${YELLOW}$msg${RESET}" ;;
        "normal") echo -e "${NC}$msg${RESET}" ;;
    esac
    echo "[${timestamp}] ${msg}" # >> "${LOG_FILE}"
}

cd ${github_dir}
log_message "normal" "Starting repository check-in routine process in $(pwd)..."

# Iterate through all directories in the GitHub folder
for dir in "$github_dir"/*/ ; do
    if [ -d "$dir" ]; then

        log_message "normal" "Checking for changes in folder: $dir"
        cd "$dir"

        # Check if there are any changes
        if [ -n "$(git status --porcelain)" ]; then
            log_message "yellow" "Changes detected in $(basename "$dir")"

            # Execute daily routine script if it exists
            daily_routine_script="${dir}/${daily_routine_script_rel_path}"
            if [ -f "$daily_routine_script" ]; then
                log_message "green" "Running daily routine script in $(basename "$dir") ..."
                bash "$daily_routine_script"
                log_message "green" "Daily routine completed in $(basename "$dir") ..."
            else
                log_message "red" "Daily routine script not found: $daily_routine_script"
            fi
        else
            log_message "green" "No changes detected in $(basename "$dir") ..."
        fi
    fi
done

# Return to original directory
cd "$github_dir"
log_message "green" "Completed daily_routine for all repositories"
