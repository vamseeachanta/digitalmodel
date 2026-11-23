#!/bin/bash


# shell script to perform daily git operations
repo_root=$(git rev-parse --show-toplevel)
# get to repo root
cd "$repo_root"

repo_name=$(basename $(git rev-parse --show-toplevel))
bash_tools_home="dev_tools/bash_tools"
today=$(date '+%Y%m%d')

# Source common utilities
source "${bash_tools_home}/common.sh"

# Constants
VALID_SOFTWARE=("orcaflex" "openfoam" "firefox" "chrome" "vscode")
VALID_COMMANDS=("list" "add" "remove")

# Registry path mappings
declare -A SOFTWARE_PATHS=(
    ["orcaflex"]="HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Orcina\OrcaFlex"
    ["firefox"]="HKEY_LOCAL_MACHINE\SOFTWARE\Mozilla\Firefox\TaskBarIDs"
    ["chrome"]="HKEY_LOCAL_MACHINE\SOFTWARE\Google\Chrome"
)

show_help() {
    cat << HELP
Usage: $(basename "$0") <software> <command> [options]

Software:
    orcaflex        Manage OrcaFlex registry entries
    firefox         Manage Firefox registry entries
    chrome          Manage Chrome registry entries

Commands:
    list            List all registry entries for specified software
    add             Add registry entry (requires --path, --key, --data)
    remove          Remove registry entry (requires --path, --key)

Options:
    --path          Override default registry path
    --key           Registry key name
    --data          Registry data (required for add)
    --help          Show this help message

Examples:
    $(basename "$0") firefox list
    $(basename "$0") chrome list
    $(basename "$0") orcaflex add --key "DefaultThreadCount" --data "dword:0000000c"
HELP
    exit 1
}

validate_software() {
    local software=$1
    for valid in "${VALID_SOFTWARE[@]}"; do
        if [ "$software" = "$valid" ]; then
            return 0
        fi
    done
    log_message "red" "Error: Invalid software '$software'. Valid options are: ${VALID_SOFTWARE[*]}"
    exit 1
}

validate_command() {
    local cmd=$1
    for valid in "${VALID_COMMANDS[@]}"; do
        if [ "$cmd" = "$valid" ]; then
            return 0
        fi
    done
    log_message "red" "Error: Invalid command '$cmd'. Valid commands are: ${VALID_COMMANDS[*]}"
    exit 1
}

list_registry() {
    local software=$1
    local override_path=$2
    
    local reg_path=${override_path:-${SOFTWARE_PATHS[$software]}}
    
    if [[ -z "$reg_path" ]]; then
        log_message "red" "Error: No registry path defined for $software"
        return 1
    fi
    
    log_message "normal" "Listing registry entries for $software at: $reg_path"
    if ! reg query "$reg_path"; then
        log_message "red" "Failed to query registry for $software"
        return 1
    fi
}

add_registry() {
    local path=$1
    local key=$2
    local data=$3
    
    if [[ -z "$path" || -z "$key" || -z "$data" ]]; then
        log_message "red" "Error: Missing required parameters for add command"
        show_help
    fi
    
    # Convert data format from dword:value to REG_DWORD:value
    if [[ ${data,,} =~ ^dword: ]]; then
        data="REG_${data^^}"
    fi
    
    log_message "normal" "Adding registry entry: $path\\$key = $data"
    
    if ! reg add "$path" /v "$key" /t REG_DWORD /d "${data#*:}" /f; then
        log_message "red" "Failed to add registry entry"
        return 1
    fi
}

remove_registry() {
    local path=$1
    local key=$2
    
    if [[ -z "$path" || -z "$key" ]]; then
        log_message "red" "Error: Missing required parameters for remove command"
        show_help
    fi
    
    log_message "normal" "Removing registry entry: $path\\$key"
    reg delete "$path" /v "$key" /f || log_message "red" "Failed to remove registry entry"
}

# Main script
[[ $# -lt 2 ]] && show_help

SOFTWARE=$1
COMMAND=$2
shift 2

validate_software "$SOFTWARE"
validate_command "$COMMAND"

# Parse remaining arguments
PATH_ARG=""
KEY_ARG=""
DATA_ARG=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --path) PATH_ARG="$2"; shift 2 ;;
        --key) KEY_ARG="$2"; shift 2 ;;
        --data) DATA_ARG="$2"; shift 2 ;;
        --help) show_help ;;
        *) log_message "red" "Unknown option: $1"; show_help ;;
    esac
done

# Execute commands
case $COMMAND in
    list)
        if ! list_registry "$SOFTWARE" "$PATH_ARG"; then
            exit 1
        fi
        ;;
    add)
        if ! add_registry "${PATH_ARG:-${SOFTWARE_PATHS[$SOFTWARE]}}" "$KEY_ARG" "$DATA_ARG"; then
            exit 1
        fi
        ;;
    remove)
        if ! remove_registry "${PATH_ARG:-${SOFTWARE_PATHS[$SOFTWARE]}}" "$KEY_ARG"; then
            exit 1
        fi
        ;;
esac

# Only show success if we get here
log_message "green" "Operation completed successfully"
