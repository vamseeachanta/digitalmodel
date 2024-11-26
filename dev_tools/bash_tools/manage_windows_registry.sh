#!/bin/bash

# Source common utilities
source "$(dirname "$0")/common.sh"

# Constants
VALID_SOFTWARE=("orcaflex" "openfoam")
VALID_COMMANDS=("list" "add" "remove")
ORCAFLEX_REG_PATH="HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Orcina\OrcaFlex"

show_help() {
    cat << HELP
Usage: $(basename "$0") <software> <command> [options]

Software:
    orcaflex        Manage OrcaFlex registry entries
    openfoam        Manage OpenFOAM registry entries (not implemented)

Commands for orcaflex:
    list            List all registry entries
    add             Add registry entry (requires --path, --key, --data)
    remove          Remove registry entry (requires --path, --key)

Options:
    --path          Registry path
    --key           Registry key name
    --data          Registry data (required for add)
    --help          Show this help message

Examples:
    $(basename "$0") orcaflex list
    $(basename "$0") orcaflex add --path "$ORCAFLEX_REG_PATH" --key "DefaultThreadCount" --data "dword:0000000c"
    $(basename "$0") orcaflex remove --path "$ORCAFLEX_REG_PATH" --key "DefaultThreadCount"
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
    local path=$1
    log_message "normal" "Listing registry entries for: $path"
    reg query "$path" || log_message "red" "Failed to query registry"
}

add_registry() {
    local path=$1
    local key=$2
    local data=$3
    
    if [[ -z "$path" || -z "$key" || -z "$data" ]]; then
        log_message "red" "Error: Missing required parameters for add command"
        show_help
    fi
    
    log_message "normal" "Adding registry entry: $path\\$key = $data"
    reg add "$path" /v "$key" /d "$data" /f || log_message "red" "Failed to add registry entry"
}

remove_registry() {
    local path=$1
    local key=$2
    
    if [[ -z "$path" || -z "$key" ]]; then
        log_message "red" "Error: Missing required parameters for remove command"
        show_help
    }
    
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
        list_registry "${PATH_ARG:-$ORCAFLEX_REG_PATH}"
        ;;
    add)
        add_registry "$PATH_ARG" "$KEY_ARG" "$DATA_ARG"
        ;;
    remove)
        remove_registry "$PATH_ARG" "$KEY_ARG"
        ;;
esac

log_message "green" "Operation completed successfully"
