#!/bin/bash

# have to hard code this into each script
# assumes this IS a git repo
repo_root=$(git rev-parse --show-toplevel)
# get to repo root
cd "$repo_root"

repo_name=$(basename "${repo_root}")

bash_tools_home="dev_tools/bash_tools"
today=$(date '+%Y%m%d')

# source common utilities
source ${bash_tools_home}/common.sh

# Function to clean Python cache
clean_python_cache() {
    log_message "green" "Starting Python cache cleanup in repo ${repo_name}..."
    
    # Find and remove all __pycache__ directories
    log_message "normal" "Removing __pycache__ directories..."
    find . -type d -name "__pycache__" -exec rm -rf {} +
    
    # Find and remove all .pyc files
    log_message "normal" "Removing .pyc files..."
    find . -type f -name "*.pyc" -delete
    
    # Find and remove all .pyo files
    log_message "normal" "Removing .pyo files..."
    find . -type f -name "*.pyo" -delete
    
    # Find and remove all .pyd files
    log_message "normal" "Removing .pyd files..."
    find . -type f -name "*.pyd" -delete
    
    log_message "green" "Cache cleanup completed successfully in repo ${repo_name}!"
}

# Error handling
if ! clean_python_cache; then
    log_message "red" "Error: Cache cleanup failed in repo ${repo_name}"
    exit 1
fi

exit 0