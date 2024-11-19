#!/bin/bash

# have to hard code this into each script
# assumes git was used to clone the repo
project_root=$(git rev-parse --show-toplevel)
cd "$project_root"

# load common.sh 
source dev_tools/bash_tools/common.sh

# Function to clean Python cache
clean_python_cache() {
    log_message "${GREEN}Starting Python cache cleanup...${NC}"
    
    # Find and remove all __pycache__ directories
    log_message "Removing __pycache__ directories..."
    find . -type d -name "__pycache__" -exec rm -rf {} +
    
    # Find and remove all .pyc files
    log_message "Removing .pyc files..."
    find . -type f -name "*.pyc" -delete
    
    # Find and remove all .pyo files
    log_message "Removing .pyo files..."
    find . -type f -name "*.pyo" -delete
    
    # Find and remove all .pyd files
    log_message "Removing .pyd files..."
    find . -type f -name "*.pyd" -delete
    
    log_message "${GREEN}Cache cleanup completed successfully!${NC}"
}

# Error handling
if ! clean_python_cache; then
    log_message "${RED}Error: Cache cleanup failed${NC}"
    exit 1
fi

exit 0