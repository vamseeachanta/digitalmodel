#!/bin/bash
# Convenience script for running mooring comparative analysis on Unix systems

# Check if python is available
if ! command -v python &> /dev/null; then
    echo "Error: Python is not installed or not in PATH"
    exit 1
fi

# Run the analysis module
python -m digitalmodel.orcaflex.analysis "$@"