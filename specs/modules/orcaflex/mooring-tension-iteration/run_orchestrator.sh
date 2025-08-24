#!/bin/bash
# Mooring Tension Iteration Orchestrator - Unix/Linux Shell Wrapper
# Runs the orchestrator from the go-by directory

echo "============================================================"
echo "MOORING TENSION ITERATION ORCHESTRATOR"
echo "============================================================"
echo ""

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Navigate to repository root for uv
cd /d/github/digitalmodel

# Run orchestrator using uv environment
uv run python specs/modules/orcaflex/mooring-tension-iteration/orchestrator.py --working-dir specs/modules/orcaflex/mooring-tension-iteration/go-by "$@"

# Check exit code
if [ $? -eq 0 ]; then
    echo ""
    echo "============================================================"
    echo "ORCHESTRATOR COMPLETED SUCCESSFULLY"
    echo "============================================================"
else
    echo ""
    echo "============================================================"
    echo "ORCHESTRATOR FAILED OR DID NOT CONVERGE"
    echo "============================================================"
fi