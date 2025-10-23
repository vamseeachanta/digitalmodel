#!/bin/bash
# ABOUTME: Bash wrapper for OrcaFlex modular YAML validation
# ABOUTME: Executes validation with proper error handling and reporting

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"

# Default parameters
CONFIG_FILES="${1:-calm_buoy_base.yml discretised_calm_buoy_base.yml}"
GENERATE_REPORT="${2:-true}"

echo "========================================================================"
echo "OrcaFlex Modular YML Validation"
echo "========================================================================"
echo ""
echo "Base Directory: $BASE_DIR"
echo "Configuration Files: $CONFIG_FILES"
echo ""

# Run validation with Python script
if [ "$GENERATE_REPORT" = "true" ]; then
    python "$SCRIPT_DIR/validate_modules.py" \
        --base-dir "$BASE_DIR" \
        --config $CONFIG_FILES \
        --report
else
    python "$SCRIPT_DIR/validate_modules.py" \
        --base-dir "$BASE_DIR" \
        --config $CONFIG_FILES
fi

EXIT_CODE=$?

echo ""
if [ $EXIT_CODE -eq 0 ]; then
    echo "✅ Validation completed successfully!"
    echo "Report: $BASE_DIR/output/VALIDATION_STATUS.md"
else
    echo "❌ Validation failed! Fix errors before proceeding."
    exit 1
fi
