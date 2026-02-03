#!/bin/bash
# Run mooring tension iteration tests with updated folder structure
# Usage: ./run_test.sh

echo "=========================================="
echo "Mooring Tension Iteration Test Runner"
echo "=========================================="

# Set paths
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
CONFIG_DIR="$SCRIPT_DIR/config"
RUN_FILES_DIR="$BASE_DIR/run_files"
OUTPUT_DIR="$BASE_DIR/output"

# Check if virtual environment exists
VENV_PATH="/d/github/digitalmodel/.venv/Scripts/python"
if [ ! -f "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH"
    echo "Please create the virtual environment first"
    exit 1
fi

echo "Configuration directory: $CONFIG_DIR"
echo "Run files directory: $RUN_FILES_DIR"
echo "Output directory: $OUTPUT_DIR"
echo ""

# Step 1: Run tension analysis
echo "Step 1: Running tension analysis..."
$VENV_PATH -m digitalmodel "$CONFIG_DIR/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml"
$VENV_PATH -m digitalmodel "$CONFIG_DIR/dm_ofx_anal_mooring_fsts_l015_125km3_sb.yml"

# Step 2: Generate .sim files from .dat files
echo ""
echo "Step 2: Generating .sim files..."
$VENV_PATH "$SCRIPT_DIR/run_models_to_sim.py" \
    dat=true \
    input_directory="$RUN_FILES_DIR/dat/" \
    output_directory="$RUN_FILES_DIR/sim/"

# Step 3: Post-process results
echo ""
echo "Step 3: Post-processing results..."
$VENV_PATH -m digitalmodel "$CONFIG_DIR/dm_ofx_post_fsts_lngc.yml" --workers 30

# Step 4: Collate results (if assetutilities is available)
if [ -f "/d/github/assetutilities/.venv/Scripts/python" ]; then
    echo ""
    echo "Step 4: Collating results..."
    /d/github/assetutilities/.venv/Scripts/python -m assetutilities "$CONFIG_DIR/au_collate.yml"
else
    echo ""
    echo "Step 4: Skipping collation (assetutilities not found)"
fi

# Step 5: Generate visualizations
echo ""
echo "Step 5: Generating visualizations..."
$VENV_PATH -m digitalmodel "$CONFIG_DIR/viz.yml" \
    "{'meta': {'label': 'viz_fsts_'}, 'file_management': {'input_directory': '$RUN_FILES_DIR/sim/', 'output_directory': '$OUTPUT_DIR/visual', 'filename': {'pattern': 'fsts_*_vessel_statics_6dof'}}}" \
    --workers 30

# Step 6: Generate plots (if visualization module is available)
echo ""
echo "Step 6: Generating mooring plots..."
$VENV_PATH "$SCRIPT_DIR/generate_mooring_plots.py"

echo ""
echo "=========================================="
echo "Test execution complete!"
echo "Check output directory: $OUTPUT_DIR"
echo "=========================================="