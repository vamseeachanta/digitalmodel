#!/bin/bash
# Main iteration script for mooring analysis
# Updated for new folder structure with proper relative path handling

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Change to script directory to ensure relative paths work correctly
cd "$SCRIPT_DIR"

# Step 1: Tension calculation
echo "Starting tension calculations..."
bash dm_pretension_iteration.sh

# Step 2: Batch run all yml files and save sim files
# universal module with new paths
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel.orcaflex.universal \
    pattern="fsts*.yml" \
    input_directory="../run_files/yml" \
    output_directory="../run_files/sim" \
    validate=false \
    max_workers=30

# Step 3: Post-process to get results
echo "Post-processing results..."
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_post_fsts_lngc.yml --max_workers 30
# /d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/dm_ofx_post_fsts_lngc.yml --file_management.filename.pattern "fsts_l015_lwl_180km3_l100_pb_vessel_statics_6dof" --max_workers 30

# Step 4: Collate results into Excel
echo "Collating results..."
/d/github/assetutilities/.venv/Scripts/python -m assetutilities config/au_collate.yml

# Step 5: Generate visualizations
echo "Generating visualizations..."
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel config/viz.yml \
    --meta label=viz_fsts_ \
    --file_management input_directory=../../run_files/sim/ \
    --file_management output_directory=../../output/visuals \
    --file_management filename.pattern="fsts_*_vessel_statics_6dof" \
    --workers 30

echo "All tasks completed!"