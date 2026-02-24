#!/bin/bash
# Run full production rainflow analysis
echo "Starting full production rainflow analysis at $(date)"
echo "Processing 2592 files..."
cd /d/github/digitalmodel/specs/modules/signal-analysis/rainflow_with_visualization
python run_rainflow_analysis.py input/rainflow_analysis_config_production.yml
echo "Completed at $(date)"