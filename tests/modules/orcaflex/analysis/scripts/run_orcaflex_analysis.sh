#!/bin/bash
# Run orcaflex_analysis OrcaFlex analysis

echo "Running orcaflex_analysis analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/orcaflex_analysis_config.yml \
    --parallel 4

echo "Analysis complete!"
