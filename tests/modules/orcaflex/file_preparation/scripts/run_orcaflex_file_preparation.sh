#!/bin/bash
# Run orcaflex_file_preparation OrcaFlex analysis

echo "Running orcaflex_file_preparation analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/orcaflex_file_preparation_config.yml \
    --parallel 4

echo "Analysis complete!"
