#!/bin/bash
# Run file_preparation OrcaFlex analysis

echo "Running file_preparation analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/file_preparation_config.yml \
    --parallel 4

echo "Analysis complete!"
