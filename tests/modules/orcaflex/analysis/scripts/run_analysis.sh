#!/bin/bash
# Run analysis OrcaFlex analysis

echo "Running analysis analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/analysis_config.yml \
    --parallel 4

echo "Analysis complete!"
