#!/bin/bash
# Run core OrcaFlex analysis

echo "Running core analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/core_config.yml \
    --parallel 4

echo "Analysis complete!"
