#!/bin/bash
# Run unresolved OrcaFlex analysis

echo "Running unresolved analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/unresolved_config.yml \
    --parallel 4

echo "Analysis complete!"
