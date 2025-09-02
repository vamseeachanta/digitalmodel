#!/bin/bash
# Run browser_interface OrcaFlex analysis

echo "Running browser_interface analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/browser_interface_config.yml \
    --parallel 4

echo "Analysis complete!"
