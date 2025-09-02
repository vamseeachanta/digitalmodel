#!/bin/bash
# Run universal OrcaFlex analysis

echo "Running universal analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/universal_config.yml \
    --parallel 4

echo "Analysis complete!"
