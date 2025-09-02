#!/bin/bash
# Run post_processing OrcaFlex analysis

echo "Running post_processing analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/post_processing_config.yml \
    --parallel 4

echo "Analysis complete!"
