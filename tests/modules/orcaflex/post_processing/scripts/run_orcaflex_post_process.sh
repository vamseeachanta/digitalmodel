#!/bin/bash
# Run orcaflex_post_process OrcaFlex analysis

echo "Running orcaflex_post_process analysis..."
cd "$(dirname "$0")/.."

# Use the universal runner
python -m digitalmodel.modules.orcaflex.universal \
    --input-directory . \
    --config scripts/orcaflex_post_process_config.yml \
    --parallel 4

echo "Analysis complete!"
