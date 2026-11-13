#!/usr/bin/env bash
# Validate only staged spec inputs; generation and solver execution are excluded.
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
cd -- "$(git rev-parse --show-toplevel)"
staged=$(mktemp)
trap 'rm -f -- "$staged"' EXIT

# Disable rename detection so a renamed spec is validated as an added input.
git diff --cached --name-only --diff-filter=ACM --no-renames -z -- \
    ':(glob)**/spec.yml' ':(glob)**/*.spec.yml' > "$staged"
if [[ ! -s "$staged" ]]; then
    echo "OK: 0 specs validated"
    exit 0
fi

# Include the solver package's eager OpenFOAM imports, without project extras.
uv run --no-project --with 'pydantic>=2.7,<3' --with 'PyYAML>=6,<7' \
    --with 'numpy>=1.24,<2' --with 'scipy>=1.10,<2' --with 'loguru>=0.7,<1' \
    python "$script_dir/check_spec_validity.py" < "$staged"
