#!/usr/bin/env bash
# Local pull-request gate: thin entry point for scripts/ci/local_gate.py.
#
#   scripts/ci/local_gate.sh --pr <number> --sha <40-char head> --work-root <dir>
#
# Run it from a trusted checkout of the default branch. See docs/ci/local-gate.md.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec python3 -I "$here/local_gate.py" "$@"
