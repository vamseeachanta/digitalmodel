#!/usr/bin/env python3
"""Emit a synthetic rule-value file for the Stage 1 CI job (#1961).

Stage 1 has no rule authority, so its CI job runs against values invented here.
These are not protected identifiers and are not shaped after any real one beyond
their broad form: an alphanumeric code that can be followed by an underscore,
and a two-name co-occurrence class.

The value lines carry an exact-line sentinel. Without it a synthetic-rule scan
of the tracked tree finds them, because the scanner enumerates its own tree --
which is the point of the self-coverage property, and which is exactly what
happened in CI before these sentinels existed. Embedding the same values in a
workflow heredoc instead is not an option: JSON has no comment syntax, so those
lines could not carry a sentinel, and the only way to silence them would be a
file-level exemption, which the schema cannot represent.

Usage:  python3 scripts/legal/synthetic_rules.py > "${RUNNER_TEMP}/rules.json"
"""

from __future__ import annotations

import json
import sys

SYNTHETIC = {
    "authority": "synthetic",
    "rules": {
        "PID-A1": {"class": "A", "values": ["zq7731"]},  # protected-identifier-synthetic
        "PID-B1": {
            "class": "B",
            "values": ["orgalpha", "projbeta"],  # protected-identifier-synthetic
        },
    },
}


def main() -> int:
    json.dump(SYNTHETIC, sys.stdout, indent=2, sort_keys=True)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
