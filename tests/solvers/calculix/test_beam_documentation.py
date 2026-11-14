"""The agent pack must distinguish offline policy from deferred native work."""

from pathlib import Path

import pytest


PACK = (Path(__file__).resolve().parents[3]
        / "src/digitalmodel/solvers/calculix/docs/agent-pack.md")


@pytest.mark.parametrize("label, required", [
    ("Beam section", (
        "CIRC: (diameter,)", "PIPE: (outer_r, thickness)",
        "CIRC requires B32", "PIPE requires B32R", "case-insensitive",
    )),
    ("Beam compatibility/connectivity", (
        "validated before writing", "v2.21", "2.22", "inference",
        "reordering remains deferred",
    )),
    ("Native circular/quadratic coverage", (
        "CIRC/B32", "PIPE rejection", "no native B32",
    )),
    ("Beam source/section qualification", (
        "Compatibility validation implemented", "connectivity reordering",
        "B32R support", "remain deferred",
    )),
])
def test_agent_pack_beam_contract_is_current(label, required):
    rows = [line for line in PACK.read_text(encoding="utf-8").splitlines()
            if line.startswith(f"| {label} |")]
    assert len(rows) == 1, f"Missing or duplicate agent-pack row: {label}"
    for phrase in required:
        assert phrase in rows[0], f"{label} omits {phrase!r}"
