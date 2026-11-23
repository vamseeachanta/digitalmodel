# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""The agent pack must distinguish offline policy from deferred native work."""

from pathlib import Path

import pytest


PACK = (Path(__file__).resolve().parents[3]
        / "src/digitalmodel/solvers/calculix/docs/agent-pack.md")


@pytest.mark.parametrize("label, required", [
    ("`nodes`, `elements`", ("default", "Gmsh B32", "shape/range")),
    ("S09", (
        "default", "Gmsh B32",
        "test_invalid_gmsh_rows_leave_destination_and_state_unchanged",
        "test_default_mode_keeps_existing_id_coercion", "integer-valued floats",
        "no detection check exists for default-mode fractional truncation",
    )),
    ("Beam section", (
        "CIRC: (diameter,)", "PIPE: (outer_r, thickness)",
        "CIRC requires B32", "PIPE requires B32R", "case-insensitive",
    )),
    ("Beam compatibility/connectivity", (
        "validated before writing", "v2.21", "2.22", "inference",
        "beam_node_order", "calculix", "gmsh", "B32 only",
    )),
    ("Native circular/quadratic coverage", (
        "CIRC/B32", "PIPE rejection", "no native B32",
    )),
    ("Beam source/section qualification", (
        "Compatibility validation implemented", "explicit Gmsh",
        "B32R support", "remain deferred",
    )),
])
def test_agent_pack_beam_contract_is_current(label, required):
    rows = [line for line in PACK.read_text(encoding="utf-8").splitlines()
            if line.startswith(f"| {label} |")]
    assert len(rows) == 1, f"Missing or duplicate agent-pack row: {label}"
    for phrase in required:
        assert phrase in rows[0], f"{label} omits {phrase!r}"
