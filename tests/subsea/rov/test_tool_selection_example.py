# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Execute the synthetic example and check independently calculated outcomes.

Input citation: examples/domains/subsea/rov_tool_selection.py, synthetic input
basis. Expected selection uses 20 >= 20 N m, 5 >= 5 turns, 19 < 20 N m and
unknown-rating rejection. Geometry is compared axis by axis: 0.4 > 0.3 m
and 0.7 > 0.6 m fail independently. Pythagoras gives sqrt(3**2 + 4**2) = 5 m;
nextafter tests the adjacent representable limits without an arbitrary tolerance.
"""

import json
import runpy
from math import inf, nextafter
from pathlib import Path

import pytest


EXAMPLE = (
    # Three containing directories: rov, subsea and tests.
    Path(__file__).resolve().parents[3]
    / "examples/domains/subsea/rov_tool_selection.py"
)


@pytest.fixture
def example(capsys, monkeypatch, tmp_path):
    """Execute the entry point from an empty directory, without catalogue files."""
    monkeypatch.chdir(tmp_path)
    namespace = runpy.run_path(str(EXAMPLE), run_name="__main__")
    result = json.loads(capsys.readouterr().out)
    assert list(tmp_path.iterdir()) == []
    return namespace, result


def test_capacity_selection_and_interface_demands(example):
    _, result = example
    assert [tool["name"] for tool in result["capacity_matches"]] == [
        "synthetic-fit",
        "synthetic-wide",
        "synthetic-family",
        "synthetic-class",
        "synthetic-approach",
    ]
    assert result["capacity_rejections"] == ["synthetic-low", "synthetic-unknown"]
    demands = {tool["name"]: tool["interface"] for tool in result["capacity_matches"]}
    assert demands["synthetic-fit"] == {
        "kind": "torque",
        "class_id": "1",
        "envelope_m": [0.2, 0.3, 0.4],
        "required_approach_m": 0.5,
    }
    assert demands["synthetic-family"]["kind"] == "hot_stab"
    assert demands["synthetic-class"]["class_id"] == "2"


def test_capacity_does_not_imply_compatible_geometry(example):
    _, result = example
    assert result["geometry"] == {
        "synthetic-fit": {"compatible": True, "reasons": []},
        "synthetic-wide": {"compatible": False, "reasons": ["envelope"]},
        "synthetic-family": {"compatible": False, "reasons": ["kind"]},
        "synthetic-class": {"compatible": False, "reasons": ["class"]},
        "synthetic-approach": {"compatible": False, "reasons": ["approach"]},
    }


@pytest.mark.parametrize(
    "reach_m,compatible,reasons",
    [
        (nextafter(5.0, -inf), False, ["reach"]),
        (5.0, True, []),
        (nextafter(5.0, inf), True, []),
    ],
)
def test_exact_reach_boundary_preserves_capacity_selection(
    example, reach_m, compatible, reasons
):
    namespace, at_boundary = example
    result = json.loads(json.dumps(namespace["select_and_screen"](reach_m)))
    assert result["capacity_matches"] == at_boundary["capacity_matches"]
    assert result["capacity_rejections"] == at_boundary["capacity_rejections"]
    assert result["geometry"]["synthetic-fit"] == {
        "compatible": compatible,
        "reasons": reasons,
    }
    if not compatible:
        # Envelope and reach remain independent and both failures are retained.
        assert result["geometry"]["synthetic-wide"]["reasons"] == ["envelope", "reach"]
        assert {
            reason
            for check in result["geometry"].values()
            for reason in check["reasons"]
        } == {"kind", "class", "envelope", "approach", "reach"}
