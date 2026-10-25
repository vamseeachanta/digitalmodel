"""Executable synthetic example; no native solver or physical reference data."""

import json
from pathlib import Path

import pytest

from digitalmodel.solvers.smoke.probes import check_orcaflex


@pytest.mark.parametrize("case_name", ["minimum_history", "zero_tension", "short_readback"])
def test_worked_smoke_example(tmp_path, fake_api, case_name):
    """Independent hand calculations, not results from the implementation:

    minimum_history: static=10; [0, 2] has count=2, min=0, max=2.
    zero_tension: static=0; [0, 0] has count=2, min=max=0, all finite.
    short_readback: solve count=2, reload count=1; 1 < 2 rejects readback.
    The two-sample minimum and one-thread requirement are smoke criteria from
    the bounded plan dated 2026-09-10, section "Bounded design",
    docs/plans/2026-09-10-issue-2082-orcaflex-smoke.html. These synthetic tensions
    test the reporting contract and do not establish engineering adequacy.
    """
    root = Path(__file__).resolve().parents[3]
    example = json.loads(
        (root / "examples/solver_smoke/finite_readback.json").read_text(encoding="utf-8")
    )
    case = example["cases"][case_name]
    fake_api.static = case["static_tension_kN"]
    fake_api.history = case["dynamic_tension_kN"]
    fake_api.reader_history = case["reloaded_dynamic_tension_kN"]

    result = check_orcaflex(tmp_path)

    for key, expected in case["expected"].items():
        assert result[key] == expected, key
    assert all(call["threadCount"] == 1 for call in fake_api.constructors)
    json.dumps(result, allow_nan=False)
