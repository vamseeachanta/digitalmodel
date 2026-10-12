"""Gate evaluation: PASS / FAIL / NOT_EVALUATED with signed deviation."""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.global_model.qualification import evaluate_gate


def test_pass_within_tolerance():
    r = evaluate_gate("G1", demand=100.4, reference=100.0, tolerance=0.005)
    assert r["status"] == "PASS"
    assert r["deviation"] == pytest.approx(0.004)


def test_fail_outside_tolerance_keeps_sign():
    r = evaluate_gate("G2", demand=98.0, reference=100.0, tolerance=0.01)
    assert r["status"] == "FAIL"
    assert r["deviation"] == pytest.approx(-0.02)


def test_boundary_is_pass():
    assert evaluate_gate("G", demand=105.0, reference=100.0, tolerance=0.05)["status"] == "PASS"


@pytest.mark.parametrize("demand,reference", [(None, 1.0), (1.0, None), (math.nan, 1.0), (1.0, 0.0)])
def test_missing_or_degenerate_values_are_not_evaluated(demand, reference):
    r = evaluate_gate("G", demand=demand, reference=reference, tolerance=0.01)
    assert r["status"] == "NOT_EVALUATED"
    assert r["deviation"] is None
