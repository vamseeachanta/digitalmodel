"""twin B #1374: the domain-neutral residual spine (no wiki, pure math)."""
from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.residual import (
    ACTIVE,
    Correction,
    ResidualModel,
    is_significant,
    rmse,
    skill_gain,
)

_FIXTURE = Path(__file__).resolve().parents[1] / "drilling_riser" / "fixtures" / "response_residual_pairs.json"


def _fixture():
    return json.loads(_FIXTURE.read_text())


def _pairs(joint):
    rows = _fixture()["pairs"][joint]
    return [r["measured_deg"] for r in rows], [r["predicted_deg"] for r in rows]


# -- fit + skill ---------------------------------------------------------------


def test_fit_recovers_known_scale_bias():
    fx = _fixture()
    measured, predicted = _pairs("upper")
    model = ResidualModel.fit(measured, predicted, lo=0.0, hi=30.0)
    assert model.active and model.status == ACTIVE
    assert model.scale == pytest.approx(fx["true_model"]["upper"]["scale"], abs=0.05)
    assert model.bias == pytest.approx(fx["true_model"]["upper"]["bias"], abs=0.05)


def test_held_out_rmse_reduction_is_significant():
    """Fit on a wide-domain subset; the held-out interior points' RMSE must fall
    by more than the fixture noise floor — not merely > 0 (T2 correctness #5)."""
    fx = _fixture()
    noise = fx["true_model"]["noise_sigma_deg"]
    measured, predicted = _pairs("lower")
    held = [3, 6]  # interior indices; min/max stay in train so held-out is in-domain
    tr_m = [measured[i] for i in range(len(measured)) if i not in held]
    tr_p = [predicted[i] for i in range(len(predicted)) if i not in held]
    ho_m = [measured[i] for i in held]
    ho_p = [predicted[i] for i in held]
    model = ResidualModel.fit(tr_m, tr_p, lo=0.0, hi=30.0)
    assert model.active
    corrected = [model.apply(p).value for p in ho_p]
    rmse_before = rmse(ho_m, ho_p)
    rmse_after = rmse(ho_m, corrected)
    assert skill_gain(rmse_before, rmse_after) > 0
    assert is_significant(rmse_before, rmse_after, noise)


# -- bounds + fail-closed ------------------------------------------------------


def test_fit_out_of_bounds_escalates_and_applies_identity():
    predicted = [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0]
    measured = [5.0 * p for p in predicted]  # scale ~5 >> SCALE_MAX
    model = ResidualModel.fit(measured, predicted, lo=0.0, hi=30.0)
    assert not model.active and model.status.startswith("escalate:fit_out_of_bounds")
    c = model.apply(2.0)
    assert c.escalated and c.value == 2.0  # falls back to raw, never the wild fit


def test_insufficient_pairs_escalates():
    model = ResidualModel.fit([1.0, 2.0, 3.0], [0.9, 1.9, 2.9], lo=0.0, hi=30.0)
    assert not model.active and model.status.startswith("escalate:insufficient_pairs")
    assert model.apply(1.5).escalated


def test_out_of_domain_query_escalates():
    measured, predicted = _pairs("upper")  # domain ~[0.40, 3.00]
    model = ResidualModel.fit(measured, predicted, lo=0.0, hi=30.0)
    assert model.active
    inside = model.apply(1.0)
    assert not inside.escalated
    outside = model.apply(10.0)
    assert outside.escalated and outside.value == 10.0 and "out_of_domain" in outside.status


def test_apply_active_clips_to_output_bounds():
    # scale 1.8 within bounds; the hi clip caps the output
    predicted = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
    measured = [1.8 * p for p in predicted]
    model = ResidualModel.fit(measured, predicted, lo=0.0, hi=10.0)
    assert model.active
    assert model.apply(8.0).value == pytest.approx(10.0)  # 1.8*8=14.4 clipped to hi


# -- transparency / not-a-standard ---------------------------------------------


def test_provenance_marks_data_driven_not_cited():
    measured, predicted = _pairs("upper")
    prov = ResidualModel.fit(measured, predicted, lo=0.0, hi=30.0).provenance()
    assert prov["data_driven"] is True
    assert prov["not_a_standard"] is True
    assert prov["not_cited"] is True
    assert "scale" in prov and "bias" in prov and "status" in prov
    assert "not a" in prov["note"].lower() and "cited" in prov["note"].lower()


def test_correction_is_a_plain_value_not_a_cited_value():
    from digitalmodel.citations.schema import CitedValue

    c = Correction(1.23)
    assert not isinstance(c, CitedValue)
    assert isinstance(c.value, float)


# -- structural wedge guard (governance #4) ------------------------------------


def test_residual_package_imports_no_citations():
    """The spine must never IMPORT the citations layer — the empirical correction
    can never become or masquerade as a cited standard value (T2 governance #4).
    Inspect the import graph (AST), not prose (the docstrings explain the rule)."""
    import ast

    import digitalmodel.residual as pkg

    pkg_dir = Path(pkg.__file__).parent
    for src in pkg_dir.glob("*.py"):
        tree = ast.parse(src.read_text())
        for node in ast.walk(tree):
            names = []
            if isinstance(node, ast.Import):
                names = [a.name for a in node.names]
            elif isinstance(node, ast.ImportFrom):
                names = [node.module or ""]
            for name in names:
                assert "citations" not in name, f"{src.name} imports {name}"


# -- cross-domain reuse (for motion_forecast #1360) ----------------------------


def test_model_is_domain_neutral_on_arbitrary_arrays():
    predicted = [10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 22.0, 24.0]
    measured = [1.3 * p + 2.0 for p in predicted]  # any float channel, not riser
    # a non-riser domain sets its own bounds (bias 2.0 is fine here, not degrees)
    model = ResidualModel.fit(measured, predicted, lo=0.0, hi=1000.0, bias_max=5.0)
    assert model.active
    assert model.scale == pytest.approx(1.3, abs=0.05)
    assert model.apply(15.0).value == pytest.approx(1.3 * 15.0 + 2.0, abs=0.1)
