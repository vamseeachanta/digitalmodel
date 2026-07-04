"""twin B #1374: riser flex-joint response correction (adapter).

Safety-critical invariants: the correction may only ever TIGHTEN a verdict
(``max(corrected, raw)``); it fails closed (ESCALATE) on untrustworthy inputs; the
limits stay the cited getters (correction only in the numerator); von-Mises is
honestly left uncorrected. Most tests are wiki-free (criteria built from the public
standard factors, as the merged envelope/telemetry tests do); the one getter-touching
traceability test skips cleanly when the wiki is absent.
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.drilling_riser.envelope import EnvelopeCriteria
from digitalmodel.drilling_riser.response_correction import (
    VON_MISES_COVERAGE,
    correct_flexjoint_response,
    fit_flexjoint_models,
    flexjoint_utilisation,
)
from digitalmodel.drilling_riser.riser_response import RiserResponse, solve_static_response

_FIXTURE = Path(__file__).parent / "fixtures" / "response_residual_pairs.json"

# public standard factors (API RP 16Q 2/4 deg; API STD 2RD 0.67) — the values the
# cited getters resolve to; used as literals for wiki-free tests, mirroring the
# merged metocean/telemetry tests. The correction never touches these.
_CRITERIA = EnvelopeCriteria(2.0, 4.0, 0.67)


def _fixture():
    return json.loads(_FIXTURE.read_text())


def _models_from_fixture():
    fx = _fixture()
    up = fx["pairs"]["upper"]
    lo = fx["pairs"]["lower"]
    return fit_flexjoint_models(
        measured_upper_deg=[r["measured_deg"] for r in up],
        predicted_upper_deg=[r["predicted_deg"] for r in up],
        measured_lower_deg=[r["measured_deg"] for r in lo],
        predicted_lower_deg=[r["predicted_deg"] for r in lo],
    )


def _response(top_offset_m: float, current_load: float = 0.0) -> RiserResponse:
    # generic synthetic 21-in riser, 1500 m — no real project values. current_load=0
    # keeps the static flex-joint angle (~offset/length) inside the fixture's fitted
    # domain (~0.4-3.0 deg) so the in-domain correction path is exercised; a larger
    # angle would (correctly) escalate via the domain gate.
    return solve_static_response(
        length_m=1500.0, top_offset_m=top_offset_m, tension_n=3.0e6,
        ei_nm2=2.07e11 * 1.0e-4, current_load_n_per_m=current_load,
    )


# -- SAFETY: conservative-only decision (T3) -----------------------------------


def test_correction_can_never_lower_the_verdict():
    """A downward-biased model (asset runs COOLER, scale<1) must NOT lower the
    decision angle or the utilisation — the correction can only tighten. THE key
    safety property."""
    # synthetic downward pairs: measured = 0.7*predicted (scale < 1, in-bounds)
    pred = [0.5, 0.8, 1.1, 1.4, 1.7, 2.0, 2.3, 2.6, 2.9, 3.2]
    meas = [0.7 * p for p in pred]
    models = fit_flexjoint_models(
        measured_upper_deg=meas, predicted_upper_deg=pred,
        measured_lower_deg=meas, predicted_lower_deg=pred,
    )
    assert models.upper.active and models.upper.scale < 1.0  # genuinely downward
    resp = _response(top_offset_m=40.0)  # ~1.53 deg, inside the fitted domain
    cf = correct_flexjoint_response(resp, models)
    # tracking estimate IS lower than physics ...
    assert cf.corrected_static_angle_deg < cf.raw_static_angle_deg
    # ... but the DECISION angle is pinned to raw (never lowered)
    assert cf.decision_static_angle_deg == pytest.approx(cf.raw_static_angle_deg)
    # and the utilisation is not lowered vs raw physics
    uc_decision = flexjoint_utilisation(cf.decision_static_angle_deg, _CRITERIA)
    uc_raw = flexjoint_utilisation(cf.raw_static_angle_deg, _CRITERIA)
    assert uc_decision == pytest.approx(uc_raw)
    assert uc_decision >= uc_raw


# -- tracks reality upward (T5) ------------------------------------------------


def test_correction_raises_verdict_when_asset_runs_hotter():
    """Upward model (measured > predicted): the decision angle and UC rise vs raw —
    the twin flags a case the raw physics under-reports."""
    models = _models_from_fixture()  # fixture scale ~1.15-1.20 > 1
    assert models.upper.active and models.upper.scale > 1.0
    resp = _response(top_offset_m=40.0)  # ~1.53 deg, inside the fitted domain
    cf = correct_flexjoint_response(resp, models)
    assert cf.corrected_static_angle_deg > cf.raw_static_angle_deg
    assert cf.decision_static_angle_deg == pytest.approx(cf.corrected_static_angle_deg)
    uc_decision = flexjoint_utilisation(cf.decision_static_angle_deg, _CRITERIA)
    uc_raw = flexjoint_utilisation(cf.raw_static_angle_deg, _CRITERIA)
    assert uc_decision > uc_raw


# -- escalate on fallback (T4) -------------------------------------------------


def test_adapter_escalates_on_insufficient_data():
    models = fit_flexjoint_models(
        measured_upper_deg=[1.0, 2.0], predicted_upper_deg=[0.9, 1.9],
        measured_lower_deg=[1.0, 2.0], predicted_lower_deg=[0.9, 1.9],
    )
    cf = correct_flexjoint_response(_response(top_offset_m=30.0), models)
    assert cf.escalated and cf.status == "escalate"
    # even escalated, the decision angle is never below raw physics (no silent GO)
    assert cf.decision_static_angle_deg == pytest.approx(cf.raw_static_angle_deg)


# -- like-for-like / magnitude convention + no double-count (T8) ---------------


def test_magnitude_convention_and_no_rao_double_count():
    resp = _response(top_offset_m=30.0)
    cf = correct_flexjoint_response(resp, _models_from_fixture())
    # raw static is the magnitude collapse, matching the envelope
    assert cf.raw_static_angle_deg == pytest.approx(
        max(abs(resp.angle_upper_deg), abs(resp.angle_lower_deg))
    )
    # the RAO term is added by flexjoint_utilisation ONLY in the max-limit numerator;
    # correcting the static angle does not pre-bake dyn into the mean-limit term.
    uc_no_dyn = flexjoint_utilisation(cf.decision_static_angle_deg, _CRITERIA, dyn_angle_deg=0.0)
    uc_with_dyn = flexjoint_utilisation(cf.decision_static_angle_deg, _CRITERIA, dyn_angle_deg=1.0)
    assert uc_with_dyn >= uc_no_dyn  # dyn only ever adds, once


# -- honest partial coverage (T9) ----------------------------------------------


def test_von_mises_marked_uncorrected():
    cf = correct_flexjoint_response(_response(top_offset_m=30.0), _models_from_fixture())
    assert cf.provenance["von_mises"] == VON_MISES_COVERAGE
    assert "physics-only" in cf.provenance["von_mises"]


# -- standards-traceability: limit unchanged by correction (T6, wiki) ----------


def test_cited_limit_is_untouched_by_correction():
    """The cited getter limit is identical whether or not a correction is applied —
    the correction lives only in the numerator. Needs the wiki; skips cleanly if absent."""
    from digitalmodel.citations.schema import CitationResolutionError
    from digitalmodel.drilling_riser.envelope import resolve_envelope_criteria

    try:
        before = resolve_envelope_criteria()
    except CitationResolutionError:
        pytest.skip("llm-wiki not resolvable — getter-backed traceability test")
    cf = correct_flexjoint_response(_response(top_offset_m=30.0), _models_from_fixture())
    _ = flexjoint_utilisation(cf.decision_static_angle_deg, before)
    after = resolve_envelope_criteria()
    assert before.flexjoint_angle_mean_deg == after.flexjoint_angle_mean_deg
    assert before.flexjoint_angle_max_deg == after.flexjoint_angle_max_deg
    # the correction provenance is explicitly non-cited
    assert cf.provenance["flexjoint_angle"]["upper"]["not_cited"] is True


# -- fixture governance (T11) — mirror twin A exactly --------------------------


def test_fixture_provenance_no_credentials_no_identity():
    fx = _fixture()
    prov = fx["provenance"]
    assert prov["source"] and prov["licence"]
    raw = _FIXTURE.read_text().lower()
    for forbidden in ("authorization", "x-api-key", "password", "token", "signature=", "aws"):
        assert forbidden not in raw
    identity_keys = {"vessel_name", "imo", "mmsi", "callsign", "field", "well", "rig"}
    for joint_rows in fx["pairs"].values():
        for rec in joint_rows:
            assert identity_keys.isdisjoint(rec.keys())
    for tok in ("imo", "mmsi", "callsign", "vessel_name"):
        assert f'"{tok}"' not in raw
