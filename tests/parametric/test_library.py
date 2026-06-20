"""Sparse atlas library coverage semantics (#801, epic #794).

Exact-match the categorical case key, interpolate the continuous axes within a
covered case, and escalate anything outside the coverage map.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.parametric.atlas import Axis
from digitalmodel.parametric.library import build_library_atlas
from digitalmodel.parametric.query import _handle_value


def _stub(omega, heading, omega_n, zeta):
    r = omega / omega_n
    t = 1.0 / math.sqrt((1.0 - r**2) ** 2 + (2.0 * zeta * r) ** 2)
    return t * (1.0 - 0.1 * math.sin(math.radians(heading)))


def _library():
    cases = {"fpso-design-draft": (0.40, 0.10), "semisub-operating": (0.26, 0.08)}
    freqs = [0.2, 0.4, 0.6, 0.8, 1.0, 1.2]
    headings = [0.0, 90.0, 180.0]
    records = [
        {"case": c, "frequency_rad_s": f, "heading_deg": h,
         "heave_rao_m_per_m": _stub(f, h, *p)}
        for c, p in cases.items() for f in freqs for h in headings
    ]
    return build_library_atlas(
        basename="diffraction_library",
        key_axis="case",
        key_values=list(cases),
        continuous_axes=[
            Axis(name="frequency_rad_s", scale="linear", grid=freqs),
            Axis(name="heading_deg", scale="linear", grid=headings),
        ],
        records=records,
        response="heave_rao_m_per_m",
        solver={"name": "STUB", "version": "STUB", "licensed": False},
        coverage_note="test stub",
    )


def test_library_provenance_marks_coverage_and_solver():
    atlas = _library()
    assert atlas.provenance["kind"] == "library"
    assert atlas.provenance["coverage"]["covered_cases"] == [
        "fpso-design-draft", "semisub-operating"]
    assert atlas.provenance["solver"]["licensed"] is False


def test_covered_case_interpolates_within():
    atlas = _library()
    # on a grid knot -> exact recorded value
    on_knot = atlas.predict({"case": "fpso-design-draft",
                             "frequency_rad_s": 0.6, "heading_deg": 90.0})
    assert on_knot.in_range
    assert on_knot.value == pytest.approx(_stub(0.6, 90.0, 0.40, 0.10), rel=1e-9)
    # between knots -> interpolates (stays positive and bracketed)
    between = atlas.predict({"case": "fpso-design-draft",
                             "frequency_rad_s": 0.5, "heading_deg": 45.0})
    assert between.in_range and between.value > 0.0


def test_uncovered_case_escalates():
    atlas = _library()
    result = _handle_value(atlas, {"case": "drillship-transit",
                                   "frequency_rad_s": 0.6, "heading_deg": 90.0})
    assert result["in_range"] is False
    assert result["action"] == "escalate"
    assert "drillship-transit" in result["reason"]


def test_out_of_solved_range_escalates():
    atlas = _library()
    pred = atlas.predict({"case": "fpso-design-draft",
                          "frequency_rad_s": 3.0, "heading_deg": 90.0})
    assert pred.in_range is False
    assert "outside" in pred.reason


# -- staleness (#831) --------------------------------------------------------

def test_committed_library_matches_its_expectation():
    from digitalmodel.parametric import refresh

    status = refresh.library_status("diffraction_library")
    assert status["stale"] is False


def test_advancing_the_expectation_makes_a_query_escalate(monkeypatch):
    from digitalmodel.parametric import refresh
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.query import _staleness

    atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, "diffraction_library")
    assert _staleness(atlas) is None  # stub matches the (stub) expectation

    # operator now requires a real OrcaWave run instead of the stub
    monkeypatch.setitem(
        refresh.LIBRARY_EXPECTATIONS, "diffraction_library",
        {**refresh.LIBRARY_EXPECTATIONS["diffraction_library"], "solver_version": "11.0"})

    reason = _staleness(atlas)
    assert reason is not None and "library stale" in reason


def test_missing_case_is_drift():
    from digitalmodel.parametric import refresh
    from digitalmodel.parametric.atlas import Atlas

    atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, "diffraction_library")
    atlas.provenance["coverage"]["covered_cases"] = ["fpso-design-draft"]  # dropped 2
    reasons = refresh.library_drift(atlas)
    assert any("covered cases" in r for r in reasons)


# -- OrcaFlex library (#832) -------------------------------------------------

def test_orcaflex_library_is_current_and_enforces_coverage():
    from digitalmodel.parametric import refresh
    from digitalmodel.parametric.atlas import Atlas
    from digitalmodel.parametric.query import _handle_value

    status = refresh.library_status("orcaflex_library")
    assert status["stale"] is False

    atlas = Atlas.load(refresh.DEFAULT_ATLAS_ROOT, "orcaflex_library")
    assert atlas.provenance["coverage"]["covered_cases"] == [
        "operating-hs2", "storm-hs5", "extreme-hs8"]
    # covered load case interpolates heading within
    covered = _handle_value(atlas, {"case": "storm-hs5", "heading_deg": 60.0})
    assert covered["in_range"] is True
    assert covered["value"] > 0.0
    # an uncovered load case escalates
    uncovered = _handle_value(atlas, {"case": "survival-hs12", "heading_deg": 60.0})
    assert uncovered["in_range"] is False
    assert "survival-hs12" in uncovered["reason"]
