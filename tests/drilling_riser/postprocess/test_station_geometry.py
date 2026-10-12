"""Section-geometry and near-End-B regressions following PR 2292."""

import pytest

from digitalmodel.drilling_riser.postprocess.evaluate import evaluate_case
from digitalmodel.drilling_riser.postprocess.station_geometry import section_lengths
from digitalmodel.drilling_riser.postprocess.stress_range import classify_stations, coupling_positions
from .test_w510_significant_range import ROW, _doc


INVALID_SECTIONS = [[], [0.0, 20.0], [-1.0, 21.0], [float("nan"), 20.0],
                    [float("inf")], [float("-inf"), 20.0], ["bad", 20.0], [None, 20.0],
                    [1e308, 1e308], [20.0, 1e-20], None, 20.0, "55", b"55", {10.0: 20.0},
                    {10.0, 20.0}, [True, 20.0], ["10", 20.0], [10**400]]


@pytest.mark.parametrize("sections", INVALID_SECTIONS)
def test_coupling_positions_refuses_invalid_section_geometry(sections):
    with pytest.raises(ValueError, match="sections_m"):
        coupling_positions(sections)


@pytest.mark.parametrize("sections", INVALID_SECTIONS)
def test_classification_refuses_invalid_section_geometry(sections):
    with pytest.raises(ValueError, match="sections_m"):
        classify_stations([10.5, 19.5], sections_m=sections, exclude_below_m=10.0)


@pytest.mark.parametrize("sections", INVALID_SECTIONS)
@pytest.mark.parametrize("wave_kind", ["irregular", "regular"])
def test_invalid_section_geometry_cannot_reach_a_cr10_verdict(sections, wave_kind):
    ctx = {"stress_line": "Riser", "wave_kind": wave_kind,
           "cr10_stations": {"sections_m": sections, "exclude_below_m": 10.0}}
    result = evaluate_case({None: _doc([10.5, 19.5], sig=[1e3, 1e3])}, ROW, ctx)
    assert result.status == "NOT_EVALUATED"
    assert "sections_m" in result.reason


@pytest.mark.parametrize("last_section", [0.00005, 0.0000005])
@pytest.mark.parametrize("explicit_end", [False, True])
def test_interior_coupling_near_end_b_still_requires_its_upper_side(last_section, explicit_end):
    st = {"sections_m": [10.0, 10.0, last_section], "exclude_below_m": 10.0,
          "exclude_above_m": 20.0 + last_section if explicit_end else None, "coupling_tol_m": 1.0}
    # 20 m is an interior boundary; the tiny last section does not make it End B.
    with pytest.raises(ValueError, match="one side"):
        classify_stations([10.5, 19.5], **st)
    ctx = {"stress_line": "Riser", "wave_kind": "irregular", "cr10_stations": st}
    result = evaluate_case({None: _doc([10.5, 19.5], sig=[1e3, 1e3])}, ROW, ctx)
    assert result.status == "NOT_EVALUATED"
    assert "one side" in result.reason


def test_near_end_b_with_a_sample_on_the_coupling_remains_valid():
    kinds = classify_stations([10.5, 19.5, 20.00005], sections_m=[10.0, 10.0, 0.00005],
                              exclude_below_m=10.0, coupling_tol_m=1.0)
    assert kinds == ["coupling", "body", "coupling"]


def test_rounded_end_b_exclusion_does_not_exempt_an_interior_coupling():
    with pytest.raises(ValueError, match="one side"):
        classify_stations([10.5, 19.5], sections_m=[10.0, 10.0, 0.00005],
                          exclude_below_m=10.0, exclude_above_m=20.000049, coupling_tol_m=1.0)


def test_explicit_exclusion_boundary_still_exempts_the_exterior_side():
    kinds = classify_stations([10.5, 19.5], sections_m=[10.0, 10.0, 0.00005],
                              exclude_below_m=10.0, exclude_above_m=20.0, coupling_tol_m=1.0)
    assert kinds == ["coupling", "coupling"]


def test_end_b_requires_a_nearby_inward_sample():
    with pytest.raises(ValueError, match="one side"):
        classify_stations([10.5, 15.0], sections_m=[10.0, 10.0],
                          exclude_below_m=10.0, coupling_tol_m=1.0)


@pytest.mark.parametrize("wave_kind, status", [("irregular", "PASS"), ("regular", "SCREENING")])
def test_valid_geometry_reaches_the_expected_cr10_status(wave_kind, status):
    ctx = {"stress_line": "Riser", "wave_kind": wave_kind,
           "cr10_stations": {"sections_m": [10.0, 10.0], "exclude_below_m": 10.0}}
    result = evaluate_case({None: _doc([10.5, 19.5], sig=[1e3, 1e3])}, ROW, ctx)
    assert result.status == status


@pytest.mark.parametrize("wave_kind", ["irregular", "regular"])
def test_missing_sections_m_is_not_evaluated(wave_kind):
    ctx = {"stress_line": "Riser", "wave_kind": wave_kind,
           "cr10_stations": {"exclude_below_m": 10.0}}
    result = evaluate_case({None: _doc([10.5, 19.5], sig=[1e3, 1e3])}, ROW, ctx)
    assert result.status == "NOT_EVALUATED"
    assert "sections_m" in result.reason


@pytest.mark.parametrize("factory", [list, tuple, iter])
def test_valid_ordered_geometry_preserves_boundaries(factory):
    assert section_lengths(factory([10, 10, 0.00005])) == [10.0, 10.0, 0.00005]
    assert coupling_positions(factory([10, 10, 0.00005])) == pytest.approx([10.0, 20.0, 20.00005])


def test_invalid_geometry_in_a_seeded_irregular_case_is_not_evaluated():
    ctx = {"stress_line": "Riser", "wave_kind": "irregular",
           "cr10_stations": {"sections_m": [float("nan"), 20.0], "exclude_below_m": 10.0}}
    doc = _doc([10.5, 19.5], sig=[1e3, 1e3])
    result = evaluate_case({1: doc, 2: doc}, ROW, ctx, seeds_expected=2)
    assert result.status == "NOT_EVALUATED"
    assert "sections_m" in result.reason


@pytest.mark.parametrize("exclude_above", [20.00002, 20.000025])
def test_explicit_boundary_nearest_the_interior_coupling_exempts_its_upper_side(exclude_above):
    kinds = classify_stations([10.5, 19.5], sections_m=[10.0, 10.0, 0.00005],
                              exclude_below_m=10.0, exclude_above_m=exclude_above, coupling_tol_m=1.0)
    assert kinds == ["coupling", "coupling"]


@pytest.mark.parametrize("exclude_above", [20.00003, 20.000029])
def test_only_the_selected_exclusion_boundary_is_exempt_near_end_b(exclude_above):
    with pytest.raises(ValueError, match="one side"):
        classify_stations([10.5, 19.5], sections_m=[10.0, 10.0, 0.00003, 0.00003],
                          exclude_below_m=10.0, exclude_above_m=exclude_above, coupling_tol_m=1.0)


def test_couplings_beyond_the_selected_boundary_do_not_require_samples():
    kinds = classify_stations([10.5, 19.5], sections_m=[10.0, 10.0, 0.00003, 0.00003],
                              exclude_below_m=10.0, exclude_above_m=20.0, coupling_tol_m=1.0)
    assert kinds == ["coupling", "coupling"]
