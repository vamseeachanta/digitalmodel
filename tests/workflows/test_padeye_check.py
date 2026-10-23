# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent calculations for the centred, straight-edge plate idealisation.

All dimensions, loads and comparison limits below are synthetic test inputs,
defined here rather than taken from a standard. Mechanics basis: OpenStax,
University Physics Volume 1 (2016), section 12.3, force divided by resisting
area. The two shear planes and the weld throat area are geometric derivations.
No synthetic comparison limit establishes ABS acceptance.
"""

from dataclasses import replace
import math

import pytest

from digitalmodel.lifting_lug.padeye_check import (
    Criterion,
    DirectWeld,
    LoadCase,
    PadeyePlate,
    ScreeningCriteria,
    Source,
    Status,
    check_padeye,
)


SYNTHETIC = Source("Synthetic hand calculation", "2026", "test module docstring")
PLATE = PadeyePlate(20, 160, 60, 50, 100)
LOAD = LoadCase(100, 1.2, 1.5, SYNTHETIC)
WELD = DirectWeld(8, 300)


def test_independent_plate_and_weld_demands():
    """P=100*1.2*1.5=180 kN=180000 N.

    Bearing A=50*20=1000 mm2 -> 180 MPa; net A=(160-60)*20=2000
    mm2 -> 90 MPa; shear A=2*(100-60/2)*20=2800 mm2 -> 450/7 MPa.
    Weld A=8*300=2400 mm2 -> 75 MPa. F/A: OpenStax (2016), 12.3.
    """
    result = check_padeye(PLATE, LOAD, weld=WELD)
    assert result.design_load_kn == pytest.approx(180)
    assert [c.area_mm2 for c in result.checks] == [1000, 2000, 2800, 2400]
    assert [c.demand_mpa for c in result.checks] == pytest.approx(
        [180, 90, 450 / 7, 75]
    )
    assert all(c.status is Status.NOT_EVALUATED for c in result.checks)
    assert all(c.utilization is None for c in result.checks)
    assert result.screening_status is Status.NOT_EVALUATED
    assert result.abs_acceptance is Status.NOT_EVALUATED


def test_supplied_criteria_and_effective_throat_sizing():
    """Synthetic limits give U=180/200, 90/100, (450/7)/100, 75/60.

    Required throat from F/(L*limit)=180000/(300*60)=10 mm. This is
    an independent rearrangement of F/A, OpenStax (2016), section 12.3.
    """
    criteria = ScreeningCriteria(
        Criterion(200, SYNTHETIC),
        Criterion(100, SYNTHETIC),
        Criterion(100, SYNTHETIC),
        Criterion(60, SYNTHETIC),
    )
    result = check_padeye(PLATE, LOAD, criteria, WELD)
    assert [c.utilization for c in result.checks] == pytest.approx(
        [0.9, 0.9, 9 / 14, 1.25]
    )
    assert [c.status for c in result.checks] == [Status.PASS] * 3 + [Status.FAIL]
    assert result.checks[-1].required_throat_mm == pytest.approx(10)
    assert result.checks[-1].criterion.source == SYNTHETIC
    assert result.screening_status is Status.FAIL
    assert result.abs_acceptance is Status.NOT_EVALUATED


def test_equality_is_pass_for_supplied_criterion_only():
    """The comparison definition demand <= supplied limit includes equality."""
    result = check_padeye(
        PLATE, LOAD, ScreeningCriteria(bearing=Criterion(180, SYNTHETIC))
    )
    assert result.checks[0].status is Status.PASS
    assert result.checks[0].utilization == 1
    assert result.screening_status is Status.NOT_EVALUATED
    assert result.checks[-1].demand_mpa is None
    assert result.checks[-1].required_throat_mm is None


def test_failure_is_retained_when_other_checks_are_unevaluated():
    """A synthetic bearing limit of 90 MPa gives U=180/90=2."""
    result = check_padeye(
        PLATE, LOAD, ScreeningCriteria(bearing=Criterion(90, SYNTHETIC))
    )
    assert result.screening_status is Status.FAIL
    assert result.checks[0].utilization == 2


def test_all_supplied_checks_pass_without_class_acceptance():
    """Synthetic 200 MPa limits exceed the independently computed demands."""
    limit = Criterion(200, SYNTHETIC)
    result = check_padeye(PLATE, LOAD, ScreeningCriteria(*([limit] * 4)), WELD)
    assert result.screening_status is Status.PASS
    assert result.abs_acceptance is Status.NOT_EVALUATED
    assert result.exclusions


def test_zero_load_is_distinct_from_missing_criteria():
    """F=0 gives zero F/A, but supplies no acceptance criterion."""
    result = check_padeye(PLATE, replace(LOAD, static_sling_load_kn=0), weld=WELD)
    assert [c.demand_mpa for c in result.checks] == [0] * 4
    assert result.screening_status is Status.NOT_EVALUATED


def test_width_and_loaded_edge_distance_are_independent():
    """Doubling width from 160 to 320 gives A_net=(320-60)*20=5200.

    Shear planes remain 2*(100-30)*20=2800 mm2, OpenStax 12.3 (2016).
    """
    result = check_padeye(replace(PLATE, width_mm=320), LOAD)
    assert result.checks[1].demand_mpa == pytest.approx(450 / 13)
    assert result.checks[2].demand_mpa == pytest.approx(450 / 7)


@pytest.mark.parametrize(
    "field",
    [
        "thickness_mm",
        "width_mm",
        "hole_diameter_mm",
        "pin_diameter_mm",
        "loaded_edge_distance_mm",
    ],
)
@pytest.mark.parametrize("bad", [0, -1, math.nan, math.inf, -math.inf, True])
def test_invalid_plate_values(field, bad):
    with pytest.raises(ValueError, match=field):
        replace(PLATE, **{field: bad})


@pytest.mark.parametrize(
    "values",
    [
        {"pin_diameter_mm": 61},
        {"width_mm": 60},
        {"loaded_edge_distance_mm": 30},
    ],
)
def test_impossible_geometry(values):
    with pytest.raises(ValueError):
        replace(PLATE, **values)


@pytest.mark.parametrize("field", ["dynamic_amplification", "skew_factor"])
@pytest.mark.parametrize("bad", [0, -1, math.nan, math.inf, True])
def test_invalid_load_factors(field, bad):
    with pytest.raises(ValueError, match=field):
        replace(LOAD, **{field: bad})


@pytest.mark.parametrize("bad", [-1, math.nan, math.inf, True])
def test_invalid_static_load(bad):
    with pytest.raises(ValueError, match="static_sling_load_kn"):
        replace(LOAD, static_sling_load_kn=bad)


@pytest.mark.parametrize("bad", [0, -1, math.nan, math.inf, True])
def test_invalid_criterion_and_weld_dimensions(bad):
    with pytest.raises(ValueError):
        Criterion(bad, SYNTHETIC)
    with pytest.raises(ValueError):
        DirectWeld(bad, 300)
    with pytest.raises(ValueError):
        DirectWeld(8, bad)


@pytest.mark.parametrize("field", ["document", "edition", "locator"])
def test_blank_source_is_rejected(field):
    with pytest.raises(ValueError, match=field):
        replace(SYNTHETIC, **{field: " "})


def test_provenance_is_required_and_typed():
    with pytest.raises(ValueError, match="source"):
        Criterion(200, None)
    with pytest.raises(ValueError, match="source"):
        replace(LOAD, source="uncited")
    with pytest.raises(ValueError, match="Criterion"):
        ScreeningCriteria(bearing=200)


def test_derived_overflow_fails_closed():
    with pytest.raises(ValueError, match="design_load"):
        check_padeye(PLATE, replace(LOAD, static_sling_load_kn=1e308))
    with pytest.raises(ValueError, match="area"):
        check_padeye(replace(PLATE, thickness_mm=1e308), LOAD)
