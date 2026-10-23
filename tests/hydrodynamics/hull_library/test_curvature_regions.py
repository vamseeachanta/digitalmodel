"""Independent weighted-sum controls for longitudinal curvature diagnostics.

Formula source: Geometry-Based Metrics for Early-Stage Hull-Form Producibility
Screening (2026), arXiv:2609.27544v1, equations (3), (4), (39).
All geometry, areas and curvature values below are synthetic test inputs.
"""

from dataclasses import asdict
import json

import numpy as np
import pytest

from digitalmodel.hydrodynamics.hull_library.curvature_regions import (
    summarize_curvature_regions,
)


def summarize(**changes):
    """Five vertices spanning four length units, with prescribed quadrature areas."""
    inputs = dict(
        longitudinal_coordinates=[0, 1, 2, 3, 4],
        gaussian_curvature=[1, -2, 0, 1, -1],
        vertex_area=[1, 2, 2, 2, 1],
        valid=[True] * 5,
        lref=2,
        end_fraction=0.25,
    )
    return summarize_curvature_regions(**(inputs | changes))


def test_hand_integrated_end_contributions_and_boundary_partition():
    """A=8; sum(A_i |K_i|)=1+4+0+2+1=8; Lref^2=4, so I_D=4.

    Cuts are x=1 and x=3. Strict end inequalities put both cut vertices in
    the middle. Lower/middle/upper integrals are 1,6,1; contributions to I_D
    are 4*(1,6,1)/8 = (0.5,3,0.5); intensity shares are (1/8,6/8,1/8).
    """
    result = summarize()
    assert result.status == "EVALUATED"
    assert result.total_area == result.valid_area == 8
    assert result.I_D == 4
    assert result.lower_cut == 1 and result.upper_cut == 3
    assert list(result.regions) == ["lower_end", "middle", "upper_end"]
    for name, area, contribution, share in zip(
        result.regions, [1, 6, 1], [0.5, 3, 0.5], [0.125, 0.75, 0.125]
    ):
        region = result.regions[name]
        assert region.status == "EVALUATED"
        assert region.area == region.valid_area == area
        assert region.I_D_contribution == contribution
        assert region.intensity_share == share
    assert sum(r.I_D_contribution for r in result.regions.values()) == result.I_D


def test_invalid_curvature_excluded_with_coverage_reported():
    """Remove area 2 and weighted |K|=4: valid A=6; I_D=4*4/6=8/3.

    The invalid value may be NaN; the remaining middle integral is 2, so its
    global-normalized contribution is 4*2/6=4/3 and its intensity share is 1/2.
    """
    result = summarize(
        gaussian_curvature=[1, np.nan, 0, 1, -1],
        valid=[True, False, True, True, True],
    )
    assert result.status == "PARTIAL"
    assert result.valid_area == 6
    assert result.I_D == pytest.approx(8 / 3)
    middle = result.regions["middle"]
    assert middle.status == "PARTIAL"
    assert middle.area == 6 and middle.valid_area == 4
    assert middle.I_D_contribution == pytest.approx(4 / 3)
    assert middle.intensity_share == 0.5


def test_zero_curvature_has_no_defined_intensity_shares():
    """For K=0, I_D=0; a region's intensity share would be 0/0, hence None."""
    result = summarize(gaussian_curvature=[0] * 5)
    assert result.status == "EVALUATED" and result.I_D == 0
    assert all(r.intensity_share is None for r in result.regions.values())
    assert all(r.I_D_contribution == 0 for r in result.regions.values())
    json.dumps(asdict(result), allow_nan=False)


def test_no_valid_area_is_not_evaluated():
    """No valid quadrature area gives a zero denominator, not zero curvature."""
    result = summarize(gaussian_curvature=[np.nan] * 5, valid=[False] * 5)
    assert result.status == "NOT_EVALUATED" and result.I_D is None
    assert result.total_area == 8 and result.valid_area == 0
    for region in result.regions.values():
        assert region.status == "NOT_EVALUATED"
        assert region.I_D_contribution is None and region.intensity_share is None
    json.dumps(asdict(result), allow_nan=False)


def test_empty_or_invalid_region_is_not_evaluated():
    result = summarize(valid=[False, True, True, True, True])
    assert result.regions["lower_end"].status == "NOT_EVALUATED"
    assert result.regions["lower_end"].I_D_contribution is None
    empty = summarize(end_fraction=0.5)
    assert empty.regions["middle"].area == 2  # Exact midpoint remains in the middle.
    absent = summarize(longitudinal_coordinates=[0, 0, 1, 4, 4], end_fraction=0.5)
    assert absent.regions["middle"].status == "NOT_EVALUATED"


def test_units_scaling_and_translation_preserve_dimensionless_results():
    """For scale s=3, A'=9A, K'=K/9, L'=3L; A'|K'|L'^2/A'=A|K|L^2/A.

    The translation by 10 length units changes the cut positions only.
    """
    base = summarize()
    scaled = summarize(
        longitudinal_coordinates=[10, 13, 16, 19, 22],
        gaussian_curvature=np.array([1, -2, 0, 1, -1]) / 9,
        vertex_area=np.array([1, 2, 2, 2, 1]) * 9,
        lref=6,
    )
    assert scaled.I_D == base.I_D
    for name in base.regions:
        assert scaled.regions[name].I_D_contribution == base.regions[name].I_D_contribution
        assert scaled.regions[name].intensity_share == base.regions[name].intensity_share


@pytest.mark.parametrize(
    "changes,match",
    [
        ({"longitudinal_coordinates": []}, "shape"),
        ({"gaussian_curvature": [[1] * 5]}, "shape"),
        ({"vertex_area": [1]}, "shape"),
        ({"valid": [True]}, "shape"),
        ({"valid": [1] * 5}, "boolean"),
        ({"longitudinal_coordinates": [1] * 5}, "span"),
        ({"longitudinal_coordinates": [0, 1, 2, 3, np.inf]}, "finite"),
        ({"vertex_area": [-1, 2, 2, 2, 1]}, "nonnegative"),
        ({"vertex_area": [np.nan, 2, 2, 2, 1]}, "finite"),
        ({"gaussian_curvature": [1, np.inf, 0, 1, -1]}, "valid curvature"),
        ({"lref": 0}, "lref"),
        ({"lref": -1}, "lref"),
        ({"lref": np.inf}, "lref"),
        ({"lref": np.nan}, "lref"),
        ({"end_fraction": 0}, "end_fraction"),
        ({"end_fraction": 0.51}, "end_fraction"),
        ({"end_fraction": np.nan}, "end_fraction"),
    ],
)
def test_invalid_inputs_rejected(changes, match):
    with pytest.raises(ValueError, match=match):
        summarize(**changes)


def test_zero_area_samples_do_not_create_false_coverage():
    """A zero-area vertex carries no quadrature weight even if its flag is false."""
    result = summarize(vertex_area=[0, 2, 2, 2, 1], valid=[False, True, True, True, True])
    assert result.status == "EVALUATED"
    assert result.regions["lower_end"].status == "NOT_EVALUATED"


def test_small_excluded_area_still_reports_partial_coverage():
    """A positive excluded weight remains missing even if floating sums round equal."""
    result = summarize(vertex_area=[1, 1e30, 1, 1, 1], valid=[False, True, True, True, True])
    assert result.total_area == result.valid_area  # Rounded equality is not coverage.
    assert result.status == "PARTIAL"


@pytest.mark.parametrize(
    "changes",
    [
        {"lref": 1e308},
        {"vertex_area": [1e308] * 5},
        {"longitudinal_coordinates": [-1e308, 0, 1, 2, 1e308]},
    ],
)
def test_overflow_is_rejected_instead_of_emitting_infinite_metrics(changes):
    with pytest.raises(ValueError, match="numerical range"):
        summarize(**changes)


def test_all_zero_area_is_not_evaluated():
    """With sum(A_i)=0 the average is undefined, even with finite K everywhere."""
    result = summarize(vertex_area=[0] * 5)
    assert result.status == "NOT_EVALUATED"
    assert result.I_D is None
    json.dumps(asdict(result), allow_nan=False)


def test_half_span_cuts_coincide_despite_decimal_rounding():
    """For a half-span fraction the cuts coincide at (0.1+0.5)/2 = 0.3."""
    result = summarize(
        longitudinal_coordinates=[0.1, 0.1, 0.3, 0.5, 0.5], end_fraction=0.5
    )
    assert result.lower_cut == result.upper_cut
    assert result.lower_cut == pytest.approx(0.3)
