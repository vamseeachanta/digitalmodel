"""Geometry regressions; synthetic dimensions use one consistent length unit.

These hand calculations qualify piecewise-linear integration only, not an
edition-specific API 579 assessment. Source: NIST DLMF (2010), §3.5(i),
Eq. 3.5.1, https://dlmf.nist.gov/3.5.E1 (applied to each linear segment).
"""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.common.API579_components import API579_components
from digitalmodel.asset_integrity.common.data import AttributeDict


def evaluate(thickness, coordinates):
    return API579_components({}).LMLLevel2Evaluation(
        pd.DataFrame(thickness, index=coordinates), {}
    )


def test_two_endpoints_define_one_pitch_and_include_both_readings():
    """Hand: L=7-4=3; integral=3*(2+6)/2=12; mean=12/3=4."""
    averages, lengths = evaluate([2, 6], [4, 7])
    assert averages == pytest.approx([4])
    assert lengths == pytest.approx([3])


def test_contiguous_intervals_preserve_separated_and_adjacent_loss():
    """Hand: unit-pitch [2,2,10] has means [2,4,6]; [2,10,2]
    has means [6,6,6]. Enumeration is start-major, then end-major.
    Whole-profile integrals are 2+6=8 and 6+6=12 over length 2.
    """
    adjacent, adjacent_lengths = evaluate([2, 2, 10], [0, 1, 2])
    separated, separated_lengths = evaluate([2, 10, 2], [0, 1, 2])
    assert adjacent == pytest.approx([2, 4, 6])
    assert separated == pytest.approx([6, 6, 6])
    assert adjacent_lengths == separated_lengths == [1, 2, 1]


def test_irregular_spacing_weights_segments_and_preserves_input():
    """Hand: x=[5,7,12], t=[2,6,10]. Areas 2*4=8, 5*8=40;
    whole mean=(8+40)/7=48/7. Interval lengths are 2,7,5.
    """
    grid = pd.DataFrame({"profile": [2, 6, 10]}, index=[5, 7, 12])
    before = grid.copy(deep=True)
    averages, lengths = API579_components({}).LMLLevel2Evaluation(grid, {})
    assert averages == pytest.approx([4, 48 / 7, 8])
    assert lengths == [2, 7, 5]
    pd.testing.assert_frame_equal(grid, before)


def test_columns_are_independent_and_translation_preserves_geometry():
    """Hand: constant walls 3 and 8 retain their value for all intervals;
    three endpoints produce 3 intervals per column, lengths 2,5,3.
    Translating every coordinate adds no segment length or area.
    """
    values = [[3, 8], [3, 8], [3, 8]]
    original = evaluate(values, [0, 2, 5])
    translated = evaluate(values, [100, 102, 105])
    assert original == translated
    assert original[0] == pytest.approx([3, 3, 3, 8, 8, 8])
    assert original[1] == [2, 5, 3, 2, 5, 3]


@pytest.mark.parametrize("coordinates", [[0, 0, 1], [2, 1, 0], [0, 2, 1]])
def test_ambiguous_coordinate_order_is_rejected(coordinates):
    """Duplicate or reversed coordinates cannot define ordered positive spans."""
    with pytest.raises(ValueError, match="strictly increasing"):
        evaluate([2, 4, 6], coordinates)


@pytest.mark.parametrize("bad", [np.nan, np.inf, -np.inf, "unknown"])
def test_invalid_coordinates_are_rejected(bad):
    """A non-finite or nonnumeric endpoint cannot define interval length."""
    with pytest.raises(ValueError, match="finite numeric"):
        evaluate([2, 4, 6], [0, bad, 2])


@pytest.mark.parametrize("bad", [np.nan, np.inf, -np.inf, "unknown", -1])
def test_invalid_thickness_is_rejected_instead_of_bridging_gaps(bad):
    """Missing readings must not be dropped and join unmeasured neighbours;
    negative wall is outside this geometric input contract; zero is permitted.
    """
    with pytest.raises(ValueError, match="finite numeric|nonnegative"):
        evaluate([2, bad, 6], [0, 1, 2])


@pytest.mark.parametrize(
    "grid", [pd.DataFrame(), pd.DataFrame([2]), pd.DataFrame(index=[0, 1])]
)
def test_insufficient_geometry_is_rejected(grid):
    """At least two endpoints and one measured profile define an interval."""
    with pytest.raises(ValueError, match="two rows and one column"):
        API579_components({}).LMLLevel2Evaluation(grid, {})


def test_zero_wall_is_retained():
    """Hand: L=2; triangular area=2*(0+4)/2=4; mean=2."""
    assert evaluate([0, 4], [0, 2]) == ([2], [2])


@pytest.mark.parametrize("entrypoint", ["API579LML", "lml"])
def test_saved_level2_numbers_carry_unqualified_status(monkeypatch, entrypoint):
    """Exercise result assembly with pressure routines isolated: their dummy
    outputs are not engineering reference values. Geometry uses three measured
    endpoints at 0, 2, 5; the exterior wall is retained for background averaging.
    No numerical acceptance of the legacy pressure routines is asserted.
    """
    cfg = AttributeDict(
        {
            "default": {"config": {"generate_plots": False}},
            "Analysis": {"result_folder": "", "file_name": "synthetic"},
            "PlotSettings": {
                "Data": {
                    "PltSupTitle": "",
                    "PltTitle": "",
                    "PltXLabel": "",
                    "PltYLabel": "",
                }
            },
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 100,
                    "Design_WT": 10,
                    "tmin": 1,
                }
            },
            "Geometry": {"NominalID": 80},
            "Design": [],
            "API579Parameters": {"RSFa": 1},
            "LML": {
                "LTA": [
                    {
                        "sIndex": [0, 3],
                        "cIndex": [0, 1],
                        "Label": "synthetic",
                        "Contour": {},
                        "FCA": [0],
                        "FCANonFlawRatio": 0,
                        "Lmsd": 100,
                        "io": None,
                        "sheet_name": None,
                        "skiprows": 0,
                        "skipfooter": 0,
                        "index_col": 0,
                        "DataCorrectionFactor": 1,
                        "grid": {
                            "values": [[4.0, 10.0], [4.0, 10.0], [8.0, 10.0]],
                            "index": [0, 2, 5],
                        },
                    }
                ]
            },
        }
    )
    component = API579_components(cfg)
    monkeypatch.setattr(component, "getMAWP", lambda *args: 1)
    monkeypatch.setattr(
        component,
        "LMLMAWPrEvaluation",
        lambda *args, **kw: {
            "MAWPr": 1,
            "Mt": 1,
            "MAWPrEvaluation": False,
            "RSF": 1,
        },
    )
    grid = pd.DataFrame([[4, 10], [4, 10], [8, 10]], index=[0, 2, 5], dtype=float)
    if entrypoint == "lml":
        results = component.lml()["Result"]["LML"][0]
    else:
        results = component.API579LML(grid, cfg, 0)
    assert results[0]["Level 2 qualification"] == "NOT_EVALUATED"
    assert "edition-specific" in results[0]["Level 2 limitation"]
