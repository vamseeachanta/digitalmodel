"""Geometry regressions; synthetic dimensions use one consistent length unit.

These hand calculations qualify piecewise-linear integration only, not an
edition-specific API 579 assessment. Source: NIST DLMF (2010), §3.5(i),
Eq. 3.5.1, https://dlmf.nist.gov/3.5.E1 (applied to each linear segment).
"""

import json

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


def test_critical_profile_and_translation_preserve_geometry():
    """Hand: constant planes 3 and 8 give critical wall 3 for all intervals;
    the minimum profile has 3 intervals, lengths 2,5,3.
    Translating every coordinate adds no segment length or area.
    """
    values = [[3, 8], [3, 8], [3, 8]]
    original = evaluate(values, [0, 2, 5])
    translated = evaluate(values, [100, 102, 105])
    assert original == translated
    assert original[0] == pytest.approx([3, 3, 3])
    assert original[1] == [2, 5, 3]


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


def test_interval_records_retain_endpoints_and_integrated_area():
    """Hand (NIST Eq. 3.5.1): row minima are [2,6,10] at [5,7,12].
    Intervals (0,1), (0,2), (1,2) have lengths 2,7,5, areas 8,48,40
    and means 4,48/7,8. Positions refer to the supplied grid, from zero.
    """
    grid = pd.DataFrame([[2, 8], [9, 6], [10, 12]], index=[5, 7, 12])
    before = grid.copy(deep=True)
    component = API579_components({})
    intervals = component.LMLLevel2Intervals(grid)
    expected = [
        (0, 1, 5, 7, 2, 8, 4),
        (0, 2, 5, 12, 7, 48, 48 / 7),
        (1, 2, 7, 12, 5, 40, 8),
    ]
    for interval, values in zip(intervals, expected, strict=True):
        assert interval == pytest.approx(
            dict(
                zip(
                    [
                        "start_position",
                        "end_position",
                        "start_coordinate",
                        "end_coordinate",
                        "length",
                        "area",
                        "mean_wall",
                    ],
                    values,
                    strict=True,
                )
            )
        )
    assert json.loads(json.dumps(intervals, allow_nan=False)) == intervals
    pd.testing.assert_frame_equal(grid, before)
    assert component.LMLLevel2Evaluation(grid, {}) == (
        [item[6] for item in expected],
        [item[4] for item in expected],
    )


def test_refining_a_linear_segment_retains_whole_interval_geometry():
    """Hand: t=x+2, x=0..4 has area 4*(2+6)/2=16 and mean 4.
    Inserting x=1,t=3 gives areas 1*2.5 + 3*4.5 = 16.
    This is a check of the stated interpolation assumption, not pressure.
    """
    component = API579_components({})
    coarse = component.LMLLevel2Intervals(pd.DataFrame([2, 6], index=[0, 4]))
    refined = component.LMLLevel2Intervals(pd.DataFrame([2, 3, 6], index=[0, 1, 4]))
    for record in (coarse[0], refined[1]):
        assert record["length"] == 4
        assert record["area"] == 16
        assert record["mean_wall"] == 4


@pytest.mark.parametrize(
    "entrypoint", ["API579LML", "lml", "short_pitch", "tie", "fca"]
)
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
    if entrypoint == "fca":
        cfg.LML["LTA"][0]["FCA"] = [1]
    component = API579_components(cfg)
    monkeypatch.setattr(component, "getMAWP", lambda *args: 1)
    calls = []

    def pressure_stub(data, cfg, Level):
        # Synthetic outputs isolate selection: pressure, RSF and Mt minima
        # deliberately occur at different intervals; no code values are used.
        if Level == 1:
            return {"MAWPr": 1, "Mt": 1, "MAWPrEvaluation": False, "RSF": 1}
        calls.append(dict(data))
        outputs = [(3, 0.1, 1), (2, 0.3, 3), (1, 0.2, 2)]
        if entrypoint == "tie":
            outputs = [(3, 0.1, 1), (1, 0.3, 3), (1, 0.2, 2)]
        pressure, rsf, mt = outputs[len(calls) - 1]
        return {"MAWPr": pressure, "Mt": mt, "MAWPrEvaluation": False, "RSF": rsf}

    monkeypatch.setattr(component, "LMLMAWPrEvaluation", pressure_stub)
    if entrypoint == "short_pitch":
        # Real pressure evaluation; only the pipe-capacity dependency is isolated.
        monkeypatch.undo()
        monkeypatch.setattr(component, "getMAWP", lambda *args: 1)
        cfg.Outer_Pipe["Geometry"].update(Nominal_OD=25, Design_WT=0.5)
        cfg.Geometry["NominalID"] = 24
        cfg.API579Parameters.update(
            RSFa=0.9,
            FoliasFactor={
                "FlawParameter": [0, 1, 20, 30],
                "Mt": {"Cylindrical": [1, 2, 4, 6]},
            },
        )
        cfg.LML["LTA"][0]["grid"] = {
            "values": [[0.3, 0.5], [0.3, 0.5], [0.4, 0.5]],
            "index": [0, 0.5, 1],
        }
        results = component.lml()["Result"]["LML"][0]
        assert np.isfinite(results[0]["Mt, L2"])
        assert np.isfinite(results[0]["MAWPr, L2"])
        assert "s uses index span + 1" in results[0]["Level 2 limitation"]
        assert "Level 2 uses endpoint distance" in results[0]["Level 2 limitation"]
        return
    grid = pd.DataFrame([[4, 10], [4, 10], [8, 10]], index=[0, 2, 5], dtype=float)
    if entrypoint in ("lml", "tie", "fca"):
        results = component.lml()["Result"]["LML"][0]
    else:
        results = component.API579LML(grid, cfg, 0)
    assert results[0]["Level 2 qualification"] == "NOT_EVALUATED"
    assert "edition-specific" in results[0]["Level 2 limitation"]
    # Hand: the third interval runs from 2 to 5; its constant-linear mean
    # is (4+8)/2=6 and its area is 3*6=18. The stub assigns pressure 1.
    governing = results[0]["Level 2 governing interval"]
    fca = cfg.LML["LTA"][0]["FCA"][0]
    expected = {
        "start_position": 1,
        "end_position": 2,
        "start_coordinate": 2,
        "end_coordinate": 5,
        "length": 3,
        "area": 18,
        "mean_wall": 6,
        "FCA": fca,
        "adjusted_mean_wall": 6 - fca,
        "MAWPr": 1,
        "RSF": 0.2,
        "Mt": 2,
    }
    if entrypoint == "tie":
        # Hand: the first tied minimum spans 0..5, with areas
        # 2*4 + 3*6 = 26, hence mean 26/5 = 5.2.
        expected.update(
            start_position=0,
            start_coordinate=0,
            length=5,
            area=26,
            mean_wall=5.2,
            adjusted_mean_wall=5.2,
            RSF=0.3,
            Mt=3,
        )
    assert governing == pytest.approx(expected)
    # FCA changes the pressure input ratio, never the measured interval area.
    assert [call["AARatio"] for call in calls] == pytest.approx(
        [(4 - fca) / 10, (5.2 - fca) / 10, (6 - fca) / 10]
    )
    assert results[0]["RSF, L2"] == 0.1  # Legacy independent minima retained.
    assert results[0]["Mt, L2"] == 1
    assert results[0]["MAWPr, L2"] == governing["MAWPr"]
    assert json.loads(json.dumps(governing, allow_nan=False)) == governing


def test_switching_minimum_planes_forms_one_critical_profile():
    """Row minima [2,2,2] give mean 2 for all three intervals;
    each original plane instead gives whole-profile mean 4 or 6.
    """
    assert evaluate([[2, 10], [10, 2], [2, 10]], [0, 1, 2]) == (
        [2, 2, 2],
        [1, 2, 1],
    )


@pytest.mark.parametrize("flaw_parameter", [0, 0.2, 0.354, 20, 21])
@pytest.mark.parametrize("level", [1, 2])
@pytest.mark.parametrize("rt", [0.1, 0.95])
def test_folias_boundaries_use_configured_table(flaw_parameter, level, rt):
    """Synthetic table: interpolation is 1 + lambda/10; no code values.
    Both screening outcomes must return a defined factor and finite pressure.
    """
    cfg = {
        "API579Parameters": {
            "FoliasFactor": {"FlawParameter": [0, 30], "Mt": {"Cylindrical": [1, 4]}}
        }
    }
    result = API579_components({}).LMLMAWPrEvaluation(
        {
            "LongitudinalFlawLengthParameter": flaw_parameter,
            "Rt": rt,
            "RSFa": 0.9,
            "MAWP": 100,
            "AARatio": rt,
        },
        cfg,
        Level=level,
    )
    assert result["Mt"] == pytest.approx(1 + flaw_parameter / 10)
    assert np.isfinite(result["MAWPr"])
    assert np.isfinite(result["RSF"])
