# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic geometry regressions; no engineering-method qualification.

Formula provenance: API 579-1/ASME FFS-1, Fitness-For-Service, 2021 edition,
Part 5 (Assessment of Local Metal Loss) and Part 4 (Assessment of General
Metal Loss). The module and _folias_factor docstrings in level2_engine.py
identify the repository approximation used by these fixtures. Its coefficients
and cap are not independently verified standard values.
No verified standard copy is in hand; these are algorithm regression oracles.

Independent derivation: decimal arithmetic at 50-digit precision, without
production imports, substitutes D = 16 - 2 tc, Rt = min(pit_wall / tc, 1),
and RSF = Rt / (1 - (1 - Rt) / Mt) into the identified approximation. The fixed
expectations below cover (L, tc, pit_wall), in inches:
(40, .35, .46), (40, .47, .46), (10, .35, .30), (40, .35, .30).
They exercise capped/uncapped lambda, Rt = 1, and singleton/multirow extent.
The singleton must also check RSF: its extent alone cannot prove strength.
"""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
    FFSComponent,
    assess_component,
)
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
from digitalmodel.asset_integrity.assessment.ffs_workflow import FFSWorkflow
from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
from digitalmodel.asset_integrity.assessment.pitting import (
    assess_pitting,
    characterize_pit_field,
)


def component():
    return FFSComponent("synthetic", "B31.8", 16.0, 0.5, 1000.0, smys_psi=52000.0)


@pytest.mark.parametrize("required,expected_mt,expected_rsf", [
    (0.35, 13.89244398944980, 1.0),
    (0.47, 13.42242458472551, 0.98027729368594),
])
def test_custom_population_preserves_extent_without_engine_flaw_candidates(
    required, expected_mt, expected_rsf,
):
    grid = pd.DataFrame([[0.46, 0.5]] * 4, index=[0.0, 10.0, 20.0, 30.0])
    assert not (grid < 0.9 * 0.5).any().any()
    result = assess_pitting(grid, nominal_od_in=16.0, nominal_wt_in=0.5,
                            t_min_in=required, pit_threshold_fraction=0.95)
    char, level2 = result["characterization"], result["level2"]
    assert char.pitted_region_length_in == 40.0
    assert level2["flaw_length_in"] == level2["equivalent_lta"]["axial_extent_in"] == 40.0
    assert level2["folias_factor"] == pytest.approx(expected_mt, rel=1e-12)
    assert level2["rsf"] == pytest.approx(expected_rsf, rel=1e-12)


@pytest.mark.parametrize("rows,expected_mt,expected_rsf", [
    (1, 3.97503413300357, 0.88909570793933),
    (4, 13.89244398944980, 0.86604850506945),
])
def test_explicit_pitch_preserves_singleton_and_multirow_extent(
    rows, expected_mt, expected_rsf,
):
    grid = GridParser.from_numpy(np.array([[0.3, 0.5]] * rows))
    result = assess_pitting(grid, nominal_od_in=16.0, nominal_wt_in=0.5,
                            t_min_in=0.35, row_spacing_in=10.0)
    char = result["characterization"]
    assert char.row_spacing_source == "explicit"
    assert char.pitted_region_length_in == rows * 10.0
    assert result["level2"]["flaw_length_in"] == rows * 10.0
    assert "pitting.missing_axial_pitch" not in result["level2"]["applicability"].flags
    assert result["level2"]["folias_factor"] == pytest.approx(expected_mt, rel=1e-12)
    assert result["level2"]["rsf"] == pytest.approx(expected_rsf, rel=1e-12)


@pytest.mark.parametrize("kind,source", [("numpy", "generated"), ("index", "index"),
                                         ("singleton", "default")])
def test_coordinate_provenance_survives_normalization(kind, source):
    if kind == "numpy":
        grid = GridParser.from_numpy(np.array([[0.3, 0.5]] * 4))
    elif kind == "index":
        grid = pd.DataFrame([[0.3, 0.5]] * 4, index=[0.0, 1.0, 2.0, 3.0])
    else:
        grid = pd.DataFrame([[0.3, 0.5]], index=[5.0])
    grid = GridParser.from_dataframe(grid)
    char = characterize_pit_field(grid, 0.5)
    assert char.to_dict()["row_spacing_source"] == source
    result = assess_pitting(grid, nominal_od_in=16.0, nominal_wt_in=0.5, t_min_in=0.35)
    assert result["level2"]["applicability"].ok == (source == "index")


@pytest.mark.parametrize("explicit_applicability", [False, True])
def test_generated_coordinates_escalate_and_report_missing_measurement(explicit_applicability):
    grid = np.array([[0.3, 0.5]] * 4)
    result = assess_component(component(), grid, force_type="PITTING")
    assert result.details["pitting_characterization"]["row_spacing_source"] == "generated"
    assert result.verdict == "ESCALATE"
    assert "pitting.missing_axial_pitch" in result.applicability.flags
    options = {"applicability": result.applicability} if explicit_applicability else {}
    report = FFSReport.generate_html(
        pd.DataFrame(grid), result.decision, "synthetic", 16.0, 0.5, result.t_min_in,
        "B31.8", 1000.0, **options,
    )
    assert "pitting.missing_axial_pitch" in report
    assert "unit pitch is an assumption, not a measurement" in report


@pytest.mark.parametrize("pitch", [None, 10.0])
def test_yaml_adapter_propagates_generated_labels_or_explicit_pitch(pitch):
    block = {"component": vars(component()), "grid": [[0.3, 0.5]] * 4,
             "force_type": "PITTING"}
    if pitch is not None:
        block["row_spacing_in"] = pitch
    result = FFSWorkflow().router({"basename": "ffs", "ffs_assessment": block})["ffs"]
    if pitch is None:
        assert result["verdict"] == "ESCALATE"
        assert "pitting.missing_axial_pitch" in result["applicability"]["flags"]
    else:
        direct = assess_component(component(), np.array(block["grid"]),
                                  force_type="PITTING", row_spacing_in=pitch)
        indexed = assess_component(component(), pd.DataFrame(block["grid"],
                                   index=[0.0, 10.0, 20.0, 30.0]), force_type="PITTING")
        assert direct.level2["flaw_length_in"] == 40.0
        assert direct.rsf == indexed.rsf == result["rsf"]
        assert "applicability" not in result


@pytest.mark.parametrize("pitch", [
    0.0, -1.0, float("nan"), float("inf"), True, False, np.True_,
    np.timedelta64(10, "ns"), np.datetime64("2025-01-01"), "10", 10 + 0j,
    [10.0],
])
def test_invalid_explicit_pitch_is_rejected(pitch):
    with pytest.raises(ValueError, match="row_spacing_in"):
        characterize_pit_field(pd.DataFrame([[0.3, 0.5]] * 4), 0.5, row_spacing_in=pitch)


def test_generated_provenance_survives_filtering_and_numeric_index_conversion():
    raw = pd.DataFrame([[0.3, 0.5], [np.nan, np.nan], [0.3, 0.5]])
    normalized = GridParser.from_dataframe(raw, drop_all_nan_rows=True)
    assert not isinstance(normalized.index, pd.RangeIndex)
    char = characterize_pit_field(normalized, 0.5)
    assert char.row_spacing_source == "generated"
    assert char.row_spacing_in == 1.0


@pytest.mark.parametrize("length", [
    0.0, -1.0, float("nan"), float("inf"), True, False, np.True_,
    np.timedelta64(10, "ns"), np.datetime64("2025-01-01"), "10", 10 + 0j,
    [10.0],
])
def test_explicit_engine_extent_requires_positive_finite_length(length):
    with pytest.raises(ValueError, match="flaw_length_in"):
        Level2Engine("LML", 16.0, 0.5, 0.35).evaluate(
            pd.DataFrame([[0.3]]), flaw_length_in=length)


@pytest.mark.parametrize("kind", ["datetime", "datetime_tz", "timedelta", "period",
                                  "boolean", "complex", "categorical"])
@pytest.mark.parametrize("pitch", [None, 10.0])
def test_nonphysical_indices_require_explicit_pitch(kind, pitch):
    indices = {
        "datetime": pd.date_range("2025-01-01", periods=3),
        "datetime_tz": pd.date_range("2025-01-01", periods=3, tz="UTC"),
        "timedelta": pd.timedelta_range("0 days", periods=3),
        "period": pd.period_range("2025-01", periods=3, freq="M"),
        "boolean": pd.Index([False, True, True]),
        "complex": pd.Index([0j, 10 + 1j, 20 + 2j]),
        "categorical": pd.CategoricalIndex([0, 10, 20]),
    }
    grid = GridParser.from_dataframe(
        pd.DataFrame([[0.3, 0.5]] * 3, index=indices[kind]))
    result = assess_pitting(grid, nominal_od_in=16.0, nominal_wt_in=0.5,
                            t_min_in=0.35, row_spacing_in=pitch)
    char = result["characterization"]
    assert char.row_spacing_source == ("default" if pitch is None else "explicit")
    assert char.row_spacing_in == (1.0 if pitch is None else pitch)
    assert char.pitted_region_length_in == (3.0 if pitch is None else 30.0)
    flags = result["level2"]["applicability"].flags
    assert ("pitting.missing_axial_pitch" in flags) == (pitch is None)


@pytest.mark.parametrize("index", [[0.0, 10.0, 25.0], ["0", "10", "25"]])
def test_validated_numeric_coordinates_supply_minimum_pitch(index):
    char = characterize_pit_field(
        pd.DataFrame([[0.3, 0.5]] * 3, index=index), 0.5)
    assert char.row_spacing_source == "index"
    assert char.row_spacing_in == 10.0
    assert char.pitted_region_length_in == 30.0


def test_yaml_adapter_rejects_boolean_pitch():
    import yaml

    block = yaml.safe_load("row_spacing_in: true")
    block.update(component=vars(component()), grid=[[0.3, 0.5]] * 4,
                 force_type="PITTING")
    with pytest.raises(ValueError, match="row_spacing_in"):
        FFSWorkflow().router({"basename": "ffs", "ffs_assessment": block})


@pytest.mark.parametrize("value", [10, 10.0, np.int64(10), np.float64(10)])
def test_real_numeric_pitch_and_extent_remain_supported(value):
    grid = pd.DataFrame([[0.3, 0.5]] * 3)
    char = characterize_pit_field(grid, 0.5, row_spacing_in=value)
    assert char.row_spacing_source == "explicit"
    assert char.pitted_region_length_in == 30.0
    result = Level2Engine("LML", 16.0, 0.5, 0.35).evaluate(
        grid, flaw_length_in=value)
    assert result["flaw_length_in"] == 10.0


@pytest.mark.parametrize("index", [[20, 10, 0], [0, 10, 10], [0, 20, 10]])
def test_unordered_coordinates_explain_pitch_requirement(index):
    result = assess_pitting(pd.DataFrame([[0.3, 0.5]] * 3, index=index),
                            nominal_od_in=16.0, nominal_wt_in=0.5, t_min_in=0.35)
    applicability = result["level2"]["applicability"]
    assert "pitting.missing_axial_pitch" in applicability.flags
    note = " ".join(applicability.notes)
    assert "could not be established" in note
    assert "strictly increasing" in note
