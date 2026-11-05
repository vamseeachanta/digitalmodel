# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic geometry regressions; no engineering-method qualification."""

import math

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


@pytest.mark.parametrize("required", [0.35, 0.47])
def test_custom_population_preserves_extent_without_engine_flaw_candidates(required):
    grid = pd.DataFrame([[0.46, 0.5]] * 4, index=[0.0, 10.0, 20.0, 30.0])
    assert not (grid < 0.9 * 0.5).any().any()
    result = assess_pitting(grid, nominal_od_in=16.0, nominal_wt_in=0.5,
                            t_min_in=required, pit_threshold_fraction=0.95)
    char, level2 = result["characterization"], result["level2"]
    assert char.pitted_region_length_in == 40.0
    assert level2["flaw_length_in"] == level2["equivalent_lta"]["axial_extent_in"] == 40.0
    lam = 1.285 * 40.0 / math.sqrt((16.0 - 2.0 * required) * required)
    mt = math.sqrt(1.0 + 0.48 * min(lam, 20.0) ** 2)
    rt = min(0.46 / required, 1.0)
    assert level2["folias_factor"] == pytest.approx(mt)
    assert level2["rsf"] == pytest.approx(rt / (1.0 - (1.0 - rt) / mt))


@pytest.mark.parametrize("rows", [1, 4])
def test_explicit_pitch_preserves_singleton_and_multirow_extent(rows):
    grid = GridParser.from_numpy(np.array([[0.3, 0.5]] * rows))
    result = assess_pitting(grid, nominal_od_in=16.0, nominal_wt_in=0.5,
                            t_min_in=0.35, row_spacing_in=10.0)
    char = result["characterization"]
    assert char.row_spacing_source == "explicit"
    assert char.pitted_region_length_in == rows * 10.0
    assert result["level2"]["flaw_length_in"] == rows * 10.0
    assert "pitting.missing_axial_pitch" not in result["level2"]["applicability"].flags
    indexed = pd.DataFrame([[0.3]] * rows, index=np.arange(rows) * 10.0)
    expected_mt = Level2Engine._folias_factor(rows * 10.0, 16.0 - 2.0 * 0.35, 0.35)
    assert result["level2"]["folias_factor"] == expected_mt
    if rows > 1:
        assert result["level2"]["rsf"] == Level2Engine("LML", 16.0, 0.5, 0.35).evaluate(indexed)["rsf"]


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


@pytest.mark.parametrize("pitch", [0.0, -1.0, float("nan"), float("inf")])
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


@pytest.mark.parametrize("length", [0.0, -1.0, float("nan"), float("inf")])
def test_explicit_engine_extent_requires_positive_finite_length(length):
    with pytest.raises(ValueError, match="flaw_length_in"):
        Level2Engine("LML", 16.0, 0.5, 0.35).evaluate(
            pd.DataFrame([[0.3]]), flaw_length_in=length)
