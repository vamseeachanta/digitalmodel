# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic checks of bounded sizing, independent of production arithmetic."""

import html
import importlib
import math
import re
from dataclasses import FrozenInstanceError
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock

import pytest


@pytest.fixture
def sizing():
    return importlib.import_module("digitalmodel.geotechnical.mudmat_sizing")


@pytest.fixture
def inputs():
    return dict(width_candidates_m=[1, 2, 3], aspect_ratio=1,
                vertical_load_kn=100, horizontal_load_kn=10,
                undrained_shear_strength_kpa=10,
                submerged_unit_weight_kn_m3=8, factor_of_safety=2)


def test_selection_and_full_trace_match_independent_arithmetic(sizing, inputs):
    result = sizing.size_mudmat(**inputs)
    assert result.status == "SELECTED"
    assert result.scope == "centred-unembedded-undrained screening"
    assert result.selected is result.evaluations[1]
    assert result.width_candidates_m == (1, 2, 3)
    for width, row in zip([1, 2, 3], result.evaluations, strict=True):
        pressure = 10 * (math.pi + 2) * 1.2
        assert row.width_b_m == row.length_l_m == width
        assert row.q_ult_kpa == pytest.approx(pressure)
        assert row.vertical_capacity_kn == pytest.approx(pressure * width**2)
        assert row.sliding_capacity_kn == 10 * width**2
        assert row.factor_of_safety == 2
        assert row.allowable_vertical_kn == pytest.approx(pressure * width**2 / 2)
        assert row.allowable_sliding_kn == 10 * width**2 / 2
        assert row.bearing_pass is (width >= 2)
        assert row.sliding_pass is (width >= 2)
        assert row.standard == "DNV-RP-C212 / Brinch Hansen (1970)"


@pytest.mark.parametrize("vertical,horizontal,selected", [
    (100, 0, 2), (1, 40, 3), (10000, 0, None), (1, 1000, None),
])
def test_bearing_and_sliding_control_and_no_candidate(
    sizing, inputs, vertical, horizontal, selected
):
    inputs.update(vertical_load_kn=vertical, horizontal_load_kn=horizontal)
    result = sizing.size_mudmat(**inputs)
    assert len(result.evaluations) == 3
    assert result.status == ("NO_CANDIDATE" if selected is None else "SELECTED")
    assert (result.selected.width_b_m if result.selected else None) == selected


def test_single_candidate_exact_boundary_and_aspect_ratio(sizing, inputs):
    # Independent rectangular shape factor: 1 + 0.2 * (2/4).
    vertical = 10 * (math.pi + 2) * 1.1 * 8 / 2
    inputs.update(width_candidates_m=[2], aspect_ratio=2,
                  vertical_load_kn=vertical, horizontal_load_kn=40)
    result = sizing.size_mudmat(**inputs)
    assert result.selected.length_l_m == 4
    assert result.selected.allowable_vertical_kn == vertical
    assert result.selected.allowable_sliding_kn == 40
    assert result.selected.bearing_pass and result.selected.sliding_pass


@pytest.mark.parametrize("field", [
    "aspect_ratio", "vertical_load_kn", "horizontal_load_kn",
    "undrained_shear_strength_kpa", "submerged_unit_weight_kn_m3",
    "factor_of_safety",
])
@pytest.mark.parametrize("value", [True, False, math.nan, math.inf, -math.inf,
                                     "1", None, 10**400])
def test_invalid_scalars_fail_before_any_capacity_call(sizing, inputs, field, value,
                                                       monkeypatch):
    spy = Mock()
    monkeypatch.setattr(sizing, "mudmat_bearing_capacity", spy)
    inputs[field] = value
    with pytest.raises(ValueError):
        sizing.size_mudmat(**inputs)
    spy.assert_not_called()


@pytest.mark.parametrize("changes", [
    {"width_candidates_m": grid} for grid in
    ([], [0], [-1], [True], [math.nan], [math.inf], [2, 1], [1, 1],
     [1, "2"], [1, math.nan], None, "123", [1, 10**400])
] + [
    {"aspect_ratio": 0.9}, {"vertical_load_kn": 0}, {"horizontal_load_kn": -1},
    {"undrained_shear_strength_kpa": 0}, {"submerged_unit_weight_kn_m3": -1},
    {"factor_of_safety": 0}, {"factor_of_safety": -1},
    {"width_candidates_m": [1, 1e308], "aspect_ratio": 2},
    {"width_candidates_m": [1e200]}, {"width_candidates_m": [1e-200]},
])
def test_invalid_grids_ranges_and_geometry_fail_before_calculation(
    sizing, inputs, changes, monkeypatch
):
    spy = Mock()
    monkeypatch.setattr(sizing, "mudmat_bearing_capacity", spy)
    inputs.update(changes)
    with pytest.raises(ValueError):
        sizing.size_mudmat(**inputs)
    spy.assert_not_called()


@pytest.mark.parametrize("unsupported", ["condition", "embedment_depth_m", "moment_knm"])
def test_unsupported_settings_are_rejected(sizing, inputs, unsupported):
    with pytest.raises(TypeError):
        sizing.size_mudmat(**inputs, **{unsupported: 1})


def test_result_is_detached_and_immutable(sizing, inputs):
    widths = inputs["width_candidates_m"][:]
    result = sizing.size_mudmat(**inputs)
    assert inputs["width_candidates_m"] == widths
    inputs["width_candidates_m"][0] = 99
    assert result.width_candidates_m == (1, 2, 3)
    assert result.vertical_load_kn == 100
    assert result.horizontal_load_kn == 10
    assert result.undrained_shear_strength_kpa == 10
    assert result.submerged_unit_weight_kn_m3 == 8
    assert result.aspect_ratio == 1 and result.factor_of_safety == 2
    with pytest.raises(FrozenInstanceError):
        result.selected.width_b_m = 99
    with pytest.raises(FrozenInstanceError):
        result.selected = None


def test_every_candidate_is_called_with_explicit_scope(sizing, inputs, monkeypatch):
    capacities = [200, 0, 300]  # A later failure must not end grid evaluation.
    spy = Mock(side_effect=[SimpleNamespace(
        standard="synthetic", q_ult_kpa=1, vertical_capacity_kn=value,
        sliding_capacity_kn=value) for value in capacities])
    monkeypatch.setattr(sizing, "mudmat_bearing_capacity", spy)
    result = sizing.size_mudmat(**inputs)
    assert result.selected.width_b_m == 1
    assert [row.bearing_pass for row in result.evaluations] == [True, False, True]
    assert spy.call_count == 3
    for width, call in zip([1, 2, 3], spy.call_args_list, strict=True):
        assert call.kwargs == dict(width_b_m=width, length_l_m=width,
                                  embedment_depth_m=0, moment_knm=0,
                                  condition="undrained", vertical_load_kn=100,
                                  submerged_unit_weight_kn_m3=8,
                                  undrained_shear_strength_kpa=10)


@pytest.mark.parametrize("changes", [
    {"undrained_shear_strength_kpa": 1e308}, {"factor_of_safety": 1e-308},
])
def test_arithmetic_overflow_is_an_error(sizing, inputs, changes):
    inputs.update(changes)
    with pytest.raises(ValueError, match="finite"):
        sizing.size_mudmat(**inputs)


def test_documented_calls(sizing):
    page = Path("docs/domains/geotechnical/mudmat-sizing.html").read_text()
    blocks = re.findall(r'<pre><code class="language-python">(.*?)</code></pre>',
                        page, flags=re.S)
    assert len(blocks) == 1
    namespace = {}
    exec(compile(html.unescape(blocks[0]), "documented example", "exec"), namespace)
    assert namespace["result"].selected.width_b_m == 2
    assert namespace["infeasible"].status == "NO_CANDIDATE"


@pytest.mark.parametrize("weight,factor,width", [(0, 2, 2), (8, 0.5, 1)])
def test_zero_unit_weight_and_positive_factor_below_one(sizing, inputs, weight,
                                                       factor, width):
    inputs.update(submerged_unit_weight_kn_m3=weight, factor_of_safety=factor)
    result = sizing.size_mudmat(**inputs)
    assert result.selected.width_b_m == width
    assert result.selected.allowable_sliding_kn == 10 * width**2 / factor
    assert result.selected.allowable_vertical_kn == pytest.approx(
        10 * (math.pi + 2) * 1.2 * width**2 / factor)
