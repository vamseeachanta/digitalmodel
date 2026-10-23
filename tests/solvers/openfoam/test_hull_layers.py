# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Layer inputs use synthetic arithmetic examples, not mesh acceptance limits.

Parameter meanings: OpenFOAM v2312 (2023), snappyHexMesh layer guide,
https://doc.openfoam.com/2312/tools/pre-processing/mesh/generation/snappyhexmesh/layers/
Sections relativeSizes, expansionRatio, firstLayerThickness and minThickness.
"""

import json
import math
import re

import pytest

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig, build_hull_case
from digitalmodel.solvers.openfoam.hull_case_regions import SurfaceRegion
from digitalmodel.solvers.openfoam.hull_double_body import (
    DoubleBodyCaseConfig,
    build_double_body_case,
)
from digitalmodel.solvers.openfoam.hull_layers import AbsoluteLayerSpec
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest


def test_geometric_stack_and_cell_ratios():
    """Hand calculation: 0.002 + 0.004 + 0.008 = 0.014 m.

    Synthetic r=2, n=3, outside cell=0.028 m give cell/first=14,
    cell/stack=2. Neither ratio is an acceptance limit.
    """
    result = AbsoluteLayerSpec(0.002, 2, 0.001).sizing(3, 0.028)
    assert result["first_layer_thickness_m"] == pytest.approx(0.002)
    assert result["final_layer_thickness_m"] == pytest.approx(0.008)
    assert result["total_thickness_m"] == pytest.approx(0.014)
    assert result["cell_to_first_layer_ratio"] == pytest.approx(14)
    assert result["cell_to_stack_ratio"] == pytest.approx(2)
    assert result["coverage_status"] == "NOT_EVALUATED"
    assert result["y_plus_status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("ratio", [1, 1 + 1e-12, 0.5])
def test_uniform_nearly_uniform_and_shrinking_stack(ratio):
    """Independent expanded sum: first + first*r + first*r*r, n=3.

    These synthetic ratios exercise the geometric-series singularity at r=1.
    """
    result = AbsoluteLayerSpec(0.002, ratio, 0.001).sizing(3)
    expected = 0.002 + 0.002 * ratio + 0.002 * ratio * ratio
    assert result["total_thickness_m"] == pytest.approx(expected)
    assert result["outside_cell_size_m"] is None
    assert result["cell_to_first_layer_ratio"] is None


@pytest.mark.parametrize(
    "field", ["first_layer_thickness_m", "expansion_ratio", "min_thickness_m"]
)
@pytest.mark.parametrize("bad", [0, -1, math.inf, -math.inf, math.nan, True])
def test_invalid_specification(field, bad):
    """Positive finite scalars are required; no physical cutoff is assumed."""
    inputs = dict(
        first_layer_thickness_m=0.002, expansion_ratio=2, min_thickness_m=0.001
    )
    inputs[field] = bad
    with pytest.raises(ValueError, match=field):
        AbsoluteLayerSpec(**inputs)


@pytest.mark.parametrize("count", [-1, 1.5, True])
def test_invalid_layer_count(count):
    with pytest.raises(ValueError, match="n_surface_layers"):
        AbsoluteLayerSpec(0.002, 2, 0.001).sizing(count)


@pytest.mark.parametrize("cell", [0, -1, math.inf, math.nan, True])
def test_invalid_outside_cell(cell):
    with pytest.raises(ValueError, match="outside_cell_size_m"):
        AbsoluteLayerSpec(0.002, 2, 0.001).sizing(3, cell)


def test_disabled_layers_are_not_a_positive_stack():
    result = AbsoluteLayerSpec(0.002, 2, 0.001).sizing(0, 0.028)
    assert result["total_thickness_m"] == 0
    assert result["first_layer_thickness_m"] is None
    assert result["final_layer_thickness_m"] is None
    assert result["cell_to_stack_ratio"] is None


def test_unrepresentable_stack_is_refused():
    with pytest.raises(ValueError, match="representable"):
        AbsoluteLayerSpec(1, 1e300, 0.001).sizing(3)


def test_minimum_thickness_compares_the_overall_stack():
    """Hand sum 0.002+0.004+0.008=0.014 m exceeds 0.003 m, but not 0.020 m."""
    assert not AbsoluteLayerSpec(0.002, 2, 0.003).sizing(3)[
        "minimum_stack_exceeds_request"
    ]
    assert AbsoluteLayerSpec(0.002, 2, 0.020).sizing(3)["minimum_stack_exceeds_request"]


def test_large_cell_ratio_is_not_an_empirical_failure():
    """Synthetic 2 / 0.002 = 1000, a diagnostic ratio without a cutoff."""
    result = AbsoluteLayerSpec(0.002, 2, 0.001).sizing(3, 2)
    assert result["cell_to_first_layer_ratio"] == pytest.approx(1000)
    assert result["coverage_status"] == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "config_type,build",
    [(HullCaseConfig, build_hull_case), (DoubleBodyCaseConfig, build_double_body_case)],
)
def test_invalid_stack_leaves_no_partial_case(
    config_type, build, manifest_dict, stl_file, tmp_path
):
    """The synthetic third layer exceeds the floating-point representation."""
    config = config_type(
        manifest=HullManifest.from_dict(manifest_dict),
        stl_path=stl_file,
        velocity=2,
        ranks=1,
        absolute_layers=AbsoluteLayerSpec(1, 1e300, 0.001),
    )
    parent = tmp_path / "cases"
    with pytest.raises(ValueError, match="representable"):
        build(config, parent)
    assert not parent.exists()


@pytest.mark.parametrize("absolute", [False, True])
@pytest.mark.parametrize("kind", ["free_surface", "double_body"])
def test_emitted_layers_and_provenance(
    kind, absolute, manifest_dict, stl_file, tmp_path
):
    """Synthetic spec: three hull layers sum to 0.014 m; two fin layers to 0.006 m.

    Existing relative defaults are inherited from the shipped v2312 templates:
    expansionRatio=1.5, finalLayerThickness=0.7, minThickness=0.25.
    Neither the fixtures nor these inherited defaults are acceptance limits.
    """
    config_type, build = (
        (HullCaseConfig, build_hull_case)
        if kind == "free_surface"
        else (DoubleBodyCaseConfig, build_double_body_case)
    )
    spec = AbsoluteLayerSpec(0.002, 2, 0.001) if absolute else None
    fin = tmp_path / "fin.stl"
    fin.write_text(stl_file.read_text())
    config = config_type(
        manifest=HullManifest.from_dict(manifest_dict),
        stl_path=stl_file,
        velocity=2,
        ranks=1,
        absolute_layers=spec,
        appendages=(SurfaceRegion("fin", fin, n_surface_layers=2),),
    )
    case = build(config, tmp_path / "cases")
    text = (case / "system" / "snappyHexMeshDict").read_text()
    text = re.sub(r"//[^\n]*|/\*.*?\*/", "", text, flags=re.S)
    provenance = json.loads((case / "case_provenance.json").read_text())["mesh"][
        "layers"
    ]
    if absolute:
        assert re.search(r"relativeSizes\s+false;", text)
        assert re.search(r"firstLayerThickness\s+0.002;", text)
        assert re.search(r"expansionRatio\s+2;", text)
        assert re.search(r"minThickness\s+0.001;", text)
        assert "finalLayerThickness" not in text
        patches = provenance["patches"]
        assert patches["hull"]["total_thickness_m"] == pytest.approx(0.014)
        assert patches["fin"]["total_thickness_m"] == pytest.approx(0.006)
        assert patches["hull"]["outside_cell_size_m"] is None
    else:
        assert re.search(r"relativeSizes\s+true;", text)
        assert re.search(r"finalLayerThickness\s+0.7;", text)
        assert re.search(r"expansionRatio\s+1.5;", text)
        assert re.search(r"minThickness\s+0.25;", text)
        assert "firstLayerThickness" not in text
    assert provenance["coverage_status"] == "NOT_EVALUATED"
