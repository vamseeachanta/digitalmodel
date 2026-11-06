"""Specified-head boundary validation and an independent Darcy-flow check."""

from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.geotechnical.slope_profile import validate_profile


def confined_profile():
    """Read synthetic inputs, with units and derivation in the example guide."""
    path = (
        Path(__file__).parents[2]
        / "examples/workflows/slope-stability/seepage-input.yml"
    )
    return yaml.safe_load(path.read_text())["profile"]


@pytest.mark.parametrize("reverse", [False, True])
def test_vertical_specified_heads_preserve_order_and_inputs(reverse):
    data = confined_profile()
    if reverse:
        for head in data["seepage"]["heads_m"]:
            head["boundary_m"].reverse()
    before = deepcopy(data)
    assert validate_profile(data)["seepage"] == before["seepage"]
    assert data == before


@pytest.mark.parametrize(
    "boundary",
    [
        [[0, 0], [0, 0]],
        [[0, 0]],
        [[0, 0], [0, float("nan")]],
        [[0, 0], [True, 1]],
        [[0, 0], [0, float("inf")]],
    ],
)
def test_invalid_head_polylines_rejected(boundary):
    data = confined_profile()
    data["seepage"]["heads_m"][0]["boundary_m"] = boundary
    with pytest.raises(ValueError, match="boundary_m"):
        validate_profile(data)


@pytest.mark.parametrize(
    "seepage", [None, [], "steady", {"element_size_m": 1, "heads_m": None}]
)
def test_malformed_seepage_rejected(seepage):
    data = confined_profile()
    data["seepage"] = seepage
    with pytest.raises(ValueError, match="seepage|heads_m"):
        validate_profile(data)


@pytest.mark.parametrize(
    "head",
    [None, {}, {"head_m": 12}, {"head_m": True, "boundary_m": [[0, 0], [0, 10]]}],
)
def test_malformed_head_blocks_rejected(head):
    data = confined_profile()
    data["seepage"]["heads_m"][0] = head
    with pytest.raises(ValueError, match="head"):
        validate_profile(data)


def test_vertical_heads_map_to_workbook(tmp_path):
    pytest.importorskip("xslope")
    from digitalmodel.geotechnical.slope_stability import build_slope_model

    data = confined_profile()
    model = build_slope_model(data, tmp_path)
    for actual, expected in zip(
        model["seepage_bc"]["specified_heads"], data["seepage"]["heads_m"], strict=True
    ):
        assert actual["coords"] == [tuple(point) for point in expected["boundary_m"]]
        assert actual["head"] == expected["head_m"]


def test_confined_discharge_matches_independent_hand_calculation(tmp_path):
    """Darcy: q/width = k * thickness * (h_left-h_right) / length.

    Source: xslope seepage overview, Governing equations (accessed 2026-10-22),
    https://xslope.org/en/latest/seep/overview/. Synthetic inputs: k=0.00001 m/s,
    thickness=10 m, length=20 m, heads=12 and 10 m; impermeable top and bottom.
    Thus the exact linear-head solution gives q/width=0.00001 m²/s.
    The 1% comparison tolerance is a numerical regression allowance, not a
    design criterion; a linear field is representable by the tri3 mesh.
    """
    pytest.importorskip("xslope")
    pytest.importorskip("gmsh")
    from digitalmodel.geotechnical.slope_stability import build_slope_model, run_seepage

    data = confined_profile()
    model = build_slope_model(data, tmp_path)
    result = run_seepage(model, data["seepage"]["element_size_m"])
    assert result.discharge_per_width == pytest.approx(0.00001, rel=0.01)
    assert (result.length_unit, result.time_unit) == ("m", "sec")
    assert result.converged
