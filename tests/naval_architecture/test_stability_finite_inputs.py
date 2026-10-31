"""Finite-input contracts for audit findings S05 and S06 in issue #3284.

All numbers below are arbitrary arithmetic fixtures, not a design basis.
Two weights, 2 t at VCG 1 m and 3 t at VCG 4 m, give displacement 5 t,
KG = (2*1 + 3*4)/5 = 2.8 m. Total FSM 5 t*m gives FSC = 5/5 = 1 m
and fluid KG = 2.8 + 1 = 3.8 m. LCG is chosen as zero for both items.
The router's two synthetic hydrostatic rows bracket 5 t at drafts 1 and
2 m, with constant KM 5 m; they are interpolation inputs, not hull data.
The five-point GZ fixture uses angles 0, 10, 20, 30, 40 deg and arms
0, 1, 2, 3, 4 m solely to exercise every array position.
"""

import math
import re

import pytest

from digitalmodel.naval_architecture.vessel_stability_screening import (
    GZCurve,
    WeightItem,
    build_loading_condition,
)
from digitalmodel.vessel_stability_screening.workflow import router


NONFINITE_VALUES = (math.nan, math.inf, -math.inf)
WEIGHT_FIELDS = ("weight_t", "vcg_m", "lcg_m", "fsm_t_m")
HEEL_DEG = (0.0, 10.0, 20.0, 30.0, 40.0)
GZ_M = (0.0, 1.0, 2.0, 3.0, 4.0)
CURVE_COORDINATES = tuple(
    (field, position)
    for field in ("heel_deg", "gz_m")
    for position in range(len(HEEL_DEG))
)


def _weight_inputs():
    return [
        {
            "name": "first",
            "weight_t": 2.0,
            "vcg_m": 1.0,
            "lcg_m": 0.0,
            "fsm_t_m": 5.0,
        },
        {
            "name": "second",
            "weight_t": 3.0,
            "vcg_m": 4.0,
            "lcg_m": 0.0,
            "fsm_t_m": 0.0,
        },
    ]


@pytest.mark.parametrize("value", NONFINITE_VALUES)
@pytest.mark.parametrize("field", WEIGHT_FIELDS)
def test_weight_item_rejects_nonfinite_fields(field, value):
    values = _weight_inputs()[0]
    values[field] = value
    message = f"weight item 'first': {field} must be finite"
    with pytest.raises(ValueError, match=rf"^{re.escape(message)}$"):
        WeightItem(**values)


@pytest.mark.parametrize("value", NONFINITE_VALUES)
@pytest.mark.parametrize(("field", "position"), CURVE_COORDINATES)
def test_gz_curve_rejects_nonfinite_coordinates(field, position, value):
    coordinates = {"heel_deg": list(HEEL_DEG), "gz_m": list(GZ_M)}
    coordinates[field][position] = value
    message = f"GZ curve: {field}[{position}] must be finite"
    with pytest.raises(ValueError, match=rf"^{re.escape(message)}$"):
        GZCurve(**{key: tuple(values) for key, values in coordinates.items()})


def test_finite_loading_condition_matches_hand_sums():
    """Products/sums are exact small integers; KG division rounds once.

    FSC is exact; adding it to KG rounds once more. Their combined error
    is at most one ULP at fluid KG. Use one ULP and no relative tolerance
    for each hand result, rather than a decimal engineering tolerance.
    """
    condition = build_loading_condition(
        "arithmetic", [WeightItem(**values) for values in _weight_inputs()]
    )
    for field, expected in (
        ("displacement_t", 5.0),
        ("kg_m", 2.8),
        ("fsm_total_t_m", 5.0),
        ("fsc_m", 1.0),
        ("kg_fluid_m", 3.8),
    ):
        assert getattr(condition, field) == pytest.approx(
            expected, rel=0.0, abs=math.ulp(expected)
        )


def test_finite_negative_coordinates_and_arms_remain_supported():
    """Synthetic signs: one 2 t item at VCG -1 m, LCG -3 m, FSM zero.

    With one item, KG = -1 and LCG = -3 exactly. Arms 0, -1, -2 m at
    0, 20, 40 deg give GZ(10) = -0.5 m by midpoint interpolation.
    """
    item = WeightItem("negative coordinates", 2.0, -1.0, -3.0)
    condition = build_loading_condition("negative coordinates", [item])
    assert condition.kg_m == -1.0
    assert condition.lcg_m == -3.0
    assert condition.kg_fluid_m == -1.0
    curve = GZCurve(heel_deg=(0.0, 20.0, 40.0), gz_m=(0.0, -1.0, -2.0))
    assert curve.gz_m == (0.0, -1.0, -2.0)
    assert curve.gz_at(10.0) == -0.5


def _workflow_config(tmp_path):
    return {
        "basename": "vessel_stability_screening",
        "vessel_stability_screening": {
            "hydrostatics": {
                "table": [
                    {"draft_m": 1.0, "displacement_t": 2.0, "km_m": 5.0},
                    {"draft_m": 2.0, "displacement_t": 8.0, "km_m": 5.0},
                ]
            },
            "loading_condition": {"name": "arithmetic", "items": _weight_inputs()},
            "gz": {"heel_deg": list(HEEL_DEG), "gz_m": list(GZ_M)},
            "output_dir": str(tmp_path / "results"),
        },
        "_config_dir_path": str(tmp_path),
    }


WORKFLOW_COORDINATES = tuple(
    ("loading_condition", field, 0) for field in WEIGHT_FIELDS
) + tuple(("gz", field, position) for field, position in CURVE_COORDINATES)


@pytest.mark.parametrize("value", NONFINITE_VALUES)
@pytest.mark.parametrize(("section", "field", "position"), WORKFLOW_COORDINATES)
def test_workflow_propagates_nonfinite_rejection(
    tmp_path, section, field, position, value
):
    cfg = _workflow_config(tmp_path)
    settings = cfg["vessel_stability_screening"]
    if section == "loading_condition":
        settings[section]["items"][position][field] = value
        message = f"weight item 'first': {field} must be finite"
    else:
        settings[section][field][position] = value
        message = f"GZ curve: {field}[{position}] must be finite"

    with pytest.raises(ValueError, match=rf"^{re.escape(message)}$"):
        router(cfg)

    assert "screening_status" not in cfg
    assert "screening_status" not in settings
    assert not (tmp_path / "results").exists()


def test_workflow_accepts_finite_control(tmp_path):
    """The same inline config reaches a verdict and outputs when finite."""
    result = router(_workflow_config(tmp_path))
    assert result["screening_status"] == "pass"
    assert (tmp_path / "results").is_dir()
