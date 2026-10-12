"""Literal, hand-calculated effective-area regressions for issue 2105."""

import math

import pytest

from digitalmodel.geotechnical.mudmat import mudmat_bearing_capacity


def calculate(**overrides):
    inputs = dict(
        width_b_m=3.0,
        length_l_m=4.0,
        embedment_depth_m=0.0,
        condition="undrained",
        submerged_unit_weight_kn_m3=8.0,
        vertical_load_kn=800.0,
        moment_knm=400.0,
        undrained_shear_strength_kpa=25.0,
    )
    return mudmat_bearing_capacity(**(inputs | overrides))


@pytest.mark.parametrize(
    "axis,b_eff,l_eff,area",
    [
        ("B", 2.0, 4.0, 8.0),  # e=400/800=0.5; (3-1)*4=8 m2
        ("L", 3.0, 3.0, 9.0),  # e=400/800=0.5; 3*(4-1)=9 m2
    ],
)
@pytest.mark.parametrize("moment", [400.0, -400.0])
def test_eccentricity_reduces_selected_plan_axis(axis, b_eff, l_eff, area, moment):
    result = calculate(eccentricity_axis=axis, moment_knm=moment)
    assert result.effective_width_m == pytest.approx(b_eff)
    assert result.effective_length_m == pytest.approx(l_eff)
    assert result.effective_area_m2 == pytest.approx(area)
    expected_q = 25.0 * (2.0 + math.pi) * (1.0 + 0.2 * b_eff / l_eff)
    assert result.vertical_capacity_kn == pytest.approx(expected_q * area)
    assert result.sliding_capacity_kn == pytest.approx(25.0 * area)
    dimension = "width" if axis == "B" else "length"
    assert result.notes == [f"effective {dimension} reduced for eccentricity e=0.500 m"]


def test_default_axis_preserves_width_eccentricity():
    assert calculate().effective_area_m2 == pytest.approx(8.0)


def test_length_eccentricity_can_exceed_half_original_width():
    # e=1280/800=1.6: L_eff=0.8, area=3*0.8=2.4 m2.
    result = calculate(eccentricity_axis="L", moment_knm=1280.0)
    assert result.effective_width_m == pytest.approx(3.0)
    assert result.effective_length_m == pytest.approx(0.8)
    assert result.effective_area_m2 == pytest.approx(2.4)
    assert result.shape_factors["sc"] == pytest.approx(1.0 + 0.2 * 0.8 / 3.0)


def test_drained_embedded_length_reduction_uses_shorter_effective_dimension():
    # B'=0.8, L'=3, A'=2.4; D/B'=0.5 and B'/L'=4/15.
    # At phi=30: Nq=3*exp(pi/sqrt(3)), Ngamma=1.5*(Nq-1)/sqrt(3).
    # sq=1+(4/15)/sqrt(3), sgamma=1-0.4*(4/15), dq=1+1/(4*sqrt(3)).
    # q=3.2*Nq*sq*dq + 3.2*Ngamma*sgamma, Vult=2.4*q.
    result = calculate(
        eccentricity_axis="L",
        moment_knm=1280.0,
        condition="drained",
        friction_angle_deg=30.0,
        embedment_depth_m=0.4,
    )
    assert result.shape_factors["sq"] == pytest.approx(1.1539600717839003)
    assert result.shape_factors["sgamma"] == pytest.approx(0.8933333333333333)
    assert result.depth_factors["dq"] == pytest.approx(1.1443375672974065)
    assert result.q_ult_kpa == pytest.approx(120.83652620892912)
    assert result.vertical_capacity_kn == pytest.approx(290.00766290142985)


@pytest.mark.parametrize("axis,moment", [("B", 1200.0), ("L", 1600.0)])
def test_overturning_limit_uses_selected_axis(axis, moment):
    with pytest.raises(ValueError, match=f"{axis} - 2e <= 0"):
        calculate(eccentricity_axis=axis, moment_knm=moment)


@pytest.mark.parametrize("axis", ["X", "b", "", None])
def test_invalid_eccentricity_axis_is_rejected(axis):
    with pytest.raises(ValueError, match="eccentricity_axis"):
        calculate(eccentricity_axis=axis)


def test_original_plan_dimension_order_is_still_required():
    with pytest.raises(ValueError, match="B <= L"):
        calculate(width_b_m=4.0, length_l_m=3.0, eccentricity_axis="L")


def test_configuration_workflow_passes_length_axis(tmp_path):
    from digitalmodel.geotechnical.cfg_workflows import MudmatBearingCapacityWorkflow

    cfg = {
        "_config_dir_path": str(tmp_path),
        "mudmat_bearing_capacity": {
            "foundation": {"width_B_m": 3.0, "length_L_m": 4.0},
            "soil": {
                "condition": "undrained",
                "submerged_unit_weight_kN_m3": 8.0,
                "undrained_shear_strength_kpa": 25.0,
            },
            "loads": {
                "vertical_kN": 800.0,
                "moment_kNm": 400.0,
                "eccentricity_axis": "L",
            },
        },
    }
    result = MudmatBearingCapacityWorkflow().router(cfg)
    assert result["mudmat_bearing_capacity"]["result"]["effective_area_m2"] == 9.0
