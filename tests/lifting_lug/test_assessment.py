# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand-calculated assessment cases; all resistance inputs are synthetic."""

from dataclasses import replace

import pytest

from digitalmodel.lifting_lug.assessment import (
    DirectWeld,
    PadeyeAllowables,
    PlateGeometry,
    SlingLoad,
    StressLimit,
    assess_padeye,
)

BASIS = "Synthetic illustration, assumed, based on previous experience; no rule value"


def example_inputs():
    return (
        PlateGeometry(20, 40, 50, 150, 75),
        SlingLoad(100, 1.5, 1.2, BASIS),
        PadeyeAllowables(
            pin_bearing=StressLimit(300, BASIS),
            net_tension=StressLimit(120, BASIS),
            shear_tearout=StressLimit(100, BASIS),
            weld_direct_shear=StressLimit(100, BASIS),
        ),
        DirectWeld(200, 8),
    )


def test_plate_and_weld_hand_calculation():
    """MIT Stress-Strain Curves (2001), Eq.1; Shear and Torsion (2000), p.2.

    Assumed load: 100*1.5*1.2=180 kN=180000 N. Bearing area 40*20=800;
    net area (150-50)*20=2000; shear area 2*(75-25)*20=2000 mm².
    Demands: 225,90,90 MPa. Assumed allowables: 300,120,100 MPa;
    UCs: 0.75,0.75,0.9. Weld area: 200*8=1600 mm²; tau=112.5 MPa;
    assumed allowable 100 MPa gives UC=1.125 and required throat=
    180000/(200*100)=9 mm. All expected values are independent arithmetic.
    """
    result = assess_padeye(*example_inputs())
    checks = {check.name: check for check in result.checks}
    assert result.design_load_kn == pytest.approx(180)
    for name, demand, utilization in (
        ("pin_bearing", 225, 0.75),
        ("net_tension", 90, 0.75),
        ("shear_tearout", 90, 0.9),
        ("weld_direct_shear", 112.5, 1.125),
    ):
        assert checks[name].demand_mpa == pytest.approx(demand)
        assert checks[name].utilization == pytest.approx(utilization)
        assert checks[name].allowable_source == BASIS
        assert checks[name].method_source
    assert checks["weld_direct_shear"].status == "FAIL"
    assert result.required_weld_throat_mm == pytest.approx(9)
    assert result.screening_status == result.status == "FAIL"


def test_resized_weld_at_exact_limit_and_rule_gaps():
    """Independent sizing above gives a=9 and tau=180000/(200*9)=100 MPa."""
    plate, load, limits, _ = example_inputs()
    result = assess_padeye(plate, load, limits, DirectWeld(200, 9))
    assert result.screening_status == "PASS"
    assert result.status == "NOT_EVALUATED"
    assert {c.name for c in result.qualification_checks} == {
        "abs_compliance", "lifting_qualification"
    }
    assert all(c.status == "NOT_EVALUATED" for c in result.qualification_checks)
    assert "13.3.2.4" in result.qualification_checks[1].method_source


def test_absence_of_limits_preserves_demands_without_pass():
    plate, load, _, weld = example_inputs()
    result = assess_padeye(plate, load, PadeyeAllowables(), weld)
    assert all(c.status == "NOT_EVALUATED" for c in result.checks)
    assert all(c.utilization is None for c in result.checks)
    assert all(c.allowable_mpa is None for c in result.checks)
    assert result.checks[0].demand_mpa == pytest.approx(225)
    assert result.required_weld_throat_mm is None
    assert result.screening_status == result.status == "NOT_EVALUATED"


def test_missing_weld_geometry_is_not_a_pass():
    plate, load, limits, _ = example_inputs()
    result = assess_padeye(plate, load, limits)
    assert result.checks[-1].demand_mpa is None
    assert result.checks[-1].status == "NOT_EVALUATED"
    assert result.required_weld_throat_mm is None
    assert result.screening_status == "NOT_EVALUATED"


def test_failure_has_priority_over_missing_checks():
    plate, load, _, _ = example_inputs()
    result = assess_padeye(
        plate, load, PadeyeAllowables(pin_bearing=StressLimit(100, BASIS))
    )
    assert result.screening_status == result.status == "FAIL"


def test_load_scaling_and_unloaded_case():
    """Force/area and sizing are linear: doubled P doubles demands and throat."""
    plate, load, limits, weld = example_inputs()
    doubled = assess_padeye(plate, replace(load, static_load_kn=200), limits, weld)
    assert doubled.checks[0].demand_mpa == pytest.approx(450)
    assert doubled.required_weld_throat_mm == pytest.approx(18)
    unloaded = assess_padeye(plate, replace(load, static_load_kn=0), limits, weld)
    assert all(c.demand_mpa == 0 for c in unloaded.checks)
    assert unloaded.required_weld_throat_mm == 0
    assert unloaded.status == "NOT_EVALUATED"


@pytest.mark.parametrize("factory", [
    lambda: SlingLoad(-1, 1, 1, BASIS),
    lambda: SlingLoad(1, 0, 1, BASIS),
    lambda: SlingLoad(1, 1, float("nan"), BASIS),
    lambda: SlingLoad(1, 1, 1, "  "),
    lambda: StressLimit(0, BASIS),
    lambda: StressLimit(float("inf"), BASIS),
    lambda: StressLimit(100, ""),
    lambda: DirectWeld(0, 8),
    lambda: DirectWeld(200, -1),
    lambda: DirectWeld(float("nan"), 8),
    lambda: PlateGeometry(20, 60, 50, 150, 75),
    lambda: PlateGeometry(20, 40, 50, 150, 25),
])
def test_invalid_inputs_rejected(factory):
    with pytest.raises(ValueError):
        factory()


def test_derived_load_overflow_rejected():
    plate, _, limits, weld = example_inputs()
    with pytest.raises(ValueError, match="finite"):
        assess_padeye(plate, SlingLoad(1e308, 10, 10, BASIS), limits, weld)
