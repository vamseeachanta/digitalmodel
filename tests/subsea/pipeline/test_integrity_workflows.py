# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Registry adapter checks; buckling physics is outside the routing doubles."""

from unittest.mock import patch

import pytest

from digitalmodel.subsea.pipeline.integrity_workflows import (
    run_lateral_buckling,
    run_on_bottom_stability,
    run_upheaval_buckling,
)


@pytest.fixture
def on_bottom_input():
    """Synthetic bare-pipe case from test_on_bottom_stability.py scenario 1.

    Densities and hydrodynamic coefficients are explicit test inputs, not
    adopted design recommendations. Acceleration is zero to isolate drag.
    """
    return {
        "on_bottom_stability": {
            "pipe": {
                "od_steel_m": 0.3239,
                "wt_steel_m": 0.0127,
                "coating_thickness_m": 0.0,
                "rho_steel": 7850.0,
                "rho_coating": 0.0,
                "rho_contents": 0.0,
                "rho_seawater": 1025.0,
            },
            "metocean": {"water_velocity_m_s": 1.5, "water_acceleration_m_s2": 0.0},
            "hydrodynamic": {"cd": 0.7, "cm": 1.0, "cl": 0.9},
            "soil": {"friction_coefficient": 0.5},
            "safety_factor": 1.0,
        }
    }


def test_on_bottom_workflow_result_keys_and_independent_drag(on_bottom_input):
    """Existing oracle: test_on_bottom_stability.py::TestDragForce::test_scenario_1.

    Hand reduction: .5*.7*1025*.3239*1.5²=261.44803125 N/m.
    The independent test accepts 261.2 N/m with relative tolerance .01.
    That legacy comparator is retained alongside the more precise hand value
    (absolute tolerance 1e-8 N/m for floating-point arithmetic).
    The unavailable legacy import in that test does not
    supply the result here: the real workflow and load calculator execute.
    """
    returned = run_on_bottom_stability(on_bottom_input)
    assert returned is on_bottom_input
    assert set(returned) == {"on_bottom_stability"}
    result = returned["on_bottom_stability"]["result"]
    assert set(result) == {
        "od_total_m",
        "submerged_weight_N_m",
        "buoyancy_N_m",
        "dry_weight_N_m",
        "horizontal_load_N_m",
        "drag_load_N_m",
        "inertia_load_N_m",
        "lift_load_N_m",
        "required_submerged_weight_N_m",
        "lateral_utilization",
        "vertical_utilization",
        "is_laterally_stable",
        "is_vertically_stable",
        "standard",
    }
    assert result["drag_load_N_m"] == pytest.approx(261.2, rel=0.01)
    assert result["drag_load_N_m"] == pytest.approx(261.44803125, abs=1e-8)


@pytest.mark.parametrize("missing", ["on_bottom_stability", "pipe"])
def test_on_bottom_workflow_refuses_missing_input(on_bottom_input, missing):
    if missing == "on_bottom_stability":
        del on_bottom_input[missing]
    else:
        del on_bottom_input["on_bottom_stability"][missing]
    with pytest.raises(KeyError, match=missing):
        run_on_bottom_stability(on_bottom_input)


@pytest.mark.parametrize(
    "runner,name",
    [
        (run_lateral_buckling, "lateral_buckling"),
        (run_upheaval_buckling, "upheaval_buckling"),
    ],
)
@pytest.mark.parametrize("calculation", [None, {"name": "old", "option": "retained"}])
def test_buckling_workflow_dispatch_preserves_mapping(runner, name, calculation):
    """Check adapter routing only; no numerical result is invented by a double.

    Independent numeric acceptance for the legacy router is NOT_EVALUATED:
    the existing pipeline buckling tests mock the engine and provide no
    independent numeric reference for this interface. The separate analytical
    buckling implementation is not the calculator dispatched by these adapters.
    """
    cfg = {"pipeline": {"input": "retained"}}
    if calculation is not None:
        cfg["calculation"] = calculation.copy()
    with patch(
        "digitalmodel.subsea.pipeline.pipeline.Pipeline.router", autospec=True
    ) as router:
        router.return_value = cfg
        result = runner(cfg)
    assert result is cfg
    assert set(result) == {"pipeline", "calculation"}
    assert result["pipeline"] == {"input": "retained"}
    assert result["calculation"]["name"] == name
    if calculation is not None:
        assert result["calculation"]["option"] == "retained"
    router.assert_called_once()
    assert router.call_args.args[1] is cfg


@pytest.mark.parametrize("runner", [run_lateral_buckling, run_upheaval_buckling])
def test_buckling_workflow_real_router_refuses_missing_input(runner):
    """The real router must refuse a missing pipeline before computation/output."""
    with pytest.raises(KeyError, match="pipeline"):
        runner({})
