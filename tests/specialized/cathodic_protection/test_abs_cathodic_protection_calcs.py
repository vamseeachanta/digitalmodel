# Standard library imports
import math

# Third party imports
import pytest  # noqa

# Reader imports
from digitalmodel.infrastructure.common.cathodic_protection import CathodicProtection


def test_abs_anode_performance_long_flush():
    cfg = {
        "inputs": {
            "calculation_type": "ABS_gn_ships_2018",
            "design_data": {"design_life": 5, "seawater_max_temperature": 20},
            "environment": {"seawater": {"resistivity": {"input": 0.2547}}},
            "structure": {
                "steel_total_area": 1000.0,
                "area_coverage": 100.0,
                "coating_initial_breakdown_factor": 2.0,
                "coating_initial_breakdown_duration": 2.0,
                "coating_yearly_breakdown_factor": 3.0,
                "coating_breakdown_factor_max": 2.0,
            },
            "design_current": {
                "coated_steel_mA_m2": 13.5,
                "uncoated_steel_mA_m2": 200.0,
            },
            "anode": {
                "material": "aluminium",
                "protection_potential": 0.8,
                "closed_circuit_anode_potential": -1.09,
                "anode_Utilisation_factor": 0.825,
                "physical_properties": {"net_weight": 29.0},
                "geometry": {
                    "type": "long_flush",
                    "length_m": 0.65,
                    "width_m": 0.125,
                },
            },
        }
    }

    cp = CathodicProtection()
    cp.ABS_gn_ships_2018(cfg)

    results = cfg["cathodic_protection"]
    breakdown = results["coating_breakdown_factors"]
    demand = results["current_demand_A"]["totals"]
    performance = results["anode_performance"]

    assert breakdown["final_factor"] <= 2.0
    assert demand["mean"] > 0.0
    assert performance["resistance_ohm"]["initial"] is not None
    assert performance["resistance_ohm"]["initial"] > 0.0
    assert performance["current_output_A"]["initial_total"] is not None
    assert performance["current_output_A"]["initial_total"] > 0.0
    assert math.isclose(performance["driving_voltage_V"], 0.29, abs_tol=0.05)
