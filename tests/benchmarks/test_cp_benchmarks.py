"""Performance benchmarks for CathodicProtection.router — all 4 standard methods.

Each bench function exercises one CP route with representative synthetic inputs.
Run with:
    cd digitalmodel && PYTHONPATH=src uv run python -m pytest \
        tests/benchmarks/test_cp_benchmarks.py --benchmark-only -q
"""

import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import (
    CathodicProtection,
)

# Shared structure inputs representative of a mid-size offshore structure
_STRUCTURE = {
    "steel_total_area": 800.0,
    "area_coverage": 100.0,
    "coating_initial_breakdown_factor": 2.0,
    "coating_initial_breakdown_duration": 2.0,
    "coating_yearly_breakdown_factor": 3.0,
    "coating_breakdown_factor_max": 2.0,
}
_DESIGN_CURRENT = {
    "coated_steel_mA_m2": 13.5,
    "uncoated_steel_mA_m2": 200.0,
}
_ANODE_LONG_FLUSH = {
    "material": "aluminium",
    "protection_potential": 0.8,
    "closed_circuit_anode_potential": -1.09,
    "anode_Utilisation_factor": 0.825,
    "physical_properties": {"net_weight": 29.0},
    "geometry": {"type": "long_flush", "length_m": 0.65, "width_m": 0.125},
}


def _abs_ships_cfg():
    return {
        "inputs": {
            "calculation_type": "ABS_gn_ships_2018",
            "design_data": {"design_life": 5, "seawater_max_temperature": 20},
            "environment": {"seawater": {"resistivity": {"input": 0.2547}}},
            "structure": _STRUCTURE,
            "design_current": _DESIGN_CURRENT,
            "anode": _ANODE_LONG_FLUSH,
        }
    }


def test_bench_cp_abs_gn_ships(benchmark):
    """Benchmark ABS_gn_ships_2018 cathodic protection route."""
    cp = CathodicProtection()
    result = benchmark(lambda: cp.router(_abs_ships_cfg()))
    assert "cathodic_protection" in result or result is not None


def test_bench_cp_dnv_rp_f103(benchmark):
    """Benchmark DNV_RP_F103_2010 cathodic protection route (pipeline)."""
    cfg = {
        "inputs": {
            "calculation_type": "DNV_RP_F103_2010",
            "design_data": {"design_life": 25},
            "environment": {
                "seawater": {"resistivity": {"input": 0.3}},
                "temperature_deg_c": 5.0,
            },
            "pipeline": {
                "outer_diameter_m": 0.3239,
                "wall_thickness_m": 0.0127,
                "length_m": 1000.0,
                "coating_breakdown_initial": 0.05,
                "coating_breakdown_final": 0.2,
            },
            "anode": {
                "material": "aluminium",
                "anode_Utilisation_factor": 0.8,
                "physical_properties": {"net_weight": 25.0},
                "geometry": {"type": "bracelet", "length_m": 0.3, "width_m": 0.3},
                "closed_circuit_anode_potential": -1.05,
                "protection_potential": 0.8,
            },
        }
    }
    cp = CathodicProtection()
    result = benchmark(lambda: cp.router(cfg))
    assert result is not None


def test_bench_cp_abs_gn_offshore(benchmark):
    """Benchmark ABS_gn_offshore_2018 cathodic protection route."""
    cfg = _abs_ships_cfg()
    cfg["inputs"]["calculation_type"] = "ABS_gn_offshore_2018"
    cp = CathodicProtection()
    result = benchmark(lambda: cp.router(cfg))
    assert result is not None


def test_bench_cp_dnv_rp_b401(benchmark):
    """Benchmark DNV_RP_B401_offshore cathodic protection route."""
    cfg = _abs_ships_cfg()
    cfg["inputs"]["calculation_type"] = "DNV_RP_B401_offshore"
    cp = CathodicProtection()
    result = benchmark(lambda: cp.router(cfg))
    assert result is not None
