"""Reproduce the newsletter's 9-to-34 F103 bracelet-anode comparison.

Run from an installed checkout:
    python examples/cathodic_protection/run_edition_study.py

Inputs reproduce the public pipeline.yml fixture used by
tests/cathodic_protection/test_engine_adapter.py (F103 edition tests).
No fixture files, sibling repositories or external data are read at runtime.
"""

from __future__ import annotations

import math
import sys

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

# Current module outputs, also pinned independently in test_run_edition_study.py.
EXPECTED_RESULTS = {
    "2010": {"mean_current_a": 1.3279, "required_mass_kg": 218.111, "anode_count": 9},
    "2019": {"mean_current_a": 5.1514, "required_mass_kg": 846.119, "anode_count": 34},
}


def run_study() -> dict[str, dict[str, float | int]]:
    """Hold the pipeline fixed; vary only inputs.design_data.edition."""
    rows = {}
    for edition in ("2010", "2019"):
        cfg = {
            "inputs": {
                "calculation_type": "DNV_RP_F103",
                "design_data": {"design_life": 30, "edition": edition},  # years
                "environment": {"seawater_resistivity_ohm_m": 0.30},
                "pipeline": {
                    "outer_diameter_m": 0.3239,
                    "wall_thickness_m": 0.0159,
                    "length_m": 1500.0,
                    "burial_condition": "non_buried",
                    "internal_fluid_temperature_C": 60.0,
                    "coating_type": "FBE",
                    "resistivity_ohm_m": 0.2e-6,  # steel, not seawater
                },
                "anode": {
                    "material": "aluminium",
                    "utilization_factor": 0.80,
                    "individual_anode_mass_kg": 25.0,  # net alloy mass
                    "length_m": 0.2,
                    "thickness_m": 0.04,
                },
            }
        }
        result = run_cathodic_protection(cfg)["results"]
        rows[edition] = {
            "mean_current_a": result["current_demand_A"]["mean_current_demand_A"],
            "required_mass_kg": result["anode_requirements"]["total_anode_mass_kg"],
            "anode_count": result["anode_requirements"]["anode_count"],
        }
    return rows


def check_results(rows: dict[str, dict[str, float | int]]) -> None:
    """Check every displayed value, including when Python uses -O."""
    if rows.keys() != EXPECTED_RESULTS.keys():
        raise ValueError("Edition study must contain exactly editions 2010 and 2019")
    tolerances = {
        "mean_current_a": 0.00005,
        "required_mass_kg": 0.0005,
        "anode_count": 0.0,
    }
    for edition, expected in EXPECTED_RESULTS.items():
        for field, value in expected.items():
            actual = rows[edition][field]
            if not math.isclose(actual, value, rel_tol=0.0, abs_tol=tolerances[field]):
                raise ValueError(
                    f"{edition} {field}: expected {value}, calculated {actual}"
                )


def main() -> int:
    rows = run_study()
    try:
        check_results(rows)
    except ValueError as exc:
        print(f"Edition study check failed: {exc}", file=sys.stderr)
        return 1
    print("DNV-RP-F103: 1,500 m FBE pipeline, 60 C fluid, 30 years, 25 kg bracelets")
    print(
        f"{'Edition':<9} {'Mean current [A]':>18} {'Required mass [kg]':>20} {'Anodes':>8}"
    )
    for edition, row in rows.items():
        print(
            f"{edition:<9} {row['mean_current_a']:>18.4f} "
            f"{row['required_mass_kg']:>20.3f} {row['anode_count']:>8d}"
        )
    print(
        "Checks passed: calculated values match the regression baseline (9 -> 34 anodes)."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
