"""Synthetic safety-case inventory: inputs, real catenary, QRA and CSV exchange.

Run with the repository dependencies installed and PYTHONPATH=src.
All scenario numbers below are assumed arithmetic examples, not operating data.
Method: IMO MSC-MEPC.2/Circ.12/Rev.2 (2018), appendix 3 section 2.2 and
appendix 5 section 3.2.3. Expected results are 0.01 and 0.1 fatalities/year;
the incomplete collision scenario remains NOT_EVALUATED.
"""

import io

from digitalmodel.asset_integrity.safety_case import (
    MAHRegister,
    VesselCollisionInputs,
    assess_mooring_failure,
    assess_vessel_collision,
)
from digitalmodel.orcaflex.mooring_design import solve_catenary


def build_register() -> MAHRegister:
    """Create two evaluated entries and one explicitly incomplete scenario."""
    # Synthetic geometry/load assumptions; solve_catenary documents the
    # Sea Loads on Ships and Offshore Structures (1990), chapter 6 catenary
    # method. T = H + w*h = 200 + 1*100.
    design = solve_catenary(
        water_depth=100,
        line_length=500,
        submerged_weight_per_m=1000,
        pretension=300,
    )
    mooring = assess_mooring_failure(
        design,
        hazard_id="line-break",
        description="Single line break and loss of station",
        # Synthetic assumptions: 0.02 * 0.5 = 0.01 fatalities/year.
        frequency_per_year=0.02,
        fatalities_per_event=0.5,
        frequency_basis="Synthetic assumed rate for one line-break scenario",
        consequence_basis="Synthetic assumed conditional mean fatalities",
        barriers=("Inspection", "Exclusion zone"),
        responsible_roles=("Integrity engineer",),
    )
    # Synthetic assumptions: 1000 * 0.0002 * 0.5 = 0.1 fatalities/year.
    traffic = VesselCollisionInputs(
        transits_per_year=1000,
        collision_probability_per_transit=0.0002,
        fatalities_per_collision=0.5,
        traffic_basis="Synthetic assumed transit exposure",
        probability_basis="Synthetic assumed collision probability per transit",
        consequence_basis="Synthetic assumed conditional mean fatalities",
    )
    collision = assess_vessel_collision(
        traffic,
        hazard_id="passing-collision",
        description="Passing traffic collision",
        barriers=("Traffic control",),
        responsible_roles=("Marine operations",),
    )
    incomplete_traffic = VesselCollisionInputs(
        transits_per_year=1000,
        collision_probability_per_transit=None,
        fatalities_per_collision=None,
        traffic_basis="Synthetic assumed transit exposure",
        probability_basis="NOT_EVALUATED: traffic causation model unavailable",
        consequence_basis="NOT_EVALUATED: collision consequence model unavailable",
    )
    incomplete = assess_vessel_collision(
        incomplete_traffic,
        hazard_id="alternative-traffic",
        description="Alternative scenario with incomplete collision assessment",
        barriers=(),
        responsible_roles=("Marine operations",),
    )
    # Alternative scenarios can overlap: do not sum these entries into a total.
    register = MAHRegister(
        hazards=(mooring.hazard, collision.hazard, incomplete.hazard)
    )
    return MAHRegister.from_csv(io.StringIO(register.to_csv()))


def main() -> None:
    """Print the hand-checkable answers without creating output files."""
    for hazard in build_register().hazards:
        value = hazard.expected_fatalities_per_year
        answer = "unknown" if value is None else f"{value:.1e}"
        print(f"{hazard.hazard_id}: {hazard.status.value}; fatalities/year={answer}")


if __name__ == "__main__":
    main()
