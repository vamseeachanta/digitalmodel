"""Show coupled Gibbs provenance with an independent static equilibrium case.

Run with PYTHONPATH=src python examples/domains/artificial_lift/dynacard_load_basis.py.
All dimensions and surface samples below are chosen synthetic inputs.
The hand calculation is documented in docs/domains/artificial_lift/load-basis.html.
"""

import json
from math import pi, sqrt

from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import DynacardPhysicsSolver


def worked_example():
    """Return chosen inputs, recorded method and rounded arithmetic answers."""
    context = DynacardAnalysisContext(
        api14="SYNTHETIC",
        surface_card=CardData(
            position=[0.0] * 5,
            load=[1000.0] * 5,
        ),
        # Circle geometry: d=sqrt(4/pi) in gives a chosen area of 1 in^2.
        rod_string=[RodSection(
            diameter=sqrt(4.0 / pi), length=100.0, density=144.0,
            weight_per_foot=1.0, modulus_of_elasticity=1440.0,
            speed_of_sound=60.0, damping_factor=0.0,
        )],
        pump=PumpProperties(diameter=1.0, depth=100.0),
        spm=1.0,
        fluid_density=72.0,
    )
    result = DynacardPhysicsSolver(context).solve_wave_equation()
    basis = result.downhole_load_basis.model_dump()
    # Display rounding is not an engineering acceptance tolerance.
    basis["buoyant_rod_weight_lbf"] = round(basis["buoyant_rod_weight_lbf"], 1)
    return {
        "inputs": {
            "rod_length_ft": context.rod_string[0].length,
            "rod_area_in2": 1.0,  # Chosen geometry above.
            "rod_density_lb_ft3": context.rod_string[0].density,
            "rod_weight_per_foot_lbf": context.rod_string[0].weight_per_foot,
            "modulus_psi": context.rod_string[0].modulus_of_elasticity,
            "wave_speed_ft_s": context.rod_string[0].speed_of_sound,
            "damping_rate_s_inv": context.rod_string[0].damping_factor,
            "fluid_density_lb_ft3": context.fluid_density,
            "surface_position_in": context.surface_card.position,
            "surface_load_lbf": context.surface_card.load,
            "pump_diameter_in": context.pump.diameter,
            "pump_depth_ft": context.pump.depth,
            "spm": context.spm,
        },
        "method": basis,
        "answer_load_lbf": [round(value, 1) for value in result.downhole_card.load],
        "answer_position_in": [round(value, 1) for value in result.downhole_card.position],
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2))
