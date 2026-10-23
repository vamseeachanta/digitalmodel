# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic hand-calculation example; no material grade or design acceptance.

Run with PYTHONPATH=src python examples/subsea/jumper_tools/basic_usage.py.
Inputs are chosen for exact arithmetic, not representative engineering data.
Equations and expected answers: docs/domains/subsea/jumper_tools.html.
"""

from dataclasses import asdict
import json

from digitalmodel.subsea.jumper_tools import (
    SectionInputs,
    calculate_catenary_config,
    calculate_section_properties,
    mesh_catenary,
    mesh_polyline,
)


def worked_example() -> dict:
    """Return inputs and results without writing files or invoking a solver."""
    inputs = SectionInputs(
        outer_diameter_m=2.0,
        wall_thickness_m=0.5,
        coating_thickness_m=0.5,
        pipe_density_kg_m3=8.0,
        coating_density_kg_m3=2.0,
        contents_density_kg_m3=4.0,
        water_density_kg_m3=3.0,
        youngs_modulus_pa=100.0,
        gravity_m_s2=10.0,
    )
    section = calculate_section_properties(inputs)
    # Synthetic a=10 m, rise=10 m, available length=20 m, two arc elements.
    catenary_inputs = dict(
        horizontal_tension_n=10.0 * section.submerged_weight_n_m,
        submerged_weight_n_m=section.submerged_weight_n_m,
        rise_m=10.0,
        total_length_m=20.0,
    )
    catenary = calculate_catenary_config(**catenary_inputs)
    return {
        "basis": "synthetic arithmetic example, not design input",
        "section_inputs": asdict(inputs),
        "section": asdict(section),
        "catenary_inputs": catenary_inputs,
        "catenary": asdict(catenary),
        "suspended_mesh": asdict(mesh_catenary(catenary, 2)),
        "polyline_mesh": asdict(
            mesh_polyline([(0, 0, 0), (3, 0, 0), (3, 4, 0)], [1, 1])
        ),
        "spreadsheet_comparison": "NOT_EVALUATED",
        "design_code_checks": "NOT_EVALUATED",
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2))
