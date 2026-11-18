"""Mass basis of the IEA 15 MW / VolturnUS-S model against the published design.

Allen et al. (2020), NREL/TP-5000-76773, section 1 and Table 1, state that at
the 20 m installed draft the unit displaces 20,206 m3 and consists of a
1,263 t tower, a 991 t RNA and a 17,839 t platform (20,093 t), with 6,065 kN
of mooring vertical pretension. Buoyancy then equals weight plus vertical
pretension, so the published design floats at zero heave.

The model's system mass (``basis.yml`` ``system.mass``) is built from the
public model inputs: ElastoDyn PtfmMass, the workbook's Floating Tower
Properties and its RNA row. It differs from Table 1, chiefly through the tower.
``mass_basis`` quantifies the effect on the still-water equilibrium by
repeating it with only the mass replaced by the published value (centre of
mass, inertia, hydrostatics and mooring unchanged).
"""

from __future__ import annotations

import dataclasses

import numpy as np

from digitalmodel.floating_wind.fowt_static_checks import (
    equilibrium,
    model_from_spec,
    mooring_loads,
)

ALLEN = "Allen et al. (2020), NREL/TP-5000-76773"
# Allen et al. (2020) Table 1, metric tonnes converted to kg.
ALLEN_TABLE_1 = {"platform": 17_839_000.0, "tower": 1_263_000.0, "rna": 991_000.0}
# Allen et al. (2020) section 1: mooring vertical pretension at the 20 m draft.
VERTICAL_PRETENSION = 6_065_000.0  # N
PUBLISHED_DRAFT_HEAVE = 0.0  # m; the published unit is in equilibrium at its draft
DRAFT_TOLERANCE = 0.05  # m; the LC0 heave tolerance


def mass_basis(spec: dict, basis: dict) -> dict:
    """Still-water equilibrium at the model's and at the published system mass (SI)."""
    model = model_from_spec(spec)
    published = basis["published"]["system_mass"]["value"]
    still = equilibrium(model)
    at_published = equilibrium(dataclasses.replace(model, mass=published))
    undisplaced = mooring_loads(model, np.zeros(6))[1]
    system = basis["system"]
    components = {
        "platform": system["platform_mass"]["value"],
        "tower": system["tower_mass"]["value"],
        "rna": system["rna_mass"]["value"],
    }
    heave = float(still["position"][2])
    heave_published = float(at_published["position"][2])
    deviation = model.mass / published - 1.0
    return {
        "system_mass": model.mass,
        "published_system_mass": published,
        "deviation": deviation,
        "components": {name: {"model": value, "published": ALLEN_TABLE_1[name]}
                       for name, value in components.items()},
        "published_draft_heave": PUBLISHED_DRAFT_HEAVE,
        "draft_tolerance": DRAFT_TOLERANCE,
        "still_water_heave": heave,
        "still_water_pitch_deg": float(np.degrees(still["position"][4])),
        "still_water_heave_at_published_mass": heave_published,
        "still_water_pitch_deg_at_published_mass": float(np.degrees(at_published["position"][4])),
        "fairlead_tensions_at_published_mass": [line["tension"]
                                                for line in at_published["lines"]],
        "fairlead_tensions_undisplaced": [line["tension"] for line in undisplaced],
        "vertical_pretension": float(sum(line["vertical"] for line in still["lines"])),
        "published_vertical_pretension": VERTICAL_PRETENSION,
        "draft_at_model_mass_within_tolerance": bool(
            abs(heave - PUBLISHED_DRAFT_HEAVE) <= DRAFT_TOLERANCE),
        "draft_at_published_mass_within_tolerance": bool(
            abs(heave_published - PUBLISHED_DRAFT_HEAVE) <= DRAFT_TOLERANCE),
        "disposition": (
            f"The model's system mass is {100.0 * deviation:.2f} % above {ALLEN} Table 1: "
            f"the workbook tower integrates to {components['tower'] / 1e3:,.1f} t against "
            f"{ALLEN_TABLE_1['tower'] / 1e3:,.0f} t, and the workbook RNA row gives "
            f"{components['rna'] / 1e3:,.1f} t against {ALLEN_TABLE_1['rna'] / 1e3:,.0f} t. "
            f"The published design floats at its 20 m draft; the model sinks "
            f"{-heave:.3f} m, outside the {DRAFT_TOLERANCE} m draft tolerance. With only "
            f"the mass set to Table 1 the heave is {heave_published:.3f} m, inside it. "
            "The sinkage is a consequence of the mass basis, not a property of the "
            "published design; the source discrepancy is unresolved."),
    }
