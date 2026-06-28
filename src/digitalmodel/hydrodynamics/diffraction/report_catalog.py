"""Standardized per-structure-type diffraction report templates (#282).

The diffraction report *content* (per-DOF RAOs, added mass/damping, hydrostatics,
roll damping, mesh quality) is already rendered by
``report_generator.generate_diffraction_report``. What #282 asks for is the
**standardization layer**: a consistent, acceptance-gated report template per
hull category (barge / ship / FPSO / LNGC / spar / semi-submersible / TLP).

This module supplies that as a catalog of skeleton-first report templates
(:class:`~digitalmodel.reporting.skeleton.ReportSkeleton`, #1021): each structure
type yields the same standard sections, with required vs optional slots tuned to
what matters for that hull form -- ship-shaped hulls require roll critical
damping; column-stabilized hulls (spar/semi/TLP) require heave/pitch natural
periods instead. Analysis fills the slots; completeness is acceptance-gated and
provenance is mandatory (#1019).

Block keys align with the existing diffraction report section keys so a filled
report can be assembled from the same builders.
"""

from __future__ import annotations

from enum import Enum
from typing import List

from digitalmodel.reporting.skeleton import BlockSpec, ReportSkeleton, SectionSpec


class StructureType(str, Enum):
    """Supported OrcaWave hull categories for standardized reporting."""

    BARGE = "barge"
    SHIP = "ship"
    FPSO = "fpso"
    LNGC = "lngc"
    SPAR = "spar"
    SEMISUBMERSIBLE = "semisubmersible"
    TLP = "tlp"


# Ship-shaped hulls: roll critical damping governs; column-stabilized hulls:
# heave/pitch natural periods govern (resonance avoidance).
_SHIP_SHAPED = frozenset(
    {StructureType.BARGE, StructureType.SHIP, StructureType.FPSO, StructureType.LNGC}
)
_COLUMN_STABILIZED = frozenset(
    {StructureType.SPAR, StructureType.SEMISUBMERSIBLE, StructureType.TLP}
)

# Multibody shielding/interaction is a standard consideration for FPSO/LNGC
# (side-by-side offloading) reports.
_MULTIBODY_RELEVANT = frozenset({StructureType.FPSO, StructureType.LNGC})


def list_structure_types() -> List[str]:
    """All supported structure-type values."""
    return [s.value for s in StructureType]


def diffraction_report_skeleton(
    structure_type: StructureType | str,
) -> ReportSkeleton:
    """Standardized diffraction report skeleton for a hull category (#282).

    The same sections appear for every type; required vs optional slots are
    tuned per hull form. Render/check with the skeleton CLI or
    ``ReportSkeleton.build_html`` / ``.completeness``.
    """
    st = StructureType(structure_type)
    ship_shaped = st in _SHIP_SHAPED
    column = st in _COLUMN_STABILIZED

    sections = [
        SectionSpec(
            key="summary",
            label="Summary & Identification",
            blocks=[
                BlockSpec(
                    key="executive_summary",
                    label="Executive summary",
                    required=True,
                ),
                BlockSpec(
                    key="identification",
                    label="Vessel & analysis identification",
                    required=True,
                ),
            ],
        ),
        SectionSpec(
            key="geometry",
            label="Geometry, Mesh & Hydrostatics",
            blocks=[
                BlockSpec(
                    key="mesh_quality",
                    label="Hull description & mesh quality",
                    required=True,
                ),
                BlockSpec(
                    key="stability",
                    label="Hydrostatic properties & stability",
                    required=True,
                ),
            ],
        ),
        SectionSpec(
            key="hydrodynamics",
            label="Hydrodynamic Results",
            blocks=[
                BlockSpec(
                    key="added_mass_diagonal",
                    label="Added mass (diagonal)",
                    required=True,
                ),
                BlockSpec(
                    key="damping_diagonal",
                    label="Radiation damping (diagonal)",
                    required=True,
                ),
                BlockSpec(
                    key="load_raos",
                    label="Wave excitation forces (load RAOs)",
                    required=True,
                ),
                BlockSpec(
                    key="dof_sections",
                    label="Displacement RAOs (per DOF)",
                    required=True,
                ),
                BlockSpec(
                    key="natural_periods",
                    label="Heave/pitch natural periods",
                    required=column,
                    description=(
                        "Resonance avoidance governs column-stabilized hulls."
                    ),
                ),
                BlockSpec(
                    key="roll_damping",
                    label="Roll critical damping analysis",
                    required=ship_shaped,
                    description="Governs ship-shaped hulls.",
                ),
                BlockSpec(
                    key="multibody_interaction",
                    label="Multibody shielding / interaction",
                    required=False,
                    description=(
                        "Side-by-side offloading interaction (FPSO/LNGC)."
                        if st in _MULTIBODY_RELEVANT
                        else "Optional for single-body analyses."
                    ),
                ),
            ],
        ),
        SectionSpec(
            key="quality",
            label="Validation & Assumptions",
            blocks=[
                BlockSpec(
                    key="validation_sanity",
                    label="Validation sanity check",
                    required=True,
                ),
                BlockSpec(
                    key="assumptions",
                    label="Assumption ledger",
                    required=True,
                ),
            ],
        ),
    ]

    return ReportSkeleton(
        title=f"Diffraction analysis report - {st.value}",
        sections=sections,
        require_provenance=True,
    )


# Pre-built catalog: structure-type value -> skeleton factory result.
def report_catalog() -> dict[str, ReportSkeleton]:
    """All standardized per-structure-type diffraction report skeletons."""
    return {s.value: diffraction_report_skeleton(s) for s in StructureType}


__all__ = [
    "StructureType",
    "list_structure_types",
    "diffraction_report_skeleton",
    "report_catalog",
]
