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
from pathlib import Path
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

from digitalmodel.reporting.provenance import Provenance
from digitalmodel.reporting.skeleton import (
    BlockSpec,
    Completeness,
    ReportSkeleton,
    SectionSpec,
)

if TYPE_CHECKING:  # avoid importing the heavy reporter at module load
    from digitalmodel.hydrodynamics.diffraction.report_data_models import (
        DiffractionReportData,
    )


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


# ---------------------------------------------------------------------------
# Live wiring: fill skeleton slots from the existing diffraction builders (#282)
# ---------------------------------------------------------------------------

# Skeleton block key -> name of the existing report_generator section builder.
# Builders return "" when their data is absent, so an unfilled slot stays empty
# and completeness() flags it -- the acceptance gate is real, not cosmetic.
_BLOCK_BUILDERS: Dict[str, str] = {
    "executive_summary": "_sec_executive_summary",
    "identification": "_sec_header",
    "mesh_quality": "_sec_hull_description",
    "stability": "_sec_stability",
    "added_mass_diagonal": "_sec_added_mass",
    "damping_diagonal": "_sec_damping",
    "load_raos": "_sec_load_raos",
    "natural_periods": "_sec_natural_periods",
    "roll_damping": "_sec_roll_damping",
    "validation_sanity": "_sec_validation",
    "assumptions": "_sec_assumptions",
}


def build_diffraction_report_content(
    report_data: "DiffractionReportData",
    *,
    assumption_ledger: Any = None,
    validation_report: Optional[dict] = None,
    include_plotlyjs: str = "cdn",
    extra_blocks: Optional[Dict[str, str]] = None,
) -> Dict[str, str]:
    """Map a ``DiffractionReportData`` to skeleton block content via the existing
    diffraction section builders.

    Only non-empty fragments are returned, so a slot whose data is missing stays
    unfilled (and is reported by completeness). ``extra_blocks`` supplies content
    the data-builders don't produce -- e.g. per-DOF RAO plots (``dof_sections``)
    or a ``multibody_interaction`` note -- and also seeds from
    ``report_data.benchmark_html_sections``.
    """
    from digitalmodel.hydrodynamics.diffraction import report_generator as rg

    content: Dict[str, str] = {}
    for block_key, builder_name in _BLOCK_BUILDERS.items():
        builder = getattr(rg, builder_name, None)
        if builder is None:  # pragma: no cover - guards builder renames
            continue
        try:
            fragment = builder(
                report_data,
                include_plotlyjs=include_plotlyjs,
                validation_report=validation_report,
                assumption_ledger=assumption_ledger,
            )
        except Exception:  # pragma: no cover - a bad block must not kill report
            fragment = ""
        if fragment:
            content[block_key] = fragment

    # Per-DOF RAOs and any other benchmark-only fragments come from the data's
    # benchmark sections (e.g. ``dof_sections``); explicit extra_blocks win.
    for key, frag in (report_data.benchmark_html_sections or {}).items():
        if frag and key not in content:
            content[key] = frag
    for key, frag in (extra_blocks or {}).items():
        if frag:
            content[key] = frag
    return content


def render_structured_report(
    structure_type: StructureType | str,
    report_data: "DiffractionReportData",
    *,
    provenance: Provenance,
    assumption_ledger: Any = None,
    validation_report: Optional[dict] = None,
    include_plotlyjs: str = "cdn",
    extra_blocks: Optional[Dict[str, str]] = None,
    output_path: Optional[str | Path] = None,
    css: str = "",
) -> Tuple[str | Path, Completeness]:
    """Render a standardized per-structure-type diffraction report from results.

    Fills the structure-type skeleton from the existing diffraction builders and
    ``report_data``, enforces mandatory provenance, and returns
    ``(html_or_path, completeness)`` so the caller can gate on acceptance (all
    required slots filled for that hull form). Provenance is required (#1019).
    """
    skeleton = diffraction_report_skeleton(structure_type)
    content = build_diffraction_report_content(
        report_data,
        assumption_ledger=assumption_ledger,
        validation_report=validation_report,
        include_plotlyjs=include_plotlyjs,
        extra_blocks=extra_blocks,
    )
    status = skeleton.completeness(content, provenance=provenance)
    out = skeleton.build_html(
        content, provenance=provenance, output_path=output_path, css=css
    )
    return out, status


__all__ = [
    "StructureType",
    "list_structure_types",
    "diffraction_report_skeleton",
    "report_catalog",
    "build_diffraction_report_content",
    "render_structured_report",
]
