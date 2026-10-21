"""Registry of citation-emitting getters for standards-derived constants.

Pilot scope (#2481 D1): mooring design factors from DNV-OS-E301 and OCIMF-MEG4.

Each getter returns a CitedValue whose citation is validated before return
(fail-closed per #2481 D2).
"""
from __future__ import annotations

from enum import Enum
from pathlib import Path
from typing import Final, Optional

from digitalmodel.citations.schema import (
    Citation,
    CitedValue,
    validate_citation,
)


_PUBLIC_REFERENCES: Final[dict[str, dict[str, str]]] = {
    "NASA-DRAG-EQUATION": {
        "publisher": "NASA",
        "revision": "Public web edition, no numbered edition stated",
        "section": "Drag Equation: drag and dynamic pressure",
        "source_url": "https://www1.grc.nasa.gov/beginners-guide-to-aeronautics/drag-equation/",
        "retrieved_on": "2026-10-20",
        "note": "Dynamic-pressure relation only; no coefficient values adopted.",
    },
    "ORCINA-LINKS-DATA": {
        "publisher": "Orcina",
        "revision": "Public web edition, no numbered edition stated",
        "section": "Links: Data; Tethers and Spring/dampers",
        "source_url": "https://www.orcina.com/webhelp/OrcaFlex/Content/html/Links,Data.htm",
        "retrieved_on": "2026-10-20",
        "note": (
            "Tethers are tension-only; spring/dampers can carry compression. "
            "No stiffness or capacity values adopted."
        ),
    },
    "PASSING-SHIP-METHOD": {
        "publisher": "digitalmodel",
        "revision": "01fc9c59728739b7c961a0a7fc77bb62846ef054",
        "section": "Passing-ship force method note: closed-form kernels and finite-depth images",
        "repository_path": "docs/domains/hydrodynamics/wang-passing-ship-forces.md",
        "retrieved_on": "2026-10-20",
        "note": (
            "Repository note accessed on the stated date; original papers were not newly retrieved "
            "or verified in this change. Reference metadata does not establish hydrodynamic qualification."
        ),
    },
}


def get_public_reference(source_id: str) -> Citation:
    """Return registered report metadata, never a validated calculation source.

    Retrieval/access dates describe the inspected web pages or repository note;
    no network request or wiki resolution is performed by this metadata API.
    Numeric getters below retain their independent fail-closed wiki validation.
    """
    if not isinstance(source_id, str) or source_id not in _PUBLIC_REFERENCES:
        raise ValueError(f"Unknown public reference: {source_id!r}")
    return Citation(code_id=source_id, **_PUBLIC_REFERENCES[source_id])


class MooringCondition(str, Enum):
    INTACT_QUASI_STATIC = "intact-quasi-static"
    DAMAGED_QUASI_STATIC = "damaged-quasi-static"


_DNV_OS_E301_CITATION_TEMPLATE: Final = {
    "code_id": "DNV-OS-E301",
    "publisher": "DNV",
    "revision": "2021-07",
    "wiki_path": "wikis/engineering/wiki/standards/dnv-os-e301.md",
}

_DNV_RP_F103_CITATION_TEMPLATE: Final = {
    "code_id": "dnv-rp-f103",
    "publisher": "DNV",
    "revision": "2010",
    "wiki_path": "wikis/engineering-standards/wiki/standards/dnv-rp-f103.md",
}

_EN400_CITATION_TEMPLATE: Final = {
    "code_id": "EN400",
    "publisher": "USNA",
    "revision": "Summer 2020",
    "wiki_path": "wikis/marine-engineering/wiki/standards/en400.md",
}

# DNV-OS-E301 Section 2.2 design load factors for mooring systems.
# Values are industry-standard and also appear in API RP 2SK Table C-1.
_MOORING_SAFETY_FACTORS: Final[dict[MooringCondition, tuple[float, str]]] = {
    MooringCondition.INTACT_QUASI_STATIC: (1.67, "Section 2.2.3 (intact, quasi-static)"),
    MooringCondition.DAMAGED_QUASI_STATIC: (1.25, "Section 2.2.3 (damaged, quasi-static)"),
}


_DNVGL_ST_E271_CITATION_TEMPLATE: Final = {
    "code_id": "dnvgl-st-e271",
    "publisher": "DNV",
    "revision": "2017",
    "wiki_path": "wikis/marine-engineering/wiki/standards/dnvgl-st-e271.md",
}

# DNVGL-ST-E271 (DNV 2.7-1) offshore-container design factors.
_OFFSHORE_CONTAINER_FACTORS: Final[dict[str, tuple[float, str]]] = {
    "primary_structure_load": (2.5, "Section 4.2.3.1 (FL = 2.5 R g)"),
    "padeye_load": (3.0, "Section 4.2.3.1 (Fp = 3 R g over n-1 pad eyes)"),
    "single_padeye_load": (5.0, "Section 4.2.3.1 (single pad eye Fp = 5 R g)"),
    "allowable_usage_factor": (0.85, "Section 4.2.1 (sigma_e <= 0.85 C; steel C = Re)"),
    "default_sling_angle_to_vertical_deg": (45.0, "Section 4.2.3.1 (v = 45 deg default)"),
}


def get_offshore_container_factor(
    name: str, *, repo_root: Optional[Path] = None
) -> CitedValue:
    """Return a DNVGL-ST-E271 (DNV 2.7-1) offshore-container design factor.

    Fail-closed: raises CitationResolutionError if the cited wiki page is missing
    or its frontmatter no longer matches the citation. When repo_root is None,
    defers to the resolver (LLM_WIKI_PATH precedence chain).
    """
    if name not in _OFFSHORE_CONTAINER_FACTORS:
        raise ValueError(f"Unknown offshore-container factor: {name!r}")
    value, section = _OFFSHORE_CONTAINER_FACTORS[name]
    citation = Citation(
        section=section,
        note=f"DNV 2.7-1 offshore-container design factor: {name}",
        **_DNVGL_ST_E271_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    units = "deg" if name.endswith("_deg") else "dimensionless"
    return CitedValue(value=value, citation=citation, units=units)


def get_mooring_safety_factor(
    condition: MooringCondition, *, repo_root: Optional[Path] = None
) -> CitedValue:
    """Return the DNV-OS-E301 mooring safety factor for the given condition.

    Fail-closed: raises CitationResolutionError if the cited wiki page is missing
    or its frontmatter no longer matches the citation.

    When repo_root is None, defers to the resolver (LLM_WIKI_PATH precedence chain).
    """
    if condition not in _MOORING_SAFETY_FACTORS:
        raise ValueError(f"Unknown mooring condition: {condition!r}")
    value, section = _MOORING_SAFETY_FACTORS[condition]
    citation = Citation(
        section=section,
        note=f"Design factor for {condition.value} condition",
        **_DNV_OS_E301_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")


# DNV-OS-E301 anchor / foundation resistance (geotechnical) safety factor.
_ANCHOR_SAFETY_FACTOR: Final[tuple[float, str]] = (
    1.5, "Section 2 (anchor resistance, geotechnical safety factor)")


def get_anchor_safety_factor(*, repo_root: Optional[Path] = None) -> CitedValue:
    """Return the DNV-OS-E301 anchor/foundation geotechnical safety factor.

    Representative value for screening (the exact partial factor in DNV-OS-E301
    varies with anchor type and consequence class). Fail-closed when the cited
    wiki page is missing/mismatched; defers to the resolver when repo_root is None.
    """
    value, section = _ANCHOR_SAFETY_FACTOR
    citation = Citation(
        section=section,
        note="Foundation/anchor resistance safety factor (screening)",
        **_DNV_OS_E301_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")


def get_dnv_f103_reference(
    section: str, *, note: str = "", repo_root: Optional[Path] = None
) -> CitedValue:
    """Return a validated DNV-RP-F103 reference citation.

    Use for F103 calculations whose numeric values come from multiple standard
    locations instead of one scalar constant. The value is a sentinel; the
    citation is the payload.
    """
    citation = Citation(
        section=section,
        note=note,
        **_DNV_RP_F103_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=1.0, citation=citation, units="reference")


def get_en400_reference(
    section: str, *, note: str = "", repo_root: Optional[Path] = None
) -> CitedValue:
    """Return a validated EN400 reference citation.

    EN400 (USNA *Principles of Ship Performance*, Summer 2020) is the teaching
    source the naval_architecture fundamentals/hydrostatics/stability/resistance
    calcs derive from. The value is a sentinel; the citation is the payload.
    """
    citation = Citation(
        section=section,
        note=note,
        **_EN400_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=1.0, citation=citation, units="reference")


def get_ittc78_transfer_reference(*, repo_root: Optional[Path] = None) -> CitedValue:
    """Validated reference to the simplified ITTC-78 resistance transfer.

    Source: https://ittc.info/media/11796/75-02-03-014.pdf (2021, Revision 05).
    Sections 2.3 and 2.4.1 identify model-test analysis and ship total resistance.
    This getter does not imply that omitted air/appendage corrections are implemented.
    The wiki target must be provisioned by its owner; missing configuration fails closed.
    """
    citation = Citation(
        code_id="ITTC-7.5-02-03-01.4",
        publisher="ITTC",
        revision="2021 Revision 05",
        section="Sections 2.3 and 2.4.1 (model resistance analysis and ship total resistance)",
        wiki_path="wikis/marine-engineering/wiki/standards/ittc-1978-performance-prediction.md",
        source_sibling="generic",
        note="Simplified form-factor resistance transfer; caller-declared allowances only",
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=1.0, citation=citation, units="reference")


def get_pianc_depth_band_reference(*, repo_root: Optional[Path] = None) -> CitedValue:
    """Validate the published PIANC depth-band reference against a wiki record.

    PIANC (1992) attribution is verified in ITTC (2002), Appendix A.1, p. 201,
    and Rzeszutko et al. (2026), section 1. No unverified PIANC clause is claimed.
    A maintainer must provision the wiki target; missing records fail closed.
    """
    citation = Citation(
        code_id="PIANC-DEPTH-BANDS",
        publisher="ITTC",
        revision="2002",
        section="23rd ITTC, volume I, Appendix A.1, p. 201 (PIANC 1992 attribution)",
        wiki_path="wikis/marine-engineering/wiki/standards/pianc-depth-bands.md",
        source_sibling="generic",
        note=("h/T cutoffs 3.0, 1.5, 1.2; equalities assigned locally to shallower class. "
              "https://www.vliz.be/imisdocs/publications/139636.pdf; "
              "Rzeszutko, Zentari, Stachel, el Moctar (2026), Applied Ocean Research "
              "176, 105289, section 1, https://doi.org/10.1016/j.apor.2026.105289"),
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=1.0, citation=citation, units="reference")
