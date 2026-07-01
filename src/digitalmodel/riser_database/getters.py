"""Citation-emitting getters for riser design factors (#1245 / #1199a).

Pattern copied from the DNV-OS-E301 mooring pilot (``citations/registry.py``):
scalar standards-derived design factors live here as code constants; each
getter builds a :class:`Citation` from a template, validates it fail-closed,
and returns a :class:`CitedValue`. Licensed table CONTENT never lives in this
public repo — only these well-known scalar factors plus clause identifiers,
with values resolving against the private llm-wiki.

Provenance discipline (plan D6): a getter only cites the standard the code
actually sources the value from. That is why this slice ships exactly two
getters — the API RP 16Q-attributed factors (top-tension SF) are deferred to
#1246 behind an llm-wiki frontmatter fix, and the Barlow wall-thickness safety
factor carries no standards citation at all (generic formula, project default).

Failure modes (plan D3):

* Direct getter calls are ALWAYS fail-closed — a missing/mismatched wiki page
  raises :class:`CitationResolutionError` carrying the ``code_id``.
* The consumer helper :func:`riser_citations` degrades standalone
  (``pip install digitalmodel`` with no wiki): one-shot ``RuntimeWarning``,
  returns ``{}`` — mirroring ``mooring_resilience.screening``.
"""
from __future__ import annotations

import warnings
from pathlib import Path
from typing import Final, Optional

from digitalmodel.citations.schema import (
    Citation,
    CitationResolutionError,
    CitedValue,
    validate_citation,
)

_DNV_OS_F201_CITATION_TEMPLATE: Final = {
    "code_id": "dnv-os-f201",
    "publisher": "DNV",
    "revision": "2010",
    "wiki_path": "wikis/engineering-standards/wiki/standards/dnv-os-f201.md",
}

_DNV_RP_C203_CITATION_TEMPLATE: Final = {
    "code_id": "dnv-rp-c203",
    "publisher": "DNV",
    "revision": "2024-10",
    "wiki_path": "wikis/engineering-standards/wiki/standards/dnv-rp-c203.md",
}

#: Matches riser_fatigue/touchdown.py TouchdownFatigueInput.dff — the live
#: riser literal these getters must stay in parity with (#1246 wires the calc).
RISER_DFF_DEFAULT: Final[float] = 10.0
#: Matches riser_fatigue touchdown.py scf default and workflow.py wave scf
#: default: 1.0 = no stress concentration applied (neutral default).
RISER_SCF_DEFAULT: Final[float] = 1.0


def get_riser_dff(
    *, override: Optional[float] = None, repo_root: Optional[Path] = None
) -> CitedValue:
    """Riser design fatigue factor (DNV-OS-F201): allowable damage = 1/DFF.

    ``override`` wins when provided (user-override-wins defense); the citation
    note then records both the override and the standard value.
    """
    value = RISER_DFF_DEFAULT
    note = "Design fatigue factor for riser fatigue verdicts (allowable damage = 1/DFF)"
    if override is not None:
        value = float(override)
        note = f"user override; standard value {RISER_DFF_DEFAULT}"
    citation = Citation(
        section="Sec.5 (design fatigue factors)",
        note=note,
        **_DNV_OS_F201_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")


def get_riser_scf(
    *, override: Optional[float] = None, repo_root: Optional[Path] = None
) -> CitedValue:
    """Riser stress concentration factor default (DNV-RP-C203 methodology).

    The standard sources the METHODOLOGY (SCF applied to nominal stress ranges
    before S-N lookup); 1.0 is the neutral no-concentration default matching
    the live riser_fatigue defaults, not a C203-tabulated value. The code
    implements the 2021 edition tables while the wiki page tracks 2024-10 —
    the drift is recorded in ``standards_crosswalk``.
    """
    value = RISER_SCF_DEFAULT
    note = (
        "Methodology source: SCF applied to nominal stress ranges; "
        "1.0 = no stress concentration (neutral default)"
    )
    if override is not None:
        value = float(override)
        note = f"user override; standard value {RISER_SCF_DEFAULT}"
    citation = Citation(
        section="Sec.2 (stress concentration factors)",
        note=note,
        **_DNV_RP_C203_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")


_CITATION_WARNED = False


def riser_citations(repo_root: Optional[Path] = None) -> dict:
    """Return ``{"dff", "scf"}`` CitedValues for the riser design factors.

    Consumer-wrapper degradation (template:
    ``mooring_resilience.screening.safety_factor_citations``): where the wiki
    resolves, a missing or mismatched page fails closed (raises); in
    standalone mode (no resolvable wiki at all) it emits a one-shot
    ``RuntimeWarning`` and returns ``{}``.
    """
    global _CITATION_WARNED
    try:
        return {
            "dff": get_riser_dff(repo_root=repo_root),
            "scf": get_riser_scf(repo_root=repo_root),
        }
    except CitationResolutionError as exc:
        if not _CITATION_WARNED:
            warnings.warn(
                "Riser design-factor citations unavailable (standalone mode): "
                f"{exc}. Configure LLM_WIKI_PATH to enable calc citations.",
                RuntimeWarning,
                stacklevel=2,
            )
            _CITATION_WARNED = True
        return {}
