"""Citation-emitting getters for riser design factors (#1245 / #1199a).

Pattern copied from the DNV-OS-E301 mooring pilot (``citations/registry.py``):
scalar standards-derived design factors live here as code constants; each
getter builds a :class:`Citation` from a template, validates it fail-closed,
and returns a :class:`CitedValue`. Licensed table CONTENT never lives in this
public repo — only these well-known scalar factors plus clause identifiers,
with values resolving against the private llm-wiki.

Provenance discipline (plan D6): a getter only cites the standard the code
actually sources the value from. #1245 shipped the two DNV factors (dff, scf);
#1246 adds the two API RP 16Q (1st Ed. 1993) §3.3 tension factors — f_wt (1.05)
and f_bt (0.96) — resolved against the now-pinned engineering-standards
``api-rp-16q`` page. The top-tension safety factor (1.25) has NO corresponding
API RP 16Q provision identified, so it carries no getter and stays a project
default in ``drilling_riser.stackup``; the Barlow wall-thickness safety factor
likewise carries no standards citation (generic formula, project default).

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

#: API RP 16Q, 1st Edition 1993 (reaffirmed 2001). The revision string pairs
#: with the 1993-numbered clause locators (§3.3); the wiki page's revision_note
#: records the 2nd-edition (2017) drift.
_API_RP_16Q_CITATION_TEMPLATE: Final = {
    "code_id": "api-rp-16q",
    "publisher": "API",
    "revision": "1993",
    "wiki_path": "wikis/engineering-standards/wiki/standards/api-rp-16q.md",
}

#: API STD 2RD, 3rd Edition 2025 (formerly API RP 2RD; API reclassified it as
#: STD 2RD). Home of the von Mises equivalent-stress design factor.
_API_STD_2RD_CITATION_TEMPLATE: Final = {
    "code_id": "api-std-2rd",
    "publisher": "API",
    "revision": "3e-2025",
    "wiki_path": "wikis/engineering-standards/wiki/standards/api-std-2rd.md",
}

#: Matches riser_fatigue/touchdown.py TouchdownFatigueInput.dff — the live
#: riser literal these getters must stay in parity with (#1246 wires the calc).
RISER_DFF_DEFAULT: Final[float] = 10.0
#: Matches riser_fatigue touchdown.py scf default and workflow.py wave scf
#: default: 1.0 = no stress concentration applied (neutral default).
RISER_SCF_DEFAULT: Final[float] = 1.0
#: Matches drilling_riser/stackup.py F_WT_DEFAULT — submerged-weight tolerance
#: factor, API RP 16Q (1st Ed. 1993) §3.3.
RISER_F_WT_DEFAULT: Final[float] = 1.05
#: Matches drilling_riser/stackup.py F_BT_DEFAULT — buoyancy loss and tolerance
#: factor, API RP 16Q (1st Ed. 1993) §3.3.
RISER_F_BT_DEFAULT: Final[float] = 0.96
#: Von Mises equivalent-stress design factor (API STD 2RD): allowable = DF*SMYS.
#: The well-known 2/3 operating design case; matches the public default in
#: orcaflex/code_check_engine.py APIRP2RDInput.design_factor. Envelope: #1281a.
RISER_VON_MISES_DESIGN_FACTOR: Final[float] = 0.67
#: Drilling operating flex-joint angle limits (API RP 16Q): the well-known
#: mean 2.0 deg / max 4.0 deg operating envelope. Envelope criteria (#1281a).
RISER_FLEXJOINT_ANGLE_MEAN_DEG: Final[float] = 2.0
RISER_FLEXJOINT_ANGLE_MAX_DEG: Final[float] = 4.0


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


def get_tension_weight_factor(
    *, override: Optional[float] = None, repo_root: Optional[Path] = None
) -> CitedValue:
    """Submerged-weight tolerance factor f_wt (API RP 16Q, 1st Ed. 1993 §3.3).

    Scales the submerged-weight term of the minimum slip-ring tension
    (``drilling_riser.stackup.minimum_slip_ring_tension``, and its default
    ``F_WT_DEFAULT``). ``override`` wins when provided (user-override-wins
    defense); the note then records both the override and the standard value.
    """
    value = RISER_F_WT_DEFAULT
    note = "Submerged-weight tolerance factor (f_wt) for minimum slip-ring tension"
    if override is not None:
        value = float(override)
        note = f"user override; standard value {RISER_F_WT_DEFAULT}"
    citation = Citation(
        section="Sec.3.3 (tension factors)",
        note=note,
        **_API_RP_16Q_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")


def get_buoyancy_tension_factor(
    *, override: Optional[float] = None, repo_root: Optional[Path] = None
) -> CitedValue:
    """Buoyancy loss and tolerance factor f_bt (API RP 16Q, 1st Ed. 1993 §3.3).

    Scales the buoyancy-uplift term of the minimum slip-ring tension (and its
    default ``F_BT_DEFAULT``). Distinct from the tensioner-system reduction
    factor Rf that 16Q §3.3.2 applies to the top-tension chain — that is a
    rig-specific input handled in
    ``drilling_riser.assembly.tensioner_system_factor``. ``override`` wins when
    provided.
    """
    value = RISER_F_BT_DEFAULT
    note = "Buoyancy loss and tolerance factor (f_bt) for minimum slip-ring tension"
    if override is not None:
        value = float(override)
        note = f"user override; standard value {RISER_F_BT_DEFAULT}"
    citation = Citation(
        section="Sec.3.3 (tension factors)",
        note=note,
        **_API_RP_16Q_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")


def get_von_mises_design_factor(
    *, override: Optional[float] = None, repo_root: Optional[Path] = None
) -> CitedValue:
    """Von Mises equivalent-stress design factor (API STD 2RD): allow = DF·SMYS.

    The well-known operating design case (2/3). Consumed by the operating-
    envelope engine (``drilling_riser.envelope``) and matches the public default
    in ``orcaflex.code_check_engine.APIRP2RDInput.design_factor``. ``override``
    wins when provided.
    """
    value = RISER_VON_MISES_DESIGN_FACTOR
    note = "Von Mises equivalent-stress design factor (allowable = DF x SMYS)"
    if override is not None:
        value = float(override)
        note = f"user override; standard value {RISER_VON_MISES_DESIGN_FACTOR}"
    citation = Citation(
        section="Sec.5 (dynamic riser design criteria)",
        note=note,
        **_API_STD_2RD_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="dimensionless")


def get_flexjoint_angle_limit(
    kind: str = "max",
    *,
    override: Optional[float] = None,
    repo_root: Optional[Path] = None,
) -> CitedValue:
    """Drilling operating flex-joint angle limit (API RP 16Q), degrees.

    ``kind`` selects the ``"mean"`` (2.0 deg) or ``"max"`` (4.0 deg) operating
    limit — the well-known drilling operating envelope. ``override`` wins when
    provided. Raises ``ValueError`` for an unknown ``kind``.
    """
    if kind not in ("mean", "max"):
        raise ValueError(f"kind must be 'mean' or 'max', got {kind!r}")
    standard = (
        RISER_FLEXJOINT_ANGLE_MEAN_DEG if kind == "mean" else RISER_FLEXJOINT_ANGLE_MAX_DEG
    )
    value = standard
    note = f"Drilling operating flex-joint {kind} angle limit"
    if override is not None:
        value = float(override)
        note = f"user override; standard value {standard}"
    citation = Citation(
        section="operating limits (flex-joint angle)",
        note=note,
        **_API_RP_16Q_CITATION_TEMPLATE,
    )
    validate_citation(citation, repo_root=repo_root)
    return CitedValue(value=value, citation=citation, units="degrees")


_CITATION_WARNED = False


def riser_citations(repo_root: Optional[Path] = None) -> dict:
    """Return ``{"dff", "scf", "f_wt", "f_bt", "von_mises_df"}`` CitedValues for
    the riser design factors.

    Consumer-wrapper degradation (template:
    ``mooring_resilience.screening.safety_factor_citations``): where the wiki
    resolves, a missing or mismatched page fails closed (raises); in
    standalone mode (no resolvable wiki at all) it emits a one-shot
    ``RuntimeWarning`` and returns ``{}``. (The parameterized flex-joint angle
    operating limit is resolved via ``get_flexjoint_angle_limit``, not bundled
    here.)
    """
    global _CITATION_WARNED
    try:
        return {
            "dff": get_riser_dff(repo_root=repo_root),
            "scf": get_riser_scf(repo_root=repo_root),
            "f_wt": get_tension_weight_factor(repo_root=repo_root),
            "f_bt": get_buoyancy_tension_factor(repo_root=repo_root),
            "von_mises_df": get_von_mises_design_factor(repo_root=repo_root),
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
