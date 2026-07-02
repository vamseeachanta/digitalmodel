"""Collaboration / Joint Industry Project (JIP) de-risking overlay (issue #1285).

The last Wood/Whooley cost lever: **cross-industry collaboration**. The deck
(slide 13) frames Joint Industry Projects -- Cability (cable stability), CALM
(cable failure RQA), SURF-IM (subsea integrity), SUREFLEX (flexible-riser
integrity) -- as the mechanism that *advances technology readiness*, de-risking
novel floater / mooring / dynamic-cable subsystems so they clear qualification.

This module is that overlay on the technology-qualification gate
(:mod:`.qualification`, #1225): a :class:`JointIndustryProject` raises the
**maturity** of the criteria it targets, and a :class:`CollaborationProgram`
(a set of JIPs) re-scores a concept -- so a concept that was ``conditional`` can
become ``qualified`` once the relevant JIPs de-risk its weak subsystem.

Uplifts targeting the same criterion **sum** but the result is **capped at 1.0**
-- collaboration cannot push a subsystem past fully-proven. Applying an empty
program is the identity.

Calibration note
----------------
The maturity uplift per JIP is a **calibration input**, not a deck number. The
packaged default gives each of the four deck JIPs a modest ~0.10 uplift (roughly
one DNV-RP-A203 TRL step of the 0..7 scale); override per assessment. Tests
assert the *mechanism* (targeted, summed, capped, requalification), not a forced
score.

References
----------
* Wood plc / A. Whooley (2021), collaboration / JIPs (slide 13).
* DNV-RP-A203 -- technology qualification / TRL (the maturity scale uplifted).
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Mapping

import yaml
from pydantic import BaseModel, Field

from digitalmodel.floating_wind.qualification import (
    ConceptMaturity,
    QualificationCriteria,
    QualificationResult,
    score_concept,
)

__all__ = [
    "JointIndustryProject",
    "CollaborationProgram",
    "apply_collaboration",
    "requalify_with_collaboration",
    "default_collaboration_program",
]

_COLLAB_YML = (
    Path(__file__).resolve().parents[1]
    / "base_configs"
    / "modules"
    / "floating_wind_economics"
    / "collaboration.yml"
)


class JointIndustryProject(BaseModel):
    """A JIP that advances the maturity of the criteria it targets."""

    name: str
    targets: list[str] = Field(
        ..., min_length=1, description="Qualification criterion keys this JIP de-risks"
    )
    maturity_uplift: float = Field(
        ..., gt=0.0, le=1.0, description="Maturity added to each targeted criterion"
    )
    description: str = ""


class CollaborationProgram(BaseModel):
    """A set of JIPs applied together as a de-risking program."""

    jips: list[JointIndustryProject] = Field(default_factory=list)

    def total_uplift(self) -> dict[str, float]:
        """Summed uplift per targeted criterion across all JIPs."""
        out: dict[str, float] = {}
        for jip in self.jips:
            for key in jip.targets:
                out[key] = out.get(key, 0.0) + jip.maturity_uplift
        return out

    def uplift_scores(self, scores: Mapping[str, float]) -> dict[str, float]:
        """Return a copy of ``scores`` with targeted maturities uplifted (cap 1.0).

        A targeted criterion absent from ``scores`` is treated as starting from 0
        (an un-evidenced subsystem the JIP begins to de-risk).
        """
        result = dict(scores)
        for key, up in self.total_uplift().items():
            result[key] = min(1.0, result.get(key, 0.0) + up)
        return result

    @classmethod
    def from_mapping(cls, data: Mapping[str, Any]) -> "CollaborationProgram":
        return cls.model_validate(dict(data))

    @classmethod
    def from_yaml(cls, path: str | Path) -> "CollaborationProgram":
        raw = yaml.safe_load(Path(path).read_text())
        if isinstance(raw, Mapping) and "collaboration" in raw:
            raw = raw["collaboration"]
        return cls.from_mapping(raw)


def apply_collaboration(
    concept: ConceptMaturity, program: CollaborationProgram
) -> ConceptMaturity:
    """Return a copy of ``concept`` with the program's maturity uplifts applied."""
    return ConceptMaturity(
        name=concept.name, scores=program.uplift_scores(concept.scores)
    )


def requalify_with_collaboration(
    concept: ConceptMaturity,
    program: CollaborationProgram,
    criteria: QualificationCriteria,
) -> tuple[QualificationResult, QualificationResult]:
    """Qualification result before and after the collaboration program.

    Returns ``(before, after)`` so callers can show the de-risking effect (e.g.
    a ``conditional -> qualified`` move).
    """
    before = score_concept(concept, criteria)
    after = score_concept(apply_collaboration(concept, program), criteria)
    return before, after


def default_collaboration_program() -> CollaborationProgram:
    """The packaged default program (the deck's four JIPs)."""
    return CollaborationProgram.from_yaml(_COLLAB_YML)
