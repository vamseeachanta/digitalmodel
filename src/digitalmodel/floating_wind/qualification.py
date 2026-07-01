"""Technology-qualification gate for floating-wind concepts (issue #1225).

The Wood/Whooley deck notes an *independent assessment of 40+ floating-wind
concepts*, of which "4 or 5 technologies will emerge". This module is that gate:
a **multi-criteria maturity scorer** that ranks shortlisted concepts and issues
a qualification verdict, mirroring the DNV-RP-A203 technology-qualification
process (staged de-risking from novel to field-proven).

A concept is scored on a set of weighted **criteria** -- technology readiness
(DNV-RP-A203 Technology Readiness Level), operating track record, supply-chain
readiness, cost confidence, etc. Each criterion score is on a 0..1 maturity
scale (a TRL is normalised onto it). The weighted mean is the qualification
score; two thresholds split concepts into ``qualified`` / ``conditional`` /
``not_qualified``.

Everything -- criteria, weights, thresholds, the TRL scale -- is an **input**
carried in a reviewable ``.yml`` (:func:`default_criteria`); nothing is
hardcoded. The gate is independent of the LCOE engine (#1221): it de-risks the
*technology*, while the tradespace ranks the *economics*. A concept's API 17N
TRL tags (#1222) feed the ``technology_readiness`` criterion.

References
----------
* DNV-RP-A203, *Technology Qualification* -- staged qualification, TRL scale.
* Wood plc / A. Whooley (2021) -- concept-evaluation / technology-selection
  (slide 11): 40+ concepts assessed, a handful emerge.
"""

from __future__ import annotations

from enum import Enum
from pathlib import Path
from typing import Any, Mapping

import yaml
from pydantic import BaseModel, Field, model_validator

__all__ = [
    "DNV_TRL_MAX",
    "trl_to_maturity",
    "QualificationVerdict",
    "Criterion",
    "QualificationCriteria",
    "ConceptMaturity",
    "QualificationResult",
    "score_concept",
    "rank_concepts",
    "default_criteria",
]

# DNV-RP-A203 technology readiness runs 0 (unproven idea) .. 7 (field proven).
DNV_TRL_MAX = 7

_CRITERIA_YML = (
    Path(__file__).resolve().parents[1]
    / "base_configs"
    / "modules"
    / "floating_wind_qualification"
    / "floating_wind_qualification.yml"
)


def trl_to_maturity(trl: int, trl_max: int = DNV_TRL_MAX) -> float:
    """Normalise a DNV-RP-A203 TRL onto the 0..1 maturity scale."""
    if trl < 0 or trl > trl_max:
        raise ValueError(f"TRL {trl} out of range 0..{trl_max}")
    return trl / trl_max


class QualificationVerdict(str, Enum):
    """Outcome of the qualification gate for a concept."""

    QUALIFIED = "qualified"
    CONDITIONAL = "conditional"
    NOT_QUALIFIED = "not_qualified"


class Criterion(BaseModel):
    """One weighted qualification criterion."""

    key: str = Field(..., description="Stable identifier, e.g. 'technology_readiness'")
    weight: float = Field(..., gt=0.0, description="Relative weight (normalised)")
    description: str = Field("", description="What this criterion assesses")


class QualificationCriteria(BaseModel):
    """The criteria set, weights and verdict thresholds.

    Weights need not sum to 1 -- they are normalised on use. The two thresholds
    split the 0..1 qualification score into the three verdicts.
    """

    criteria: list[Criterion] = Field(..., min_length=1)
    qualified_threshold: float = Field(
        0.70, ge=0.0, le=1.0, description="Score at/above -> qualified"
    )
    conditional_threshold: float = Field(
        0.50, ge=0.0, le=1.0, description="Score at/above -> conditional"
    )

    @model_validator(mode="after")
    def _check(self) -> "QualificationCriteria":
        if self.conditional_threshold > self.qualified_threshold:
            raise ValueError("conditional_threshold must be <= qualified_threshold")
        keys = [c.key for c in self.criteria]
        if len(keys) != len(set(keys)):
            raise ValueError("duplicate criterion keys")
        return self

    @property
    def total_weight(self) -> float:
        return sum(c.weight for c in self.criteria)

    def verdict_for(self, score: float) -> QualificationVerdict:
        if score >= self.qualified_threshold:
            return QualificationVerdict.QUALIFIED
        if score >= self.conditional_threshold:
            return QualificationVerdict.CONDITIONAL
        return QualificationVerdict.NOT_QUALIFIED

    @classmethod
    def from_mapping(cls, data: Mapping[str, Any]) -> "QualificationCriteria":
        return cls.model_validate(dict(data))

    @classmethod
    def from_yaml(cls, path: str | Path) -> "QualificationCriteria":
        raw = yaml.safe_load(Path(path).read_text())
        if isinstance(raw, Mapping) and "floating_wind_qualification" in raw:
            raw = raw["floating_wind_qualification"]
        return cls.from_mapping(raw)


class ConceptMaturity(BaseModel):
    """A concept and its per-criterion maturity scores (0..1)."""

    name: str
    scores: dict[str, float] = Field(
        ..., description="criterion key -> maturity score in [0, 1]"
    )

    @model_validator(mode="after")
    def _in_range(self) -> "ConceptMaturity":
        for key, val in self.scores.items():
            if not 0.0 <= val <= 1.0:
                raise ValueError(f"score for {key!r} out of range 0..1: {val}")
        return self


class QualificationResult(BaseModel):
    """A concept's qualification score and verdict."""

    name: str
    score: float
    verdict: QualificationVerdict


def score_concept(
    concept: ConceptMaturity, criteria: QualificationCriteria
) -> QualificationResult:
    """Weighted-mean qualification score + verdict for one concept.

    A criterion missing from ``concept.scores`` is treated as 0 maturity (a
    gap the concept has not yet evidenced), so omissions cannot inflate a score.
    """
    weighted = sum(
        c.weight * concept.scores.get(c.key, 0.0) for c in criteria.criteria
    )
    score = weighted / criteria.total_weight
    return QualificationResult(
        name=concept.name, score=score, verdict=criteria.verdict_for(score)
    )


def rank_concepts(
    concepts: list[ConceptMaturity], criteria: QualificationCriteria
) -> list[QualificationResult]:
    """Score every concept and return them ranked best-first."""
    results = [score_concept(c, criteria) for c in concepts]
    return sorted(results, key=lambda r: r.score, reverse=True)


def default_criteria() -> QualificationCriteria:
    """The packaged default qualification criteria set."""
    return QualificationCriteria.from_yaml(_CRITERIA_YML)
