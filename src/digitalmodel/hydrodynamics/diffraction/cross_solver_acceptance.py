"""Tiered acceptance for comparing two diffraction programs.

A single tolerance across every quantity is the wrong shape for a code-to-code
comparison, because the quantities do not carry the same uncertainty:

* **Hydrostatics** is a closed-form integral over the wetted surface and
  waterplane. Two programs given the same mesh can match it almost exactly, and
  on a free-floating box both can be checked against ``rho g L B T GM``. A
  difference here is a setting or a convention, not a numerical limitation.
* **Excitation and added mass** are well-conditioned boundary-integral results.
  They converge quickly with refinement and agree closely between programs.
* **Radiation damping**, and the **roll response at resonance** which damping
  sets, are the most mesh- and formulation-sensitive quantities either program
  produces. Holding them to the hydrostatic tolerance fails results that are as
  good as the field achieves; holding hydrostatics to theirs passes errors that
  should never pass.

So each quantity is judged against the tier its physics supports. The tiers are
keyed to the physical quantity, not to either program, so the same criteria
judge any pair of programs.

**The convergence qualifier.** Agreement between two programs means little if
they sit at different distances from their own converged answers: a coarse mesh
in one and a fine mesh in the other can agree by coincidence, or disagree for
reasons that vanish with refinement. A comparison inside its threshold without
evidence of comparable convergence is therefore ``PASS_CONDITIONAL``, not
``PASS``. Outside the threshold it fails regardless, since better convergence
evidence cannot turn a large disagreement into agreement.

**Normalisation.** Every threshold is a fraction of a stated denominator. For
the frequency-dependent quantities this is the peak magnitude of that mode over
the solved grid, which keeps a small difference on a near-zero value from
reading as a large one. It also means a local relative difference can be much
larger than the peak-normalised one; a verdict records which was used.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from typing import Mapping

__all__ = [
    "Category",
    "Criterion",
    "ConvergenceEvidence",
    "Verdict",
    "Evaluation",
    "DEFAULT_CRITERIA",
    "DEFAULT_CONVERGENCE_FRACTION",
    "category_for",
    "convergence_from_self_study",
    "evaluate",
]


class Category(Enum):
    HYDROSTATICS = "hydrostatics"
    EXCITATION = "excitation"
    ADDED_MASS = "added_mass"
    RADIATION_DAMPING = "radiation_damping"
    MOTION_RAO = "motion_rao"
    RESONANT_ROLL_RAO = "resonant_roll_rao"


class Verdict(Enum):
    PASS = "pass"
    PASS_CONDITIONAL = "pass_conditional"
    FAIL = "fail"


@dataclass(frozen=True)
class Criterion:
    """One tier: a threshold, what it is a fraction of, and why it is that."""

    threshold: float
    normalisation: str
    rationale: str


_PEAK = "peak magnitude of that mode over the solved frequency grid"

DEFAULT_CRITERIA: Mapping[Category, Criterion] = {
    Category.HYDROSTATICS: Criterion(
        threshold=0.005,
        normalisation="the reference program's value of the same term",
        rationale=(
            "a closed-form integral over the same mesh; a difference beyond "
            "0.5% is a setting or convention, not a numerical limitation")),
    Category.EXCITATION: Criterion(
        threshold=0.05, normalisation=_PEAK,
        rationale="well-conditioned diffraction result, converges quickly"),
    Category.ADDED_MASS: Criterion(
        threshold=0.05, normalisation=_PEAK,
        rationale="well-conditioned radiation result, converges quickly"),
    Category.RADIATION_DAMPING: Criterion(
        threshold=0.10, normalisation=_PEAK,
        rationale=(
            "the most mesh- and formulation-sensitive radiation quantity; "
            "5 to 10% between programs is typical of the field")),
    Category.MOTION_RAO: Criterion(
        threshold=0.05, normalisation=_PEAK,
        rationale=(
            "away from roll resonance the response is set by excitation, "
            "added mass and stiffness, so it inherits their tier")),
    Category.RESONANT_ROLL_RAO: Criterion(
        threshold=0.10, normalisation=_PEAK,
        rationale=(
            "the roll peak is set by radiation damping, so it inherits the "
            "damping tier; in potential flow it is also governed by damping "
            "absent from both models and is not a design value")),
}

_QUANTITY = {
    "restoring_stiffness": Category.HYDROSTATICS,
    "hydrostatic_stiffness": Category.HYDROSTATICS,
    "excitation": Category.EXCITATION,
    "added_mass": Category.ADDED_MASS,
    "damping": Category.RADIATION_DAMPING,
    "radiation_damping": Category.RADIATION_DAMPING,
}


def category_for(quantity: str, mode: str) -> Category:
    """The category a quantity belongs to.

    Raises ``ValueError`` for an unrecognised quantity rather than defaulting to
    a tier: judging an unknown quantity by some convenient threshold is the
    failure a category scheme exists to prevent.
    """
    q = quantity.strip().lower()
    if q in ("motion_rao", "displacement_rao", "rao"):
        return (Category.RESONANT_ROLL_RAO if mode.strip().lower() == "roll"
                else Category.MOTION_RAO)
    if q in _QUANTITY:
        return _QUANTITY[q]
    raise ValueError(f"no acceptance category for quantity {quantity!r}")


@dataclass(frozen=True)
class ConvergenceEvidence:
    """Whether both programs are at comparable distances from convergence."""

    comparable: bool
    basis: str


#: A program's own mesh-convergence residual must be at most this fraction of
#: the tier for the tier to resolve agreement from convergence noise. With a
#: residual of a quarter of the band on each side, two programs that agree to
#: within the band cannot be doing so only because of where their meshes sit.
DEFAULT_CONVERGENCE_FRACTION = 0.25


def convergence_from_self_study(
    residual_a: float,
    residual_b: float,
    category: Category,
    basis: str,
    fraction: float = DEFAULT_CONVERGENCE_FRACTION,
    criteria: Mapping[Category, Criterion] = DEFAULT_CRITERIA,
) -> ConvergenceEvidence:
    """Judge comparability from each program's own convergence residual.

    ``residual_a`` and ``residual_b`` are each program's change between the
    mesh the comparison uses and its finest available mesh, as fractions.
    Convergence is comparable when both are small against the tier being
    tested: at most ``fraction`` of its threshold.

    The residual to the finest mesh understates the distance to true
    convergence, more so for a program that converges slowly. That is why the
    allowance is a fraction of the tier rather than the whole of it.
    """
    for r in (residual_a, residual_b, fraction):
        if not isinstance(r, (int, float)) or not math.isfinite(r) or r < 0:
            raise ValueError(f"residuals and fraction must be finite and "
                             f"non-negative, got {r!r}")
    limit = fraction * criteria[category].threshold
    worst = max(residual_a, residual_b)
    ok = worst <= limit
    return ConvergenceEvidence(
        comparable=ok,
        basis=(f"{basis}; own-mesh residuals {100 * residual_a:.2f}% and "
               f"{100 * residual_b:.2f}% against a limit of "
               f"{100 * limit:.2f}% ({100 * fraction:.0f}% of the "
               f"{100 * criteria[category].threshold:.1f}% tier)"))


@dataclass(frozen=True)
class Evaluation:
    category: Category
    observed: float
    threshold: float
    verdict: Verdict
    reason: str
    normalisation: str

    @property
    def margin(self) -> float:
        """Threshold minus observed; negative when outside."""
        return self.threshold - self.observed


def evaluate(
    category: Category,
    difference: float,
    convergence: ConvergenceEvidence,
    criteria: Mapping[Category, Criterion] = DEFAULT_CRITERIA,
) -> Evaluation:
    """Judge one comparison against its tier.

    ``difference`` is the absolute difference between the two programs as a
    fraction of the tier's normalisation, so 0.05 means 5%.
    """
    if not isinstance(difference, (int, float)) or not math.isfinite(difference):
        raise ValueError(f"difference must be finite, got {difference!r}")
    if difference < 0:
        raise ValueError(f"difference must be non-negative, got {difference!r}")

    crit = criteria[category]
    inside = difference <= crit.threshold
    if not inside:
        verdict = Verdict.FAIL
        reason = (f"{100 * difference:.2f}% exceeds the "
                  f"{100 * crit.threshold:.1f}% {category.value} tier")
    elif convergence.comparable:
        verdict = Verdict.PASS
        reason = (f"{100 * difference:.2f}% within the "
                  f"{100 * crit.threshold:.1f}% {category.value} tier, with "
                  f"comparable convergence ({convergence.basis})")
    else:
        verdict = Verdict.PASS_CONDITIONAL
        reason = (f"{100 * difference:.2f}% within the "
                  f"{100 * crit.threshold:.1f}% {category.value} tier, but "
                  f"comparable convergence is not demonstrated "
                  f"({convergence.basis})")
    return Evaluation(category=category, observed=float(difference),
                      threshold=crit.threshold, verdict=verdict,
                      reason=reason, normalisation=crit.normalisation)
