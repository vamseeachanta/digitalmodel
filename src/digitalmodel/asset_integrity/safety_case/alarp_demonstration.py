"""ALARP (As Low As Reasonably Practicable) risk-evaluation framework.

This module provides a deterministic, unit-testable implementation of the ALARP
"carrot" / "triangle" model used to demonstrate that the risk from a Major
Accident Hazard (MAH) has been reduced As Low As Reasonably Practicable, as
required by a Safety Case.

Safety-engineering basis
------------------------
The three-region ALARP model originates from the UK HSE document *Reducing Risks,
Protecting People* (R2P2, 2001), section "The Tolerability of Risk framework",
and is referenced by:

* **UK HSE Safety Case Regulations 2015 (SCR-2015)** -- duty to demonstrate MAH
  risks are ALARP.
* **NORSOK Z-013** -- Risk and emergency-preparedness assessment (risk
  acceptance criteria).
* **ISO 17776** -- Major-accident hazard identification and risk evaluation.

Risk is expressed as an *individual risk per annum* (IRPA), i.e. the probability
that an individual is killed in a given year. Three regions are defined by two
threshold lines:

* **Intolerable** -- risk above the upper limit; cannot be justified save in
  extraordinary circumstances. R2P2 cites an upper line of **1e-3 /yr** for
  workers (1e-4 /yr for members of the public).
* **Tolerable / ALARP** -- risk between the two lines; tolerable only if it has
  been reduced ALARP, demonstrated by cost-benefit analysis with a gross-
  disproportion bias.
* **Broadly Acceptable** -- risk below the lower limit; further risk reduction is
  not normally required (though good practice still applies). R2P2 cites a lower
  line of **1e-6 /yr**.

The default thresholds below (1e-3 and 1e-6 /yr) are the R2P2 worker values.
They are configurable via :class:`ALARPThresholds` for public-risk criteria or
project-specific acceptance criteria (e.g. NORSOK Z-013 risk-matrix limits).

Gross disproportion
-------------------
Within the ALARP region a risk-reducing measure must be implemented *unless* its
cost is **grossly disproportionate** to the safety benefit it delivers. The test
applied here is:

    implement the measure  iff  cost < GDF * benefit

where ``GDF`` (the gross-disproportion factor) is >= 1 and biases the decision in
favour of safety. HSE guidance applies a sliding GDF that increases with the
residual risk; common values range from ~1 (broadly acceptable end) up to ~10
(intolerable end). A default of ``GDF = 3`` is used here and is overridable.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum


class ALARPRegion(str, Enum):
    """The three regions of the ALARP / Tolerability-of-Risk triangle."""

    BROADLY_ACCEPTABLE = "broadly_acceptable"
    TOLERABLE_ALARP = "tolerable_alarp"
    INTOLERABLE = "intolerable"


@dataclass(frozen=True)
class ALARPThresholds:
    """Risk-acceptance threshold lines (individual risk per annum, IRPA).

    Parameters
    ----------
    upper_intolerable:
        Risk at or above this value is *Intolerable*. Default ``1e-3`` /yr
        (R2P2 worker upper limit).
    lower_broadly_acceptable:
        Risk at or below this value is *Broadly Acceptable*. Default ``1e-6``
        /yr (R2P2 lower limit).

    Boundary convention: a risk exactly equal to ``upper_intolerable`` is
    classed Intolerable; a risk exactly equal to ``lower_broadly_acceptable`` is
    classed Broadly Acceptable. The open interval between the lines is the
    Tolerable/ALARP region.
    """

    upper_intolerable: float = 1e-3
    lower_broadly_acceptable: float = 1e-6

    def __post_init__(self) -> None:
        if self.upper_intolerable <= 0 or self.lower_broadly_acceptable <= 0:
            raise ValueError("Threshold risks must be positive (per-annum probabilities).")
        if self.lower_broadly_acceptable >= self.upper_intolerable:
            raise ValueError(
                "lower_broadly_acceptable must be < upper_intolerable "
                f"(got {self.lower_broadly_acceptable} >= {self.upper_intolerable})."
            )


@dataclass(frozen=True)
class ALARPResult:
    """Outcome of classifying a risk against the ALARP thresholds."""

    risk: float
    region: ALARPRegion
    thresholds: ALARPThresholds
    requires_alarp_demonstration: bool
    rationale: str


@dataclass(frozen=True)
class GrossDisproportionResult:
    """Outcome of an ALARP gross-disproportion cost-benefit check.

    Attributes
    ----------
    cost:
        Cost of the risk-reducing measure (currency units).
    benefit:
        Monetised safety benefit of the measure, typically
        ``risk_reduction * value_of_preventing_a_fatality``.
    gdf:
        Gross-disproportion factor applied (>= 1).
    disproportion_ratio:
        ``cost / benefit``. Compared against ``gdf``.
    is_grossly_disproportionate:
        True when ``cost >= gdf * benefit``; the measure may be ruled out.
    measure_required:
        True when the measure must be implemented to satisfy ALARP
        (i.e. NOT grossly disproportionate).
    rationale:
        Human-readable explanation.
    """

    cost: float
    benefit: float
    gdf: float
    disproportion_ratio: float
    is_grossly_disproportionate: bool
    measure_required: bool
    rationale: str


def classify_risk(
    risk: float,
    thresholds: ALARPThresholds | None = None,
) -> ALARPResult:
    """Classify an individual risk per annum into an ALARP region.

    Parameters
    ----------
    risk:
        Individual risk per annum (IRPA). May be supplied directly, or computed
        upstream as ``likelihood * consequence`` (see
        :func:`risk_from_likelihood_consequence`).
    thresholds:
        Threshold lines. Defaults to the R2P2 worker values (1e-3 / 1e-6 /yr).

    Returns
    -------
    ALARPResult
        The region, whether an ALARP demonstration is required, and a rationale.

    Worked example
    --------------
    With default thresholds, ``risk = 5e-5`` /yr lies between 1e-6 and 1e-3, so
    it is classified ``TOLERABLE_ALARP`` and ``requires_alarp_demonstration`` is
    True.
    """
    if thresholds is None:
        thresholds = ALARPThresholds()
    if risk < 0:
        raise ValueError("risk must be non-negative.")

    if risk >= thresholds.upper_intolerable:
        region = ALARPRegion.INTOLERABLE
        requires = False  # Risk must be reduced regardless; ALARP CBA not the gate.
        rationale = (
            f"risk {risk:.3e}/yr >= upper intolerable line "
            f"{thresholds.upper_intolerable:.3e}/yr: INTOLERABLE -- "
            "must be reduced irrespective of cost (save extraordinary circumstances)."
        )
    elif risk <= thresholds.lower_broadly_acceptable:
        region = ALARPRegion.BROADLY_ACCEPTABLE
        requires = False
        rationale = (
            f"risk {risk:.3e}/yr <= lower line "
            f"{thresholds.lower_broadly_acceptable:.3e}/yr: BROADLY ACCEPTABLE -- "
            "no further risk reduction normally required."
        )
    else:
        region = ALARPRegion.TOLERABLE_ALARP
        requires = True
        rationale = (
            f"risk {risk:.3e}/yr lies in the ALARP region "
            f"({thresholds.lower_broadly_acceptable:.3e} < risk < "
            f"{thresholds.upper_intolerable:.3e}/yr): TOLERABLE only if reduced "
            "ALARP (demonstrate via gross-disproportion cost-benefit analysis)."
        )

    return ALARPResult(
        risk=risk,
        region=region,
        thresholds=thresholds,
        requires_alarp_demonstration=requires,
        rationale=rationale,
    )


def risk_from_likelihood_consequence(likelihood: float, consequence: float) -> float:
    """Compute a risk metric from likelihood x consequence.

    Parameters
    ----------
    likelihood:
        Event frequency or probability per annum (>= 0).
    consequence:
        Consequence measure, e.g. probability of fatality given the event, or
        fatalities per event. For an IRPA the product yields fatalities/yr.

    Returns
    -------
    float
        ``likelihood * consequence``.
    """
    if likelihood < 0 or consequence < 0:
        raise ValueError("likelihood and consequence must be non-negative.")
    return likelihood * consequence


def gross_disproportion_check(
    cost: float,
    benefit: float,
    gdf: float = 3.0,
) -> GrossDisproportionResult:
    """Apply the ALARP gross-disproportion cost-benefit test to a mitigation.

    A risk-reducing measure is required unless its cost is grossly
    disproportionate to the benefit:

        measure required  iff  cost < gdf * benefit

    Parameters
    ----------
    cost:
        Cost of implementing the risk-reducing measure (> 0 expected; 0 allowed).
    benefit:
        Monetised safety benefit (>= 0). Often
        ``risk_reduction * value_of_preventing_a_fatality (VPF)``.
    gdf:
        Gross-disproportion factor (>= 1). Higher values bias more strongly in
        favour of implementing the measure. Default ``3.0``.

    Returns
    -------
    GrossDisproportionResult

    Worked example
    --------------
    ``cost = 2.0e6``, ``benefit = 1.0e6``, ``gdf = 3`` ->
    ``gdf * benefit = 3.0e6`` > cost, so the measure is NOT grossly
    disproportionate and ``measure_required`` is True. (ratio = 2.0 < 3.)
    Raise cost to ``4.0e6`` and the measure becomes grossly disproportionate
    (ratio 4.0 >= 3) -- it may be ruled out.
    """
    if gdf < 1:
        raise ValueError("gdf (gross-disproportion factor) must be >= 1.")
    if cost < 0 or benefit < 0:
        raise ValueError("cost and benefit must be non-negative.")

    threshold = gdf * benefit
    grossly_disproportionate = cost >= threshold
    measure_required = not grossly_disproportionate

    if benefit == 0:
        disproportion_ratio = float("inf") if cost > 0 else 0.0
    else:
        disproportion_ratio = cost / benefit

    if measure_required:
        rationale = (
            f"cost {cost:.3e} < gdf*benefit ({gdf:g} * {benefit:.3e} = "
            f"{threshold:.3e}): measure is NOT grossly disproportionate -- "
            "implement it to satisfy ALARP."
        )
    else:
        rationale = (
            f"cost {cost:.3e} >= gdf*benefit ({gdf:g} * {benefit:.3e} = "
            f"{threshold:.3e}): measure IS grossly disproportionate -- "
            "may be ruled out (risk already ALARP without it)."
        )

    return GrossDisproportionResult(
        cost=cost,
        benefit=benefit,
        gdf=gdf,
        disproportion_ratio=disproportion_ratio,
        is_grossly_disproportionate=grossly_disproportionate,
        measure_required=measure_required,
        rationale=rationale,
    )
