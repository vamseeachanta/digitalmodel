"""
ABOUTME: Referent-free resistance scoring (#2023). Scores a calm-water towing
run for a hull with NO published Ct: numerical verification and uncertainty per
ITTC 7.5-03-01-01, physical plausibility bands, and an explicit statement of
what cannot be established. It never loads a referent and it never returns a
validation verdict.

WHY THIS MODULE EXISTS SEPARATELY FROM ``ship_resistance``
----------------------------------------------------------
``ship_resistance.evaluate_ship_resistance_run`` is a VALIDATION apparatus. Its
four criteria score a computed coefficient against a measured one (V1), against
a decomposition of it (V2a), against the correlation line the publishing
workshop reduced its own data with (V2b), and against a second mesh level (V3).
Three of the four need a referent, and ``_assert_normalisation_area`` encodes a
KCS-only invariant - it refuses any normalisation area near the GENERATED
9.5609 m^2, because for KCS the published area is the only admissible one.

For a hull with no publication, that guard is inverted: the mesh-derived area
is the ONLY area there is. So this is a separate entry point rather than a flag
on the existing one. The KCS path keeps its referent, its guard and its
aggregate verdict, unchanged and still tested.

THE CATEGORY DIFFERENCE, STATED ONCE
------------------------------------
Verification asks "am I solving the equations right?" and needs no experiment.
Validation asks "am I solving the right equations?" and cannot be done without
one. Losing the referent loses validation ENTIRELY - it does not weaken it.

What this module can therefore produce is a PREDICTION with a numerical error
band, plus a set of plausibility observations. It cannot produce a tolerance on
the true resistance, because modelling error - turbulence closure, wall
function, free-surface treatment, fixed attitude - is not observable from the
simulation alone at any grid density.

The wording throughout is deliberate. Plausibility verdicts are
``implausible`` / ``not_implausible``, never ``passed``. A band cannot confirm;
it can only fail to contradict. See ``scripts/cfd/yplus_after.sh``, which made
the same call for y+ and says why: a criterion invented after looking at the
answer is not a criterion.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

from digitalmodel.solvers.openfoam.validation.ship_resistance import (
    GRAVITY,
    froude_number,
    ittc57_friction_coefficient,
    kinematic_viscosity_for_reynolds,
    read_force_rows,
    resistance_coefficient,
)

# --------------------------------------------------------------------------- #
#  The averaging window - a STATED parameter with a DERIVED minimum
#
#  The committed KCS artifact reports `averaging_window: 2`: the production
#  mean is over two force samples, and the accompanying "iterative scatter" of
#  9.38e-7 is half the gap between two numbers, not a standard deviation.
#
#  The floor below is derived, not chosen. Sampling a periodic signal n times
#  per period recovers at worst cos(pi/n) of its true amplitude - the sampled
#  extremum sits at most half a sample interval away from the true one - so the
#  observed half-range under-estimates U_I by up to 1 - cos(pi/n). Declaring a
#  budget for that deficit fixes n; declaring how many periods must be resolved
#  fixes the sample count. Nothing here was tuned against a result. It happens
#  to fail the committed KCS run, which is the point.
# --------------------------------------------------------------------------- #

#: Largest tolerated under-estimate of the oscillation half-range caused by
#: sampling it at finite rate.
SAMPLED_PEAK_DEFICIT_BUDGET = 0.05

#: n such that 1 - cos(pi/n) < the budget above. At n = 10 the deficit is
#: 4.89%; at n = 9 it is 6.03% and misses.
SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT = 10

#: ITTC's oscillatory criterion takes the extrema over a whole number of
#: periods. One period cannot show that the extrema have stopped drifting.
MIN_OSCILLATION_PERIODS = 2

#: The stated minimum averaging window, in SAMPLES.
AVERAGING_WINDOW_MIN_SAMPLES = (
    SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT * MIN_OSCILLATION_PERIODS
)

#: Below this there is no statistic at all: a two-point spread is a difference,
#: and a two-point mean has no residual degrees of freedom.
MIN_SAMPLES_FOR_A_STATISTIC = 3

# --------------------------------------------------------------------------- #
#  Grid convergence - Roache / ITTC 7.5-03-01-01
# --------------------------------------------------------------------------- #

#: Roache's factor of safety when the observed order is MEASURED from three
#: grids. Matches the existing use in scripts/cfd/run_sloshing_convergence.py.
ROACHE_FS_THREE_GRID = 1.25

#: Roache's factor of safety when the order is ASSUMED because only two grids
#: exist. The larger factor is the price of the assumption, not a refinement.
ROACHE_FS_TWO_GRID = 3.0

#: The order assumed on a two-grid study. Second order is the formal order of
#: the schemes this case uses; it is an assumption, not an observation.
ASSUMED_ORDER = 2.0

#: Roache's recommended minimum refinement ratio. Below it the difference
#: between levels is comparable to the noise it is meant to measure.
MIN_REFINEMENT_RATIO = 1.3

# --------------------------------------------------------------------------- #
#  Plausibility bands - the WEAKEST tier, and labelled as such
#
#  For a conventional displacement hull the viscous coefficient is the
#  equivalent flat-plate friction times a form factor: Cv = (1 + k) * Cf. The
#  form factor is a geometric property, positive for any hull whose curvature
#  accelerates the flow over it, and for conventional displacement forms it is
#  commonly reported in the neighbourhood of 1.1 to 1.35 by the Prohaska
#  low-speed method. The band below is deliberately WIDER than that on both
#  sides, because this repository holds no primary source for it and a band
#  quoted tighter than its evidence is a manufactured criterion.
#
#  PROVENANCE: engineering judgement, widened. NOT a published interval.
#  A hull outside the declared class does not get a narrower band - it gets no
#  band at all.
#
#  Note what this band means for V2b. V2b scores Cv against Cf at +/-5%, which
#  is the assertion (1 + k) in [0.95, 1.05] - a form factor of ZERO. That is
#  the right referent for KCS only because the workshop reduced its own data
#  with the ITTC-57 line. As a referent-free criterion it is centred on a
#  physically wrong value, and recentring it on a real form factor moves the
#  committed KCS result FURTHER out, not closer.
# --------------------------------------------------------------------------- #

FORM_FACTOR_BAND: Tuple[float, float] = (1.05, 1.45)

#: Hull classes the band above is declared to cover.
BANDED_HULL_CLASSES = frozenset({"conventional_displacement"})

_AREA_PROVENANCE = frozenset({"published", "mesh_derived", "declared"})


# --------------------------------------------------------------------------- #
#  Configuration
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class NormalisationArea:
    """The area every coefficient here is divided by, with its provenance.

    A coefficient is defined by what it is divided by. On the KCS path the
    published area is used and the mesh-derived area is a disclosed diagnostic;
    on this path there is no publication, so the mesh-derived area is normally
    the only one available. That inversion is exactly why the provenance is a
    required field rather than a comment: the number that leaves this module
    must carry the statement of which area produced it.
    """

    value_m2: float
    provenance: str
    source: str

    def __post_init__(self) -> None:
        if self.value_m2 <= 0:
            raise ValueError(
                f"normalisation area must be positive, got {self.value_m2}"
            )
        if self.provenance not in _AREA_PROVENANCE:
            raise ValueError(
                f"normalisation area provenance {self.provenance!r} is not one "
                f"of {sorted(_AREA_PROVENANCE)}. An area without a declared "
                f"provenance cannot be reported, because the coefficient it "
                f"produces is not interpretable without it."
            )
        if not str(self.source).strip():
            raise ValueError(
                "normalisation area carries no citation: state the source of "
                "this area (which surface, at which waterline, integrated how)."
            )


@dataclass(frozen=True)
class ReferentFreeConfig:
    """Condition of a run on a hull with no published coefficient.

    There is no ``wetted_surface: float`` here on purpose - it is a
    :class:`NormalisationArea`, so an area cannot enter without its provenance.
    """

    name: str
    lpp: float
    velocity: float
    reynolds: float
    density: float
    wetted_surface: NormalisationArea
    #: Averaging window in ITERATIONS, not rows. Stated, never defaulted: the
    #: #1173 defect was a window whose units were assumed.
    averaging_window: int
    half_domain: bool = True
    drag_axis: int = 0
    hull_class: str = "conventional_displacement"
    form_factor_band: Tuple[float, float] = FORM_FACTOR_BAND
    gravity: float = GRAVITY

    def __post_init__(self) -> None:
        for name in ("lpp", "velocity", "reynolds", "density"):
            if getattr(self, name) <= 0:
                raise ValueError(f"{name} must be positive")
        if self.averaging_window <= 0:
            raise ValueError(
                "averaging_window must be a positive ITERATION span; a window "
                "of zero silently averages the start-up transient into the mean"
            )

    @property
    def froude(self) -> float:
        return froude_number(self.velocity, self.lpp, self.gravity)

    @property
    def cf_ittc57(self) -> float:
        return ittc57_friction_coefficient(self.reynolds)

    @property
    def nu(self) -> float:
        return kinematic_viscosity_for_reynolds(
            self.velocity, self.lpp, self.reynolds
        )


@dataclass(frozen=True)
class GridLevel:
    """One mesh level of a grid-convergence study."""

    name: str
    cells: int
    value: float


# --------------------------------------------------------------------------- #
#  Averaging window adequacy
# --------------------------------------------------------------------------- #

def averaging_window_adequacy(
    *, samples: int, window_iterations: int
) -> Dict[str, Any]:
    """Is this window long enough for the mean to be an average?

    Three states, all decided from the derivation above rather than from any
    result:

    * ``INADEQUATE`` - fewer than three samples. There is no statistic: the
      mean has no residual degrees of freedom and the "scatter" is half a gap.
    * ``MARGINAL``   - a statistic exists but the window under-resolves the
      oscillation, so U_I is a LOWER BOUND on the iterative uncertainty.
    * ``ADEQUATE``   - at or above the stated minimum.

    A sample count without its iteration span is uninterpretable, so the span
    is carried in the verdict.
    """
    if samples < 1:
        raise ValueError(f"samples must be positive, got {samples}")
    statistic_available = samples >= MIN_SAMPLES_FOR_A_STATISTIC
    adequate = samples >= AVERAGING_WINDOW_MIN_SAMPLES

    if not statistic_available:
        verdict = "INADEQUATE"
        note = (
            f"{samples} force sample(s) in the window. This is not an average "
            f"and the reported scatter is not a standard deviation: below "
            f"{MIN_SAMPLES_FOR_A_STATISTIC} samples the mean has no residual "
            f"degrees of freedom. Iterative convergence is NOT demonstrated, "
            f"and the mean must not be quoted with an uncertainty band."
        )
    elif not adequate:
        verdict = "MARGINAL"
        note = (
            f"{samples} force samples against a stated minimum of "
            f"{AVERAGING_WINDOW_MIN_SAMPLES}. A mean exists, but the window "
            f"under-resolves the oscillation, so the sampled half-range "
            f"under-estimates U_I: at fewer than "
            f"{SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT} samples per period the "
            f"deficit exceeds the stated "
            f"{SAMPLED_PEAK_DEFICIT_BUDGET:.0%} budget. Treat U_I as a lower "
            f"bound."
        )
    else:
        verdict = "ADEQUATE"
        note = (
            f"{samples} force samples over {window_iterations} iterations, at "
            f"or above the stated minimum of {AVERAGING_WINDOW_MIN_SAMPLES}."
        )

    return {
        "verdict": verdict,
        "adequate": adequate,
        "statistic_available": statistic_available,
        "samples": samples,
        "window_iterations": window_iterations,
        "minimum_samples": AVERAGING_WINDOW_MIN_SAMPLES,
        "minimum_derivation": (
            f"{MIN_OSCILLATION_PERIODS} resolved oscillation periods at "
            f"{SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT} samples per period, the "
            f"rate at which the sampled half-range under-estimates the true "
            f"half-range by less than "
            f"{SAMPLED_PEAK_DEFICIT_BUDGET:.0%} "
            f"(1 - cos(pi/n) = "
            f"{1.0 - math.cos(math.pi / SAMPLES_PER_PERIOD_FOR_PEAK_DEFICIT):.4f})."
        ),
        "note": note,
    }


# --------------------------------------------------------------------------- #
#  Iterative uncertainty - ITTC 7.5-03-01-01 oscillatory criterion
# --------------------------------------------------------------------------- #

def iterative_uncertainty(series: Sequence[float]) -> Dict[str, Any]:
    """U_I = 0.5 * (S_U - S_L) over the converged oscillation.

    ITTC 7.5-03-01-01 estimates iterative uncertainty from the amplitude of the
    residual oscillation in the quantity of interest, taken over a whole number
    of periods. This function reports the half-range, the number of turning
    points it saw and the number of periods those imply, so a reader can tell a
    resolved oscillation from a series that merely has a maximum and a minimum.

    A MONOTONE series has not oscillated about anything. Its half-range is a
    measure of the remaining drift, not of the converged amplitude, and the
    value is returned labelled rather than withheld - withholding it would make
    an unconverged run look like a converged one with no data.
    """
    values = [float(v) for v in series]
    n = len(values)
    if n < MIN_SAMPLES_FOR_A_STATISTIC:
        return {
            "available": False,
            "u_i": None,
            "samples": n,
            "s_upper": None,
            "s_lower": None,
            "turning_points": None,
            "resolved_periods": None,
            "oscillatory": None,
            "note": (
                f"{n} sample(s): fewer than {MIN_SAMPLES_FOR_A_STATISTIC}. "
                f"Two points define a difference, not an oscillation, so no "
                f"iterative uncertainty can be formed from them."
            ),
        }

    s_upper, s_lower = max(values), min(values)
    u_i = 0.5 * (s_upper - s_lower)

    turning_points = 0
    last_sign = 0
    for a, b in zip(values, values[1:]):
        d = b - a
        sign = (d > 0) - (d < 0)
        if sign == 0:
            continue
        if last_sign != 0 and sign != last_sign:
            turning_points += 1
        last_sign = sign
    resolved_periods = turning_points / 2.0
    oscillatory = turning_points >= 2

    if turning_points == 0:
        note = (
            "the series is monotone over the window: this half-range measures "
            "residual drift, not a converged oscillation amplitude, and the "
            "run has not reached iterative convergence."
        )
    elif resolved_periods < MIN_OSCILLATION_PERIODS:
        note = (
            f"{resolved_periods:.1f} oscillation period(s) resolved against a "
            f"stated minimum of {MIN_OSCILLATION_PERIODS}; the extrema have "
            f"not been shown to have stopped drifting, so U_I is a lower bound."
        )
    else:
        note = (
            f"{resolved_periods:.1f} oscillation periods resolved over "
            f"{n} samples; U_I is the half-range of the oscillation per ITTC "
            f"7.5-03-01-01."
        )

    return {
        "available": True,
        "u_i": u_i,
        "samples": n,
        "s_upper": s_upper,
        "s_lower": s_lower,
        "turning_points": turning_points,
        "resolved_periods": resolved_periods,
        "oscillatory": oscillatory,
        "note": note,
    }


# --------------------------------------------------------------------------- #
#  Grid uncertainty
# --------------------------------------------------------------------------- #

def grid_uncertainty(levels: Sequence[GridLevel]) -> Dict[str, Any]:
    """Discretisation uncertainty from a grid-refinement study.

    Three levels give the ITTC / Roache procedure in full: the convergence
    ratio R classifies the sequence, and on monotonic convergence the observed
    order p is MEASURED, Richardson extrapolation gives the fine-grid error and
    GCI = 1.25 * |delta_RE| is the uncertainty.

    Two levels give strictly less, and the difference is the whole point:

    * R cannot be formed, so the sequence CANNOT be classified. A two-level
      difference of 5.6% is equally consistent with slow monotonic convergence
      and with an oscillation whose amplitude happens to be 5.6%.
    * p cannot be measured, so it must be assumed. The output says so in a
      field named ``order_is_assumed``.
    * Roache's factor of safety rises from 1.25 to 3.0. That is the price of
      the assumption, not a refinement of it.

    One level is not a study at all and returns no estimate. That is a real
    outcome, not an error: it means the number cannot be quoted with a band.
    """
    ordered = sorted(levels, key=lambda lv: lv.cells, reverse=True)
    n = len(ordered)
    base: Dict[str, Any] = {
        "levels": n,
        "level_names": [lv.name for lv in ordered],
        "cells": [lv.cells for lv in ordered],
        "values": [lv.value for lv in ordered],
        "estimable": False,
        "u_g": None,
        "observed_order": None,
        "order_is_assumed": None,
        "richardson_error": None,
        "extrapolated_value": None,
        "convergence_ratio": None,
        "classification": None,
        "factor_of_safety": None,
        "refinement_ratio": None,
        "refinement_ratio_adequate": None,
        "note": "",
    }

    if n < 2:
        base["note"] = (
            "a single mesh level is not a grid-convergence study. "
            "Discretisation error is not estimable, and the coefficient must "
            "not be quoted with a numerical uncertainty band."
        )
        return base

    s1, s2 = ordered[0].value, ordered[1].value
    r21 = (ordered[0].cells / ordered[1].cells) ** (1.0 / 3.0)
    base["refinement_ratio"] = r21
    base["refinement_ratio_adequate"] = r21 >= MIN_REFINEMENT_RATIO
    eps21 = s2 - s1

    if n == 2:
        denom = r21**ASSUMED_ORDER - 1.0
        if denom <= 0:
            base["note"] = (
                "refinement ratio too close to unity to form a Richardson "
                "estimate at the assumed order."
            )
            return base
        delta_re = eps21 / denom
        base.update(
            estimable=True,
            classification="indeterminate",
            order_is_assumed=True,
            observed_order=None,
            assumed_order=ASSUMED_ORDER,
            richardson_error=delta_re,
            extrapolated_value=s1 - delta_re,
            factor_of_safety=ROACHE_FS_TWO_GRID,
            u_g=ROACHE_FS_TWO_GRID * abs(delta_re),
            note=(
                f"two levels only. The observed order of accuracy is NOT "
                f"measured here - it is assumed to be {ASSUMED_ORDER} - and "
                f"the sequence cannot be classified as monotonic, oscillatory "
                f"or divergent, because that classification needs a third "
                f"level. Roache's factor of safety is therefore "
                f"{ROACHE_FS_TWO_GRID} rather than {ROACHE_FS_THREE_GRID}."
            ),
        )
        if not base["refinement_ratio_adequate"]:
            base["note"] += (
                f" The refinement ratio {r21:.3f} is below the recommended "
                f"{MIN_REFINEMENT_RATIO}: the level-to-level difference is not "
                f"cleanly separated from iterative noise."
            )
        return base

    s3 = ordered[2].value
    r32 = (ordered[1].cells / ordered[2].cells) ** (1.0 / 3.0)
    eps32 = s3 - s2
    r = math.sqrt(r21 * r32)
    base["refinement_ratio"] = r
    base["refinement_ratio_adequate"] = min(r21, r32) >= MIN_REFINEMENT_RATIO
    base["constant_refinement_ratio"] = abs(r21 - r32) <= 0.02 * r

    if eps32 == 0.0:
        base["classification"] = "degenerate"
        base["note"] = (
            "the two coarser levels return identical values; the convergence "
            "ratio is undefined and no estimate can be formed."
        )
        return base

    ratio = eps21 / eps32
    base["convergence_ratio"] = ratio

    if ratio > 1.0 or ratio <= -1.0:
        base["classification"] = "divergent"
        base["note"] = (
            f"convergence ratio R = {ratio:.3f}. The sequence is divergent: "
            f"refining the mesh is moving the answer further, so there is no "
            f"discretisation-error estimate to make and no band to quote. "
            f"This is a result, not a missing measurement."
        )
        return base

    if ratio < 0.0:
        s_u, s_l = max(s1, s2, s3), min(s1, s2, s3)
        base.update(
            classification="oscillatory",
            estimable=True,
            order_is_assumed=False,
            u_g=0.5 * (s_u - s_l),
            factor_of_safety=None,
            note=(
                f"convergence ratio R = {ratio:.3f}. Oscillatory convergence: "
                f"Richardson extrapolation does not apply, so the uncertainty "
                f"is bounded by the half-range of the levels per ITTC "
                f"7.5-03-01-01. No order of accuracy is defined."
            ),
        )
        return base

    if ratio == 0.0:
        base["classification"] = "degenerate"
        base["note"] = (
            "the two finest levels return identical values; the observed order "
            "is undefined."
        )
        return base

    p = math.log(abs(eps32 / eps21)) / math.log(r)
    denom = r**p - 1.0
    if denom <= 0:
        base["classification"] = "degenerate"
        base["note"] = "r^p - 1 is non-positive; no Richardson estimate exists."
        return base
    delta_re = eps21 / denom
    base.update(
        classification="monotonic",
        estimable=True,
        order_is_assumed=False,
        observed_order=p,
        richardson_error=delta_re,
        extrapolated_value=s1 - delta_re,
        factor_of_safety=ROACHE_FS_THREE_GRID,
        u_g=ROACHE_FS_THREE_GRID * abs(delta_re),
        note=(
            f"convergence ratio R = {ratio:.3f} (monotonic). The order of "
            f"accuracy p = {p:.3f} is MEASURED from the three levels, not "
            f"assumed, so Roache's factor of safety is "
            f"{ROACHE_FS_THREE_GRID}."
        ),
    )
    if not base["refinement_ratio_adequate"]:
        base["note"] += (
            f" Refinement ratio {min(r21, r32):.3f} is below the recommended "
            f"{MIN_REFINEMENT_RATIO}."
        )
    return base


# --------------------------------------------------------------------------- #
#  Plausibility - REPORTED, never gated
# --------------------------------------------------------------------------- #

def plausibility_report(
    *,
    ct: float,
    cp: float,
    cv: float,
    cf: float,
    froude: float,
    hull_class: str = "conventional_displacement",
    form_factor_band: Tuple[float, float] = FORM_FACTOR_BAND,
) -> Dict[str, Any]:
    """Physical plausibility of a coefficient set, with no referent.

    Three tiers, and the tier is on every check because they are not the same
    kind of statement:

    * ``identity``  - arithmetic. Ct = Cp + Cv holds by construction; a failure
      here is a bookkeeping defect, not physics.
    * ``sign``      - follows from the definitions plus "this is a displacement
      hull being towed in steady calm water". Cheap and hard to argue with.
    * ``band``      - engineering judgement over a hull population. This tier
      can only say ``implausible`` or ``not_implausible``. A number inside a
      band has not been confirmed by anything; it has merely failed to
      contradict a weak expectation. Outside the declared hull class the band
      declines to answer rather than guessing.

    None of these gate. They are evidence in the deliverable, in exactly the
    way ``scripts/cfd/yplus_after.sh`` made y+ evidence rather than a gate.
    """
    checks: List[Dict[str, Any]] = []
    identity_residual = abs((cp + cv) - ct)
    checks.append({
        "name": "ct_identity",
        "tier": "identity",
        "quantity": "Ct - (Cp + Cv)",
        "value": identity_residual,
        "verdict": (
            "holds"
            if identity_residual <= 1e-9 + 1e-6 * abs(ct)
            else "violated"
        ),
        "note": (
            "arithmetic, not evidence: the decomposition must sum or the "
            "component numbers mean nothing."
        ),
    })

    for name, value, label in (
        ("pressure_component_sign", cp, "Cp"),
        ("viscous_component_sign", cv, "Cv"),
    ):
        checks.append({
            "name": name,
            "tier": "sign",
            "quantity": label,
            "value": value,
            "verdict": "holds" if value > 0 else "violated",
            "note": (
                f"{label} <= 0 on a towed displacement hull in steady calm "
                f"water is unphysical - it would be thrust."
            ),
        })

    cr = ct - cf
    checks.append({
        "name": "residuary_sign",
        "tier": "sign",
        "quantity": "Ct - Cf(ITTC-57)",
        "value": cr,
        "verdict": "holds" if cr > 0 else "violated",
        "note": (
            "a total below the flat-plate correlation line at the same "
            "Reynolds number leaves no residuary resistance, which no "
            "wave-making hull can do."
        ),
    })

    implied_k1 = cv / cf if cf else float("nan")
    lo, hi = form_factor_band
    if hull_class in BANDED_HULL_CLASSES:
        if lo <= implied_k1 <= hi:
            ff_verdict = "not_implausible"
        else:
            ff_verdict = "implausible"
        ff_note = (
            f"Cv/Cf is the implied form factor (1 + k). The band "
            f"[{lo}, {hi}] is ENGINEERING JUDGEMENT widened around the range "
            f"conventional displacement hulls are commonly reported in by the "
            f"Prohaska method; this repository holds no primary source for it. "
            f"A value inside the band is not corroboration of anything - it "
            f"has only failed to contradict a weak expectation. A value below "
            f"1.0 is the strong signal: it says the hull generates less "
            f"viscous resistance than the equivalent flat plate."
        )
    else:
        ff_verdict = "not_applicable"
        ff_note = (
            f"hull class {hull_class!r} is outside the declared envelope "
            f"{sorted(BANDED_HULL_CLASSES)} for this band. A band applied "
            f"outside the population it was drawn from is worse than no band."
        )
    checks.append({
        "name": "implied_form_factor",
        "tier": "band",
        "quantity": "Cv / Cf(ITTC-57) = 1 + k",
        "value": implied_k1,
        "band": [lo, hi],
        "hull_class": hull_class,
        "verdict": ff_verdict,
        "note": ff_note,
    })

    checks.append({
        "name": "residuary_fraction",
        "tier": "reported",
        "quantity": "(Ct - Cf) / Ct",
        "value": cr / ct if ct else float("nan"),
        "band": None,
        "froude": froude,
        "verdict": "reported",
        "note": (
            "strongly Froude-dependent, and this repository holds no "
            "Froude-conditioned source for it. Quoting a band here would be "
            "an invented criterion, so the number is reported bare."
        ),
    })

    return {
        "checks": checks,
        "hull_class": hull_class,
        "froude": froude,
        "cf_ittc57": cf,
        "implausible_count": sum(
            1 for c in checks
            if c["verdict"] in ("implausible", "violated")
        ),
        "note": (
            "PLAUSIBILITY IS NOT VERIFICATION AND IT IS NOT A REFERENT. These "
            "observations can only distinguish 'implausible' from 'not "
            "implausible'. Nothing here confirms a number, and no verdict in "
            "this block gates anything."
        ),
    }


# --------------------------------------------------------------------------- #
#  Scoring a run with no referent
# --------------------------------------------------------------------------- #

def _reduce(force_dat: Path | str, config: ReferentFreeConfig):
    """Coefficient series and means for one level, on the declared area."""
    rows = read_force_rows(
        force_dat, window=config.averaging_window, drag_axis=config.drag_axis
    )
    factor = 2.0 if config.half_domain else 1.0
    mean_total = sum(r.total for r in rows) / len(rows)
    sign = -1.0 if mean_total < 0 else 1.0

    def c(value: float) -> float:
        return resistance_coefficient(
            sign * value * factor,
            config.density,
            config.wetted_surface.value_m2,
            config.velocity,
        )

    ct_series = [c(r.total) for r in rows]
    return {
        "rows": rows,
        "ct_series": ct_series,
        "ct": sum(ct_series) / len(ct_series),
        "cp": sum(c(r.pressure) for r in rows) / len(rows),
        "cv": sum(c(r.viscous) for r in rows) / len(rows),
        "force_total_N": sign * mean_total * factor,
        "samples": len(rows),
        "first_iteration": rows[0].iteration,
        "last_iteration": rows[-1].iteration,
    }


def evaluate_referent_free_run(
    force_dat: Path | str,
    config: ReferentFreeConfig,
    *,
    companions: Sequence[Tuple[Path | str, int]] = (),
    mesh_cells: Optional[int] = None,
) -> Dict[str, Any]:
    """Score a solved run for a hull with no published coefficient.

    ``companions`` are ``(force_dat, cell_count)`` pairs for the coarser mesh
    levels of a grid study, reduced on the SAME normalisation area and the same
    averaging window as the production level.

    The manifest deliberately does NOT contain ``criteria`` or ``all_passed``.
    Those keys belong to the KCS path, where there is something to have passed.
    What it contains instead:

    * ``validation``      - available: False, with the reason.
    * ``admissibility``   - can this number be REPORTED with a band at all?
      Three conditions, none of which is a statement about accuracy: the
      decomposition must sum, the averaging window must be an average, and the
      numerical uncertainty must be estimable. A run failing any of these is
      not wrong; it is not yet quotable.
    * ``plausibility``    - reported, never gated.
    * ``cross_check``     - declared blocked, not silently absent.
    * ``cannot_establish``- the explicit list.
    """
    production = _reduce(force_dat, config)
    cf = config.cf_ittc57

    iterative = iterative_uncertainty(production["ct_series"])
    window = averaging_window_adequacy(
        samples=production["samples"],
        window_iterations=config.averaging_window,
    )

    levels: List[GridLevel] = []
    if mesh_cells:
        levels.append(GridLevel("production", int(mesh_cells), production["ct"]))
    companion_blocks = []
    for index, (path, cells) in enumerate(companions):
        reduced = _reduce(path, config)
        levels.append(GridLevel(f"companion_{index}", int(cells), reduced["ct"]))
        companion_blocks.append({
            "name": f"companion_{index}",
            "cells": int(cells),
            "ct": reduced["ct"],
            "cp": reduced["cp"],
            "cv": reduced["cv"],
            "samples": reduced["samples"],
        })
    grid = grid_uncertainty(levels) if levels else grid_uncertainty([])

    u_i = iterative["u_i"] if window["statistic_available"] else None
    u_g = grid["u_g"]
    u_sn = (
        math.sqrt((u_i or 0.0) ** 2 + (u_g or 0.0) ** 2)
        if (u_g is not None and window["adequate"])
        else None
    )

    plausibility = plausibility_report(
        ct=production["ct"],
        cp=production["cp"],
        cv=production["cv"],
        cf=cf,
        froude=config.froude,
        hull_class=config.hull_class,
        form_factor_band=config.form_factor_band,
    )
    identity_holds = next(
        c["verdict"] == "holds"
        for c in plausibility["checks"] if c["name"] == "ct_identity"
    )

    reasons: List[str] = []
    if not identity_holds:
        reasons.append(
            "the force decomposition does not sum to the total: Ct = Cp + Cv "
            "must hold identically."
        )
    if not window["adequate"]:
        reasons.append(
            f"the averaging window holds {production['samples']} force "
            f"sample(s) against a stated minimum of "
            f"{AVERAGING_WINDOW_MIN_SAMPLES}; the mean is not a converged "
            f"average, so it must not be quoted with a band."
        )
    if not grid["estimable"]:
        reasons.append(
            "discretisation uncertainty is not estimable from the levels "
            f"supplied ({grid['levels']} level(s), classification "
            f"{grid['classification']!r}); a coefficient without a numerical "
            "band is a single number with unknown error."
        )
    admissible = not reasons

    ct = production["ct"]
    if admissible and u_sn is not None:
        statement = (
            f"Ct = {ct:.4e} +/- {u_sn:.2e} ({u_sn / ct:.2%}). The band is "
            f"NUMERICAL uncertainty only - iterative and discretisation - per "
            f"ITTC 7.5-03-01-01. It does NOT include modelling error "
            f"(turbulence closure, wall treatment, free-surface handling, "
            f"fixed attitude), which is not observable from the simulation "
            f"alone. This is a verified PREDICTION. It is not a validated "
            f"result: no measurement of this hull was used and none exists "
            f"here."
        )
    else:
        statement = (
            "No band may be quoted for this run. It is not a validated result "
            "and it is not yet a reportable prediction; the numerical "
            "uncertainty is not established. See admissibility.reasons."
        )

    return {
        "mode": "referent_free_prediction",
        "issue": "#2023",
        "validation": {
            "available": False,
            "referent_loaded": False,
            "reason": (
                "This hull has no published resistance coefficient. Validation "
                "compares a computed quantity with a measured one; with no "
                "measurement there is nothing to compare against, so "
                "validation is ABSENT rather than weakened. Everything below "
                "is verification, plausibility or reporting."
            ),
        },
        "provenance": {
            "case_name": config.name,
            "hull_class": config.hull_class,
            "lpp": config.lpp,
            "velocity": config.velocity,
            "reynolds": config.reynolds,
            "froude": config.froude,
            "density": config.density,
            "nu": config.nu,
            "half_domain": config.half_domain,
            "drag_axis": config.drag_axis,
            "averaging_window_iterations": config.averaging_window,
            "cf_ittc57": cf,
        },
        "measurement": {
            "ct": production["ct"],
            "cp": production["cp"],
            "cv": production["cv"],
            "force_total_N": production["force_total_N"],
            "samples": production["samples"],
            "window_first_iteration": production["first_iteration"],
            "window_last_iteration": production["last_iteration"],
            "mesh_cells": mesh_cells,
        },
        "normalisation": {
            "area_m2": config.wetted_surface.value_m2,
            "provenance": config.wetted_surface.provenance,
            "source": config.wetted_surface.source,
            "applies_to": ["ct", "cp", "cv"],
            "note": (
                "A coefficient is defined by what it is divided by. On the KCS "
                "path the published area is the gated one and the mesh-derived "
                "area is a disclosed diagnostic; here there is no publication, "
                "so the guard that refuses the mesh-derived area does not "
                "apply and the provenance is carried instead. Any comparison "
                "of this coefficient with any other number is meaningless "
                "unless that number used the same area."
            ),
        },
        "averaging_window": window,
        "iterative_convergence": iterative,
        "grid_convergence": grid,
        "companions": companion_blocks,
        "plausibility": plausibility,
        "cross_check": {
            "method": "holtrop_mennen",
            "status": "unavailable",
            "blocked_on": "#2020",
            "note": (
                "An empirical prediction from principal dimensions would be "
                "corroboration - NOT validation - and it is unavailable. "
                "digitalmodel's Holtrop-Mennen implementation returns a Ct "
                "that is near-identical for a Series 60 and a tanker (#2020), "
                "and that issue is blocked on obtaining the primary papers. "
                "Wiring a half-remembered formula in here would produce "
                "agreement that means nothing. When #2020 closes, the "
                "comparison must also assert the hull lies inside "
                "Holtrop-Mennen's stated applicability envelope; outside it "
                "the comparison is meaningless in either direction."
            ),
        },
        "uncertainty": {
            "u_i": u_i,
            "u_g": u_g,
            "u_sn": u_sn,
            "u_sn_percent": (u_sn / ct * 100.0) if u_sn is not None else None,
            "components": {
                "iterative": "ITTC 7.5-03-01-01 oscillatory criterion, "
                             "U_I = 0.5 * (S_U - S_L)",
                "grid": grid["note"],
            },
            "combination": "U_SN = sqrt(U_I^2 + U_G^2)",
            "excludes": [
                "turbulence-model form error",
                "wall-function error (the y+ regime is reported, not gated)",
                "free-surface / interface-compression modelling error",
                "geometric idealisation of the supplied hull surface",
                "attitude: the body is fixed, a real hull sinks and trims",
                "round-off, which is assumed negligible in double precision "
                "and has not been measured here",
            ],
        },
        "admissibility": {
            "admissible": admissible,
            "identity_holds": identity_holds,
            "averaging_window_adequate": window["adequate"],
            "numerical_uncertainty_estimable": grid["estimable"],
            "reasons": reasons,
            "note": (
                "Admissibility asks whether this number may be REPORTED with a "
                "band, not whether it is right. Failing it says the run is not "
                "quotable yet; passing it says nothing at all about accuracy."
            ),
        },
        "reported_result": {
            "ct": ct,
            "plus_minus": u_sn,
            "statement": statement,
        },
        "cannot_establish": [
            "the accuracy of Ct: with no measurement of this hull, the "
            "difference between the computed and the true value is not "
            "observable at any grid density.",
            "modelling (model-form) error: the turbulence closure, wall "
            "treatment and free-surface method are not verifiable against "
            "themselves.",
            "a validation uncertainty U_V: it requires an experimental "
            "uncertainty U_D, and there is no experiment.",
            "that agreement with an empirical method would constitute "
            "validation: an empirical regression over a ship population is "
            "corroboration with its own scatter and its own envelope.",
            "that a coefficient inside a plausibility band is correct: a band "
            "can only fail to contradict.",
            "transfer of the KCS validation to this hull, unless the regime - "
            "block coefficient, Froude number, Reynolds number, appendage "
            "state, attitude - is stated and checked. Regime transfer is a "
            "claim about similarity, and an unstated one is an assumption.",
        ],
    }
