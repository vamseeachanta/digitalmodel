"""
ABOUTME: Form-factor extraction from a double-body run (#2023). A double-body
solve replaces the free surface with a symmetry plane at the waterline, so it
generates no waves and the resistance it reports is viscous alone. Dividing
that by the ITTC-57 correlation line at the same Reynolds number gives the
form factor:

    (1 + k) = C_v,double-body / C_f,ITTC-57

WHAT THIS MODULE WILL AND WILL NOT SAY
--------------------------------------
It follows the posture already set by ``referent_free_resistance``: band
verdicts are ``implausible`` / ``not_implausible``, never ``passed`` and never
``validated``. A band cannot confirm a number; it can only fail to contradict
it. The vocabulary is deliberately shared with that module so the two surfaces
cannot come to mean different things, and the band itself is imported from it
rather than restated.

There is ONE verdict here that is stronger than a band, and it is not a band
verdict at all:

    (1 + k) < 1.0 IS PHYSICALLY IMPOSSIBLE.

It asserts that the hull generates LESS viscous resistance than the flat plate
of the same wetted area at the same Reynolds number - that curvature, boundary
layer thickening and viscous form drag together subtract from friction. No
hull does this. A run that returns it has a defect, and the defect is reported
as a defect rather than as a small form factor. The KCS validation on this
codebase returned an implied form factor near 0.91 while its force integral
was reading the dry topsides at water density (see
``tests/solvers/openfoam/test_forces_density_source.py``), and that defect is
known to follow the code. Four hours of double-body solve that ends in this
verdict is worth more than four hundred that ends in a plausible-looking
total.

WHY THE TOTAL AND NOT THE FRICTION INTEGRAL
-------------------------------------------
``C_v,double-body`` is this run's TOTAL coefficient: pressure plus friction.
The pressure part of a double-body result is the viscous form drag, and that
is exactly the quantity k measures. Reducing only the friction component
returns (1 + k) near unity by construction - it would compare the hull's
friction with a flat plate's friction and find, unsurprisingly, that they are
similar - and the number it produced would look like a converged answer.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple

from digitalmodel.solvers.openfoam.validation.referent_free_resistance import (
    BANDED_HULL_CLASSES,
    FORM_FACTOR_BAND,
    MIN_SAMPLES_FOR_A_STATISTIC,
    NormalisationArea,
)
from digitalmodel.solvers.openfoam.validation.ship_resistance import (
    ittc57_friction_coefficient,
    read_force_rows,
    resistance_coefficient,
)

__all__ = [
    "FORCE_STABILITY_BUDGET",
    "FORM_FACTOR_FLOOR",
    "FORM_FACTOR_REPORTING_RESOLUTION",
    "DoubleBodyRunConfig",
    "evaluate_double_body_run",
    "form_factor_from_coefficients",
    "form_factor_verdict",
]

#: The floor, and it is not a tolerance. Below it the result is not a small
#: form factor, it is a hull with less viscous resistance than a flat plate.
FORM_FACTOR_FLOOR = 1.0

#: (1 + k) is quoted to two decimal places wherever it is used - a form factor
#: is 1.25, not 1.2473 - so this is the resolution the last reported digit
#: claims.
FORM_FACTOR_REPORTING_RESOLUTION = 0.01

#: DERIVED from the line above, not chosen and not tuned against a result: for
#: (1 + k) near the middle of the declared band, a mean force that still moves
#: by more than this fraction cannot support the digit being printed.
FORCE_STABILITY_BUDGET = FORM_FACTOR_REPORTING_RESOLUTION / (
    sum(FORM_FACTOR_BAND) / 2.0
)


class DoubleBodyRunConfig:
    """Condition of a converged double-body run.

    Not a dataclass with a wetted-surface float: the area is a
    :class:`NormalisationArea`, so it cannot enter without its provenance. A
    coefficient is defined by what it is divided by, and a form factor is a
    ratio of two coefficients - if the double-body area and the area behind
    the ITTC-57 line's application are not the same surface, the ratio is not
    a form factor.
    """

    def __init__(
        self,
        *,
        name: str,
        lpp: float,
        velocity: float,
        reynolds: float,
        density: float,
        wetted_surface: NormalisationArea,
        averaging_window: int,
        half_domain: bool = True,
        drag_axis: int = 0,
        hull_class: str = "conventional_displacement",
        form_factor_band: Tuple[float, float] = FORM_FACTOR_BAND,
    ) -> None:
        for label, value in (
            ("lpp", lpp),
            ("velocity", velocity),
            ("reynolds", reynolds),
            ("density", density),
        ):
            if value <= 0:
                raise ValueError(f"{label} must be positive, got {value}")
        if averaging_window <= 0:
            raise ValueError(
                "averaging_window must be a positive ITERATION span; a window "
                "of zero silently averages the start-up transient into the mean"
            )
        self.name = name
        self.lpp = lpp
        self.velocity = velocity
        self.reynolds = reynolds
        self.density = density
        self.wetted_surface = wetted_surface
        self.averaging_window = averaging_window
        self.half_domain = half_domain
        self.drag_axis = drag_axis
        self.hull_class = hull_class
        self.form_factor_band = form_factor_band

    @property
    def cf_ittc57(self) -> float:
        return ittc57_friction_coefficient(self.reynolds)


# --------------------------------------------------------------------------- #
#  The ratio
# --------------------------------------------------------------------------- #

def form_factor_from_coefficients(
    cv_double_body: float, cf_ittc57: float
) -> Tuple[float, float]:
    """``((1 + k), k)`` from the double-body coefficient and the ITTC-57 line."""
    if cf_ittc57 <= 0:
        raise ValueError(
            f"C_f(ITTC-57) must be positive, got {cf_ittc57}. The form factor "
            f"is a ratio to it and is undefined otherwise."
        )
    one_plus_k = cv_double_body / cf_ittc57
    return one_plus_k, one_plus_k - 1.0


def form_factor_verdict(
    *,
    cv_double_body: float,
    cf_ittc57: float,
    hull_class: str = "conventional_displacement",
    form_factor_band: Tuple[float, float] = FORM_FACTOR_BAND,
) -> Dict[str, Any]:
    """(1 + k), k, and an honest verdict on them.

    TWO checks, at two different tiers, because they are not the same kind of
    statement and only one of them is class-conditional:

    * ``sign``  - (1 + k) >= 1. This follows from what viscous resistance IS,
      for any hull of any class, and a failure is a DEFECT rather than a
      number. It is never suppressed by an unrecognised hull class.
    * ``band``  - engineering judgement over a hull population, imported from
      ``referent_free_resistance`` so the two surfaces cannot disagree. It can
      only say ``implausible`` or ``not_implausible``, and outside the
      declared class it declines to answer rather than guessing.
    """
    one_plus_k, k = form_factor_from_coefficients(cv_double_body, cf_ittc57)
    lo, hi = form_factor_band
    checks = [_sign_check(one_plus_k, k), _band_check(one_plus_k, hull_class, lo, hi)]
    defect = checks[0]["verdict"] == "violated"

    return {
        "quantity": "C_v,double-body / C_f(ITTC-57) = 1 + k",
        "one_plus_k": one_plus_k,
        "k": k,
        "cv_double_body": cv_double_body,
        "cf_ittc57": cf_ittc57,
        "hull_class": hull_class,
        "band": [lo, hi],
        "defect": defect,
        "verdict": checks[0]["verdict"] if defect else checks[1]["verdict"],
        "checks": checks,
        "note": (
            "A FORM FACTOR IS NOT A VALIDATION. Nothing here compares this "
            "hull with a measurement of this hull, and no verdict in this "
            "block confirms a number - the band can only fail to contradict "
            "a weak expectation. The sign check is different in kind: it can "
            "only reject."
        ),
    }


def _sign_check(one_plus_k: float, k: float) -> Dict[str, Any]:
    violated = one_plus_k < FORM_FACTOR_FLOOR
    return {
        "name": "form_factor_exceeds_the_flat_plate",
        "tier": "sign",
        "quantity": "1 + k",
        "value": one_plus_k,
        "floor": FORM_FACTOR_FLOOR,
        "verdict": "violated" if violated else "holds",
        "note": (
            (
                f"(1 + k) = {one_plus_k:.4f}, i.e. k = {k:+.1%}. THIS IS "
                f"PHYSICALLY IMPOSSIBLE AND IS REPORTED AS A DEFECT, NOT AS A "
                f"FORM FACTOR: it says the hull generates less viscous "
                f"resistance than the flat plate of the same wetted area at "
                f"the same Reynolds number, so curvature and viscous form "
                f"drag would have to subtract from friction. Look at the "
                f"force integral before the hull. A value near 0.91 was "
                f"measured on this codebase's benchmark run while its forces "
                f"function object integrated the dry topsides at water "
                f"density; that class of defect hits the friction integral "
                f"and leaves pressure almost unmoved, so compare the two "
                f"components before concluding anything about the hull."
            )
            if violated
            else (
                f"(1 + k) = {one_plus_k:.4f} is at or above the flat-plate "
                f"floor of {FORM_FACTOR_FLOOR}. This is the weakest possible "
                f"statement about the result and the only one that holds for "
                f"every hull class: it has not been shown to be impossible."
            )
        ),
    }


def _band_check(
    one_plus_k: float, hull_class: str, lo: float, hi: float
) -> Dict[str, Any]:
    if hull_class not in BANDED_HULL_CLASSES:
        verdict = "not_applicable"
        note = (
            f"hull class {hull_class!r} is outside the declared envelope "
            f"{sorted(BANDED_HULL_CLASSES)} for this band. A band applied "
            f"outside the population it was drawn from is worse than no band. "
            f"The sign check above still applies: it is not class-conditional."
        )
    elif lo <= one_plus_k <= hi:
        verdict = "not_implausible"
        note = (
            f"(1 + k) = {one_plus_k:.4f} lies inside the band [{lo}, {hi}]. "
            f"That band is ENGINEERING JUDGEMENT widened around the range "
            f"conventional displacement hulls are commonly reported in by the "
            f"Prohaska method; this repository holds no primary source for "
            f"it. A value inside it is not corroboration of anything - it has "
            f"only failed to contradict a weak expectation."
        )
    elif one_plus_k < lo:
        verdict = "implausible"
        note = (
            f"(1 + k) = {one_plus_k:.4f} is above the flat-plate floor but "
            f"below the band [{lo}, {hi}]: the hull is being reported as "
            f"nearly a flat plate, which a displacement form is not. This is "
            f"the SUSPICIOUS region rather than the impossible one - a "
            f"partially-corrected force integral, an under-resolved boundary "
            f"layer or a wetted surface that is too large all land here, and "
            f"so does a genuinely fine hull with an unusually low form "
            f"factor. It cannot be told apart from this number alone."
        )
    else:
        verdict = "implausible"
        note = (
            f"(1 + k) = {one_plus_k:.4f} is above the band [{lo}, {hi}]. Check "
            f"the normalisation area and whether wave-making has leaked into "
            f"this run - a double-body case that still carries a free surface "
            f"reports residuary resistance as if it were viscous."
        )
    return {
        "name": "implied_form_factor",
        "tier": "band",
        "quantity": "1 + k",
        "value": one_plus_k,
        "band": [lo, hi],
        "hull_class": hull_class,
        "verdict": verdict,
        "note": note,
    }


# --------------------------------------------------------------------------- #
#  Reducing a solved run
# --------------------------------------------------------------------------- #

def _reduce(force_dat: Path | str, config: DoubleBodyRunConfig) -> Dict[str, Any]:
    """Coefficients and the force series over the declared averaging window."""
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

    series = [c(r.total) for r in rows]
    return {
        "series": series,
        "cv_double_body": sum(series) / len(series),
        "cv_pressure": sum(c(r.pressure) for r in rows) / len(rows),
        "cv_friction": sum(c(r.viscous) for r in rows) / len(rows),
        "force_total_N": sign * mean_total * factor,
        "samples": len(rows),
        "first_iteration": rows[0].iteration,
        "last_iteration": rows[-1].iteration,
    }


def force_stability(series: Sequence[float]) -> Dict[str, Any]:
    """Has the steady solve's force stopped moving?

    NOT the ITTC oscillatory criterion. That criterion estimates iterative
    uncertainty from the amplitude of a residual OSCILLATION, which is what a
    pseudo-transient free-surface run produces. A converged steady SIMPLE run
    approaches its answer monotonically and has no oscillation to take a
    half-range of, so the same statistic applied here would report a tiny
    number for a run that had merely slowed down. Drift is the quantity that
    matters, and it is scored against a budget derived from the resolution
    (1 + k) is reported to.
    """
    values = [float(v) for v in series]
    n = len(values)
    mean = sum(values) / n if n else float("nan")
    if n < MIN_SAMPLES_FOR_A_STATISTIC:
        return {
            "verdict": "insufficient_samples",
            "samples": n,
            "relative_drift": None,
            "relative_half_range": None,
            "budget": FORCE_STABILITY_BUDGET,
            "note": (
                f"{n} sample(s) in the window: fewer than "
                f"{MIN_SAMPLES_FOR_A_STATISTIC}. Two points define a "
                f"difference, not a trend, and the mean has no residual "
                f"degrees of freedom."
            ),
        }
    drift = abs(values[-1] - values[0]) / abs(mean) if mean else float("inf")
    half_range = 0.5 * (max(values) - min(values)) / abs(mean) if mean else float("inf")
    stable = max(drift, half_range) <= FORCE_STABILITY_BUDGET
    return {
        "verdict": "stable" if stable else "drifting",
        "samples": n,
        "relative_drift": drift,
        "relative_half_range": half_range,
        "budget": FORCE_STABILITY_BUDGET,
        "note": (
            f"end-to-end drift {drift:.2%} and half-range {half_range:.2%} "
            f"over {n} samples, against a budget of "
            f"{FORCE_STABILITY_BUDGET:.2%}. That budget is derived, not "
            f"chosen: (1 + k) is reported to "
            f"{FORM_FACTOR_REPORTING_RESOLUTION}, and near the middle of the "
            f"declared band that digit needs a force mean stable to this "
            f"fraction. A drifting run has not produced a form factor; it has "
            f"produced the value the force happened to have when the solver "
            f"stopped."
        ),
    }


def evaluate_double_body_run(
    force_dat: Path | str,
    config: DoubleBodyRunConfig,
    *,
    mesh_cells: Optional[int] = None,
) -> Dict[str, Any]:
    """Score a converged double-body run and extract the form factor."""
    reduced = _reduce(force_dat, config)
    cf = config.cf_ittc57
    verdict = form_factor_verdict(
        cv_double_body=reduced["cv_double_body"],
        cf_ittc57=cf,
        hull_class=config.hull_class,
        form_factor_band=config.form_factor_band,
    )
    stability = force_stability(reduced["series"])
    reasons = _admissibility_reasons(reduced, stability, verdict)

    return {
        "mode": "double_body_form_factor",
        "issue": "#2023",
        "provenance": {
            "case_name": config.name,
            "hull_class": config.hull_class,
            "lpp": config.lpp,
            "velocity": config.velocity,
            "reynolds": config.reynolds,
            "density": config.density,
            "half_domain": config.half_domain,
            "drag_axis": config.drag_axis,
            "averaging_window_iterations": config.averaging_window,
            "cf_ittc57": cf,
            "mesh_cells": mesh_cells,
        },
        "normalisation": {
            "area_m2": config.wetted_surface.value_m2,
            "provenance": config.wetted_surface.provenance,
            "source": config.wetted_surface.source,
            "note": (
                "the form factor is a ratio of coefficients, so the area "
                "cancels ONLY if the ITTC-57 line is applied on this same "
                "surface. It is carried here so that comparison is checkable."
            ),
        },
        "measurement": {
            "cv_double_body": reduced["cv_double_body"],
            "cv_pressure": reduced["cv_pressure"],
            "cv_friction": reduced["cv_friction"],
            "pressure_fraction": (
                reduced["cv_pressure"] / reduced["cv_double_body"]
                if reduced["cv_double_body"]
                else float("nan")
            ),
            "force_total_N": reduced["force_total_N"],
            "samples": reduced["samples"],
            "window_first_iteration": reduced["first_iteration"],
            "window_last_iteration": reduced["last_iteration"],
            "note": (
                "cv_double_body is the TOTAL: with no free surface there is "
                "no wave-making component for it to contain. The split is "
                "reported because a defect in the friction integral moves one "
                "component and not the other."
            ),
        },
        "force_convergence": stability,
        "form_factor": verdict,
        "admissibility": {
            "admissible": not reasons,
            "reasons": reasons,
            "note": (
                "Admissibility asks whether this form factor may be REPORTED, "
                "not whether it is right. Failing it says the run is not "
                "quotable; passing it says nothing about accuracy."
            ),
        },
        "cannot_establish": [
            "that k is correct: a form factor derived this way is a property "
            "of the computation - grid, turbulence closure and wall treatment "
            "included - and no measurement of this hull was used.",
            "that the ITTC-57 line is the right flat-plate reference: it is a "
            "correlation line fitted to towing-tank data, not a measured "
            "friction coefficient, and k inherits whatever it carries.",
            "that k is speed-independent: it is treated as constant by the "
            "form-factor method, and a single double-body run cannot show "
            "that it is.",
            "that the free-surface run this k will be applied to shares its "
            "boundary-layer resolution. If the two meshes differ near the "
            "wall, the difference lands in k.",
        ],
    }


def _admissibility_reasons(
    reduced: Dict[str, Any],
    stability: Dict[str, Any],
    verdict: Dict[str, Any],
) -> List[str]:
    reasons: List[str] = []
    if verdict["defect"]:
        reasons.append(
            f"(1 + k) = {verdict['one_plus_k']:.4f} is below the flat-plate "
            f"floor of {FORM_FACTOR_FLOOR}: this is a defect in the run, not "
            f"a form factor, and it must not be carried forward as one."
        )
    if stability["verdict"] != "stable":
        reasons.append(
            f"the force has not settled ({stability['verdict']}): "
            f"{stability['note']}"
        )
    if reduced["cv_pressure"] <= 0:
        reasons.append(
            f"the pressure component of the double-body force is "
            f"{reduced['cv_pressure']:.4e}, which is not positive. Viscous "
            f"form drag on a towed hull is a resistance; a non-positive value "
            f"means the integral, the sign convention or the patch selection "
            f"is wrong, and the form factor built on it is meaningless."
        )
    if not math.isfinite(verdict["one_plus_k"]):
        reasons.append("(1 + k) is not finite")
    return reasons
