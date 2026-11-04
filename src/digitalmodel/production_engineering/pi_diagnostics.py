# ABOUTME: Productivity index from well tests, Pwf sensitivity, Darcy radial PI and skin
# ABOUTME: Ordered pre-stimulation triage: Pwf source, depletion, mobility, comparability, skin

"""
Productivity Index Diagnostics
==============================
Computes the productivity index (PI, J) from a production test, the band on J
that a stated flowing bottomhole pressure (Pwf) error produces, the Darcy
radial PI for oil, skin from the ratio of tested to theoretical PI, the skin
pressure drop, flow efficiency and the PI ratio J2/J1 for a change of skin.

The triage orders the checks that come before a falling PI is read as
formation damage (issue #3830):

1. Pwf uncertainty: any stated Pwf error may leave the PI change unresolved.
2. Depletion: rate fell while PI held, reservoir pressure declined beyond the
   caller threshold, and operating conditions including Pwf stayed comparable.
3. Mobility: water cut or GOR changed, which lowers oil PI without damage.
4. Comparability: the tests are not stabilised or not taken under the same
   flow regime, choke, lift setting and Pwf within the caller tolerance.
5. Skin: only after steps 1 to 4 is the tested PI compared with the Darcy PI.

The triage stops at the first step that finds a cause and returns the cause,
the step number and the reason.

Equation basis (oil, field units: q in STB/d, k in md, h in ft, p in psi,
mu in cp, Bo in rb/STB, re and rw in ft):

    J = q / (Pr - Pwf)                                    (definition)
    J = k h / [141.2 mu Bo (ln(re/rw) - 0.75 + S)]        (pseudo-steady state)
    S = (ln(re/rw) - 0.75) (J_ideal / J_test - 1)         (from the J ratio)
    dP_skin = 141.2 q mu Bo S / (k h)
    FE = (Pr - Pwf - dP_skin) / (Pr - Pwf)
    J2/J1 = (ln(re/rw) - 0.75 + S1) / (ln(re/rw) - 0.75 + S2)

Equation references: Petroleum Production Systems, second edition (2013),
section 1.3.1, equations (1-6) and (1-8), pp. 11-13, gives the PI definition
and circular-drainage pseudo-steady-state geometry term. Section 1.4,
Example 1-1, equation (1-10), p. 16, gives the field-unit coefficient and
units. The publisher's open sample was inspected; a worked-example
qualification of this implementation remains unestablished:
https://ptgmedia.pearsoncmg.com/images/9780137031580/samplepages/0137031580.pdf

All triage thresholds must be supplied explicitly by the caller, including
reservoir-pressure decline and Pwf comparability tolerances in psi. No field
screening criterion is supplied by this module. A positive nominal reservoir
pressure decline beyond that tolerance is evidence only within the stated
single-phase model; reservoir-pressure uncertainty is not propagated.

Limits: single-phase oil above the bubble point (linear IPR); vertical well,
circular drainage area, no non-Darcy (rate-dependent) skin. Below the bubble
point or with free gas, J is not constant and these results do not apply.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from typing import Optional, Union

#: Field-unit coefficient: Petroleum Production Systems, 2nd ed., Eq. (1-10).
DARCY_OIL_FIELD_CONSTANT = 141.2

#: Circular-drainage term: Petroleum Production Systems, 2nd ed., Eq. (1-8).
_PSS_SHAPE_TERM = 0.75

# A few dozen binary64 ulps cover roundoff in the radial PI arithmetic and
# ratio cancellation at zero skin. This is numerical, not a screening limit.
_PI_ROUNDOFF_REL_TOL = 32.0 * math.ulp(1.0)


class PwfSource(Enum):
    """How the flowing bottomhole pressure of a test was obtained."""

    MEASURED = "measured"  # downhole gauge at or corrected to the perforations
    CALCULATED = "calculated"  # from wellhead pressure through a VLP or gradient


class TriageCause(Enum):
    """Outcome of the ordered pre-stimulation triage."""

    PWF_UNCERTAINTY = "pwf_uncertainty"
    DEPLETION = "depletion"
    NO_DECLINE = "no_decline"
    MOBILITY_CHANGE = "mobility_change"
    NOT_COMPARABLE = "not_comparable"
    SKIN_DAMAGE = "skin_damage"
    NO_SKIN_DAMAGE = "no_skin_damage"
    INSUFFICIENT_DATA = "insufficient_data"


# ---------------------------------------------------------------------------
# Validation helpers
# ---------------------------------------------------------------------------


def _finite(name: str, value: float) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite, got {value!r}")
    return value


def _positive(name: str, value: float) -> float:
    value = _finite(name, value)
    if value <= 0.0:
        raise ValueError(f"{name} must be positive, got {value!r}")
    return value


def _non_negative(name: str, value: float) -> float:
    value = _finite(name, value)
    if value < 0.0:
        raise ValueError(f"{name} must not be negative, got {value!r}")
    return value


def _as_source(pwf_source: Union[PwfSource, str]) -> PwfSource:
    if isinstance(pwf_source, PwfSource):
        return pwf_source
    try:
        return PwfSource(str(pwf_source).lower())
    except ValueError as exc:
        raise ValueError(
            f"pwf_source must be 'measured' or 'calculated', got {pwf_source!r}"
        ) from exc


def _geometry_term(re_ft: float, rw_ft: float) -> float:
    """ln(re/rw) - 0.75 for a circular drainage area (dimensionless)."""
    re_ft = _positive("drainage_radius_ft", re_ft)
    rw_ft = _positive("wellbore_radius_ft", rw_ft)
    if re_ft <= rw_ft:
        raise ValueError(
            f"drainage_radius_ft ({re_ft}) must exceed wellbore_radius_ft ({rw_ft})"
        )
    return math.log(re_ft / rw_ft) - _PSS_SHAPE_TERM


# ---------------------------------------------------------------------------
# 1. PI from a test with a Pwf band
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class PiTestResult:
    """PI of one test and its band for a stated Pwf error.

    Units: rates STB/d, pressures psi, PI STB/d/psi. The low PI corresponds
    to Pwf - error (larger drawdown); the high PI to Pwf + error (smaller
    drawdown). When the error is not less than the drawdown, the high end is
    unbounded (infinite) and ``band_bounded`` is False. At zero rate the
    nominal and low PI are zero; the upper bound retains that pressure-domain
    rule. Relative changes from zero nominal PI are undefined (NaN).
    """

    q_oil_stb_d: float
    drawdown_psi: float
    pi_stb_d_psi: float
    pwf_source: PwfSource
    pwf_error_psi: float
    pi_low_stb_d_psi: float
    pi_high_stb_d_psi: float
    band_bounded: bool

    @property
    def low_change_fraction(self) -> float:
        """(J_low - J)/J; -0.25 means -25 %; NaN at zero nominal PI."""
        if self.pi_stb_d_psi == 0.0:
            return math.nan
        return self.pi_low_stb_d_psi / self.pi_stb_d_psi - 1.0

    @property
    def high_change_fraction(self) -> float:
        """(J_high - J)/J; infinite if unbounded; NaN at zero nominal PI."""
        if self.pi_stb_d_psi == 0.0:
            return math.nan
        return self.pi_high_stb_d_psi / self.pi_stb_d_psi - 1.0


def pi_from_test(
    q: float,
    pr: float,
    pwf: float,
    pwf_source: Union[PwfSource, str],
    pwf_error_psi: Optional[float] = None,
) -> PiTestResult:
    """PI of a test, J = q/(Pr - Pwf), with a +/- Pwf error band.

    Args:
        q: oil rate, STB/d (>= 0).
        pr: average reservoir pressure, psi.
        pwf: flowing bottomhole pressure, psi (< pr).
        pwf_source: ``PwfSource`` or 'measured' / 'calculated'.
        pwf_error_psi: +/- error on Pwf, psi. Required for a calculated Pwf;
            defaults to 0 for a measured Pwf.

    Example: q = 300, Pr = 2,000, Pwf = 1,700 and error 100 psi give
    J = 1.0 and a band of 0.75 to 1.5 STB/d/psi (-25 % / +50 %).

    Raises:
        ValueError: on non-finite input, negative rate or error, a
            non-positive drawdown, or a calculated Pwf without an error.
    """
    q = _non_negative("q", q)
    pr = _finite("pr", pr)
    pwf = _finite("pwf", pwf)
    source = _as_source(pwf_source)
    if pwf_error_psi is None:
        if source is PwfSource.CALCULATED:
            raise ValueError(
                "pwf_error_psi is required when the Pwf is calculated; "
                "state the error of the VLP or gradient used"
            )
        pwf_error_psi = 0.0
    error = _non_negative("pwf_error_psi", pwf_error_psi)
    drawdown = pr - pwf
    if drawdown <= 0.0:
        raise ValueError(f"drawdown Pr - Pwf must be positive, got {drawdown!r} psi")

    pi = q / drawdown
    pi_low = q / (drawdown + error)
    bounded = error < drawdown
    pi_high = q / (drawdown - error) if bounded else math.inf
    return PiTestResult(
        q_oil_stb_d=q,
        drawdown_psi=drawdown,
        pi_stb_d_psi=pi,
        pwf_source=source,
        pwf_error_psi=error,
        pi_low_stb_d_psi=pi_low,
        pi_high_stb_d_psi=pi_high,
        band_bounded=bounded,
    )


# ---------------------------------------------------------------------------
# 2. Darcy radial PI, skin, dP_skin, flow efficiency and PI ratio
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ReservoirRockFluid:
    """Rock, fluid and geometry inputs for the Darcy radial PI (field units)."""

    permeability_md: float
    net_pay_ft: float
    oil_viscosity_cp: float
    oil_fvf_rb_stb: float
    drainage_radius_ft: float
    wellbore_radius_ft: float

    def kh_md_ft(self) -> float:
        return _positive("permeability_md", self.permeability_md) * _positive(
            "net_pay_ft", self.net_pay_ft
        )

    def mu_bo(self) -> float:
        return _positive("oil_viscosity_cp", self.oil_viscosity_cp) * _positive(
            "oil_fvf_rb_stb", self.oil_fvf_rb_stb
        )

    def geometry_term(self) -> float:
        """ln(re/rw) - 0.75 (dimensionless)."""
        return _geometry_term(self.drainage_radius_ft, self.wellbore_radius_ft)


def darcy_radial_pi(rock_fluid: ReservoirRockFluid, skin: float = 0.0) -> float:
    """Pseudo-steady-state Darcy radial PI for oil, STB/d/psi.

    J = k h / [141.2 mu Bo (ln(re/rw) - 0.75 + S)], field units.

    Raises:
        ValueError: on invalid inputs or when ln(re/rw) - 0.75 + S <= 0.
    """
    skin = _finite("skin", skin)
    kh = rock_fluid.kh_md_ft()
    mu_bo = rock_fluid.mu_bo()
    denominator = rock_fluid.geometry_term() + skin
    if denominator <= 0.0:
        raise ValueError(
            f"ln(re/rw) - 0.75 + S must be positive, got {denominator!r}; "
            "the skin is outside the range of the radial equation"
        )
    return kh / (DARCY_OIL_FIELD_CONSTANT * mu_bo * denominator)


def skin_from_pi(
    pi_test: float, pi_ideal: float, re_ft: float, rw_ft: float
) -> float:
    """Skin from tested and zero-skin theoretical PI (dimensionless).

    S = (ln(re/rw) - 0.75) (J_ideal / J_test - 1). ``pi_ideal`` is the Darcy
    PI at S = 0 for the same rock, fluid and geometry.
    """
    pi_test = _positive("pi_test", pi_test)
    pi_ideal = _positive("pi_ideal", pi_ideal)
    return _geometry_term(re_ft, rw_ft) * (pi_ideal / pi_test - 1.0)


def skin_pressure_drop_psi(
    q: float, rock_fluid: ReservoirRockFluid, skin: float
) -> float:
    """Pressure drop across the skin zone, psi.

    dP_skin = 141.2 q mu Bo S / (k h), with q in STB/d. Negative for a
    stimulated well (S < 0).
    """
    q = _non_negative("q", q)
    skin = _finite("skin", skin)
    return DARCY_OIL_FIELD_CONSTANT * q * rock_fluid.mu_bo() * skin / rock_fluid.kh_md_ft()


def flow_efficiency(pr: float, pwf: float, dp_skin_psi: float) -> float:
    """Flow efficiency FE = (Pr - Pwf - dP_skin)/(Pr - Pwf) (dimensionless)."""
    pr = _finite("pr", pr)
    pwf = _finite("pwf", pwf)
    dp_skin_psi = _finite("dp_skin_psi", dp_skin_psi)
    drawdown = pr - pwf
    if drawdown <= 0.0:
        raise ValueError(f"drawdown Pr - Pwf must be positive, got {drawdown!r} psi")
    return (drawdown - dp_skin_psi) / drawdown


def pi_ratio(re_ft: float, rw_ft: float, skin_before: float, skin_after: float) -> float:
    """PI ratio J2/J1 for a change of skin from S1 to S2 (dimensionless).

    J2/J1 = (ln(re/rw) - 0.75 + S1) / (ln(re/rw) - 0.75 + S2).
    """
    x = _geometry_term(re_ft, rw_ft)
    s1 = _finite("skin_before", skin_before)
    s2 = _finite("skin_after", skin_after)
    if x + s1 <= 0.0 or x + s2 <= 0.0:
        raise ValueError("ln(re/rw) - 0.75 + S must be positive for both skins")
    return (x + s1) / (x + s2)


# ---------------------------------------------------------------------------
# 3. Ordered pre-stimulation triage
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class WellTestPoint:
    """One production test as used by the triage.

    Units: q in STB/d, pressures in psi, water cut as a fraction (0-1),
    GOR in scf/STB, choke in 64ths of an inch.
    """

    q_oil_stb_d: float
    pr_psi: float
    pwf_psi: float
    pwf_source: Union[PwfSource, str]
    pwf_error_psi: Optional[float]
    water_cut_fraction: float
    gor_scf_stb: float
    stabilised: bool
    flow_regime: str
    choke_64ths: Optional[float]
    lift_setting: Optional[str]

    def pi_result(self) -> PiTestResult:
        return pi_from_test(
            self.q_oil_stb_d,
            self.pr_psi,
            self.pwf_psi,
            self.pwf_source,
            pwf_error_psi=self.pwf_error_psi,
        )


@dataclass(frozen=True)
class TriageThresholds:
    """Required caller screening tolerances; no published defaults.

    Fractions are dimensionless, choke is in 64ths of an inch, pressure
    tolerances are in psi. Every value must be finite and non-negative.
    """

    pi_change_fraction: float
    rate_drop_fraction: float
    water_cut_change: float
    gor_change_fraction: float
    choke_change_64ths: float
    reservoir_pressure_drop_psi: float
    pwf_change_psi: float

    def __post_init__(self) -> None:
        for name in self.__dataclass_fields__:
            object.__setattr__(self, name, _non_negative(name, getattr(self, name)))


@dataclass(frozen=True)
class TriageFinding:
    """Result of one triage step."""

    step: int
    name: str
    cause_found: bool
    detail: str


@dataclass(frozen=True)
class TriageResult:
    """Outcome of the triage: the first cause found and the steps evaluated."""

    cause: TriageCause
    step: int
    reason: str
    findings: tuple[TriageFinding, ...]
    current_pi: PiTestResult
    previous_pi: PiTestResult
    skin: Optional[float] = None
    skin_pressure_drop_psi: Optional[float] = None
    flow_efficiency: Optional[float] = None
    pi_ratio_if_skin_removed: Optional[float] = None


def _comparability_issues(
    current: WellTestPoint, previous: WellTestPoint, thresholds: TriageThresholds
) -> list[str]:
    """Operating differences that preclude attributing decline to depletion or skin."""
    issues = []
    if not current.stabilised or not previous.stabilised:
        issues.append("a test is not stabilised")
    if current.flow_regime != previous.flow_regime:
        issues.append(f"flow regime differs ({previous.flow_regime} to {current.flow_regime})")
    if current.choke_64ths is not None and previous.choke_64ths is not None:
        if abs(current.choke_64ths - previous.choke_64ths) > thresholds.choke_change_64ths:
            issues.append(
                f"choke differs ({previous.choke_64ths:g} to {current.choke_64ths:g} /64 in)"
            )
    elif (current.choke_64ths is None) != (previous.choke_64ths is None):
        issues.append("choke recorded on one test only")
    if current.lift_setting != previous.lift_setting:
        issues.append(f"lift setting differs ({previous.lift_setting} to {current.lift_setting})")
    if abs(current.pwf_psi - previous.pwf_psi) > thresholds.pwf_change_psi:
        issues.append("flowing bottomhole pressure changed beyond tolerance")
    return issues


def pre_stimulation_triage(
    current: WellTestPoint,
    previous: WellTestPoint,
    rock_fluid: Optional[ReservoirRockFluid],
    thresholds: TriageThresholds,
) -> TriageResult:
    """Ordered checks before a PI fall is attributed to skin.

    All screening thresholds are required caller inputs. Zero flow on either
    test yields insufficient data before relative PI or skin inversion.
    Steps: 1 Pwf uncertainty, 2 depletion (pressure evidence and comparable
    conditions required), 3 mobility, 4 comparability, 5 skin. Positive skin
    within 32 binary64 ulps of equal tested/ideal PI is snapped to zero.
    A current PI band spanning zero skin leaves the skin sign unresolved,
    even when the historical PI bands are separate. Only Pwf uncertainty is
    propagated; ideal PI from the supplied rock/fluid inputs is held fixed.
    """
    cur = current.pi_result()
    prev = previous.pi_result()
    findings: list[TriageFinding] = []

    def stop(cause: TriageCause, step: int, name: str, reason: str, **extra) -> TriageResult:
        findings.append(TriageFinding(step, name, True, reason))
        return TriageResult(
            cause=cause,
            step=step,
            reason=reason,
            findings=tuple(findings),
            current_pi=cur,
            previous_pi=prev,
            **extra,
        )

    def passed(step: int, name: str, detail: str) -> None:
        findings.append(TriageFinding(step, name, False, detail))

    if cur.q_oil_stb_d == 0.0 or prev.q_oil_stb_d == 0.0:
        return stop(
            TriageCause.INSUFFICIENT_DATA,
            1,
            "flow",
            "positive oil flow is required on both tests for relative PI and skin diagnosis",
        )

    # Step 1: Pwf uncertainty applies to measured and calculated pressures.
    uncertain = [
        f"{label} ({res.pwf_source.value})"
        for label, res in (("current", cur), ("previous", prev))
        if res.pwf_error_psi > 0.0
    ]
    if uncertain:
        overlap = (
            cur.pi_low_stb_d_psi <= prev.pi_high_stb_d_psi
            and prev.pi_low_stb_d_psi <= cur.pi_high_stb_d_psi
        )
        if not cur.band_bounded or not prev.band_bounded or overlap:
            return stop(
                TriageCause.PWF_UNCERTAINTY,
                1,
                "pwf_source",
                f"Pwf uncertainty on the {' and '.join(uncertain)} test leaves "
                "the PI change unresolved: bands overlap or an upper bound is "
                f"unbounded (current {cur.pi_low_stb_d_psi:.3f} to "
                f"{cur.pi_high_stb_d_psi:.3f}, previous {prev.pi_low_stb_d_psi:.3f} "
                f"to {prev.pi_high_stb_d_psi:.3f} STB/d/psi); "
                "reduce Pwf uncertainty before further diagnosis",
            )
        passed(1, "pwf_source", "stated Pwf errors give bounded, separate PI bands")
    else:
        passed(1, "pwf_source", "no Pwf error stated on either test")

    # Step 2: depletion (rate against PI).
    pi_change = cur.pi_stb_d_psi / prev.pi_stb_d_psi - 1.0
    rate_drop = 1.0 - cur.q_oil_stb_d / prev.q_oil_stb_d
    if abs(pi_change) <= thresholds.pi_change_fraction:
        if rate_drop > thresholds.rate_drop_fraction:
            issues = _comparability_issues(current, previous, thresholds)
            if issues:
                return stop(
                    TriageCause.NOT_COMPARABLE, 2, "depletion",
                    "; ".join(issues) + "; constant PI and falling rate do not resolve depletion",
                )
            pressure_drop = previous.pr_psi - current.pr_psi
            if pressure_drop <= thresholds.reservoir_pressure_drop_psi:
                return stop(
                    TriageCause.INSUFFICIENT_DATA, 2, "depletion",
                    "constant PI and falling rate lack reservoir-pressure decline "
                    "beyond the stated tolerance; depletion is unresolved",
                )
            return stop(
                TriageCause.DEPLETION,
                2,
                "depletion",
                f"rate fell {rate_drop:.1%} while PI held ({pi_change:+.1%}); the "
                f"drawdown fell from {prev.drawdown_psi:.1f} to {cur.drawdown_psi:.1f} "
                f"psi, reservoir pressure fell {pressure_drop:.1f} psi beyond the "
                "stated tolerance with comparable conditions; consistent with depletion",
            )
        return stop(
            TriageCause.NO_DECLINE,
            2,
            "depletion",
            f"PI held ({pi_change:+.1%}) and rate change {-rate_drop:+.1%}; no decline to diagnose",
        )
    if pi_change > 0:
        return stop(
            TriageCause.NO_DECLINE,
            2,
            "depletion",
            f"PI rose {pi_change:+.1%}; no decline to diagnose",
        )
    passed(2, "depletion", f"PI fell {pi_change:+.1%}, beyond the PI tolerance")

    # Step 3: mobility (water cut and GOR).
    reasons = []
    d_wc = _finite("water_cut_fraction", current.water_cut_fraction) - _finite(
        "water_cut_fraction", previous.water_cut_fraction
    )
    if abs(d_wc) > thresholds.water_cut_change:
        reasons.append(
            f"water cut changed from {previous.water_cut_fraction:.3f} to "
            f"{current.water_cut_fraction:.3f}"
        )
    gor_prev = _finite("gor_scf_stb", previous.gor_scf_stb)
    gor_cur = _finite("gor_scf_stb", current.gor_scf_stb)
    if gor_prev == 0.0 and gor_cur > 0.0:
        reasons.append(f"GOR rose from zero to {gor_cur:.0f} scf/STB")
    elif gor_prev > 0:
        d_gor = gor_cur / gor_prev - 1.0
        if abs(d_gor) > thresholds.gor_change_fraction:
            reasons.append(f"GOR changed {d_gor:+.1%} ({gor_prev:.0f} to {gor_cur:.0f} scf/STB)")
    if reasons:
        return stop(
            TriageCause.MOBILITY_CHANGE,
            3,
            "mobility",
            "; ".join(reasons) + "; oil PI falls with mobility without new damage",
        )
    passed(3, "mobility", "water cut and GOR within tolerance")

    # Step 4: comparability.
    issues = _comparability_issues(current, previous, thresholds)
    if issues:
        return stop(
            TriageCause.NOT_COMPARABLE,
            4,
            "comparability",
            "; ".join(issues) + "; retest under the same conditions before diagnosis",
        )
    passed(4, "comparability", "tests stabilised and taken under the same conditions")

    # Step 5: skin.
    if rock_fluid is None:
        return stop(
            TriageCause.INSUFFICIENT_DATA,
            5,
            "skin",
            "skin step needs k, h, mu, Bo, re and rw; theoretical PI not calculated",
        )
    pi_ideal = darcy_radial_pi(rock_fluid, skin=0.0)
    skin = skin_from_pi(
        cur.pi_stb_d_psi,
        pi_ideal,
        rock_fluid.drainage_radius_ft,
        rock_fluid.wellbore_radius_ft,
    )
    if math.isclose(cur.pi_stb_d_psi, pi_ideal, rel_tol=_PI_ROUNDOFF_REL_TOL, abs_tol=0.0):
        skin = 0.0
    if cur.pwf_error_psi > 0.0:
        # Skin decreases as PI increases: invert the high PI for the low skin.
        skin_low, skin_high = (
            0.0
            if math.isclose(pi, pi_ideal, rel_tol=_PI_ROUNDOFF_REL_TOL, abs_tol=0.0)
            else skin_from_pi(
                pi, pi_ideal, rock_fluid.drainage_radius_ft, rock_fluid.wellbore_radius_ft
            )
            for pi in (cur.pi_high_stb_d_psi, cur.pi_low_stb_d_psi)
        )
        if skin_low <= 0.0 <= skin_high:
            return stop(
                TriageCause.PWF_UNCERTAINTY,
                5,
                "skin",
                f"current Pwf uncertainty gives a skin band from {skin_low:.2f} "
                f"to {skin_high:.2f} that includes zero skin; separate historical "
                "PI bands resolve a decline but not its attribution to positive "
                "skin; reduce Pwf uncertainty before further diagnosis",
            )
    dp_skin = skin_pressure_drop_psi(cur.q_oil_stb_d, rock_fluid, skin)
    fe = flow_efficiency(current.pr_psi, current.pwf_psi, dp_skin)
    ratio = pi_ratio(
        rock_fluid.drainage_radius_ft, rock_fluid.wellbore_radius_ft, skin, 0.0
    )
    values = dict(
        skin=skin,
        skin_pressure_drop_psi=dp_skin,
        flow_efficiency=fe,
        pi_ratio_if_skin_removed=ratio,
    )
    if skin > 0.0:
        return stop(
            TriageCause.SKIN_DAMAGE,
            5,
            "skin",
            f"tested PI {cur.pi_stb_d_psi:.3f} against Darcy PI {pi_ideal:.3f} STB/d/psi "
            f"gives S = {skin:.2f}, dP_skin = {dp_skin:.1f} psi, FE = {fe:.3f}; "
            f"removing the skin would multiply PI by {ratio:.3f}",
            **values,
        )
    return stop(
        TriageCause.NO_SKIN_DAMAGE,
        5,
        "skin",
        f"tested PI {cur.pi_stb_d_psi:.3f} against Darcy PI {pi_ideal:.3f} STB/d/psi "
        f"gives S = {skin:.2f}; the PI fall is not explained by positive skin",
        **values,
    )


__all__ = [
    "DARCY_OIL_FIELD_CONSTANT",
    "PiTestResult",
    "PwfSource",
    "ReservoirRockFluid",
    "TriageCause",
    "TriageFinding",
    "TriageResult",
    "TriageThresholds",
    "WellTestPoint",
    "darcy_radial_pi",
    "flow_efficiency",
    "pi_from_test",
    "pi_ratio",
    "pre_stimulation_triage",
    "skin_from_pi",
    "skin_pressure_drop_psi",
]
