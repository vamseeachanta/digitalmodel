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

1. Pwf source: a calculated Pwf with a stated error may place the PI change
   inside the measurement band.
2. Depletion: rate fell while PI held, so the drawdown fell.
3. Mobility: water cut or GOR changed, which lowers oil PI without damage.
4. Comparability: the tests are not stabilised or not taken under the same
   flow regime, choke and lift setting.
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

The pseudo-steady-state radial inflow equation for a well centred in a closed
circular drainage area, with Pr the average reservoir pressure, is the
standard textbook form (for example Ahmed, T., Reservoir Engineering Handbook;
Dake, L.P., Fundamentals of Reservoir Engineering). The constant
141.2 = 1/0.00708 converts Darcy units to the field units above. No edition
or page is pinned in this module; the tests are hand-checked reproductions of
the arithmetic, and a published worked example is not established.

Thresholds of the triage (PI and rate tolerances, water-cut and GOR change
limits) are screening inputs supplied by the caller. Their defaults are
round screening values, not values from a published criterion.

Limits: single-phase oil above the bubble point (linear IPR); vertical well,
circular drainage area, no non-Darcy (rate-dependent) skin. Below the bubble
point or with free gas, J is not constant and these results do not apply.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from typing import Optional, Union

#: Darcy-to-field-unit constant for oil radial flow (STB/d, md, ft, psi, cp, rb/STB).
DARCY_OIL_FIELD_CONSTANT = 141.2

#: Pseudo-steady-state shape term for a circular drainage area (dimensionless).
_PSS_SHAPE_TERM = 0.75


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
    unbounded (infinite) and ``band_bounded`` is False.
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
        """(J_low - J)/J; -0.25 means -25 %."""
        return self.pi_low_stb_d_psi / self.pi_stb_d_psi - 1.0

    @property
    def high_change_fraction(self) -> float:
        """(J_high - J)/J; +0.50 means +50 %; infinite when unbounded."""
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
    """Screening tolerances of the triage (caller-supplied; defaults are round
    screening values, not from a published criterion)."""

    pi_change_fraction: float = 0.10  # |dJ|/J_prev within this -> PI held
    rate_drop_fraction: float = 0.10  # (q_prev - q)/q_prev above this -> rate fell
    water_cut_change: float = 0.05  # absolute change in water-cut fraction
    gor_change_fraction: float = 0.20  # |dGOR|/GOR_prev
    choke_change_64ths: float = 0.0  # any choke change beyond this -> not comparable


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


def pre_stimulation_triage(
    current: WellTestPoint,
    previous: WellTestPoint,
    rock_fluid: Optional[ReservoirRockFluid],
    thresholds: TriageThresholds = TriageThresholds(),
) -> TriageResult:
    """Ordered checks before a PI fall is attributed to skin.

    Steps, in order, stopping at the first that finds a cause:
    1 Pwf source, 2 depletion (rate against PI), 3 mobility (water cut and
    GOR), 4 comparability (stabilisation, flow regime, choke, lift), 5 skin
    (tested PI of the current test against the Darcy PI at S = 0).
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

    # Step 1: Pwf source.
    calculated = [
        label
        for label, res in (("current", cur), ("previous", prev))
        if res.pwf_source is PwfSource.CALCULATED
    ]
    if calculated:
        overlap = (
            cur.pi_low_stb_d_psi <= prev.pi_high_stb_d_psi
            and prev.pi_low_stb_d_psi <= cur.pi_high_stb_d_psi
        )
        if overlap:
            return stop(
                TriageCause.PWF_UNCERTAINTY,
                1,
                "pwf_source",
                f"Pwf of the {' and '.join(calculated)} test is calculated; the PI "
                f"bands overlap (current {cur.pi_low_stb_d_psi:.3f} to "
                f"{cur.pi_high_stb_d_psi:.3f}, previous {prev.pi_low_stb_d_psi:.3f} "
                f"to {prev.pi_high_stb_d_psi:.3f} STB/d/psi), so the PI change is "
                "not resolved; measure Pwf downhole before further diagnosis",
            )
        passed(1, "pwf_source", "Pwf calculated but the PI bands do not overlap")
    else:
        passed(1, "pwf_source", "Pwf measured on both tests")

    # Step 2: depletion (rate against PI).
    pi_change = cur.pi_stb_d_psi / prev.pi_stb_d_psi - 1.0 if prev.pi_stb_d_psi > 0 else math.inf
    if prev.q_oil_stb_d > 0:
        rate_drop = 1.0 - cur.q_oil_stb_d / prev.q_oil_stb_d
    else:
        rate_drop = 0.0
    if abs(pi_change) <= thresholds.pi_change_fraction:
        if rate_drop > thresholds.rate_drop_fraction:
            return stop(
                TriageCause.DEPLETION,
                2,
                "depletion",
                f"rate fell {rate_drop:.1%} while PI held ({pi_change:+.1%}); the "
                f"drawdown fell from {prev.drawdown_psi:.1f} to {cur.drawdown_psi:.1f} "
                "psi, which points to depletion, not damage",
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
    if gor_prev > 0:
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
