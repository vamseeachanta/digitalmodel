# ABOUTME: Holtrop-Mennen (1984) statistical resistance prediction method
# ABOUTME: RT = RF(1+k1) + RAPP + RW + RB + RTR + RA for ship powering
"""Holtrop-Mennen resistance prediction (1984 revision).

Refs: Holtrop & Mennen (1982), Holtrop (1984).
"""

import math

from digitalmodel.naval_architecture.holtrop_coefficients import (
    G,
    NU_SW,
    RHO_SW,
    coeff_c1,
    coeff_c2,
    coeff_c3,
    coeff_c4,
    coeff_c5,
    coeff_c12,
    coeff_c13,
    coeff_c15,
    exponent_m1,
    lambda_wave,
    length_of_run,
)


def _validate_positive(**kwargs: float) -> None:
    """Raise ValueError if any argument is non-positive."""
    for name, value in kwargs.items():
        if value <= 0:
            raise ValueError(f"{name} must be positive, got {value}")


def form_factor_k1(
    lwl: float, beam: float, draft: float,
    cp: float, lcb_pct: float, cstern: int,
) -> float:
    """Form factor (1+k1) from hull parameters. Typically 1.1-1.45."""
    _validate_positive(lwl=lwl, beam=beam, draft=draft, cp=cp)
    t_over_l = draft / lwl
    lr = length_of_run(lwl, cp, lcb_pct)
    _c12 = coeff_c12(t_over_l)
    _c13 = coeff_c13(cstern)
    k1_inner = (
        0.93
        + _c12 * (beam / lr) ** 0.92497
        * (0.95 - cp) ** (-0.521448)
        * (1.0 - cp + 0.0225 * lcb_pct) ** 0.6906
    )
    return _c13 * k1_inner


def wetted_surface_holtrop(
    lwl: float, beam: float, draft: float,
    cb: float, cm: float, cwp: float, abt: float,
) -> float:
    """Holtrop wetted surface approximation (m^2)."""
    s = lwl * (2.0 * draft + beam) * math.sqrt(cm) * (
        0.4530 + 0.4425 * cb - 0.2862 * cm
        - 0.003467 * beam / draft + 0.3696 * cwp
    ) + 2.38 * abt / cb
    return s


def frictional_resistance(
    lwl: float, speed_ms: float, wetted_surface: float,
) -> float:
    """Frictional resistance RF using ITTC 1957 (N)."""
    _validate_positive(lwl=lwl, speed_ms=speed_ms)
    rn = speed_ms * lwl / NU_SW
    cf = 0.075 / (math.log10(rn) - 2.0) ** 2
    return 0.5 * RHO_SW * speed_ms ** 2 * wetted_surface * cf


def wave_resistance(
    lwl: float, beam: float, draft: float,
    cb: float, cm: float, cwp: float, cp: float,
    lcb_pct: float, cstern: int, speed_ms: float,
    abt: float, tf: float, hb: float, at: float,
) -> float:
    """Wave-making resistance RW (N) for Fn < 0.4."""
    _validate_positive(lwl=lwl, beam=beam, draft=draft, speed_ms=speed_ms)
    fn = speed_ms / math.sqrt(G * lwl)
    vol = lwl * beam * draft * cb
    weight = RHO_SW * G * vol

    lr = length_of_run(lwl, cp, lcb_pct)
    _c1 = coeff_c1(lwl, beam, draft, cb, cwp, cp, lcb_pct, lr)
    _c3 = coeff_c3(abt, beam, draft, tf)
    _c2 = coeff_c2(_c3)
    _c5 = coeff_c5(at, beam, draft, cwp)

    _m1 = exponent_m1(lwl, draft, beam, vol, cp)
    d = -0.9
    l3_vol = lwl ** 3 / vol
    _c15 = coeff_c15(l3_vol)
    m4 = _c15 * 0.4 * math.exp(-0.034 * fn ** (-3.29))
    _lambda = lambda_wave(lwl, beam, cp)

    rw_over_w = (
        _c1 * _c2 * _c5
        * math.exp(_m1 * fn ** d + m4 * math.cos(_lambda * fn ** (-2)))
    )
    return max(0.0, rw_over_w * weight)


def appendage_resistance(
    speed_ms: float, appendage_areas: list[float],
    appendage_factors: list[float], lwl: float,
) -> float:
    """Appendage resistance RAPP (N)."""
    if not appendage_areas:
        return 0.0
    _validate_positive(speed_ms=speed_ms, lwl=lwl)
    rn = speed_ms * lwl / NU_SW
    cf = 0.075 / (math.log10(rn) - 2.0) ** 2
    s_app = sum(appendage_areas)
    k2_equiv = sum(
        a * f for a, f in zip(appendage_areas, appendage_factors)
    ) / s_app
    return 0.5 * RHO_SW * speed_ms ** 2 * s_app * cf * k2_equiv


def bulbous_bow_resistance(
    speed_ms: float, abt: float, tf: float, hb: float,
) -> float:
    """Bulbous bow resistance RB (N)."""
    if abt <= 0.0:
        return 0.0
    depth = tf - hb - 0.25 * math.sqrt(abt)
    if depth <= 0.0:
        return 0.0
    fn_i = speed_ms / math.sqrt(G * depth)
    pb = 0.56 * math.sqrt(abt) / (tf - 1.5 * hb)
    return (
        0.11 * math.exp(-3.0 * pb ** (-2))
        * fn_i ** 3 * abt ** 1.5 * RHO_SW * G
        / (1.0 + fn_i ** 2)
    )


def transom_resistance(
    speed_ms: float, at: float, beam: float,
    cwp: float, lwl: float,
) -> float:
    """Immersed transom resistance RTR (N)."""
    if at <= 0.0:
        return 0.0
    fn_t = speed_ms / math.sqrt(
        2.0 * G * at / (beam + beam * cwp) + 1e-10
    )
    c6 = max(0.0, 0.2 * (1.0 - 0.2 * fn_t))
    return 0.5 * RHO_SW * speed_ms ** 2 * at * c6


def correlation_allowance(lwl: float, cb: float, draft: float = 0.0) -> float:
    """Model-ship correlation allowance CA (dimensionless)."""
    t_over_l = draft / lwl if draft > 0 else 0.04
    _c4 = coeff_c4(t_over_l)
    ca = (
        0.006 * (lwl + 100.0) ** (-0.16)
        - 0.00205
        + 0.003 * math.sqrt(lwl / 7.5)
        * cb ** 4 * (0.04 - _c4)
    )
    return ca


def total_resistance(
    lwl: float, beam: float, draft: float,
    cb: float, cm: float, cwp: float, cp: float,
    lcb_pct: float, cstern: int, speed_ms: float,
    abt: float, tf: float, hb: float, at: float,
    appendage_areas: list[float] | None = None,
    appendage_factors: list[float] | None = None,
) -> float:
    """Total resistance RT (N) combining all components."""
    _validate_positive(lwl=lwl, speed_ms=speed_ms)
    s = wetted_surface_holtrop(lwl, beam, draft, cb, cm, cwp, abt)
    k1 = form_factor_k1(lwl, beam, draft, cp, lcb_pct, cstern)
    rf = frictional_resistance(lwl, speed_ms, s)
    rw = wave_resistance(
        lwl, beam, draft, cb, cm, cwp, cp,
        lcb_pct, cstern, speed_ms, abt, tf, hb, at,
    )
    rapp = appendage_resistance(
        speed_ms, appendage_areas or [], appendage_factors or [], lwl,
    )
    rb = bulbous_bow_resistance(speed_ms, abt, tf, hb)
    rtr = transom_resistance(speed_ms, at, beam, cwp, lwl)
    ca = correlation_allowance(lwl, cb, draft)
    ra = 0.5 * RHO_SW * speed_ms ** 2 * s * ca
    return rf * k1 + rapp + rw + rb + rtr + ra


def effective_power(resistance_n: float, speed_ms: float) -> float:
    """Effective power PE = RT * V (watts)."""
    return resistance_n * speed_ms
