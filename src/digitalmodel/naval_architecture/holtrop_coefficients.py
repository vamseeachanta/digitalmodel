# ABOUTME: Holtrop-Mennen (1984) regression coefficients for resistance prediction
# ABOUTME: Intermediate coefficients c1-c16, m1, m4, lambda used by holtrop_mennen.py
"""
Holtrop-Mennen (1984) regression coefficients.

Separated from the main module to keep files under 200 lines.
All coefficients follow the 1984 revision paper notation.
"""

import math

# --- Physical constants (SI) ---
RHO_SW = 1025.0
NU_SW = 1.1892e-6
G = 9.80665


def length_of_run(lwl: float, cp: float, lcb_pct: float) -> float:
    """Approximate length of run LR (m)."""
    return lwl * (1.0 - cp + 0.06 * cp * lcb_pct / (4.0 * cp - 1.0))


def coeff_c12(t_over_l: float) -> float:
    """Stern shape coefficient c12 = f(T/L)."""
    if t_over_l < 0.02:
        return 0.479948
    elif t_over_l > 0.05:
        return 0.479948
    else:
        return 48.20 * (t_over_l - 0.02) ** 2.078 + 0.479948


def coeff_c13(cstern: int) -> float:
    """c13 = 1 + 0.003 * Cstern."""
    return 1.0 + 0.003 * cstern


def coeff_c7(b_over_l: float) -> float:
    """Wave resistance coefficient c7 = f(B/LWL)."""
    if b_over_l < 0.11:
        return 0.229577 * b_over_l ** 0.33333
    elif b_over_l < 0.25:
        return b_over_l
    else:
        return 0.5 - 0.0625 / b_over_l


def half_angle_of_entrance(
    lwl: float, beam: float, cwp: float,
    cp: float, lcb_pct: float, lr: float, cb: float,
) -> float:
    """Half angle of entrance iE (degrees). Holtrop 1984 Eq. 2."""
    return 1.0 + 89.0 * math.exp(
        -(lwl / beam) ** 0.80856
        * (1.0 - cwp) ** 0.30484
        * (1.0 - cp - 0.0225 * lcb_pct) ** 0.6367
        * (lr / beam) ** 0.34574
        * (100.0 * cb / lwl) ** 0.16302
    )


def coeff_c1(
    lwl: float, beam: float, draft: float,
    cb: float, cwp: float, cp: float,
    lcb_pct: float, lr: float,
) -> float:
    """Wave resistance coefficient c1. Holtrop 1984 Eq. 1."""
    _c7 = coeff_c7(beam / lwl)
    ie = half_angle_of_entrance(
        lwl, beam, cwp, cp, lcb_pct, lr, cb,
    )
    return (
        2223105.0 * _c7 ** 3.78613
        * (draft / beam) ** 1.07961
        * (90.0 - ie) ** (-1.37565)
    )


def coeff_c3(abt: float, beam: float, draft: float, tf: float) -> float:
    """Bulbous bow coefficient c3."""
    if abt <= 0.0:
        return 0.0
    return 0.56 * abt ** 1.5 / (beam * draft * (0.31 * math.sqrt(abt) + tf))


def coeff_c2(c3_val: float) -> float:
    """c2 = exp(-1.89 * sqrt(c3))."""
    return math.exp(-1.89 * math.sqrt(c3_val))


def coeff_c4(t_over_l: float) -> float:
    """c4 = T/LWL when T/LWL <= 0.04, else 0.04."""
    return min(t_over_l, 0.04)


def coeff_c5(at: float, beam: float, draft: float, cwp: float) -> float:
    """Transom coefficient c5."""
    if at <= 0.0:
        return 1.0
    return 1.0 - 0.8 * at / (beam * draft * cwp)


def coeff_c16(cp: float) -> float:
    """c16 = f(Cp) for exponent m1."""
    if cp < 0.80:
        return 8.07981 * cp - 13.8673 * cp ** 2 + 6.984388 * cp ** 3
    else:
        return 1.73014 - 0.7067 * cp


def coeff_c15(l3_over_vol: float) -> float:
    """c15 = f(L^3/nabla) for exponent m4."""
    if l3_over_vol < 512.0:
        return -1.69385
    elif l3_over_vol > 1726.91:
        return 0.0
    else:
        return -1.69385 + (l3_over_vol / 2.36) / 512.0


def exponent_m1(
    lwl: float, draft: float, beam: float,
    vol: float, cp: float,
) -> float:
    """Exponent m1 for wave resistance. Holtrop 1984."""
    _c16 = coeff_c16(cp)
    return (
        0.0140407 * lwl / draft
        - 1.75254 * vol ** (1.0 / 3.0) / lwl
        - 4.79323 * beam / lwl
        - _c16
    )


def lambda_wave(lwl: float, beam: float, cp: float) -> float:
    """Wave resistance lambda coefficient."""
    l_over_b = lwl / beam
    if l_over_b < 12.0:
        return 1.446 * cp - 0.03 * l_over_b
    else:
        return 1.446 * cp - 0.36
