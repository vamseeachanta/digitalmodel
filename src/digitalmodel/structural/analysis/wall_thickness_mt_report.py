# ABOUTME: Multi-code pipeline/riser wall thickness M-T interaction HTML report generator
# ABOUTME: Supports API RP 1111, API RP 2RD, API STD 2RD with per-code conditions and envelopes

"""
Wall Thickness — Moment-Tension Interaction Report

Generates a professional HTML report presenting:
0. Executive Summary (PASS/FAIL verdict, governing check, margin)
1. Pressure-only checks (burst, collapse, propagation/hoop)
2. Capacity limits by operating condition (per-code design factors)
3. Combined loading Von Mises interaction contour (M vs T)
4. Allowable bending envelope by operating condition
5. Key engineering points summary table (with margin %)
6. Input data (geometry, material, design conditions, derived properties)

Supports: API RP 1111, API RP 2RD, API STD 2RD.
API STD 2RD additionally traces Method 1 (linear) and Method 2 (cosine)
M-T interaction envelopes.

Uses existing WallThicknessAnalyzer infrastructure and Plotly for the
interactive contour chart and envelope plot.
"""

import math
import logging
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY

logger = logging.getLogger(__name__)

# API RP 1111 operating conditions with design factors.
# Designed as a replaceable data structure so other codes (DNV, PD 8010)
# can supply their own condition-factor mappings.
API_RP_1111_CONDITIONS = [
    {"name": "Normal Operating", "f_d": 0.72, "color": "#1f77b4", "dash": "solid"},
    {"name": "Installation",     "f_d": 0.80, "color": "#2ca02c", "dash": "dash"},
    {"name": "Extreme",          "f_d": 0.96, "color": "#ff7f0e", "dash": "dashdot"},
    {"name": "Survival",         "f_d": 1.00, "color": "#d62728", "dash": "dot"},
]

API_RP_2RD_CONDITIONS = [
    {"name": "Operating",    "f_d": 0.667, "color": "#1f77b4", "dash": "solid"},
    {"name": "Installation", "f_d": 0.800, "color": "#2ca02c", "dash": "dash"},
    {"name": "Extreme",      "f_d": 0.800, "color": "#ff7f0e", "dash": "dashdot"},
    {"name": "Survival",     "f_d": 1.000, "color": "#d62728", "dash": "dot"},
]

API_STD_2RD_CONDITIONS = [
    {"name": "SLS (Operating)",  "f_d": 0.80, "color": "#1f77b4", "dash": "solid"},
    {"name": "ULS (Extreme)",    "f_d": 0.80, "color": "#ff7f0e", "dash": "dash"},
    {"name": "ALS (Accidental)", "f_d": 1.00, "color": "#d62728", "dash": "dot"},
]

CONDITIONS_BY_CODE = {
    DesignCode.API_RP_1111: API_RP_1111_CONDITIONS,
    DesignCode.API_RP_2RD:  API_RP_2RD_CONDITIONS,
    DesignCode.API_STD_2RD: API_STD_2RD_CONDITIONS,
}


def generate_mt_report(
    geometry: PipeGeometry,
    material: PipeMaterial,
    internal_pressure: float,
    external_pressure: float,
    code: DesignCode = DesignCode.API_RP_1111,
    output_path: Optional[str] = None,
    edition: Optional[int] = None,
) -> str:
    """Generate a wall thickness M-T interaction HTML report.

    Supports API RP 1111, API RP 2RD, and API STD 2RD design codes.
    For API STD 2RD, Method 1 (linear) and Method 2 (cosine) envelopes
    are overlaid on the envelope chart.

    Args:
        geometry: Pipe geometric properties.
        material: Pipe material properties.
        internal_pressure: Internal pressure in Pa.
        external_pressure: External pressure in Pa.
        code: Design code (default API RP 1111).
        output_path: If provided, write HTML to this path.
        edition: Optional edition year (e.g. 2015 for API RP 1111 4th Ed).

    Returns:
        Complete HTML string.
    """
    strategy_cls = CODE_REGISTRY.get(code)
    if strategy_cls is None:
        raise ValueError(f"No strategy registered for {code}")
    if edition is not None:
        try:
            strategy = strategy_cls(edition=edition)
        except TypeError:
            strategy = strategy_cls()
    else:
        strategy = strategy_cls()

    # Build edition label for report header
    edition_label = ""
    if hasattr(strategy, "edition_info"):
        edition_label = f" — {strategy.edition_info.edition_label} ({strategy.edition_info.edition_year})"

    conditions = CONDITIONS_BY_CODE.get(code, API_RP_1111_CONDITIONS)

    loads = DesignLoads(
        internal_pressure=internal_pressure,
        external_pressure=external_pressure,
    )
    factors = DesignFactors()

    # Run pressure-only checks
    raw_results = strategy.run_checks(geometry, material, loads, factors)

    # Compute section properties
    D = geometry.outer_diameter
    t = geometry.wall_thickness
    d_i = D - 2 * t

    A = math.pi / 4 * (D**2 - d_i**2)
    I_val = math.pi / 64 * (D**4 - d_i**4)
    Z = I_val / (D / 2)

    # Plastic capacities
    M_p = strategy.compute_plastic_moment(geometry, material)
    T_y = strategy.compute_plastic_tension(geometry, material)

    # Hoop stress and allowable (use most restrictive condition for key points)
    p_net = internal_pressure - external_pressure
    sigma_h = p_net * d_i / (2 * t) if t > 0 else 0.0
    f_d = conditions[0]["f_d"]
    sigma_allow = f_d * material.smys

    # Build key engineering points
    key_points = _compute_key_points(T_y, M_p, A, Z, sigma_h, sigma_allow)

    # Build contour chart HTML
    contour_html = _build_contour_chart(
        D, t, d_i, material.smys, material.smts,
        internal_pressure, external_pressure,
        f_d, A, Z, sigma_h, sigma_allow, T_y, M_p,
        code=code,
    )

    # Compute capacity limits and envelopes for all operating conditions
    capacity_limits = _compute_capacity_limits(
        A, Z, sigma_h, material.smys, T_y, M_p, conditions,
    )

    envelopes = {}
    for cond in conditions:
        lim = next(c for c in capacity_limits if c["name"] == cond["name"])
        T_neg = lim["T_neg_kN"] * 1e3  # back to SI
        T_pos = lim["T_pos_kN"] * 1e3
        T_vals, M_vals = _trace_envelope(
            A, Z, sigma_h, material.smys, cond["f_d"],
            T_neg, T_pos, M_p, n_points=200,
        )
        envelopes[cond["name"]] = (T_vals, M_vals)

    # For API STD 2RD, also trace Method 1 (linear) and Method 2 (cosine) envelopes
    method_envelopes = {}
    if code == DesignCode.API_STD_2RD:
        smys = material.smys
        smts = material.smts
        for cond in conditions:
            f_d_cond = cond["f_d"]
            lim = next(c for c in capacity_limits if c["name"] == cond["name"])
            T_neg_si = lim["T_neg_kN"] * 1e3
            T_pos_si = lim["T_pos_kN"] * 1e3

            m1_T, m1_M = _trace_envelope_method1(
                D, t, A, smys, smts, f_d_cond, p_net,
                T_neg_si, T_pos_si, M_p, T_y,
            )
            m2_T, m2_M = _trace_envelope_method2(
                D, t, A, smys, smts, f_d_cond, p_net,
                T_neg_si, T_pos_si, M_p, T_y,
            )
            method_envelopes[cond["name"]] = {
                "method1": (m1_T, m1_M),
                "method2": (m2_T, m2_M),
            }

    envelope_html = _build_envelope_chart(
        envelopes, T_y, M_p, conditions, method_envelopes,
    )

    # Pressure sensitivity: baseline, +50%, +100%
    pressure_factors = [1.0, 1.5, 2.0]
    pressure_sensitivity = _compute_pressure_sensitivity(
        A, Z, d_i, t, internal_pressure, external_pressure,
        material.smys, T_y, M_p, conditions,
        pressure_factors,
    )
    pressure_chart_html = _build_pressure_sensitivity_chart(
        pressure_sensitivity, T_y, M_p, internal_pressure, conditions,
    )

    # Build full HTML
    html = _build_html(
        geometry, material, internal_pressure, external_pressure,
        code, raw_results, A, Z, I_val, T_y, M_p, sigma_h,
        key_points, contour_html, capacity_limits, envelope_html,
        envelopes, pressure_sensitivity, pressure_chart_html,
        conditions, edition_label=edition_label,
    )

    if output_path is not None:
        path = Path(output_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            f.write(html)
        logger.info("M-T report written to %s", output_path)

    return html


# ---------------------------------------------------------------------------
# Key engineering points
# ---------------------------------------------------------------------------

def _compute_key_points(
    T_y: float,
    M_p: float,
    A: float,
    Z: float,
    sigma_h: float,
    sigma_allow: float,
) -> List[Dict]:
    """Compute utilisation at key engineering points on the M-T plane."""
    fractions = [0.0, 0.5, 0.8, 1.0]
    points = []

    # Pure tension points
    for frac in fractions:
        T = frac * T_y
        M = 0.0
        util = _von_mises_util_single(T, M, A, Z, sigma_h, sigma_allow)
        label = "Pressure only (origin)" if frac == 0.0 else f"T = {frac:.1f} T_y"
        points.append(_point_row(label, T, M, T_y, M_p, util))

    # Pure bending points (skip origin, already added)
    for frac in fractions[1:]:
        T = 0.0
        M = frac * M_p
        util = _von_mises_util_single(T, M, A, Z, sigma_h, sigma_allow)
        label = f"M = {frac:.1f} M_p"
        points.append(_point_row(label, T, M, T_y, M_p, util))

    # Combined points
    combos = [(0.5, 0.5), (0.5, 0.8), (0.8, 0.5)]
    for t_frac, m_frac in combos:
        T = t_frac * T_y
        M = m_frac * M_p
        util = _von_mises_util_single(T, M, A, Z, sigma_h, sigma_allow)
        label = f"T = {t_frac:.1f} T_y + M = {m_frac:.1f} M_p"
        points.append(_point_row(label, T, M, T_y, M_p, util))

    return points


def _von_mises_util_single(
    T: float, M: float,
    A: float, Z: float,
    sigma_h: float, sigma_allow: float,
) -> float:
    """Von Mises utilisation at a single (T, M) point, checking both fibres."""
    sigma_a = T / A
    sigma_b = M / Z

    # Tension fibre
    sl_t = sigma_a + sigma_b
    vm_t = math.sqrt(max(sl_t**2 - sl_t * sigma_h + sigma_h**2, 0.0))

    # Compression fibre
    sl_c = sigma_a - sigma_b
    vm_c = math.sqrt(max(sl_c**2 - sl_c * sigma_h + sigma_h**2, 0.0))

    return max(vm_t, vm_c) / sigma_allow if sigma_allow > 0 else float("inf")


def _point_row(
    label: str, T: float, M: float,
    T_y: float, M_p: float, util: float,
) -> Dict:
    return {
        "label": label,
        "T_kN": T / 1e3,
        "M_kNm": M / 1e3,
        "T_ratio": T / T_y if T_y > 0 else 0.0,
        "M_ratio": M / M_p if M_p > 0 else 0.0,
        "util": util,
        "margin": max(0.0, (1.0 - util) * 100),
        "status": "PASS" if util <= 1.0 else "FAIL",
    }


# ---------------------------------------------------------------------------
# Capacity limits and envelope tracing
# ---------------------------------------------------------------------------


def _find_limit_bisection(
    A: float, Z: float, sigma_h: float, smys: float, f_d: float,
    T_fixed: float, M_fixed: float, vary: str,
    search_sign: int = 1,
) -> float:
    """Find T or M limit at util=1.0 via bisection.

    Args:
        A: Cross-sectional area (m^2).
        Z: Elastic section modulus (m^3).
        sigma_h: Hoop stress (Pa).
        smys: Specified minimum yield strength (Pa).
        f_d: Design factor for the operating condition.
        T_fixed: Fixed tension when vary="M" (N).
        M_fixed: Fixed moment when vary="T" (N-m).
        vary: "T" to find tension limit, "M" to find moment limit.
        search_sign: +1 for positive direction, -1 for negative.

    Returns:
        Limit value in SI units (N or N-m), signed per search_sign.
    """
    sigma_allow = f_d * smys

    if vary == "M":
        hi = sigma_allow * Z * 1.5
        lo = 0.0
        for _ in range(100):
            mid = (lo + hi) / 2
            M_test = mid * search_sign
            util = _von_mises_util_single(T_fixed, M_test, A, Z, sigma_h, sigma_allow)
            if util < 1.0:
                lo = mid
            else:
                hi = mid
            if hi - lo < 100:  # 100 N-m tolerance
                break
        return (lo + hi) / 2 * search_sign

    elif vary == "T":
        hi = sigma_allow * A * 1.5
        lo = 0.0
        for _ in range(100):
            mid = (lo + hi) / 2
            T_test = mid * search_sign
            util = _von_mises_util_single(T_test, M_fixed, A, Z, sigma_h, sigma_allow)
            if util < 1.0:
                lo = mid
            else:
                hi = mid
            if hi - lo < 100:  # 100 N tolerance
                break
        return (lo + hi) / 2 * search_sign

    raise ValueError(f"vary must be 'T' or 'M', got '{vary}'")


def _compute_capacity_limits(
    A: float, Z: float, sigma_h: float, smys: float,
    T_y: float, M_p: float,
    conditions: List[Dict],
) -> List[Dict]:
    """Compute pure tension and bending limits for each operating condition.

    Returns:
        List of dicts with keys: name, f_d, T_pos_kN, T_neg_kN, M_allow_kNm.
    """
    results = []
    for cond in conditions:
        f_d = cond["f_d"]

        T_pos = _find_limit_bisection(
            A, Z, sigma_h, smys, f_d,
            T_fixed=0.0, M_fixed=0.0, vary="T", search_sign=1,
        )
        T_neg = _find_limit_bisection(
            A, Z, sigma_h, smys, f_d,
            T_fixed=0.0, M_fixed=0.0, vary="T", search_sign=-1,
        )
        M_allow = _find_limit_bisection(
            A, Z, sigma_h, smys, f_d,
            T_fixed=0.0, M_fixed=0.0, vary="M", search_sign=1,
        )

        results.append({
            "name": cond["name"],
            "f_d": f_d,
            "T_pos_kN": T_pos / 1e3,
            "T_neg_kN": T_neg / 1e3,
            "M_allow_kNm": M_allow / 1e3,
        })

    return results


def _trace_envelope(
    A: float, Z: float, sigma_h: float, smys: float, f_d: float,
    T_neg: float, T_pos: float, M_p: float,
    n_points: int = 80,
) -> Tuple[List[float], List[float]]:
    """Trace the allowable bending envelope at util=1.0.

    Sweeps tension from T_neg to T_pos and bisects for M_allow at each T.

    Returns:
        (T_values, M_values) lists in SI units (N, N-m).
    """
    T_values = []
    M_values = []

    for i in range(n_points + 1):
        frac = i / n_points
        T = T_neg + frac * (T_pos - T_neg)

        M_allow = _find_limit_bisection(
            A, Z, sigma_h, smys, f_d,
            T_fixed=T, M_fixed=0.0, vary="M", search_sign=1,
        )

        T_values.append(T)
        M_values.append(abs(M_allow))

    return T_values, M_values


def _trace_envelope_method1(
    D: float, t: float, A: float,
    smys: float, smts: float, f_d: float, p_net: float,
    T_neg: float, T_pos: float, M_p: float, T_y: float,
    n_points: int = 200,
) -> Tuple[List[float], List[float]]:
    """API STD 2RD Method 1: Linear M-T interaction envelope.

    M_limit = M_y * max(F_d_corr - |T| / T_y, 0)
    where F_d_corr = sqrt(max(f_d^2 - (p_net/p_b)^2, 0)).
    """
    p_b = 0.45 * (smys + smts) * math.log(D / (D - 2 * t))
    pressure_ratio = (p_net / p_b) if p_b > 0 else 0.0
    F_d_corr = math.sqrt(max(f_d**2 - pressure_ratio**2, 0.0))

    # M_y for thin-wall: (pi/4) * SMYS * (D-t)^2 * t
    M_y = (math.pi / 4) * smys * (D - t) ** 2 * t

    T_vals = []
    M_vals = []
    for i in range(n_points + 1):
        frac = i / n_points
        T = T_neg + frac * (T_pos - T_neg)
        T_ratio = abs(T) / T_y if T_y > 0 else 0.0
        M_limit = M_y * max(F_d_corr - T_ratio, 0.0)
        T_vals.append(T)
        M_vals.append(max(M_limit, 0.0))

    return T_vals, M_vals


def _trace_envelope_method2(
    D: float, t: float, A: float,
    smys: float, smts: float, f_d: float, p_net: float,
    T_neg: float, T_pos: float, M_p: float, T_y: float,
    n_points: int = 200,
) -> Tuple[List[float], List[float]]:
    """API STD 2RD Method 2: Cosine M-T interaction envelope.

    M_limit = M_p * F_d_corr * cos(pi/2 * (T/T_y) / F_d_corr)
    where F_d_corr = sqrt(max(f_d^2 - (p_net/p_b)^2, 0)).
    """
    p_b = 0.45 * (smys + smts) * math.log(D / (D - 2 * t))
    pressure_ratio = (p_net / p_b) if p_b > 0 else 0.0
    F_d_corr = math.sqrt(max(f_d**2 - pressure_ratio**2, 0.0))

    T_vals = []
    M_vals = []
    for i in range(n_points + 1):
        frac = i / n_points
        T = T_neg + frac * (T_pos - T_neg)

        if F_d_corr > 0 and T_y > 0:
            arg = (math.pi / 2) * abs(T) / T_y / F_d_corr
            if arg <= math.pi / 2:
                M_limit = M_p * F_d_corr * math.cos(arg)
            else:
                M_limit = 0.0
        else:
            M_limit = 0.0

        T_vals.append(T)
        M_vals.append(max(M_limit, 0.0))

    return T_vals, M_vals


def _compute_pressure_sensitivity(
    A: float, Z: float, d_i: float, t: float,
    pi_base: float, pe: float, smys: float,
    T_y: float, M_p: float,
    conditions: List[Dict],
    pressure_factors: List[float],
) -> List[Dict]:
    """Compute capacity limits at multiple internal pressure levels.

    Returns:
        List of dicts with keys: pi_factor, pi_MPa, sigma_h_MPa,
        limits (list of per-condition dicts), envelopes (dict of
        condition_name -> (T_vals, M_vals)).
    """
    results = []
    for pf in pressure_factors:
        pi = pi_base * pf
        p_net = pi - pe
        sigma_h = p_net * d_i / (2 * t) if t > 0 else 0.0

        limits = _compute_capacity_limits(
            A, Z, sigma_h, smys, T_y, M_p, conditions,
        )

        # Trace envelope for Normal Operating only (f_d=0.72) to keep chart readable
        envelopes = {}
        for cond in conditions:
            lim = next(c for c in limits if c["name"] == cond["name"])
            T_neg = lim["T_neg_kN"] * 1e3
            T_pos = lim["T_pos_kN"] * 1e3
            T_vals, M_vals = _trace_envelope(
                A, Z, sigma_h, smys, cond["f_d"],
                T_neg, T_pos, M_p, n_points=200,
            )
            envelopes[cond["name"]] = (T_vals, M_vals)

        results.append({
            "pi_factor": pf,
            "pi_MPa": pi / 1e6,
            "sigma_h_MPa": sigma_h / 1e6,
            "limits": limits,
            "envelopes": envelopes,
        })

    return results


def _build_pressure_sensitivity_chart(
    sensitivity: List[Dict],
    T_y: float, M_p: float,
    pi_base: float,
    conditions: Optional[List[Dict]] = None,
) -> str:
    """Build Plotly chart comparing envelopes across pressure levels.

    Shows the first (most restrictive) condition's envelope at each pressure level.
    """
    if conditions is None:
        conditions = API_RP_1111_CONDITIONS
    try:
        import plotly.graph_objects as go
    except ImportError:
        return (
            '<div class="card" style="padding:2em;color:#888;">'
            "Plotly not available — pressure sensitivity chart skipped.</div>"
        )

    fig = go.Figure()

    # Colors for pressure levels: blue (baseline), orange (+50%), red (+100%)
    pressure_colors = ["#1f77b4", "#ff7f0e", "#d62728"]
    pressure_dashes = ["solid", "dash", "dot"]

    # Plot from highest pressure (outermost shrinks) to baseline so fills layer
    for i, case in enumerate(reversed(sensitivity)):
        idx = len(sensitivity) - 1 - i  # original index for color/dash
        pf = case["pi_factor"]
        label_suffix = "" if pf == 1.0 else f" (+{(pf - 1) * 100:.0f}%)"
        pi_label = f"P_i = {case['pi_MPa']:.0f} MPa{label_suffix}"

        # Show the first (most restrictive) condition's envelope
        first_cond_name = conditions[0]["name"]
        env = case["envelopes"].get(first_cond_name)
        if env:
            color = pressure_colors[idx % len(pressure_colors)]
            r, g, b = int(color[1:3], 16), int(color[3:5], 16), int(color[5:7], 16)

            # Build closed polygon: +M forward, -M reversed
            T_kN_fwd = [v / 1e3 for v in env[0]]
            M_kNm_pos = [v / 1e3 for v in env[1]]
            T_kN_rev = list(reversed(T_kN_fwd))
            M_kNm_neg = [-v / 1e3 for v in reversed(env[1])]

            poly_T = T_kN_fwd + T_kN_rev
            poly_M = M_kNm_pos + M_kNm_neg

            fig.add_trace(go.Scatter(
                x=poly_T, y=poly_M,
                mode="lines",
                name=pi_label,
                line=dict(
                    color=color,
                    dash=pressure_dashes[idx % len(pressure_dashes)],
                    width=2,
                ),
                fill="toself",
                fillcolor=f"rgba({r},{g},{b},0.10)",
                hovertemplate=(
                    f"{pi_label}<br>"
                    "T = %{x:.0f} kN<br>"
                    "M = %{y:.0f} kN·m<extra></extra>"
                ),
            ))

    first_cond = conditions[0]
    fig.update_layout(
        title=(
            f"Pressure Sensitivity — {first_cond['name']} Envelope "
            f"(f<sub>d</sub> = {first_cond['f_d']})"
        ),
        xaxis_title="Effective Tension (kN)",
        yaxis_title="Bending Moment (kN·m)",
        template="plotly_white",
        hovermode="closest",
        showlegend=True,
        legend=dict(
            x=0.01, y=0.99,
            bgcolor="rgba(255,255,255,0.8)",
            bordercolor="#ccc", borderwidth=1,
        ),
        width=900,
        height=600,
        margin=dict(l=60, r=40, t=80, b=60),
    )

    return fig.to_html(full_html=False, include_plotlyjs="cdn")


# ---------------------------------------------------------------------------
# Contour chart (Plotly)
# ---------------------------------------------------------------------------

def _build_contour_chart(
    D: float, t: float, d_i: float,
    smys: float, smts: float,
    pi: float, pe: float,
    f_d: float,
    A: float, Z: float,
    sigma_h: float, sigma_allow: float,
    T_y: float, M_p: float,
    n_points: int = 300,
    code: DesignCode = DesignCode.API_RP_1111,
) -> str:
    """Build an embedded Plotly contour chart as HTML div."""
    try:
        import numpy as np
        import plotly.graph_objects as go
    except ImportError:
        return (
            '<div class="card" style="padding:2em;color:#888;">'
            "Plotly/NumPy not available — contour chart skipped.</div>"
        )

    T_max = T_y * 1.15
    M_max = M_p * 1.15

    T_1d = np.linspace(-T_max, T_max, n_points)
    M_1d = np.linspace(-M_max, M_max, n_points)
    T_grid, M_grid = np.meshgrid(T_1d, M_1d)

    # Von Mises utilisation grid (both fibres)
    sigma_a = T_grid / A
    sigma_b = M_grid / Z

    sl_t = sigma_a + sigma_b
    vm_t = np.sqrt(np.maximum(sl_t**2 - sl_t * sigma_h + sigma_h**2, 0.0))

    sl_c = sigma_a - sigma_b
    vm_c = np.sqrt(np.maximum(sl_c**2 - sl_c * sigma_h + sigma_h**2, 0.0))

    util_grid = np.maximum(vm_t, vm_c) / sigma_allow

    T_kN = T_1d / 1e3
    M_kNm = M_1d / 1e3

    OD_mm = D * 1000
    WT_mm = t * 1000

    fig = go.Figure()

    # Filled contour
    fig.add_trace(go.Contour(
        x=T_kN.tolist(),
        y=M_kNm.tolist(),
        z=util_grid.tolist(),
        contours=dict(
            start=0.0, end=2.0, size=0.2,
            showlabels=True,
            labelfont=dict(size=10, color="white"),
        ),
        colorscale=[
            [0.0, "#2166ac"],
            [0.25, "#67a9cf"],
            [0.5, "#fddbc7"],
            [0.75, "#ef8a62"],
            [1.0, "#b2182b"],
        ],
        colorbar=dict(title="Utilisation", tickformat=".2f"),
        zmin=0.0, zmax=2.0,
        hovertemplate=(
            "T = %{x:.0f} kN<br>M = %{y:.0f} kN·m<br>"
            "Util = %{z:.2f}<extra></extra>"
        ),
        name="Von Mises Utilisation",
    ))

    # Bold unity contour (util = 1.0)
    fig.add_trace(go.Contour(
        x=T_kN.tolist(),
        y=M_kNm.tolist(),
        z=util_grid.tolist(),
        contours=dict(
            start=1.0, end=1.0, size=0,
            coloring="none",
            showlabels=True,
            labelfont=dict(size=12, color="black"),
        ),
        line=dict(color="black", width=3),
        showscale=False,
        hoverinfo="skip",
        name="Util = 1.0",
    ))

    # Dashed contour at util = 0.8
    fig.add_trace(go.Contour(
        x=T_kN.tolist(),
        y=M_kNm.tolist(),
        z=util_grid.tolist(),
        contours=dict(
            start=0.8, end=0.8, size=0,
            coloring="none",
            showlabels=True,
            labelfont=dict(size=10, color="#444"),
        ),
        line=dict(color="#444", width=2, dash="dash"),
        showscale=False,
        hoverinfo="skip",
        name="Util = 0.8",
    ))

    # Reference lines at T_y
    fig.add_trace(go.Scatter(
        x=[T_y / 1e3, -T_y / 1e3],
        y=[0, 0],
        mode="markers+text",
        marker=dict(color="#D62728", size=10, symbol="diamond"),
        text=[f"T<sub>y</sub> = {T_y / 1e3:.0f} kN", f"\u2013T<sub>y</sub>"],
        textposition=["top center", "top center"],
        textfont=dict(size=11),
        name="Yield Tension",
    ))

    # Reference lines at M_p
    fig.add_trace(go.Scatter(
        x=[0, 0],
        y=[M_p / 1e3, -M_p / 1e3],
        mode="markers+text",
        marker=dict(color="#2CA02C", size=10, symbol="diamond"),
        text=[f"M<sub>p</sub> = {M_p / 1e3:.0f} kN\u00b7m", f"\u2013M<sub>p</sub>"],
        textposition=["middle right", "middle right"],
        textfont=dict(size=11),
        name="Plastic Moment",
    ))

    # Origin marker
    fig.add_trace(go.Scatter(
        x=[0], y=[0],
        mode="markers",
        marker=dict(color="black", size=6, symbol="x"),
        showlegend=False,
    ))

    fig.update_layout(
        title=(
            f"{code.value} Von Mises Tension\u2013Moment Interaction<br>"
            f"<sub>OD={OD_mm:.1f} mm, WT={WT_mm:.1f} mm, "
            f"SMYS={smys / 1e6:.0f} MPa, f<sub>d</sub>={f_d}, "
            f"P<sub>i</sub>={pi / 1e6:.1f} MPa, "
            f"P<sub>e</sub>={pe / 1e6:.1f} MPa</sub>"
        ),
        xaxis_title="Effective Tension (kN)",
        yaxis_title="Bending Moment (kN\u00b7m)",
        template="plotly_white",
        hovermode="closest",
        showlegend=True,
        legend=dict(
            x=0.01, y=0.99,
            bgcolor="rgba(255,255,255,0.8)",
            bordercolor="#ccc", borderwidth=1,
        ),
        width=900,
        height=750,
        margin=dict(l=60, r=40, t=100, b=60),
    )

    chart_div = fig.to_html(full_html=False, include_plotlyjs="cdn")
    return chart_div


# ---------------------------------------------------------------------------
# Envelope chart (Plotly)
# ---------------------------------------------------------------------------


def _build_envelope_chart(
    envelopes: Dict[str, Tuple[List[float], List[float]]],
    T_y: float, M_p: float,
    conditions: Optional[List[Dict]] = None,
    method_envelopes: Optional[Dict] = None,
) -> str:
    """Build an embedded Plotly envelope chart as HTML div.

    Each condition is shown as a closed symmetric polygon (lens shape)
    spanning from +M_allow to -M_allow. Bending is symmetric about M=0
    because flipping M sign just swaps which fibre governs.

    For API STD 2RD, method_envelopes overlays Method 1 (linear) and
    Method 2 (cosine) envelopes as additional traces.

    Args:
        envelopes: Dict mapping condition name to (T_values, M_values) in SI.
        T_y: Yield tension (N) for reference marker.
        M_p: Plastic moment (N-m) for reference marker.
        conditions: Operating condition definitions with f_d, color, dash.
        method_envelopes: Optional Method 1/2 envelopes per condition (API STD 2RD).
    """
    if conditions is None:
        conditions = API_RP_1111_CONDITIONS

    try:
        import plotly.graph_objects as go
    except ImportError:
        return (
            '<div class="card" style="padding:2em;color:#888;">'
            "Plotly not available — envelope chart skipped.</div>"
        )

    fig = go.Figure()

    cond_styles = {c["name"]: c for c in conditions}

    # Plot from outermost (last condition) to innermost (first condition)
    # so fills layer correctly with outermost behind
    ordered_names = list(reversed([c["name"] for c in conditions]))

    for name in ordered_names:
        if name not in envelopes:
            continue
        T_vals, M_vals = envelopes[name]
        style = cond_styles.get(name, {})
        color = style.get("color", "#333")

        # Build closed polygon: +M forward, then -M reversed
        T_kN_fwd = [v / 1e3 for v in T_vals]
        M_kNm_pos = [v / 1e3 for v in M_vals]
        T_kN_rev = list(reversed(T_kN_fwd))
        M_kNm_neg = [-v / 1e3 for v in reversed(M_vals)]

        poly_T = T_kN_fwd + T_kN_rev
        poly_M = M_kNm_pos + M_kNm_neg

        # Parse hex color for semi-transparent fill
        r, g, b = int(color[1:3], 16), int(color[3:5], 16), int(color[5:7], 16)

        fig.add_trace(go.Scatter(
            x=poly_T, y=poly_M,
            mode="lines",
            name=f"{name} (f_d={style.get('f_d', '?')})",
            line=dict(
                color=color,
                dash=style.get("dash", "solid"),
                width=2,
            ),
            fill="toself",
            fillcolor=f"rgba({r},{g},{b},0.10)",
            hovertemplate=(
                f"{name}<br>"
                "T = %{x:.0f} kN<br>"
                "M = %{y:.0f} kN·m<extra></extra>"
            ),
        ))

    # Method 1/2 envelopes (API STD 2RD only)
    if method_envelopes:
        method_colors = {"method1": "#9467bd", "method2": "#8c564b"}
        method_labels = {"method1": "Method 1 (Linear)", "method2": "Method 2 (Cosine)"}
        method_dashes = {"method1": "dash", "method2": "solid"}

        for name in ordered_names:
            if name not in method_envelopes:
                continue
            style = cond_styles.get(name, {})
            for method_key in ["method1", "method2"]:
                T_vals_m, M_vals_m = method_envelopes[name][method_key]

                T_kN_fwd = [v / 1e3 for v in T_vals_m]
                M_kNm_pos = [v / 1e3 for v in M_vals_m]
                T_kN_rev = list(reversed(T_kN_fwd))
                M_kNm_neg = [-v / 1e3 for v in reversed(M_vals_m)]

                poly_T = T_kN_fwd + T_kN_rev
                poly_M = M_kNm_pos + M_kNm_neg

                fig.add_trace(go.Scatter(
                    x=poly_T, y=poly_M,
                    mode="lines",
                    name=f"{name} — {method_labels[method_key]}",
                    line=dict(
                        color=method_colors[method_key],
                        dash=method_dashes[method_key],
                        width=2,
                    ),
                    hovertemplate=(
                        f"{name} {method_labels[method_key]}<br>"
                        "T = %{x:.0f} kN<br>"
                        "M = %{y:.0f} kN·m<extra></extra>"
                    ),
                ))

    # Reference markers
    fig.add_trace(go.Scatter(
        x=[T_y / 1e3, -T_y / 1e3],
        y=[0, 0],
        mode="markers+text",
        marker=dict(color="#D62728", size=8, symbol="diamond"),
        text=["T_y", "-T_y"],
        textposition="bottom center",
        textfont=dict(size=9),
        showlegend=False,
    ))
    fig.add_trace(go.Scatter(
        x=[0, 0],
        y=[M_p / 1e3, -M_p / 1e3],
        mode="markers+text",
        marker=dict(color="#2CA02C", size=8, symbol="diamond"),
        text=["M_p", "-M_p"],
        textposition=["middle right", "middle right"],
        textfont=dict(size=9),
        showlegend=False,
    ))

    fig.update_layout(
        title="Allowable Bending Envelope by Operating Condition",
        xaxis_title="Effective Tension (kN)",
        yaxis_title="Bending Moment (kN·m)",
        template="plotly_white",
        hovermode="closest",
        showlegend=True,
        legend=dict(
            x=0.01, y=0.99,
            bgcolor="rgba(255,255,255,0.8)",
            bordercolor="#ccc", borderwidth=1,
        ),
        width=900,
        height=700,
        margin=dict(l=60, r=40, t=80, b=60),
    )

    return fig.to_html(full_html=False, include_plotlyjs="cdn")


# ---------------------------------------------------------------------------
# HTML builder
# ---------------------------------------------------------------------------

def _fmt_mpa(val_pa: float) -> str:
    """Format Pa value as MPa string."""
    return f"{val_pa / 1e6:.2f}"


def _fmt_mm(val_m: float) -> str:
    """Format metres as mm string."""
    return f"{val_m * 1000:.2f}"


def _status_tag(status: str) -> str:
    """HTML tag for PASS/FAIL status."""
    if status == "PASS":
        return '<span class="tag pass">PASS</span>'
    return '<span class="tag fail">FAIL</span>'


def _pressure_check_row(
    name: str,
    section_ref: str,
    details: Dict,
    capacity_key: str,
    factor_key: str,
    allowable_key: str,
    demand_key: str,
) -> str:
    """Build a table row for a pressure check."""
    capacity = details.get(capacity_key, 0.0)
    factor = details.get(factor_key, 0.0)
    allowable = details.get(allowable_key, 0.0)
    demand = details.get(demand_key, 0.0)
    util = details.get("utilisation", 0.0)
    status = "PASS" if util <= 1.0 else "FAIL"

    return (
        f"<tr>"
        f"<td><strong>{name}</strong><br><small>{section_ref}</small></td>"
        f"<td>{_fmt_mpa(capacity)}</td>"
        f"<td>{factor:.2f}</td>"
        f"<td>{_fmt_mpa(allowable)}</td>"
        f"<td>{_fmt_mpa(demand)}</td>"
        f"<td>{util:.3f}</td>"
        f"<td>{_status_tag(status)}</td>"
        f"</tr>"
    )


def _build_html(
    geometry: PipeGeometry,
    material: PipeMaterial,
    pi: float, pe: float,
    code: DesignCode,
    raw_results: Dict,
    A: float, Z: float, I_val: float,
    T_y: float, M_p: float, sigma_h: float,
    key_points: List[Dict],
    contour_html: str,
    capacity_limits: Optional[List[Dict]] = None,
    envelope_html: Optional[str] = None,
    envelopes: Optional[Dict[str, Tuple[List[float], List[float]]]] = None,
    pressure_sensitivity: Optional[List[Dict]] = None,
    pressure_chart_html: Optional[str] = None,
    conditions: Optional[List[Dict]] = None,
    edition_label: str = "",
) -> str:
    """Assemble the complete HTML report."""
    if conditions is None:
        conditions = API_RP_1111_CONDITIONS

    D = geometry.outer_diameter
    t = geometry.wall_thickness
    d_i = D - 2 * t
    p_net = pi - pe

    code_label = code.value + edition_label

    # Pressure check rows — build dynamically from available results
    pressure_rows = ""
    check_configs = {
        "burst": ("Burst", "p_b", "f_d", "p_b_design", "p_net"),
        "collapse": ("Collapse", "p_c", "f_c", "p_c_design", "p_e"),
        "propagation": ("Propagation", "p_pr", "f_p", "p_pr_design", "p_e"),
        "hoop": ("Hoop Stress", "sigma_allow", "utilisation", "sigma_allow", "sigma_h"),
    }
    for check_name, (util, details) in raw_results.items():
        cfg = check_configs.get(check_name)
        if cfg:
            label, cap_key, fac_key, allow_key, demand_key = cfg
            pressure_rows += _pressure_check_row(
                label, code_label, details,
                cap_key, fac_key, allow_key, demand_key,
            )
        else:
            # Generic row for unknown checks
            pressure_rows += (
                f"<tr><td><strong>{check_name.title()}</strong></td>"
                f"<td colspan='5'>util = {util:.3f}</td>"
                f"<td>{_status_tag('PASS' if util <= 1.0 else 'FAIL')}</td></tr>"
            )

    # Key points rows (with margin %)
    kp_rows = ""
    for pt in key_points:
        kp_rows += (
            f"<tr>"
            f"<td>{pt['label']}</td>"
            f"<td>{pt['T_kN']:.1f}</td>"
            f"<td>{pt['M_kNm']:.1f}</td>"
            f"<td>{pt['T_ratio']:.3f}</td>"
            f"<td>{pt['M_ratio']:.3f}</td>"
            f"<td>{pt['util']:.3f}</td>"
            f"<td>{pt['margin']:.1f}%</td>"
            f"<td>{_status_tag(pt['status'])}</td>"
            f"</tr>"
        )

    # Capacity limits rows
    cl_rows = ""
    if capacity_limits:
        for cl in capacity_limits:
            cl_rows += (
                f"<tr>"
                f"<td>{cl['name']}</td>"
                f"<td>{cl['f_d']:.2f}</td>"
                f"<td>{cl['T_pos_kN']:.1f}</td>"
                f"<td>{cl['T_neg_kN']:.1f}</td>"
                f"<td>{cl['M_allow_kNm']:.1f}</td>"
                f"</tr>"
            )

    # Envelope data table (sample every N-th point for readability)
    envelope_table_html = ""
    if envelopes:
        cond_names = [c["name"] for c in conditions]
        # Use ~20 rows: sample every 10th point from the 201-point envelope
        first_key = cond_names[0]
        n_total = len(envelopes[first_key][0])
        step = max(1, n_total // 20)
        indices = list(range(0, n_total, step))
        if indices[-1] != n_total - 1:
            indices.append(n_total - 1)

        # Header row
        hdr_cells = "<th>Tension (kN)</th>"
        for name in cond_names:
            hdr_cells += f"<th>{name}<br>M<sub>allow</sub> (kN&middot;m)</th>"
        envelope_table_html += (
            f"<table><thead><tr>{hdr_cells}</tr></thead><tbody>"
        )

        # Use the first condition's T values as the common T axis
        T_ref = envelopes[first_key][0]
        for idx in indices:
            T_kN = T_ref[idx] / 1e3
            row = f"<td>{T_kN:.0f}</td>"
            for name in cond_names:
                M_kNm = envelopes[name][1][idx] / 1e3
                row += f"<td>{M_kNm:.1f}</td>"
            envelope_table_html += f"<tr>{row}</tr>"
        envelope_table_html += "</tbody></table>"

    # Pressure sensitivity table
    ps_table_html = ""
    if pressure_sensitivity:
        ps_table_html = (
            "<table><thead><tr>"
            "<th>P<sub>i</sub> (MPa)</th>"
            "<th>&sigma;<sub>h</sub> (MPa)</th>"
            "<th>Condition</th>"
            "<th>f<sub>d</sub></th>"
            "<th>T<sub>+</sub> (kN)</th>"
            "<th>T<sub>&minus;</sub> (kN)</th>"
            "<th>M<sub>allow</sub> (kN&middot;m)</th>"
            "</tr></thead><tbody>"
        )
        for case in pressure_sensitivity:
            pf = case["pi_factor"]
            label = f"{case['pi_MPa']:.0f}"
            if pf != 1.0:
                label += f" (+{(pf - 1) * 100:.0f}%)"
            for j, lim in enumerate(case["limits"]):
                first_col = (
                    f"<td rowspan=\"{len(case['limits'])}\">{label}</td>"
                    f"<td rowspan=\"{len(case['limits'])}\">{case['sigma_h_MPa']:.1f}</td>"
                    if j == 0 else ""
                )
                ps_table_html += (
                    f"<tr>{first_col}"
                    f"<td>{lim['name']}</td>"
                    f"<td>{lim['f_d']:.2f}</td>"
                    f"<td>{lim['T_pos_kN']:.1f}</td>"
                    f"<td>{lim['T_neg_kN']:.1f}</td>"
                    f"<td>{lim['M_allow_kNm']:.1f}</td>"
                    f"</tr>"
                )
        ps_table_html += "</tbody></table>"

    # Governing check
    max_util = 0.0
    governing = ""
    for name, (util, _) in raw_results.items():
        if util > max_util:
            max_util = util
            governing = name

    overall_status = "PASS" if max_util <= 1.0 else "FAIL"
    overall_margin = max(0.0, (1.0 - max_util) * 100)
    governing_label = governing.replace('_', ' ').title()

    # Executive summary: count pass/fail across pressure checks
    n_checks = len(raw_results)
    n_pass = sum(1 for _, (u, _) in raw_results.items() if u <= 1.0)
    n_fail = n_checks - n_pass

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{code_label} Wall Thickness Report</title>
    <style>
        * {{ box-sizing: border-box; }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            margin: 0; padding: 0; color: #333; background: #f5f6fa;
            line-height: 1.6;
        }}
        .header {{
            background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
            color: white; padding: 2em 2em 1.5em; margin-bottom: 0;
        }}
        .header h1 {{ margin: 0; font-size: 1.8em; }}
        .header p {{ margin: 0.3em 0 0; opacity: 0.8; font-size: 0.95em; }}
        .header .summary-bar {{
            display: flex; gap: 2em; margin-top: 1em;
            flex-wrap: wrap;
        }}
        .header .summary-item {{
            background: rgba(255,255,255,0.12); border-radius: 6px;
            padding: 0.5em 1em; font-size: 0.9em;
        }}
        .header .summary-item strong {{ color: #67a9cf; }}
        .container {{ max-width: 1200px; margin: 0 auto; padding: 1.5em 2em 2em; }}
        .card {{
            background: white; border-radius: 8px; padding: 1.5em 2em;
            box-shadow: 0 2px 8px rgba(0,0,0,0.08); margin-bottom: 1.5em;
        }}
        .card h2 {{
            color: #16213e; margin: 0 0 0.8em; font-size: 1.3em;
            border-bottom: 2px solid #e8e8e8; padding-bottom: 0.4em;
        }}
        .card h3 {{ color: #333; margin: 1em 0 0.5em; font-size: 1.05em; }}
        table {{
            width: 100%; border-collapse: collapse; font-size: 0.9em;
        }}
        th {{
            background: #f0f2f5; text-align: left; padding: 0.6em 0.8em;
            font-weight: 600; border-bottom: 2px solid #ddd;
        }}
        td {{
            padding: 0.5em 0.8em; border-bottom: 1px solid #eee;
        }}
        tr:hover td {{ background: #f8f9fb; }}
        .data-grid {{
            display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 1em;
        }}
        .data-group {{ }}
        .data-group h3 {{
            margin: 0 0 0.5em; font-size: 0.95em; color: #0f3460;
            text-transform: uppercase; letter-spacing: 0.5px;
        }}
        .data-group table td:first-child {{
            color: #666; width: 55%;
        }}
        .data-group table td:last-child {{
            font-weight: 600; text-align: right;
        }}
        .tag {{
            display: inline-block; padding: 0.15em 0.6em; border-radius: 4px;
            font-size: 0.85em; font-weight: 700; letter-spacing: 0.5px;
        }}
        .tag.pass {{ background: #d4edda; color: #155724; }}
        .tag.fail {{ background: #f8d7da; color: #721c24; }}
        .exec-summary {{
            display: grid; grid-template-columns: auto 1fr; gap: 1.5em;
            align-items: start;
        }}
        .verdict-badge {{
            font-size: 2.5em; font-weight: 800; padding: 0.2em 0.5em;
            border-radius: 10px; text-align: center; min-width: 140px;
        }}
        .verdict-badge.pass {{
            background: #d4edda; color: #155724; border: 3px solid #28a745;
        }}
        .verdict-badge.fail {{
            background: #f8d7da; color: #721c24; border: 3px solid #dc3545;
        }}
        .exec-details {{ font-size: 0.95em; }}
        .exec-details table td:first-child {{ color: #666; width: 200px; }}
        .exec-details table td:last-child {{ font-weight: 600; }}
        .chart-container {{ margin: 1em 0; }}
        .footer {{
            text-align: center; padding: 1.5em; color: #888; font-size: 0.82em;
        }}
        @media (max-width: 768px) {{
            .container {{ padding: 1em; }}
            .card {{ padding: 1em; }}
            .data-grid {{ grid-template-columns: 1fr; }}
            .exec-summary {{ grid-template-columns: 1fr; }}
            table {{ font-size: 0.8em; }}
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>{code_label} &mdash; Wall Thickness Assessment</h1>
        <p>Moment-Tension Interaction Report</p>
        <div class="summary-bar">
            <div class="summary-item">
                <strong>OD</strong> {_fmt_mm(D)} mm
            </div>
            <div class="summary-item">
                <strong>WT</strong> {_fmt_mm(t)} mm
            </div>
            <div class="summary-item">
                <strong>Grade</strong> {material.grade}
            </div>
            <div class="summary-item">
                <strong>Governing</strong> {governing_label}
                &mdash; {max_util:.3f}
                {_status_tag(overall_status)}
            </div>
        </div>
    </div>

    <div class="container">

        <!-- Executive Summary -->
        <div class="card">
            <h2>Executive Summary</h2>
            <div class="exec-summary">
                <div class="verdict-badge {'pass' if overall_status == 'PASS' else 'fail'}">
                    {overall_status}
                </div>
                <div class="exec-details">
                    <table>
                        <tr><td>Overall Verdict</td><td>{_status_tag(overall_status)} &mdash; {n_pass}/{n_checks} checks passed</td></tr>
                        <tr><td>Governing Check</td><td>{governing_label}</td></tr>
                        <tr><td>Governing Utilisation</td><td>{max_util:.3f}</td></tr>
                        <tr><td>Margin</td><td>{overall_margin:.1f}%</td></tr>
                        <tr><td>Design Code</td><td>{code_label}</td></tr>
                        <tr><td>Pipe</td><td>OD {_fmt_mm(D)} mm &times; WT {_fmt_mm(t)} mm, {material.grade}</td></tr>
                        <tr><td>Pressure</td><td>P<sub>i</sub>&nbsp;=&nbsp;{_fmt_mpa(pi)}&nbsp;MPa, P<sub>e</sub>&nbsp;=&nbsp;{_fmt_mpa(pe)}&nbsp;MPa (P<sub>net</sub>&nbsp;=&nbsp;{_fmt_mpa(p_net)}&nbsp;MPa)</td></tr>
                    </table>
                </div>
            </div>
        </div>

        <!-- Section 1: Pressure-Only Checks -->
        <div class="card">
            <h2>1. Pressure-Only Checks ({code_label})</h2>
            <p>Independent of bending moment and tension &mdash; these set the baseline utilisation.</p>
            <table>
                <thead>
                    <tr>
                        <th>Check</th>
                        <th>Capacity (MPa)</th>
                        <th>Design Factor</th>
                        <th>Allowable (MPa)</th>
                        <th>Demand (MPa)</th>
                        <th>Utilisation</th>
                        <th>Status</th>
                    </tr>
                </thead>
                <tbody>
                    {pressure_rows}
                </tbody>
            </table>
        </div>

        <!-- Section 2: Capacity Limits by Operating Condition -->
        <div class="card">
            <h2>2. Capacity Limits by Operating Condition</h2>
            <p>
                Pure tension and pure bending limits at util&nbsp;=&nbsp;1.0 for each
                {code_label} operating condition. Tension limits are
                <strong>asymmetric</strong> due to hoop stress interaction: positive
                tension (pipe stretching) is more favourable than compression when
                net internal pressure is positive.
            </p>
            <table>
                <thead>
                    <tr>
                        <th>Condition</th>
                        <th>f<sub>d</sub></th>
                        <th>T<sub>+</sub> (kN)</th>
                        <th>T<sub>&minus;</sub> (kN)</th>
                        <th>M<sub>allow</sub> (kN&middot;m)</th>
                    </tr>
                </thead>
                <tbody>
                    {cl_rows}
                </tbody>
            </table>
        </div>

        <!-- Section 2a: Pressure Sensitivity -->
        <div class="card">
            <h2>2a. Pressure Sensitivity &mdash; Effect of Internal Pressure</h2>
            <p>
                Capacity limits recomputed at baseline, +50%, and +100% internal pressure.
                Higher hoop stress shrinks all envelopes and increases the asymmetry
                between positive and negative tension limits.
            </p>
            {ps_table_html}
            <div class="chart-container">
                {pressure_chart_html if pressure_chart_html else ""}
            </div>
        </div>

        <!-- Section 3: Combined Loading -->
        <div class="card">
            <h2>3. Combined Loading &mdash; Von Mises Interaction</h2>
            <p>
                The {code_label} combined check evaluates the Von Mises equivalent stress:
                &sigma;<sub>vm</sub> = &radic;(&sigma;<sub>L</sub>&sup2;
                &minus; &sigma;<sub>L</sub>&middot;&sigma;<sub>h</sub>
                + &sigma;<sub>h</sub>&sup2;) &le; f<sub>d</sub>&middot;SMYS
            </p>
            <p>
                The contour plot below maps utilisation across the bending moment vs. tension
                envelope. The <strong>bold black contour</strong> at util&nbsp;=&nbsp;1.0 is
                the acceptance boundary; the <strong>dashed contour</strong> at 0.8 marks a
                typical design target.
            </p>
            <div class="chart-container">
                {contour_html}
            </div>
        </div>

        <!-- Section 4: Allowable Bending Envelope -->
        <div class="card">
            <h2>4. Allowable Bending Envelope</h2>
            <p>
                Each curve traces the maximum allowable bending moment as a function of
                applied tension, at util&nbsp;=&nbsp;1.0 for that operating condition.
                The innermost envelope (most restrictive condition) is the most
                conservative; the outermost envelope represents the full yield limit.
            </p>
            <div class="chart-container">
                {envelope_html if envelope_html else ""}
            </div>
            <h3>Envelope Data &mdash; Tension vs Allowable Bending Moment</h3>
            {envelope_table_html}
        </div>

        <!-- Section 5: Key Points Summary -->
        <div class="card">
            <h2>5. Key Points Summary</h2>
            <p>Utilisation at selected engineering reference points on the M&ndash;T plane.</p>
            <table>
                <thead>
                    <tr>
                        <th>Point</th>
                        <th>T (kN)</th>
                        <th>M (kN&middot;m)</th>
                        <th>T / T<sub>y</sub></th>
                        <th>M / M<sub>p</sub></th>
                        <th>Utilisation</th>
                        <th>Margin</th>
                        <th>Status</th>
                    </tr>
                </thead>
                <tbody>
                    {kp_rows}
                </tbody>
            </table>
        </div>

        <!-- Section 6: Input Data -->
        <div class="card">
            <h2>6. Input Data</h2>
            <div class="data-grid">
                <div class="data-group">
                    <h3>Pipe Geometry</h3>
                    <table>
                        <tr><td>Outer Diameter (OD)</td><td>{_fmt_mm(D)} mm</td></tr>
                        <tr><td>Wall Thickness (WT)</td><td>{_fmt_mm(t)} mm</td></tr>
                        <tr><td>Inner Diameter (ID)</td><td>{_fmt_mm(d_i)} mm</td></tr>
                        <tr><td>D/t Ratio</td><td>{geometry.d_over_t:.1f}</td></tr>
                        <tr><td>Corrosion Allowance</td><td>{_fmt_mm(geometry.corrosion_allowance)} mm</td></tr>
                    </table>
                </div>
                <div class="data-group">
                    <h3>Material Properties</h3>
                    <table>
                        <tr><td>Grade</td><td>{material.grade}</td></tr>
                        <tr><td>SMYS</td><td>{_fmt_mpa(material.smys)} MPa</td></tr>
                        <tr><td>SMTS</td><td>{_fmt_mpa(material.smts)} MPa</td></tr>
                        <tr><td>Young&rsquo;s Modulus</td><td>{material.youngs_modulus / 1e9:.0f} GPa</td></tr>
                        <tr><td>Poisson&rsquo;s Ratio</td><td>{material.poissons_ratio:.2f}</td></tr>
                        <tr><td>Fabrication Type</td><td>{material.fabrication_type.value.title()}</td></tr>
                    </table>
                </div>
                <div class="data-group">
                    <h3>Design Conditions</h3>
                    <table>
                        <tr><td>Internal Pressure (P<sub>i</sub>)</td><td>{_fmt_mpa(pi)} MPa</td></tr>
                        <tr><td>External Pressure (P<sub>e</sub>)</td><td>{_fmt_mpa(pe)} MPa</td></tr>
                        <tr><td>Net Pressure (P<sub>net</sub>)</td><td>{_fmt_mpa(p_net)} MPa</td></tr>
                        <tr><td>Hoop Stress (&sigma;<sub>h</sub>)</td><td>{_fmt_mpa(sigma_h)} MPa</td></tr>
                    </table>
                </div>
                <div class="data-group">
                    <h3>Derived Section Properties</h3>
                    <table>
                        <tr><td>Cross-sectional Area (A)</td><td>{A * 1e4:.2f} cm&sup2;</td></tr>
                        <tr><td>Section Modulus (Z)</td><td>{Z * 1e6:.2f} cm&sup3;</td></tr>
                        <tr><td>Second Moment of Area (I)</td><td>{I_val * 1e8:.2f} cm&#8308;</td></tr>
                        <tr><td>Yield Tension (T<sub>y</sub>)</td><td>{T_y / 1e3:.1f} kN</td></tr>
                        <tr><td>Plastic Moment (M<sub>p</sub>)</td><td>{M_p / 1e3:.1f} kN&middot;m</td></tr>
                    </table>
                </div>
            </div>
        </div>

    </div>

    <div class="footer">
        Generated by digitalmodel &mdash; {code_label} wall thickness assessment
    </div>
</body>
</html>"""

    return html
