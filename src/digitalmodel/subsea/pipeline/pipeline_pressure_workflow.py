"""API RP 1111 internal pressure checks and config-driven sizing workflow.

WRK-355: Pipeline and flexibles module — pressure containment checks.

Implements:
- API RP 1111 burst pressure formula (Section 4.3.1)
- API RP 1111 internal pressure design check
- API RP 1111 combined loading check (Section 4.3.4)
- Config-driven YAML wall thickness sizing workflow

All pressures in MPa; all dimensions in metres (SI throughout).
"""
from __future__ import annotations

import math
import os
from typing import Any

from digitalmodel.subsea.pipeline.pipeline_pressure import (
    MATERIAL_LIBRARY,
    GAMMA_INC,
    DF_COLLAPSE,
    PipeMaterial,
    _E_STEEL,
    _NU_STEEL,
    dnv_wall_thickness,
    dnv_pressure_containment_check,
    api_water_pressure,
    api_collapse_pressure,
    api_external_collapse_check,
    api_propagating_buckle_check,
)
from digitalmodel.subsea.pipeline.pipeline_pressure_dnv import (
    dnv_external_collapse_check,
    dnv_propagating_buckle_check,
)


# ---------------------------------------------------------------------------
# API RP 1111: internal pressure design factor
# ---------------------------------------------------------------------------

#: Design factor for burst/internal pressure (API RP 1111 Table 1)
DF_BURST: float = 0.72


# ---------------------------------------------------------------------------
# API RP 1111: burst pressure
# ---------------------------------------------------------------------------

def api_burst_pressure(
    D: float,
    t: float,
    mat: PipeMaterial,
) -> float:
    """Burst pressure per API RP 1111 Section 4.3.1.

    Two-range formula based on D/t ratio::

        If D/t <= 15:  p_b = 0.45 * (SMYS + SMTS) * ln(D / (D - 2*t))
        else:          p_b = 0.90 * (SMYS + SMTS) * t / (D - t)

    Args:
        D:    Outside diameter [m].
        t:    Wall thickness [m].
        mat:  Pipeline material (SMYS and SMTS used).

    Returns:
        Burst pressure p_b [MPa].
    """
    d_over_t = D / t
    if d_over_t <= 15.0:
        return 0.45 * (mat.SMYS + mat.SMTS) * math.log(D / (D - 2.0 * t))
    return 0.90 * (mat.SMYS + mat.SMTS) * t / (D - t)


# ---------------------------------------------------------------------------
# API RP 1111: internal pressure check
# ---------------------------------------------------------------------------

def api_internal_pressure_check(
    p_i: float,
    p_e: float,
    D: float,
    t: float,
    mat: PipeMaterial,
    df_burst: float = DF_BURST,
) -> dict[str, object]:
    """Check net internal pressure against API RP 1111 burst resistance.

    Criterion (API RP 1111 Section 4.3.1)::

        p_i - p_e <= df_burst * p_b

    Args:
        p_i:       Internal (design) pressure [MPa].
        p_e:       External pressure [MPa].
        D:         Outside diameter [m].
        t:         Wall thickness [m].
        mat:       Pipeline material.
        df_burst:  Design factor for burst. Default 0.72.

    Returns:
        Dict with keys: ``pass``, ``utilization``, ``p_b``, ``p_allowable``.
    """
    p_b = api_burst_pressure(D=D, t=t, mat=mat)
    p_allowable = df_burst * p_b
    p_net = p_i - p_e
    utilization = p_net / p_allowable if p_allowable > 0.0 else float("inf")

    return {
        "pass": utilization <= 1.0,
        "utilization": utilization,
        "p_b": p_b,
        "p_allowable": p_allowable,
    }


# ---------------------------------------------------------------------------
# API RP 1111: combined loading check
# ---------------------------------------------------------------------------

def api_combined_loading_check(
    D: float,
    t: float,
    mat: PipeMaterial,
    M_Sd: float,
    S_Sd: float,
    p_i: float,
    p_e: float,
    df_moment: float = 0.72,
) -> dict[str, object]:
    """Combined loading utilization per API RP 1111 Section 4.3.4.

    Interaction equation::

        U = sqrt( (M_Sd / (df_moment * M_p))^2
                 + (S_Sd / (df_moment * S_p))^2
                 + |(p_i - p_e) / p_b|^2 )  <= 1

    Plastic reference quantities::

        M_p = SMYS [Pa] * (D^3 - d_i^3) / 6
        S_p = pi/4 * (D^2 - d_i^2) * SMYS [Pa]

    Args:
        D:          Outside diameter [m].
        t:          Wall thickness [m].
        mat:        Pipeline material (SMYS in MPa).
        M_Sd:       Design bending moment [N·m].
        S_Sd:       Design effective tension [N].
        p_i:        Internal pressure [MPa].
        p_e:        External pressure [MPa].
        df_moment:  Design factor for moment/tension terms. Default 0.72.

    Returns:
        Dict with keys: ``pass``, ``utilization``, ``M_p``, ``S_p``.
    """
    d_i = D - 2.0 * t
    smys_pa = mat.SMYS * 1.0e6

    M_p = smys_pa * (D ** 3 - d_i ** 3) / 6.0
    S_p = math.pi / 4.0 * (D ** 2 - d_i ** 2) * smys_pa

    p_b = api_burst_pressure(D=D, t=t, mat=mat)

    if M_p > 0.0 and S_p > 0.0 and p_b > 0.0:
        m_ref = df_moment * M_p
        s_ref = df_moment * S_p
        moment_term = (abs(M_Sd) / m_ref) ** 2
        tension_term = (abs(S_Sd) / s_ref) ** 2
        pressure_term = abs(p_i - p_e) / p_b
        utilization = math.sqrt(moment_term + tension_term + pressure_term ** 2)
    else:
        utilization = float("inf")

    return {
        "pass": utilization <= 1.0,
        "utilization": utilization,
        "M_p": M_p,
        "S_p": S_p,
    }


# ---------------------------------------------------------------------------
# Internal helper: minimum WT for API RP 1111 collapse
# ---------------------------------------------------------------------------

def _min_wt_for_api_collapse(
    D: float,
    p_water: float,
    SMYS: float,
    E: float = _E_STEEL,
    nu: float = _NU_STEEL,
    tol: float = 1e-7,
    max_iter: int = 100,
) -> float:
    """Binary-search minimum WT such that API collapse check passes.

    Finds smallest t where p_water <= DF_COLLAPSE * p_c(D, t, SMYS).

    Args:
        D:         Outside diameter [m].
        p_water:   External hydrostatic pressure [MPa].
        SMYS:      Specified Minimum Yield Strength [MPa].
        E:         Young's modulus [MPa].
        nu:        Poisson's ratio.
        tol:       Convergence tolerance [m]. Default 1e-7 m.
        max_iter:  Maximum bisection iterations.

    Returns:
        Minimum required wall thickness [m], or 0.0 if no external pressure.
    """
    if p_water <= 0.0:
        return 0.0

    lo, hi = 1e-6, D / 2.0 - 1e-6

    def _util(t: float) -> float:
        p_c = api_collapse_pressure(D=D, t=t, SMYS=SMYS, E=E, nu=nu)
        p_allowable = DF_COLLAPSE * p_c
        return p_water / p_allowable if p_allowable > 0.0 else float("inf")

    if _util(hi) > 1.0:
        return hi

    for _ in range(max_iter):
        mid = (lo + hi) / 2.0
        if hi - lo < tol:
            return hi
        if _util(mid) <= 1.0:
            hi = mid
        else:
            lo = mid
    return hi


# ---------------------------------------------------------------------------
# Config-driven wall thickness sizing workflow
# ---------------------------------------------------------------------------

class WallThicknessSizingWorkflow:
    """Config-driven wall thickness sizing workflow.

    Reads pipeline design parameters from a dict (sourced from YAML or
    ASCII config) and returns the governing minimum wall thickness together
    with all pressure containment checks per DNV-ST-F101 and API RP 1111.

    The governing WT is the maximum of:
    - DNV-ST-F101 burst/pressure-containment requirement
    - API RP 1111 external collapse requirement

    Example config structure::

        pipeline:
          outer_diameter_m: 0.3239
          design_pressure_MPa: 20.0
          depth_m: 500.0
          material: "X65"
          safety_class: "normal"
          corrosion_allowance_m: 0.001
          fab_tol_pct: 12.5

    All checks use SI inputs (metres, MPa).
    """

    def __init__(self, config: dict[str, Any]) -> None:
        self._cfg = config["pipeline"]

    def run(self) -> dict[str, Any]:
        """Execute sizing workflow and return results dict.

        Returns:
            Dict with keys: ``t_min_m``, ``t_min_dnv_m``, ``t_min_collapse_m``,
            ``governing_mode``, ``material_grade``, ``dnv_pressure_containment``,
            ``api_external_collapse``, ``api_propagating_buckle``,
            ``dnv_external_collapse``, ``dnv_propagating_buckle``.
        """
        cfg = self._cfg
        D = float(cfg["outer_diameter_m"])
        p_d = float(cfg["design_pressure_MPa"])
        depth = float(cfg["depth_m"])
        grade = str(cfg["material"])
        safety_class = str(cfg.get("safety_class", "normal"))
        t_corr = float(cfg.get("corrosion_allowance_m", 0.001))
        fab_tol = float(cfg.get("fab_tol_pct", 12.5))

        mat = MATERIAL_LIBRARY[grade]
        p_water = api_water_pressure(depth=depth)

        t_min_dnv = dnv_wall_thickness(
            p_d=p_d, p_e=p_water, D=D, mat=mat,
            safety_class=safety_class,
            t_corr=t_corr, fab_tol_pct=fab_tol,
        )
        t_min_collapse = _min_wt_for_api_collapse(
            D=D, p_water=p_water, SMYS=mat.SMYS,
        )

        t_min = max(t_min_dnv, t_min_collapse)
        governing = "collapse" if t_min_collapse > t_min_dnv else "burst"

        dnv_cont = dnv_pressure_containment_check(
            p_li=p_d * GAMMA_INC, D=D, t_nom=t_min, mat=mat,
            safety_class=safety_class,
        )
        api_collapse_result = api_external_collapse_check(
            depth=depth, D=D, t=t_min, SMYS=mat.SMYS
        )
        api_prop_result = api_propagating_buckle_check(
            depth=depth, D=D, t=t_min, SMYS=mat.SMYS
        )
        dnv_collapse_result = dnv_external_collapse_check(
            depth=depth, D=D, t=t_min, mat=mat, safety_class=safety_class,
        )
        dnv_prop_result = dnv_propagating_buckle_check(
            depth=depth, D=D, t=t_min, mat=mat, safety_class=safety_class,
        )

        return {
            "t_min_m": t_min,
            "t_min_dnv_m": t_min_dnv,
            "t_min_collapse_m": t_min_collapse,
            "governing_mode": governing,
            "material_grade": grade,
            "dnv_pressure_containment": dnv_cont,
            "api_external_collapse": api_collapse_result,
            "api_propagating_buckle": api_prop_result,
            "dnv_external_collapse": dnv_collapse_result,
            "dnv_propagating_buckle": dnv_prop_result,
        }


def run_sizing_from_yaml(path: str) -> dict[str, Any]:
    """Load a YAML config file and run the wall thickness sizing workflow.

    Args:
        path:  Absolute or relative path to a YAML config file.

    Returns:
        Sizing result dict from :class:`WallThicknessSizingWorkflow`.

    Raises:
        FileNotFoundError: If the config file does not exist.
    """
    try:
        import yaml
    except ImportError as exc:
        raise ImportError("PyYAML is required for run_sizing_from_yaml") from exc

    if not os.path.exists(path):
        raise FileNotFoundError(f"Config file not found: {path}")

    with open(path, "r", encoding="utf-8") as fh:
        config = yaml.safe_load(fh)

    return WallThicknessSizingWorkflow(config).run()
