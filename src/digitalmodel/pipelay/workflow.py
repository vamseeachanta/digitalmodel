"""Durable workflow: J-lay and reel-lay pipelay screening (offline).

Routes a ``basename: pipelay`` input through the analytical pipelay
pre-processing utilities in :mod:`digitalmodel.orcaflex.pipelay_analysis`.
The whole workflow is analytical and runs OFFLINE -- it never imports or
calls OrcFxAPI, so no OrcaFlex license is required.

Two lay methods are supported, selected by ``pipelay.method``:

- ``J-Lay``: near-vertical sagbend catenary geometry (deepwater), via
  :class:`~digitalmodel.orcaflex.pipelay_analysis.JLayConfig`. Reports the
  catenary parameter, sagbend radius / strain / stress, touchdown offset and
  suspended length, and screens the sagbend bending strain against an
  allowable strain limit.
- ``Reel-Lay``: reel + straightener residual-curvature screening. The pipe is
  spooled onto a reel of given core radius, then passed through a
  straightener. The reeling bending strain ``D / (2 * R_reel)`` must stay below
  the plastic-strain limit, and the residual ovalisation / out-of-straightness
  after straightening is reported.

References:
    - Bai & Bai (2014): Subsea Pipeline Design, Analysis and Installation,
      Ch. 5 (J-lay) and Ch. 6 (reel-lay).
    - DNV-OS-F101: Submarine Pipeline Systems, Section 11 (installation).
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Dict

from digitalmodel.orcaflex.pipelay_analysis import (
    JLayConfig,
    LayMethod,
    LayOperability,
    PipelayPipeProperties,
    calculate_stinger_radius,
)


def _pipe_from_cfg(pipe_cfg: Dict[str, Any]) -> PipelayPipeProperties:
    """Build pipe properties, accepting only known fields."""
    allowed = set(PipelayPipeProperties.model_fields)
    filtered = {k: v for k, v in (pipe_cfg or {}).items() if k in allowed}
    return PipelayPipeProperties(**filtered)


def _operability(operability_cfg: Dict[str, Any]) -> Dict[str, Any]:
    """Run the lay-rate vs sea-state operability assessment, if requested."""
    allowed = set(LayOperability.model_fields)
    filtered = {k: v for k, v in (operability_cfg or {}).items() if k in allowed}
    op = LayOperability(**filtered)
    return {
        "max_hs_lay_m": op.max_hs_lay,
        "max_hs_welding_m": op.max_hs_welding,
        "max_hs_abandonment_recovery_m": op.max_hs_aband_recovery,
        "lay_rate_table": op.generate_operability_table(),
    }


def run_jlay(settings: Dict[str, Any]) -> Dict[str, Any]:
    """J-lay sagbend screening (offline, analytical)."""
    pipe = _pipe_from_cfg(settings.get("pipe", {}))
    config = settings.get("config", {})

    jlay = JLayConfig(
        water_depth=float(config.get("water_depth_m", 1500.0)),
        pipe=pipe,
        tower_angle=float(config.get("tower_angle_deg", 0.0)),
        top_tension=float(config.get("top_tension_kN", 800.0)),
    )
    sagbend = jlay.calculate_sagbend()

    strain_limit = float(config.get("allowable_sagbend_strain", 0.002))
    sagbend_strain = sagbend["sagbend_bending_strain"]
    utilisation = sagbend_strain / strain_limit if strain_limit > 0 else float("inf")

    result: Dict[str, Any] = {
        "method": LayMethod.J_LAY.value,
        "water_depth_m": jlay.water_depth,
        "tower_angle_deg": jlay.tower_angle,
        "top_tension_kN": jlay.top_tension,
        "pipe_outer_diameter_m": pipe.outer_diameter,
        "pipe_submerged_weight_N_per_m": round(pipe.submerged_weight_per_m, 3),
        "sagbend": sagbend,
        "allowable_sagbend_strain": strain_limit,
        "sagbend_strain_utilisation": round(utilisation, 4),
        "screening_status": "pass" if utilisation <= 1.0 else "fail",
    }
    if "operability" in settings:
        result["operability"] = _operability(settings["operability"])
    return result


def run_reel_lay(settings: Dict[str, Any]) -> Dict[str, Any]:
    """Reel-lay reeling-strain + residual-curvature screening (offline).

    The reeling bending strain is ``epsilon = D / (2 * R_reel)`` where
    ``R_reel`` is the reel core radius plus the part of the pipe already
    wound on. The pipe yields plastically on the reel (this is intended in
    reel-lay) but must stay below an allowable plastic strain. After
    straightening, a residual curvature remains; the residual
    out-of-straightness over a span is reported.

    Reference: Bai & Bai (2014) Ch. 6.
    """
    pipe = _pipe_from_cfg(settings.get("pipe", {}))
    config = settings.get("config", {})

    reel_core_radius = float(config.get("reel_core_radius_m", 8.0))
    if reel_core_radius <= 0.0:
        raise ValueError("reel_core_radius_m must be positive")
    allowable_reeling_strain = float(config.get("allowable_reeling_strain", 0.02))
    straightener_efficiency = float(config.get("straightener_efficiency", 0.95))
    if not 0.0 <= straightener_efficiency <= 1.0:
        raise ValueError("straightener_efficiency must be in [0, 1]")
    residual_span_m = float(config.get("residual_out_of_straightness_span_m", 12.0))

    D = pipe.outer_diameter

    # Reeling bending strain at the reel core (worst case = innermost wrap).
    reeling_strain = D / (2.0 * reel_core_radius)
    reeling_utilisation = (
        reeling_strain / allowable_reeling_strain
        if allowable_reeling_strain > 0
        else float("inf")
    )

    # Residual curvature after the straightener removes most of the reeling
    # curvature. kappa_reel = 1/R_reel; residual = (1 - efficiency) * kappa_reel.
    reel_curvature = 1.0 / reel_core_radius
    residual_curvature = (1.0 - straightener_efficiency) * reel_curvature
    residual_radius = (
        1.0 / residual_curvature if residual_curvature > 0 else float("inf")
    )
    residual_strain = D / (2.0 * residual_radius) if residual_radius > 0 else 0.0

    # Out-of-straightness (mid-span sagitta) over a measurement span from the
    # residual curvature: delta = kappa * L^2 / 8.
    residual_oos_m = residual_curvature * residual_span_m**2 / 8.0

    result: Dict[str, Any] = {
        "method": LayMethod.REEL_LAY.value,
        "reel_core_radius_m": reel_core_radius,
        "pipe_outer_diameter_m": D,
        "pipe_submerged_weight_N_per_m": round(pipe.submerged_weight_per_m, 3),
        "reeling_bending_strain": round(reeling_strain, 6),
        "allowable_reeling_strain": allowable_reeling_strain,
        "reeling_strain_utilisation": round(reeling_utilisation, 4),
        "straightener_efficiency": straightener_efficiency,
        "residual_curvature_1_per_m": round(residual_curvature, 8),
        "residual_radius_m": round(residual_radius, 1),
        "residual_bending_strain": round(residual_strain, 6),
        "residual_out_of_straightness_span_m": residual_span_m,
        "residual_out_of_straightness_m": round(residual_oos_m, 5),
        "screening_status": "pass" if reeling_utilisation <= 1.0 else "fail",
    }
    if "operability" in settings:
        result["operability"] = _operability(settings["operability"])
    return result


def router(cfg: dict) -> dict:
    """Engine entry point for ``basename: pipelay``."""
    settings = cfg.get("pipelay") or {}
    method = str(settings.get("method", LayMethod.J_LAY.value)).strip().lower()

    if method in ("j-lay", "jlay", "j_lay"):
        result = run_jlay(settings)
    elif method in ("reel-lay", "reel_lay", "reellay", "reel"):
        result = run_reel_lay(settings)
    else:
        raise ValueError(
            f"Unknown pipelay method '{settings.get('method')}'. "
            "Use 'J-Lay' or 'Reel-Lay'."
        )

    # Minimum stinger radius is a useful reference for any method.
    result["min_stinger_radius_m"] = round(
        calculate_stinger_radius(
            result["pipe_outer_diameter_m"],
            float(settings.get("config", {}).get("allowable_overbend_strain", 0.002)),
        ),
        1,
    )

    summary_path = _write_summary(cfg, settings.get("outputs", {}), result)
    result["summary_json"] = str(summary_path)

    cfg["pipelay"] = {**settings, **result}
    cfg["screening_status"] = result["screening_status"]
    return cfg


def _write_summary(cfg: dict, output_cfg: dict, payload: Dict[str, Any]) -> Path:
    directory = Path(output_cfg.get("directory", "results/pipelay"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get("summary_json", "pipelay_summary.json")
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return summary_path
