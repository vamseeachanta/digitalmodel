# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
  the total reeling-strain criterion, and the residual curvature / out-of-straightness
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
    calculate_stinger_radius,
)
from digitalmodel.pipelay.validation import OUTPUT_DEFAULTS, validate_settings


def _operability(operability_cfg: Dict[str, Any], *, welding_required=False) -> Dict[str, Any]:
    """Run the lay-rate vs sea-state operability assessment, if requested."""
    op = LayOperability(**operability_cfg)
    return {
        "max_hs_lay_m": op.max_hs_lay,
        "max_hs_welding_m": op.max_hs_welding,
        "max_hs_abandonment_recovery_m": op.max_hs_aband_recovery,
        "operating_limit_m": min(op.max_hs_lay, op.max_hs_welding) if welding_required else op.max_hs_lay,
        "operating_limit_basis": "lay_and_welding" if welding_required else "lay_only",
        "lay_rate_table": op.generate_operability_table(welding_required=welding_required),
    }


def run_jlay(settings: Dict[str, Any]) -> Dict[str, Any]:
    """J-lay sagbend screening (offline, analytical)."""
    pipe, config, defaults = validate_settings(settings, LayMethod.J_LAY.value)

    jlay = JLayConfig(
        water_depth=config["water_depth_m"],
        pipe=pipe,
        tower_angle=config["tower_angle_deg"],
        top_tension=config["top_tension_kN"],
    )
    sagbend = jlay.calculate_sagbend()

    strain_limit = config["allowable_sagbend_strain"]
    sagbend_strain = sagbend["sagbend_bending_strain"]
    utilisation = sagbend_strain / strain_limit

    result: Dict[str, Any] = {
        "method": LayMethod.J_LAY.value,
        "water_depth_m": jlay.water_depth,
        "tower_angle_deg": jlay.tower_angle,
        "top_tension_kN": sagbend["derived_top_tension_kN"],
        "supplied_top_tension_kN": jlay.top_tension,
        "applied_defaults": defaults,
        "pipe_outer_diameter_m": pipe.outer_diameter,
        "pipe_submerged_weight_N_per_m": pipe.submerged_weight_per_m,
        "sagbend": sagbend,
        "allowable_sagbend_strain": strain_limit,
        "sagbend_strain_utilisation": utilisation,
        "screening_status": "pass" if utilisation <= 1.0 else "fail",
    }
    if "operability" in settings:
        result["operability"] = _operability(settings["operability"], welding_required=True)
    return result


def run_reel_lay(settings: Dict[str, Any]) -> Dict[str, Any]:
    """Reel-lay reeling-strain + residual-curvature screening (offline).

    The reeling bending strain is ``epsilon = D / (2 * R_reel)`` where
    ``R_reel`` is the reel core radius plus the part of the pipe already
    wound on. The pipe yields plastically on the reel (this is intended in
    reel-lay); the screen compares total bending strain with the supplied criterion. After
    straightening, a residual curvature remains; the residual
    out-of-straightness over a span is reported.

    Reference: Bai & Bai (2014) Ch. 6.
    """
    pipe, config, defaults = validate_settings(settings, LayMethod.REEL_LAY.value)
    result = _reel_result(pipe, config)
    result['applied_defaults'] = defaults
    if "operability" in settings:
        result["operability"] = _operability(settings["operability"])
    return result


def _reel_result(pipe, config) -> Dict[str, Any]:
    """Evaluate screening-basis.md E5-E6 on validated input quantities."""
    reel_core_radius = config["reel_core_radius_m"]
    allowable_reeling_strain = config["allowable_reeling_strain"]
    straightener_efficiency = config["straightener_efficiency"]
    residual_span_m = config["residual_out_of_straightness_span_m"]

    D = pipe.outer_diameter

    # screening-basis.md E5-E6: innermost wrap and residual-curvature assumption.
    reeling_strain = D / (2.0 * reel_core_radius)
    reeling_utilisation = reeling_strain / allowable_reeling_strain

    # E6: the straightener removes the supplied fraction of reel curvature.
    reel_curvature = 1.0 / reel_core_radius
    residual_curvature = (1.0 - straightener_efficiency) * reel_curvature
    residual_radius = (
        1.0 / residual_curvature if residual_curvature > 0 else None
    )
    residual_strain = D * residual_curvature / 2.0

    # E6: small-slope mid-span sagitta, delta = kappa * L^2 / 8.
    residual_oos_m = residual_curvature * residual_span_m**2 / 8.0

    return {
        "method": LayMethod.REEL_LAY.value,
        "reel_core_radius_m": reel_core_radius,
        "pipe_outer_diameter_m": D,
        "pipe_submerged_weight_N_per_m": pipe.submerged_weight_per_m,
        "reeling_bending_strain": reeling_strain,
        "allowable_reeling_strain": allowable_reeling_strain,
        "reeling_strain_utilisation": reeling_utilisation,
        "straightener_efficiency": straightener_efficiency,
        "residual_curvature_1_per_m": residual_curvature,
        "residual_radius_m": residual_radius,
        "residual_radius_status": "finite" if residual_radius is not None else "unbounded_zero_curvature",
        "residual_bending_strain": residual_strain,
        "residual_out_of_straightness_span_m": residual_span_m,
        "residual_out_of_straightness_m": residual_oos_m,
        "screening_status": "pass" if reeling_utilisation <= 1.0 else "fail",
    }


def router(cfg: dict) -> dict:
    """Engine entry point for ``basename: pipelay``."""
    settings = cfg.get("pipelay", {})
    if not isinstance(settings, dict):
        raise ValueError('pipelay must be a mapping')
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
    result["min_stinger_radius_m"] = calculate_stinger_radius(
        result["pipe_outer_diameter_m"],
        float(settings.get("config", {}).get("allowable_overbend_strain", 0.002)),
    )

    result['applied_defaults'].update({f'outputs.{key}': value
        for key, value in OUTPUT_DEFAULTS.items() if key not in settings.get('outputs', {})})
    summary_path = _write_summary(cfg, settings.get("outputs", {}), result)
    result["summary_json"] = str(summary_path)

    cfg["pipelay"] = {**settings, **result}
    cfg["screening_status"] = result["screening_status"]
    return cfg


def _write_summary(cfg: dict, output_cfg: dict, payload: Dict[str, Any]) -> Path:
    # Encode before creating output: unexpected non-finite values fail closed.
    text = json.dumps(payload, indent=2, allow_nan=False)
    directory = Path(output_cfg.get("directory", "results/pipelay"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get("summary_json", "pipelay_summary.json")
    summary_path.write_text(text, encoding="utf-8", newline="\n")
    return summary_path
