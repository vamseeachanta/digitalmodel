# ABOUTME: Electric submersible pump (ESP) hydraulics — TDH, stage count, motor sizing.
# ABOUTME: Total dynamic head + Hazen-Williams tubing friction + pump-curve stage sizing.
"""Electric submersible pump (ESP) hydraulics screening.

Sizes an ESP for a producing well from the total dynamic head (TDH) and the
selected pump's per-stage curve values:

    TDH = net_lift + friction_head + discharge_head            [m]
        net_lift      = dynamic fluid level (depth from surface to the
                        producing fluid level the pump must lift from)
        friction_head = Hazen-Williams tubing loss over the pump setting depth
        discharge_head = wellhead pressure expressed as head

    stages       = ceil(TDH / head_per_stage)
    brake_power  = stages * bhp_per_stage * specific_gravity   [hp]

The screen passes when the required stages fit the pump housing, the brake
power is within the motor rating, and the design rate is inside the pump's
recommended operating range.

Hazen-Williams (SI): hf = 10.67 * L * Q^1.852 / (C^1.852 * d^4.87), with Q in
m^3/s, d and L in m, hf in m.

References: standard ESP sizing procedure (e.g. Centrilift / Schlumberger ESP
handbooks); Hazen-Williams pipe-friction correlation.
"""

from __future__ import annotations

import json
import math
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any

_G = 9.81  # m/s^2
_SECONDS_PER_DAY = 86400.0


@dataclass
class ESPResult:
    standard: str
    flow_rate_m3_per_day: float
    net_lift_m: float
    friction_head_m: float
    discharge_head_m: float
    total_dynamic_head_m: float
    stages_required: int
    brake_power_hp: float
    checks: list[dict[str, Any]]
    governing_check: str
    screening_status: str


def hazen_williams_head_m(
    flow_rate_m3_per_day: float,
    length_m: float,
    inner_diameter_m: float,
    c_factor: float,
) -> float:
    """Hazen-Williams friction head (m) for liquid flow in a circular conduit."""
    if inner_diameter_m <= 0.0:
        raise ValueError("inner_diameter_m must be positive")
    if c_factor <= 0.0:
        raise ValueError("hazen_williams_C must be positive")
    if length_m < 0.0:
        raise ValueError("length_m must be non-negative")
    q = flow_rate_m3_per_day / _SECONDS_PER_DAY  # m^3/s
    return 10.67 * length_m * q**1.852 / (c_factor**1.852 * inner_diameter_m**4.87)


def size_esp(
    flow_rate_m3_per_day: float,
    dynamic_fluid_level_m: float,
    pump_setting_depth_m: float,
    wellhead_pressure_kpa: float,
    specific_gravity: float,
    tubing_inner_diameter_m: float,
    head_per_stage_m: float,
    bhp_per_stage_hp: float,
    max_stages: int,
    motor_rating_hp: float,
    min_rate_m3_per_day: float,
    max_rate_m3_per_day: float,
    hazen_williams_c: float = 120.0,
) -> ESPResult:
    """Total dynamic head, stage count, motor brake power and pass/fail checks."""
    if specific_gravity <= 0.0:
        raise ValueError("specific_gravity must be positive")
    if head_per_stage_m <= 0.0:
        raise ValueError("head_per_stage_m must be positive")
    if bhp_per_stage_hp <= 0.0:
        raise ValueError("bhp_per_stage_hp must be positive")
    if max_stages <= 0:
        raise ValueError("max_stages must be positive")

    net_lift = dynamic_fluid_level_m
    friction_head = hazen_williams_head_m(
        flow_rate_m3_per_day,
        pump_setting_depth_m,
        tubing_inner_diameter_m,
        hazen_williams_c,
    )
    discharge_head = wellhead_pressure_kpa * 1.0e3 / (specific_gravity * 1000.0 * _G)
    tdh = net_lift + friction_head + discharge_head

    stages = math.ceil(tdh / head_per_stage_m)
    brake_power = stages * bhp_per_stage_hp * specific_gravity

    checks = [
        _check(
            "pump_stages",
            stages,
            max_stages,
            f"stages {stages} vs housing max {max_stages}",
        ),
        _check(
            "motor_power",
            brake_power,
            motor_rating_hp,
            f"brake power {brake_power:.1f} hp vs motor rating {motor_rating_hp:.1f} hp",
        ),
        _check(
            "rate_within_range",
            flow_rate_m3_per_day,
            max_rate_m3_per_day,
            f"design rate {flow_rate_m3_per_day:.0f} vs pump max {max_rate_m3_per_day:.0f} m3/d",
            lower=min_rate_m3_per_day,
        ),
    ]
    governing = max(checks, key=lambda c: c["utilization"])
    passes = all(c["passes"] for c in checks)

    return ESPResult(
        standard="ESP sizing (TDH) + Hazen-Williams",
        flow_rate_m3_per_day=flow_rate_m3_per_day,
        net_lift_m=net_lift,
        friction_head_m=friction_head,
        discharge_head_m=discharge_head,
        total_dynamic_head_m=tdh,
        stages_required=stages,
        brake_power_hp=brake_power,
        checks=checks,
        governing_check=governing["name"],
        screening_status="pass" if passes else "fail",
    )


def _check(
    name: str,
    demand: float,
    allowable: float,
    description: str,
    lower: float | None = None,
) -> dict[str, Any]:
    utilization = demand / allowable if allowable > 0 else float("inf")
    passes = demand <= allowable
    if lower is not None and demand < lower:
        passes = False
    return {
        "name": name,
        "demand": demand,
        "allowable": allowable,
        "utilization": utilization,
        "passes": passes,
        "description": description,
    }


def router(cfg: dict) -> dict:
    settings = cfg.get("esp_pump_hydraulics") or {}
    well = settings["well"]
    fluid = settings["fluid"]
    tubing = settings["tubing"]
    pump = settings["pump"]
    motor = settings["motor"]

    result = size_esp(
        flow_rate_m3_per_day=float(fluid["flow_rate_m3_per_day"]),
        dynamic_fluid_level_m=float(well["dynamic_fluid_level_m"]),
        pump_setting_depth_m=float(well["pump_setting_depth_m"]),
        wellhead_pressure_kpa=float(well["wellhead_pressure_kpa"]),
        specific_gravity=float(fluid["specific_gravity"]),
        tubing_inner_diameter_m=float(tubing["inner_diameter_m"]),
        head_per_stage_m=float(pump["head_per_stage_m"]),
        bhp_per_stage_hp=float(pump["bhp_per_stage_hp"]),
        max_stages=int(pump["max_stages"]),
        motor_rating_hp=float(motor["rating_hp"]),
        min_rate_m3_per_day=float(pump["min_rate_m3_per_day"]),
        max_rate_m3_per_day=float(pump["max_rate_m3_per_day"]),
        hazen_williams_c=float(fluid.get("hazen_williams_C", 120.0)),
    )

    payload = {
        **settings,
        "result": asdict(result),
        "screening_status": result.screening_status,
    }
    payload["summary_json"] = str(
        _write_summary(cfg, settings.get("outputs", {}), payload)
    )
    cfg["esp_pump_hydraulics"] = payload
    cfg["screening_status"] = result.screening_status
    return cfg


def _write_summary(cfg: dict, output_cfg: dict, payload: dict[str, Any]) -> Path:
    directory = Path(output_cfg.get("directory", "results/esp_pump_hydraulics"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get(
        "summary_json", "esp_pump_hydraulics_summary.json"
    )
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return summary_path
