"""Durable workflow: lifting lug / padeye strength screening.

Checks a circular padeye (lifting lug) under a sling load against three
governing failure modes, using AISC allowable-stress design:

- Pin-hole bearing:        sigma_b = P / (d_pin * t)       <= 0.90 * Fy
- Net-section tension:     sigma_t = P / (2 * t * (R - r)) <= 0.60 * Fy
- Shear tear-out to edge:  tau     = P / (2 * t * (R - r)) <= 0.40 * Fy

where t = main-plate thickness + 2 * cheek-plate thickness (total thickness at
the hole), R = padeye outer radius (hole centre to loaded edge), r = hole
radius, and P = static sling load x dynamic amplification x skew factor.

References: AISC ASD allowable stresses (0.60/0.40/0.90 Fy);
DNV-ST-0378 / DNV 2.7-3 (offshore lifting appliances — padeye design).
"""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any

# AISC allowable-stress factors on yield.
BEARING_FACTOR = 0.90
TENSION_FACTOR = 0.60
SHEAR_FACTOR = 0.40


@dataclass
class LugCheck:
    name: str
    demand_mpa: float
    allowable_mpa: float
    utilization: float
    passes: bool


def design_load_kn(
    static_load_kn: float, dynamic_amplification: float, skew_factor: float
) -> float:
    """Factored sling load P = static * DAF * skew."""
    if static_load_kn <= 0.0:
        raise ValueError("static_load_kN must be positive")
    if dynamic_amplification <= 0.0:
        raise ValueError("dynamic_amplification_factor must be positive")
    if skew_factor <= 0.0:
        raise ValueError("skew_load_factor must be positive")
    return static_load_kn * dynamic_amplification * skew_factor


def lug_checks(
    design_load_kn: float,
    total_thickness_mm: float,
    pin_diameter_mm: float,
    hole_diameter_mm: float,
    outer_radius_mm: float,
    yield_strength_mpa: float,
) -> list[LugCheck]:
    """Bearing, net-tension and shear-tear-out checks for a circular padeye."""
    if total_thickness_mm <= 0.0:
        raise ValueError("total thickness must be positive")
    if hole_diameter_mm < pin_diameter_mm:
        raise ValueError("hole_diameter_mm must be >= pin_diameter_mm")
    ligament_mm = outer_radius_mm - hole_diameter_mm / 2.0
    if ligament_mm <= 0.0:
        raise ValueError(
            "outer_radius_mm must exceed the hole radius (positive ligament)"
        )
    if yield_strength_mpa <= 0.0:
        raise ValueError("yield_strength_mpa must be positive")

    p_n = design_load_kn * 1.0e3  # kN -> N
    t = total_thickness_mm

    bearing_area = pin_diameter_mm * t  # mm^2
    net_tension_area = 2.0 * t * ligament_mm  # mm^2, two ligaments
    shear_area = 2.0 * t * ligament_mm  # mm^2, two tear-out planes

    sigma_bearing = p_n / bearing_area  # N/mm^2 = MPa
    sigma_tension = p_n / net_tension_area
    tau_shear = p_n / shear_area

    checks = [
        _check("pin_bearing", sigma_bearing, BEARING_FACTOR * yield_strength_mpa),
        _check("net_tension", sigma_tension, TENSION_FACTOR * yield_strength_mpa),
        _check("shear_tearout", tau_shear, SHEAR_FACTOR * yield_strength_mpa),
    ]
    return checks


def _check(name: str, demand: float, allowable: float) -> LugCheck:
    utilization = demand / allowable
    return LugCheck(
        name=name,
        demand_mpa=demand,
        allowable_mpa=allowable,
        utilization=utilization,
        passes=utilization <= 1.0,
    )


def router(cfg: dict) -> dict:
    settings = cfg.get("lifting_lug") or {}
    lug = settings["lug"]
    load = settings["load"]

    p_design = design_load_kn(
        static_load_kn=float(load["static_load_kN"]),
        dynamic_amplification=float(load.get("dynamic_amplification_factor", 1.0)),
        skew_factor=float(load.get("skew_load_factor", 1.0)),
    )
    total_thickness = float(lug["plate_thickness_mm"]) + 2.0 * float(
        lug.get("cheek_plate_thickness_mm", 0.0)
    )
    checks = lug_checks(
        design_load_kn=p_design,
        total_thickness_mm=total_thickness,
        pin_diameter_mm=float(lug["pin_diameter_mm"]),
        hole_diameter_mm=float(lug["hole_diameter_mm"]),
        outer_radius_mm=float(lug["outer_radius_mm"]),
        yield_strength_mpa=float(lug["yield_strength_mpa"]),
    )

    governing = max(checks, key=lambda c: c.utilization)
    payload = {
        **settings,
        "standard": "AISC ASD / DNV-ST-0378",
        "design_load_kN": p_design,
        "total_thickness_mm": total_thickness,
        "checks": [asdict(c) for c in checks],
        "governing_check": governing.name,
        "governing_utilization": governing.utilization,
        "screening_status": "pass" if all(c.passes for c in checks) else "fail",
    }
    payload["summary_json"] = str(
        _write_summary(cfg, settings.get("outputs", {}), payload)
    )
    cfg["lifting_lug"] = payload
    cfg["screening_status"] = payload["screening_status"]
    return cfg


def _write_summary(cfg: dict, output_cfg: dict, payload: dict[str, Any]) -> Path:
    directory = Path(output_cfg.get("directory", "results/lifting_lug"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get(
        "summary_json", "lifting_lug_summary.json"
    )
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return summary_path
