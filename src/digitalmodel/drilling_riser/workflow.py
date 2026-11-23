"""UV-workflow router for drilling riser stack-up calculations."""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

from digitalmodel.drilling_riser.stackup import (
    F_BT_DEFAULT,
    F_WT_DEFAULT,
    SAFETY_FACTOR_BARLOW,
    SAFETY_FACTOR_TENSION,
    effective_tension,
    minimum_slip_ring_tension,
    top_tension_required,
    wall_thickness_required,
)


REPO_ROOT = Path(__file__).resolve().parents[3]
SLIP_RING_FIELDS = (
    "buoyancy_uplift_kn",
    "internal_area_m2",
    "mud_density_kn_m3",
    "mud_column_m",
    "seawater_density_kn_m3",
    "seawater_column_m",
)


def router(cfg: dict) -> dict:
    settings = cfg.get("riser_stackup") or {}
    submerged_weight_kn = _required_float(settings, "submerged_weight_kn")
    dynamic_factor = _optional_float(
        settings,
        "dynamic_factor",
        SAFETY_FACTOR_TENSION,
        aliases=("top_tension_safety_factor",),
    )
    top_tension_kn = top_tension_required(submerged_weight_kn, dynamic_factor)
    wall_thickness_mm = wall_thickness_required(
        od_mm=_required_float(settings, "od_mm"),
        design_pressure_mpa=_required_float(settings, "design_pressure_mpa"),
        smys_mpa=_required_float(settings, "smys_mpa"),
        safety_factor=_optional_float(
            settings,
            "barlow_safety_factor",
            SAFETY_FACTOR_BARLOW,
            aliases=("wall_safety_factor", "safety_factor_barlow"),
        ),
    )

    depth_factors = _depth_factors(settings)
    profile = [
        (depth_factor, effective_tension(top_tension_kn, submerged_weight_kn, depth_factor))
        for depth_factor in depth_factors
    ]
    csv_path = _profile_csv_path(cfg, settings)
    _write_profile_csv(csv_path, profile)

    summary = {
        "submerged_weight_kn": submerged_weight_kn,
        "top_tension_required_kn": top_tension_kn,
        "wall_thickness_required_mm": wall_thickness_mm,
        "points": len(profile),
        "profile_csv": _display_path(csv_path),
    }
    min_slip_ring_tension_kn = _optional_slip_ring_tension(
        settings, submerged_weight_kn
    )
    if min_slip_ring_tension_kn is not None:
        summary["min_slip_ring_tension_kn"] = min_slip_ring_tension_kn

    cfg["riser_stackup"] = summary
    return cfg


def _depth_factors(settings: dict) -> list[float]:
    grid = settings.get("depth_factors", settings.get("depth_grid"))
    if isinstance(grid, dict):
        start = float(grid["start"])
        stop = float(grid["stop"])
        num = int(grid["num"])
        if num < 2:
            raise ValueError("riser_stackup depth_grid.num must be at least 2")
        step = (stop - start) / (num - 1)
        depth_factors = [start + index * step for index in range(num)]
    elif isinstance(grid, list):
        depth_factors = [float(value) for value in grid]
    else:
        raise ValueError("riser_stackup requires depth_factors as a list or mapping")

    if not depth_factors:
        raise ValueError("riser_stackup depth_factors must not be empty")
    if any(value < 0.0 or value > 1.0 for value in depth_factors):
        raise ValueError("riser_stackup depth_factors must be between 0 and 1")
    if any(
        depth_factors[index] <= depth_factors[index - 1]
        for index in range(1, len(depth_factors))
    ):
        raise ValueError("riser_stackup depth_factors must be strictly increasing")
    return depth_factors


def _optional_slip_ring_tension(
    settings: dict[str, Any], submerged_weight_kn: float
) -> float | None:
    provided = [name for name in SLIP_RING_FIELDS if settings.get(name) is not None]
    if not provided:
        return None

    missing = [name for name in SLIP_RING_FIELDS if settings.get(name) is None]
    if missing:
        joined = ", ".join(missing)
        raise ValueError(f"riser_stackup slip-ring inputs missing: {joined}")

    return minimum_slip_ring_tension(
        submerged_weight_kn=submerged_weight_kn,
        buoyancy_uplift_kn=_required_float(settings, "buoyancy_uplift_kn"),
        internal_area_m2=_required_float(settings, "internal_area_m2"),
        mud_density_kn_m3=_required_float(settings, "mud_density_kn_m3"),
        mud_column_m=_required_float(settings, "mud_column_m"),
        seawater_density_kn_m3=_required_float(settings, "seawater_density_kn_m3"),
        seawater_column_m=_required_float(settings, "seawater_column_m"),
        f_wt=_optional_float(settings, "f_wt", F_WT_DEFAULT),
        f_bt=_optional_float(settings, "f_bt", F_BT_DEFAULT),
    )


def _profile_csv_path(cfg: dict, settings: dict) -> Path:
    cfg_dir = _config_dir(cfg)
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = cfg_dir / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_riser_stackup.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "riser_stackup"))


def _write_profile_csv(path: Path, profile: list[tuple[float, float]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["depth_factor", "effective_tension_kn"])
        writer.writerows(profile)


def _required_float(settings: dict[str, Any], name: str) -> float:
    value = settings.get(name)
    if value is None:
        raise ValueError(f"riser_stackup {name} is required")
    return float(value)


def _optional_float(
    settings: dict[str, Any],
    name: str,
    default: float,
    aliases: tuple[str, ...] = (),
) -> float:
    for key in (name, *aliases):
        if settings.get(key) is not None:
            return float(settings[key])
    return default


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
