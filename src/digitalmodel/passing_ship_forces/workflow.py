"""Durable workflow: Wang passing-ship forces on a moored vessel.

Wires the ported legacy Wang closed-form calculator
(``digitalmodel.hydrodynamics.wang_passing_ship``) into a registry
workflow: parabolic-sectional-area slender-body forces (surge, sway, yaw)
on a moored ship over a passing-ship stagger sweep, deep water or finite
depth via the legacy 21-image bottom correction.

This complements ``passing_ship`` (the spec-era package with its own
configuration/CLI stack): the ``passing_ship_forces`` chain reproduces the
legacy VBA/MathCAD tool's arithmetic exactly — closed-form inner
integrals, image-method depth correction — so results are traceable to
the validated legacy calculator.

Config schema (YAML basename ``passing_ship_forces``)::

    basename: passing_ship_forces
    passing_ship_forces:
      moored_vessel:  {length: 941.109, midship_area: 8103.879}
      passing_vessel: {length: 941.109, midship_area: 8103.879}
      water_density: 1.9905          # slug/ft^3 (or kg/m^3 for SI)
      passing_velocity: 6.0          # ft/s (or m/s); sign = direction
      separation_distance: 364.042   # centreline to centreline
      water_depth: 55.0              # optional; omit for deep water
      n_images: 10                   # optional (legacy default 10)
      stagger:                       # either explicit values ...
        values: [-1882.2, 0.0, 1882.2]
      # ... or a generated range (normalized multiplies by moored length):
      #   range: {start: -2.0, stop: 2.0, num: 81, normalized: true}
      output_dir: results

Units: any consistent set (legacy tool: ft, ft^2, slug/ft^3, ft/s ->
lbf, ft-lbf; SI inputs give N, N-m).
"""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

import numpy as np

from digitalmodel.hydrodynamics.wang_passing_ship import (
    DEFAULT_N_IMAGES,
    DEFAULT_QUAD_NODES,
    WangVessel,
    wang_passing_forces,
)

REPO_ROOT = Path(__file__).resolve().parents[3]


def router(cfg: dict) -> dict:
    settings = cfg.get("passing_ship_forces") or {}
    moored = _vessel(settings, "moored_vessel")
    passing = _vessel(settings, "passing_vessel")
    density = _positive_float(settings, "water_density")
    separation = _positive_float(settings, "separation_distance")
    velocity = _finite_float(settings, "passing_velocity")
    depth = _optional_positive_float(settings, "water_depth")
    n_images = int(settings.get("n_images", DEFAULT_N_IMAGES))
    if n_images < 1:
        raise ValueError("passing_ship_forces n_images must be >= 1")
    n_nodes = int(settings.get("quadrature_nodes", DEFAULT_QUAD_NODES))
    stagger = _stagger(settings, moored)

    result = wang_passing_forces(
        stagger,
        separation,
        moored,
        passing,
        velocity,
        density,
        depth=depth,
        n_images=n_images,
        n_nodes=n_nodes,
    )

    rows = [
        {
            "stagger": float(xi),
            "surge_force": float(fx),
            "sway_force": float(fy),
            "yaw_moment": float(mz),
        }
        for xi, fx, fy, mz in zip(
            result.stagger, result.surge, result.sway, result.yaw
        )
    ]
    summary = _summary(result, depth, n_images, separation)

    csv_path = _output_path(cfg, settings, "passing_ship_forces.csv")
    summary_path = _output_path(cfg, settings, "passing_ship_forces_summary.csv")
    _write_csv(csv_path, rows)
    _write_csv(summary_path, [summary])

    cfg["passing_ship_forces"] = {
        "moored_vessel": {
            "length": moored.length,
            "midship_area": moored.midship_area,
        },
        "passing_vessel": {
            "length": passing.length,
            "midship_area": passing.midship_area,
        },
        "water_density": density,
        "passing_velocity": velocity,
        "separation_distance": separation,
        "water_depth": depth,
        **summary,
        "forces": rows,
        "results_csv": _display_path(csv_path),
        "summary_csv": _display_path(summary_path),
    }
    return cfg


def _summary(result, depth, n_images, separation) -> dict[str, Any]:
    surge, sway, yaw = result.surge, result.sway, result.yaw
    stagger = result.stagger

    def peak(values):
        index = int(np.argmax(np.abs(values)))
        return float(values[index]), float(stagger[index])

    peak_surge, at_surge = peak(surge)
    peak_sway, at_sway = peak(sway)
    peak_yaw, at_yaw = peak(yaw)
    return {
        "method": "wang_1975_slender_body",
        "depth_mode": "deep" if depth is None else "finite_image_method",
        "n_images": None if depth is None else n_images,
        "n_stagger": int(stagger.size),
        "peak_surge_force": peak_surge,
        "peak_surge_stagger": at_surge,
        "peak_sway_force": peak_sway,
        "peak_sway_stagger": at_sway,
        "peak_yaw_moment": peak_yaw,
        "peak_yaw_stagger": at_yaw,
        "max_sway_attraction": float(np.max(sway)),
        "max_sway_repulsion": float(np.min(sway)),
    }


def _vessel(settings: dict[str, Any], name: str) -> WangVessel:
    raw = settings.get(name)
    if not isinstance(raw, dict):
        raise ValueError(f"passing_ship_forces {name} mapping is required")
    length = _positive_float(raw, "length", f"{name}.length")
    area = _positive_float(raw, "midship_area", f"{name}.midship_area")
    return WangVessel(length=length, midship_area=area)


def _stagger(settings: dict[str, Any], moored: WangVessel) -> np.ndarray:
    raw = settings.get("stagger")
    if not isinstance(raw, dict):
        raise ValueError(
            "passing_ship_forces stagger mapping is required "
            "(either {values: [...]} or {range: {start, stop, num}})"
        )
    values = raw.get("values")
    rng = raw.get("range")
    if (values is None) == (rng is None):
        raise ValueError(
            "passing_ship_forces stagger needs exactly one of values or range"
        )
    if values is not None:
        if not isinstance(values, list) or not values:
            raise ValueError("passing_ship_forces stagger.values must be a non-empty list")
        return np.asarray([float(v) for v in values])
    if not isinstance(rng, dict):
        raise ValueError("passing_ship_forces stagger.range must be a mapping")
    start = _finite_float(rng, "start", "stagger.range.start")
    stop = _finite_float(rng, "stop", "stagger.range.stop")
    num = int(rng.get("num", 0))
    if num < 2:
        raise ValueError("passing_ship_forces stagger.range.num must be >= 2")
    scale = moored.length if rng.get("normalized", False) else 1.0
    return np.linspace(start * scale, stop * scale, num)


def _positive_float(source: dict[str, Any], name: str, label: str | None = None) -> float:
    label = label or name
    value = source.get(name)
    if value is None:
        raise ValueError(f"passing_ship_forces {label} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"passing_ship_forces {label} must be positive")
    return value


def _optional_positive_float(source: dict[str, Any], name: str) -> float | None:
    value = source.get(name)
    if value is None:
        return None
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"passing_ship_forces {name} must be positive when given")
    return value


def _finite_float(source: dict[str, Any], name: str, label: str | None = None) -> float:
    label = label or name
    value = source.get(name)
    if value is None:
        raise ValueError(f"passing_ship_forces {label} is required")
    value = float(value)
    if not np.isfinite(value):
        raise ValueError(f"passing_ship_forces {label} must be finite")
    return value


def _output_path(cfg: dict, settings: dict[str, Any], suffix: str) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir / f"{_input_stem(cfg)}_{suffix}"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "passing_ship_forces"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("passing_ship_forces cannot write empty results")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
