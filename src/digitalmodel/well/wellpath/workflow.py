"""UV-workflow router for minimum-curvature directional surveys."""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[4]


def router(cfg: dict) -> dict:
    settings = cfg.get("wellpath") or {}
    stations = _stations(settings)
    dls_normalization = _positive_float(settings, "dls_normalization", 30.0)

    rows = _minimum_curvature_survey(stations, dls_normalization)
    csv_path = _results_csv_path(cfg, settings)
    _write_results_csv(csv_path, rows)

    max_dls_row = max(rows, key=lambda row: float(row["dls"]))
    cfg["wellpath"] = {
        "points": len(rows),
        "total_depth": float(rows[-1]["md"]),
        "total_tvd": float(rows[-1]["tvd"]),
        "max_dls": float(max_dls_row["dls"]),
        "max_dls_depth": float(max_dls_row["md"]),
        "dls_normalization": dls_normalization,
        "wellpath_csv": _display_path(csv_path),
    }
    return cfg


def _minimum_curvature_survey(
    stations: list[dict[str, float]],
    dls_normalization: float,
) -> list[dict[str, float]]:
    rows = [
        {
            "md": stations[0]["measured_depth_m"],
            "inc": stations[0]["inclination_deg"],
            "azi": stations[0]["azimuth_deg"],
            "tvd": 0.0,
            "north": 0.0,
            "east": 0.0,
            "dls": 0.0,
        }
    ]

    tvd = north = east = 0.0
    for previous, current in zip(stations, stations[1:]):
        delta_md = current["measured_depth_m"] - previous["measured_depth_m"]
        inc_1 = math.radians(previous["inclination_deg"])
        inc_2 = math.radians(current["inclination_deg"])
        azi_1 = math.radians(previous["azimuth_deg"])
        azi_2 = math.radians(current["azimuth_deg"])

        dogleg = _dogleg_angle(inc_1, inc_2, azi_1, azi_2)
        ratio_factor = _ratio_factor(dogleg)
        tvd += (
            delta_md
            * 0.5
            * (math.cos(inc_1) + math.cos(inc_2))
            * ratio_factor
        )
        north += (
            delta_md
            * 0.5
            * (
                math.sin(inc_1) * math.cos(azi_1)
                + math.sin(inc_2) * math.cos(azi_2)
            )
            * ratio_factor
        )
        east += (
            delta_md
            * 0.5
            * (
                math.sin(inc_1) * math.sin(azi_1)
                + math.sin(inc_2) * math.sin(azi_2)
            )
            * ratio_factor
        )

        rows.append(
            {
                "md": current["measured_depth_m"],
                "inc": current["inclination_deg"],
                "azi": current["azimuth_deg"],
                "tvd": tvd,
                "north": north,
                "east": east,
                "dls": math.degrees(dogleg) * dls_normalization / delta_md,
            }
        )
    return rows


def _dogleg_angle(
    inc_1: float,
    inc_2: float,
    azi_1: float,
    azi_2: float,
) -> float:
    cosine = (
        math.sin(inc_1) * math.sin(inc_2) * math.cos(azi_2 - azi_1)
        + math.cos(inc_1) * math.cos(inc_2)
    )
    return math.acos(max(-1.0, min(1.0, cosine)))


def _ratio_factor(dogleg: float) -> float:
    if abs(dogleg) <= 1.0e-12:
        return 1.0
    return (2.0 / dogleg) * math.tan(dogleg / 2.0)


def _stations(settings: dict[str, Any]) -> list[dict[str, float]]:
    raw_stations = settings.get("stations")
    if not isinstance(raw_stations, list) or not raw_stations:
        raise ValueError("wellpath stations must be a non-empty list")

    stations = []
    for index, station in enumerate(raw_stations):
        if not isinstance(station, dict):
            raise ValueError(f"wellpath stations[{index}] must be a mapping")
        stations.append(
            {
                "measured_depth_m": _required_float(
                    station, "measured_depth_m", f"stations[{index}]"
                ),
                "inclination_deg": _required_float(
                    station, "inclination_deg", f"stations[{index}]"
                ),
                "azimuth_deg": _required_float(
                    station, "azimuth_deg", f"stations[{index}]"
                ),
            }
        )

    previous_md = None
    for index, station in enumerate(stations):
        measured_depth = station["measured_depth_m"]
        inclination = station["inclination_deg"]
        if measured_depth < 0.0:
            raise ValueError(f"wellpath stations[{index}].measured_depth_m must be >= 0")
        if previous_md is not None and measured_depth <= previous_md:
            raise ValueError("wellpath measured_depth_m values must be increasing")
        if not 0.0 <= inclination <= 180.0:
            raise ValueError(
                f"wellpath stations[{index}].inclination_deg must be between 0 and 180"
            )
        previous_md = measured_depth
    return stations


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_wellpath.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "wellpath"))


def _write_results_csv(path: Path, rows: list[dict[str, float]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(
            stream, fieldnames=["md", "inc", "azi", "tvd", "north", "east", "dls"]
        )
        writer.writeheader()
        writer.writerows(rows)


def _required_float(source: dict[str, Any], name: str, context: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"wellpath {context}.{name} is required")
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"wellpath {context}.{name} must be finite")
    return value


def _positive_float(settings: dict[str, Any], name: str, default: float) -> float:
    value = settings.get(name, default)
    value = float(value)
    if value <= 0.0 or not math.isfinite(value):
        raise ValueError(f"wellpath {name} must be positive")
    return value


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)

