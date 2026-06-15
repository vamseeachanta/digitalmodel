"""UV-workflow router for offline wave energy spectra."""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any

from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


REPO_ROOT = Path(__file__).resolve().parents[4]


def router(cfg: dict) -> dict:
    settings_key = _settings_key(cfg)
    settings = cfg.get(settings_key) or {}
    spectrum = _spectrum_name(settings)
    significant_height = _positive_float(settings, "Hs")
    peak_period = _positive_float(settings, "Tp")
    gamma = _positive_float(settings, "gamma", 3.3)
    frequency_grid = _frequency_grid(settings)

    generator = WaveSpectra()
    if spectrum == "jonswap":
        frequencies, spectral_density = generator.jonswap(
            hs=significant_height,
            tp=peak_period,
            gamma=gamma,
            freq_min=frequency_grid["start"],
            freq_max=frequency_grid["stop"],
            n_points=frequency_grid["num"],
        )
    elif spectrum == "pierson_moskowitz":
        frequencies, spectral_density = generator.pierson_moskowitz(
            hs=significant_height,
            tp=peak_period,
            freq_min=frequency_grid["start"],
            freq_max=frequency_grid["stop"],
            n_points=frequency_grid["num"],
        )
    else:
        raise ValueError(f"Unsupported wave_spectrum spectrum: {spectrum}")

    rows = [
        {"frequency": float(frequency), "S": float(density)}
        for frequency, density in zip(frequencies, spectral_density)
    ]
    csv_path = _spectrum_csv_path(cfg, settings)
    _write_spectrum_csv(csv_path, rows)

    m0 = generator.spectral_moment(frequencies, spectral_density, n=0)
    peak_frequency = float(
        generator.peak_frequency_from_spectrum(frequencies, spectral_density)
    )
    cfg[settings_key] = {
        "spectrum": spectrum,
        "Hs_input": significant_height,
        "Hs_check": 4.0 * math.sqrt(m0),
        "Tp": peak_period,
        "gamma": gamma if spectrum == "jonswap" else None,
        "m0": m0,
        "peak_frequency": peak_frequency,
        "frequency_units": "rad_s",
        "points": len(rows),
        "spectrum_csv": _display_path(csv_path),
    }
    return cfg


def _settings_key(cfg: dict) -> str:
    if cfg.get("basename") == "wave_spectra" or "wave_spectra" in cfg:
        return "wave_spectra"
    return "wave_spectrum"


def _spectrum_name(settings: dict[str, Any]) -> str:
    raw_value = (
        settings.get("spectrum")
        or settings.get("spectrum_type")
        or settings.get("type")
    )
    if raw_value is None:
        raise ValueError("wave_spectrum spectrum is required")
    normalized = str(raw_value).strip().lower().replace("-", "_")
    aliases = {
        "pm": "pierson_moskowitz",
        "p_m": "pierson_moskowitz",
        "pierson_moskowitz": "pierson_moskowitz",
        "piersonmoskowitz": "pierson_moskowitz",
        "jonswap": "jonswap",
    }
    if normalized not in aliases:
        raise ValueError(f"Unsupported wave_spectrum spectrum: {raw_value}")
    return aliases[normalized]


def _frequency_grid(settings: dict[str, Any]) -> dict[str, float | int]:
    grid = settings.get("frequency_grid")
    if not isinstance(grid, dict):
        raise ValueError("wave_spectrum frequency_grid must be a mapping")

    start = _required_positive_float(grid, "start", "frequency_grid")
    stop = _required_positive_float(grid, "stop", "frequency_grid")
    num = int(grid.get("num", 0))
    if num < 2:
        raise ValueError("wave_spectrum frequency_grid.num must be at least 2")
    if stop <= start:
        raise ValueError("wave_spectrum frequency_grid.stop must be greater than start")

    units = str(grid.get("units", "rad_s")).strip().lower().replace("/", "_")
    units = units.replace("rad_per_s", "rad_s").replace("radian_s", "rad_s")
    if units in {"hz", "hertz", "cycles_s", "cycles_per_s"}:
        start *= 2.0 * math.pi
        stop *= 2.0 * math.pi
    elif units not in {"rad_s", "rad_sec", "omega"}:
        raise ValueError("wave_spectrum frequency_grid.units must be rad_s or hz")

    return {"start": start, "stop": stop, "num": num}


def _spectrum_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_spectrum.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "wave_spectrum"))


def _write_spectrum_csv(path: Path, rows: list[dict[str, float]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=["frequency", "S"])
        writer.writeheader()
        writer.writerows(rows)


def _positive_float(
    settings: dict[str, Any],
    name: str,
    default: float | None = None,
) -> float:
    value = settings.get(name, default)
    if value is None:
        raise ValueError(f"wave_spectrum {name} is required")
    value = float(value)
    if value <= 0.0 or not math.isfinite(value):
        raise ValueError(f"wave_spectrum {name} must be positive")
    return value


def _required_positive_float(
    settings: dict[str, Any],
    name: str,
    context: str,
) -> float:
    value = settings.get(name)
    if value is None:
        raise ValueError(f"wave_spectrum {context}.{name} is required")
    value = float(value)
    if value <= 0.0 or not math.isfinite(value):
        raise ValueError(f"wave_spectrum {context}.{name} must be positive")
    return value


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
