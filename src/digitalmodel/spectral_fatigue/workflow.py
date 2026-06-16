"""Durable workflow for frequency-domain (spectral) fatigue screening.

Wires the tested spectral-fatigue library (digitalmodel.fatigue.spectral_fatigue)
into a registry workflow: stress response PSD -> spectral moments -> damage
estimator (Dirlik default; narrow-band / Wirsching-Light / Benasciutti-Tovo) ->
DNV-RP-C203 S-N -> damage accumulated over sea states by occurrence.
"""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any

from digitalmodel.fatigue.spectral_fatigue import (
    benasciutti_tovo_damage,
    compute_spectral_moments,
    dirlik_damage,
    narrow_band_damage,
    wirsching_light_damage,
)


REPO_ROOT = Path(__file__).resolve().parents[3]

# Damage estimators, keyed by config name. All share the signature
# (moments, sn_slope, sn_intercept, duration) -> SpectralFatigueResult.
METHODS = {
    "dirlik": dirlik_damage,
    "narrow_band": narrow_band_damage,
    "wirsching_light": wirsching_light_damage,
    "benasciutti_tovo": benasciutti_tovo_damage,
}


def router(cfg: dict) -> dict:
    settings = cfg.get("spectral_fatigue") or {}
    design_life_years = _positive_float(settings, "design_life_years")
    dff = _positive_float(settings, "dff")
    damage_fn, method_name = _method(settings)
    sn_slope, sn_intercept = _sn_curve(settings)

    rows: list[dict[str, Any]] = []
    summaries: list[dict[str, Any]] = []
    for location in _locations(settings):
        location_rows, summary = _evaluate_location(
            location, damage_fn, method_name, sn_slope, sn_intercept,
            design_life_years, dff,
        )
        rows.extend(location_rows)
        summaries.append(summary)

    governing = min(summaries, key=lambda item: item["margin"])
    csv_path = _results_csv_path(cfg, settings)
    summary_path = _summary_csv_path(cfg, settings)
    _write_csv(csv_path, rows)
    _write_csv(summary_path, summaries)

    cfg["spectral_fatigue"] = {
        "design_life_years": design_life_years,
        "dff": dff,
        "method": method_name,
        "sn_curve": {"slope": sn_slope, "log_intercept": sn_intercept},
        "governing_location": governing["location_id"],
        "governing_margin": governing["margin"],
        "governing_fatigue_life_years": governing["fatigue_life_years"],
        # Top-level pass/fail (governing location is worst-case): drives the
        # deckhand report template's mitigation-on-fail section.
        "screening_status": "pass" if all(s["passes"] for s in summaries) else "fail",
        "locations": summaries,
        "results_csv": _display_path(csv_path),
        "summary_csv": _display_path(summary_path),
    }
    cfg["screening_status"] = cfg["spectral_fatigue"]["screening_status"]
    return cfg


def _evaluate_location(
    location: dict[str, Any],
    damage_fn: Any,
    method_name: str,
    sn_slope: float,
    sn_intercept: float,
    design_life_years: float,
    dff: float,
) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    location_id = str(location.get("id", location.get("name", "location")))
    sea_states = _sea_states(location, location_id)
    rows: list[dict[str, Any]] = []
    annual_damage = 0.0
    for index, sea_state in enumerate(sea_states):
        frequency, psd = _psd(sea_state, location_id)
        occurrence = _occurrence(sea_state, location_id)
        moments = compute_spectral_moments(frequency, psd)
        result = damage_fn(moments, sn_slope, sn_intercept, duration=1.0)
        contribution = result.damage_per_year * occurrence
        annual_damage += contribution
        rows.append(
            {
                "location_id": location_id,
                "sea_state_index": index,
                "occurrence_fraction": occurrence,
                "method": method_name,
                "rms_stress_MPa": moments.rms,
                "bandwidth_parameter": moments.bandwidth_parameter,
                "peak_rate_Hz": moments.nu_p,
                "damage_per_year_full": result.damage_per_year,
                "damage_per_year_weighted": contribution,
            }
        )

    if annual_damage <= 0.0:
        raise ValueError(f"{location_id} accumulated zero damage; check the PSD inputs")
    fatigue_life_years = 1.0 / annual_damage
    required_years = design_life_years * dff
    margin = fatigue_life_years / required_years
    summary = {
        "location_id": location_id,
        "method": method_name,
        "annual_damage": annual_damage,
        "fatigue_life_years": fatigue_life_years,
        "required_life_years": required_years,
        "margin": margin,
        "passes": margin >= 1.0,
    }
    return rows, summary


def _method(settings: dict[str, Any]) -> tuple[Any, str]:
    name = str(settings.get("method", "dirlik")).strip().lower()
    if name not in METHODS:
        raise ValueError(
            f"spectral_fatigue method must be one of {sorted(METHODS)}; got {name!r}"
        )
    return METHODS[name], name


def _sn_curve(settings: dict[str, Any]) -> tuple[float, float]:
    sn = settings.get("sn_curve") or {}
    slope = _positive_float(sn, "slope", "sn_curve.slope")
    intercept = _positive_float(sn, "log_intercept", "sn_curve.log_intercept")
    return slope, intercept


def _locations(settings: dict[str, Any]) -> list[dict[str, Any]]:
    locations = settings.get("locations")
    if not isinstance(locations, list) or not locations:
        raise ValueError("spectral_fatigue locations must be a non-empty list")
    return locations


def _sea_states(location: dict[str, Any], location_id: str) -> list[dict[str, Any]]:
    sea_states = location.get("sea_states")
    if not isinstance(sea_states, list) or not sea_states:
        raise ValueError(f"{location_id} sea_states must be a non-empty list")
    return sea_states


def _psd(sea_state: dict[str, Any], location_id: str) -> tuple[list[float], list[float]]:
    frequency = sea_state.get("frequency_Hz")
    psd = sea_state.get("stress_psd_MPa2_Hz")
    if not isinstance(frequency, list) or not isinstance(psd, list):
        raise ValueError(
            f"{location_id} sea_state needs list frequency_Hz and stress_psd_MPa2_Hz"
        )
    if len(frequency) != len(psd) or len(frequency) < 2:
        raise ValueError(
            f"{location_id} frequency_Hz and stress_psd_MPa2_Hz must be equal-length (>=2)"
        )
    return [float(v) for v in frequency], [float(v) for v in psd]


def _occurrence(sea_state: dict[str, Any], location_id: str) -> float:
    value = sea_state.get("occurrence_fraction", 1.0)
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"{location_id} occurrence_fraction must be positive")
    return value


def _positive_float(source: dict[str, Any], name: str, label: str | None = None) -> float:
    label = label or name
    value = source.get(name)
    if value is None:
        raise ValueError(f"spectral_fatigue {label} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"spectral_fatigue {label} must be positive")
    return value


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / f"{_input_stem(cfg)}_spectral_fatigue.csv"


def _summary_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / f"{_input_stem(cfg)}_spectral_fatigue_summary.csv"


def _output_dir(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "spectral_fatigue"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("spectral_fatigue cannot write empty results")
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
