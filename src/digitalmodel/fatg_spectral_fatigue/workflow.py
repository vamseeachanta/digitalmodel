"""Durable workflow: legacy FATG closed-form spectral fatigue.

Wires the ported legacy FATG method (``digitalmodel.fatigue.fatg``) into a
registry workflow: stress RAO x Bretschneider (Tz-parameterised, evaluated on
the RAO's own frequency grid) x wave scatter table x directional spreading x
one-slope S-N -> Miner damage per year.

This complements ``rao_spectral_fatigue`` (which builds a stress PSD on a
generated spectral grid and applies Dirlik/narrow-band closed forms): the FATG
chain instead reproduces the legacy Fortran program's arithmetic exactly --
significant-response trapezoid, three-block Rayleigh discretisation and the
fixed 0/45/90-degree spreading matrix -- so results are traceable to the
validated legacy tool.

Config schema (YAML basename ``fatg_spectral_fatigue``)::

    basename: fatg_spectral_fatigue
    fatg_spectral_fatigue:
      design_life_years: 20.0
      dff: 1.0
      sn_curve: F                # legacy E/F/T (psi), or {log10_a: ..., slope: 3.0}
      member_type: general       # general | chord | diagonal
      rao:
        omega_rad_per_s: [ ... ] # strictly increasing (legacy decks used 18 points)
        stress_per_wave_height: [ ... ]  # stress RAO ordinate, per unit wave height
      sea_states:
        - hs: 3.0
          tz: 5.0
          occurrence_fraction: 0.55
          spreading: {p0: 0.6, p45: 0.3, p90: 0.1}   # omit for uniform mode
      output_dir: results

Units: consistent with the S-N intercept (the legacy E/F/T constants are psi;
Hs in the RAO's wave-height unit; Tz in seconds).
"""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

from digitalmodel.fatigue.fatg import (
    FATG_SN_CURVES,
    FatgSeaState,
    fatg_annual_damage,
)

REPO_ROOT = Path(__file__).resolve().parents[3]


def router(cfg: dict) -> dict:
    settings = cfg.get("fatg_spectral_fatigue") or {}
    design_life_years = _positive_float(settings, "design_life_years")
    dff = _positive_float(settings, "dff")
    sn_curve, log10_a, slope = _sn_curve(settings)
    member_type = str(settings.get("member_type", "general")).strip().lower()
    omega, stress_rao = _rao(settings)
    sea_states = _sea_states(settings)

    result = fatg_annual_damage(
        omega,
        stress_rao,
        sea_states,
        sn_curve=sn_curve,
        log10_a=log10_a,
        slope=slope,
        member_type=member_type,
    )

    fatigue_life_years = result.fatigue_life_years
    required_years = design_life_years * dff
    margin = fatigue_life_years / required_years
    screening_status = "pass" if margin >= 1.0 else "fail"

    rows = [
        {
            "sea_state_index": index,
            "hs": item.hs,
            "tz": item.tz,
            "tp": item.tp,
            "occurrence_fraction": item.occurrence,
            "significant_stress_range": item.significant_stress_range,
            "damage_per_year": item.damage_per_year,
        }
        for index, item in enumerate(result.sea_states)
    ]
    summary = {
        "method": "fatg_closed_form_narrow_band",
        "member_type": member_type,
        "sn_log10_a": log10_a if log10_a is not None else FATG_SN_CURVES[sn_curve],
        "sn_slope": slope,
        "annual_damage": result.annual_damage,
        "fatigue_life_years": fatigue_life_years,
        "life_used_percent_1yr": result.life_used_percent_1yr,
        "life_used_percent_10yr": result.life_used_percent_10yr,
        "required_life_years": required_years,
        "margin": margin,
        "passes": margin >= 1.0,
    }

    csv_path = _output_path(cfg, settings, "fatg_spectral_fatigue.csv")
    summary_path = _output_path(cfg, settings, "fatg_spectral_fatigue_summary.csv")
    _write_csv(csv_path, rows)
    _write_csv(summary_path, [summary])

    cfg["fatg_spectral_fatigue"] = {
        "design_life_years": design_life_years,
        "dff": dff,
        **summary,
        "screening_status": screening_status,
        "sea_states": rows,
        "results_csv": _display_path(csv_path),
        "summary_csv": _display_path(summary_path),
    }
    cfg["screening_status"] = screening_status
    return cfg


def _sn_curve(settings: dict[str, Any]) -> tuple[str | None, float | None, float]:
    sn = settings.get("sn_curve", "F")
    if isinstance(sn, dict):
        log10_a = _positive_float(sn, "log10_a", "sn_curve.log10_a")
        slope = float(sn.get("slope", 3.0))
        if slope <= 0.0:
            raise ValueError("fatg_spectral_fatigue sn_curve.slope must be positive")
        return None, log10_a, slope
    name = str(sn).strip().upper()
    if name not in FATG_SN_CURVES:
        raise ValueError(
            "fatg_spectral_fatigue sn_curve must be one of "
            f"{sorted(FATG_SN_CURVES)} or a {{log10_a, slope}} mapping; got {sn!r}"
        )
    return name, None, 3.0


def _rao(settings: dict[str, Any]) -> tuple[list[float], list[float]]:
    rao = settings.get("rao")
    if not isinstance(rao, dict):
        raise ValueError("fatg_spectral_fatigue rao mapping is required")
    omega = rao.get("omega_rad_per_s")
    stress = rao.get("stress_per_wave_height")
    if not isinstance(omega, list) or not isinstance(stress, list):
        raise ValueError(
            "fatg_spectral_fatigue rao needs list omega_rad_per_s and "
            "stress_per_wave_height"
        )
    return [float(v) for v in omega], [float(v) for v in stress]


def _sea_states(settings: dict[str, Any]) -> list[FatgSeaState]:
    raw = settings.get("sea_states")
    if not isinstance(raw, list) or not raw:
        raise ValueError("fatg_spectral_fatigue sea_states must be a non-empty list")
    states: list[FatgSeaState] = []
    for index, item in enumerate(raw):
        if not isinstance(item, dict):
            raise ValueError(f"sea_states[{index}] must be a mapping")
        label = f"sea_states[{index}]"
        hs = _positive_float(item, "hs", f"{label}.hs")
        tz = _positive_float(item, "tz", f"{label}.tz")
        occurrence = _positive_float(
            item, "occurrence_fraction", f"{label}.occurrence_fraction"
        )
        spreading = None
        if item.get("spreading") is not None:
            spread = item["spreading"]
            if not isinstance(spread, dict):
                raise ValueError(f"{label}.spreading must be a mapping (p0/p45/p90)")
            spreading = (
                _non_negative_float(spread, "p0", f"{label}.spreading.p0"),
                _non_negative_float(spread, "p45", f"{label}.spreading.p45"),
                _non_negative_float(spread, "p90", f"{label}.spreading.p90"),
            )
        states.append(
            FatgSeaState(hs=hs, tz=tz, occurrence=occurrence, spreading=spreading)
        )
    return states


def _positive_float(
    source: dict[str, Any], name: str, label: str | None = None
) -> float:
    label = label or name
    value = source.get(name)
    if value is None:
        raise ValueError(f"fatg_spectral_fatigue {label} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"fatg_spectral_fatigue {label} must be positive")
    return value


def _non_negative_float(source: dict[str, Any], name: str, label: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"fatg_spectral_fatigue {label} is required")
    value = float(value)
    if value < 0.0:
        raise ValueError(f"fatg_spectral_fatigue {label} must be non-negative")
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
    return str(cfg.get("basename", "fatg_spectral_fatigue"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("fatg_spectral_fatigue cannot write empty results")
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
