"""Current-envelope lock-in screening with annual cross-flow Miner damage."""

import csv
import math
from pathlib import Path

import pandas as pd
import yaml

from digitalmodel.fatigue.damage import miner_damage
from digitalmodel.fatigue.sn_curves import get_sn_curve
from digitalmodel.orcaflex.viv_screening import estimate_response_amplitude
from digitalmodel.viv_screening.inputs import parse_inputs

REPO_ROOT = Path(__file__).resolve().parents[3]
SECONDS_PER_YEAR = 365.25 * 24.0 * 3600.0
DETAILED_TOOL = "SHEAR7/VIVANA-class detailed VIV analysis"
STANDARD = (
    "DNV-RP-C205 (2019) Sec 9 reduced-velocity screening; "
    "DNV-RP-F105 (2017) Sec 4 natural frequency; "
    "DNV-RP-C203 S-N Miner damage rate"
)


def router(cfg: dict) -> dict:
    settings, beam, viv = parse_inputs(cfg)
    rows = _mode_rows(settings, beam, viv)
    reasons = _escalation_reasons(settings, rows, viv)
    curve_settings = settings["sn_curve"]
    try:
        curve = get_sn_curve(curve_settings["curve"], curve_settings["environment"])
    except ValueError as exc:
        raise ValueError(f"viv_screening sn_curve: {exc}") from exc
    rate = None if reasons else _damage_rate(rows, settings, curve)
    life = None if rate is None else (1 / rate if rate > 0 else math.inf)
    required_life = settings["design_life_years"] * settings["dff"]
    locked = [row["mode"] for row in rows if row["cf_lock_in"] or row["il_lock_in"]]
    status = (
        "escalate"
        if reasons
        else ("fail" if locked and life < required_life else "pass")
    )
    totals = {
        "screening_status": status,
        "modes_checked": len(rows),
        "lock_in_count": len(locked),
        "cf_lock_in_count": sum(row["cf_lock_in"] for row in rows),
        "il_lock_in_count": sum(row["il_lock_in"] for row in rows),
        "il_lock_in_unassessed": any(row["il_lock_in"] for row in rows),
        "screening_damage_rate_per_year": rate,
        "screening_life_years": life,
        "required_life_years": required_life,
        "dff_margin": None if life is None else life / required_life,
    }
    cfg["viv_screening"] = _result_block(settings, rows, viv, locked, totals, reasons)
    _write_outputs(cfg, settings, rows, totals)
    return cfg


def _intersection(
    low: float, high: float, lower: float, upper: float, peak: float
) -> float | None:
    lo, hi = max(low, lower), min(high, upper)
    if lo > hi:
        return None
    return min(hi, max(lo, peak))


def _mode_rows(settings: dict, beam, viv) -> list[dict]:
    envelope = settings["current_envelope_m_s"]
    rows = []
    for mode in range(1, settings["n_modes"] + 1):
        frequency = beam.natural_frequency(mode)
        if not math.isfinite(frequency) or frequency <= 0:
            raise ValueError(
                "viv_screening natural frequency must be finite and positive"
            )
        low = envelope["minimum"] / (frequency * beam.outer_diameter)
        high = envelope["maximum"] / (frequency * beam.outer_diameter)
        cf = _intersection(low, high, 4.0, 8.0, 6.0)
        il = _intersection(low, high, 1.0, 3.5, 2.25)
        amplitude = (
            estimate_response_amplitude(
                cf, beam.outer_diameter, viv.stability_parameter
            )
            if cf is not None
            else 0.0
        )
        rows.append(
            {
                "mode": mode,
                "f_n_Hz": frequency,
                "Vr_lo": low,
                "Vr_hi": high,
                "governing_Vr": cf if cf is not None else il,
                "reynolds": viv.reynolds_number,
                "strouhal": viv.st,
                "Ks": viv.stability_parameter,
                "cf_lock_in": cf is not None,
                "il_lock_in": il is not None,
                "A_over_D": amplitude,
                "stress_range_MPa": None,
                "cycles_per_year": None,
                "damage_per_year": None,
            }
        )
    return rows


def _escalation_reasons(settings: dict, rows: list[dict], viv) -> list[str]:
    locked = [row for row in rows if row["cf_lock_in"] or row["il_lock_in"]]
    reasons = []
    if locked and settings["unit_modal_stress_MPa"] is None:
        reasons.append("T1: lock-in requires unit_modal_stress_MPa")
    if any(row["cf_lock_in"] and row["A_over_D"] > 0.8 for row in rows):
        reasons.append("T2: cross-flow A/D exceeds 0.8 screening confidence limit")
    if len(locked) >= 3:
        reasons.append("T3: three or more unique modes locked in (CF and IL combined)")
    if rows[-1]["f_n_Hz"] < viv.vortex_shedding_frequency:
        reasons.append("T4: vortex shedding frequency exceeds last checked mode")
    if not 300 <= viv.reynolds_number <= 1e7:
        reasons.append(
            "T5: Reynolds number outside workflow applicability window [300, 1e7]"
        )
    return reasons


def _damage_rate(rows: list[dict], settings: dict, curve) -> float:
    total = 0.0
    for row in rows:
        row.update(stress_range_MPa=0.0, cycles_per_year=0.0, damage_per_year=0.0)
        if not row["cf_lock_in"]:
            continue
        stress = 2 * settings["unit_modal_stress_MPa"] * row["A_over_D"]
        cycles = row["f_n_Hz"] * settings["exposure_fraction"] * SECONDS_PER_YEAR
        row.update(stress_range_MPa=stress, cycles_per_year=cycles)
        if stress == 0:
            continue
        histogram = pd.DataFrame([{"stress_range": stress, "cycles": cycles}])
        damage = miner_damage(histogram, curve).attrs["total_damage"]
        if not math.isfinite(damage) or damage < 0:
            raise ValueError(
                "viv_screening annual damage must be finite and nonnegative"
            )
        row["damage_per_year"] = damage
        total += damage
    return total


def _result_block(settings, rows, viv, locked, totals, reasons) -> dict:
    critical = next((row for row in rows if row["cf_lock_in"]), None)
    return {
        **settings,
        **totals,
        "lock_in_modes": locked,
        "critical_mode": critical["mode"] if critical else None,
        "critical_vr": critical["governing_Vr"] if critical else None,
        "stability_parameter_Ks": viv.stability_parameter,
        "mode_flags": [
            {key: row[key] for key in ("mode", "cf_lock_in", "il_lock_in")}
            for row in rows
        ],
        "escalation": {
            "required": bool(reasons),
            "reason": "; ".join(reasons) if reasons else None,
            "recommended_tool": DETAILED_TOOL,
        },
        "standard": STANDARD,
        "provenance": _provenance(),
    }


def _provenance() -> dict:
    return {
        "method": "Reduced-velocity lock-in screening (Vr envelope over current profile) "
        "+ narrow-band Miner damage-rate estimate",
        "code_editions": ["DNV-RP-C205:2019", "DNV-RP-F105:2017", "DNV-RP-C203"],
        "assumptions": [
            "single-structure uniform beam modes",
            "A/D from RP-C205 Fig 9-3 simplified curve",
            "user-supplied unit modal stress at A/D=1",
            "envelope (not sheared) current screening",
        ],
        "units": {"stress": "MPa", "frequency": "Hz", "speed": "m/s"},
        "validated_path": "screening",
        "limitations": [
            "cross-flow damage only; in-line lock-in flags only",
            "synthetic analytic golden; corpus validation deferred",
        ],
        "applicability_basis": "Escalation thresholds and Reynolds window from workflow plan",
    }


def _write_outputs(cfg: dict, settings: dict, rows: list[dict], totals: dict) -> None:
    directory = _output_dir(cfg, settings)
    stem = _input_stem(cfg)
    results = directory / f"{stem}_viv_screening.csv"
    summary = directory / f"{stem}_viv_screening_summary.csv"
    _write_csv(results, rows)
    _write_csv(summary, [totals])
    cfg["viv_screening"].update(
        results_csv=_display_path(results), summary_csv=_display_path(summary)
    )


def _output_dir(cfg: dict, settings: dict) -> Path:
    value = settings.get("output_dir", "results")
    if not isinstance(value, str) or not value.strip():
        raise ValueError("viv_screening output_dir must be a nonempty path string")
    output_dir = Path(value)
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
    return str(cfg.get("basename", "viv_screening"))


def _write_csv(path: Path, rows: list[dict]) -> None:
    if not rows:
        raise ValueError("viv_screening cannot write empty results")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        for row in rows:
            writer.writerow(
                {
                    key: format(value, ".12g") if isinstance(value, float) else value
                    for key, value in row.items()
                }
            )


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return resolved.relative_to(REPO_ROOT.resolve()).as_posix()
    except ValueError:
        return str(resolved)


def save_echo(cfg: dict) -> None:
    """Write the workflow echo without shared runtime paths and timestamps."""
    result = cfg["viv_screening"].copy()
    config_dir = _config_dir(cfg).resolve()
    for key in ("results_csv", "summary_csv"):
        path = Path(result[key])
        if not path.is_absolute():
            path = REPO_ROOT / path
        try:
            result[key] = path.resolve().relative_to(config_dir).as_posix()
        except ValueError:
            result[key] = path.name
    echo = {
        "basename": "viv_screening",
        "default": cfg["default"],
        "viv_screening": result,
    }
    path = _output_dir(cfg, cfg["viv_screening"]) / f"{_input_stem(cfg)}.yml"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(yaml.safe_dump(echo, sort_keys=False), encoding="utf-8")
