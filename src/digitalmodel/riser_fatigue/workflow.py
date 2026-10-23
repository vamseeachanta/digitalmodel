"""Durable workflow for combined wave + VIV riser fatigue screening.

Offline-analytical and licence-free: invoked through the digitalmodel engine
basename ``riser_fatigue`` via

    uv run python -m digitalmodel examples/workflows/riser-fatigue/input.yml

For each riser segment the workflow accumulates Palmgren-Miner fatigue damage
from two independent contributions and sums them (DNV-RP-C203 / DNV-OS-F201):

  * **Wave (first/second-order) fatigue** — a long-term stress-range histogram
    (cycles per stress bin over a histogram period). Evaluated through the
    license-free :func:`assess_touchdown_fatigue` core (SCF + thickness
    correction + bilinear S-N + Miner).
  * **VIV (vortex-induced vibration) fatigue** — one or more narrow-band VIV
    cases, each a (stress range, frequency, annual exposure) triple. Cycles =
    frequency x exposure-seconds; damage via the same S-N / Miner engine.

The OrcaFlex / Shear7 / VIVANA front-ends that *produce* the stress histogram
and the VIV stress amplitudes are a deferred licensed follow-on (issue #810);
this workflow consumes their (real or synthetic) output and adds no S-N math of
its own. The cfg output shape mirrors ``mooring_fatigue`` so the deckhand
report template can render the governing segment and the pass/fail verdict.
"""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any, Optional

import pandas as pd

from digitalmodel.fatigue.damage import miner_damage
from digitalmodel.fatigue.sn_curves import get_sn_curve
from digitalmodel.riser_database.getters import get_riser_dff
from digitalmodel.riser_fatigue.validation import (
    evidence_text, finite_number, positive_damage,
)
from digitalmodel.riser_fatigue.touchdown import (
    RiserSection,
    TouchdownFatigueInput,
    assess_touchdown_fatigue,
)

REPO_ROOT = Path(__file__).resolve().parents[3]
SECONDS_PER_YEAR = 365.25 * 24.0 * 3600.0


def resolve_dff(settings: dict, repo_root: Optional[Path] = None) -> float:
    """Resolve the design fatigue factor (DFF) for a riser-fatigue run (#1246).

    Precedence (fail-closed):

    * an explicit ``dff`` in ``settings`` wins — validated exactly as before
      (finite positive float); a present null is invalid;
    * when ``dff`` is absent, the cited standard default resolves via
      :func:`digitalmodel.riser_database.getters.get_riser_dff` (DNV-OS-F201) —
      where the citation resolves, and raises ``CitationResolutionError``
      standalone. A pass/fail-driving input never silently defaults.
    """
    if "dff" in settings:
        return _positive_float(settings, "dff")
    return finite_number(get_riser_dff(repo_root=repo_root).value, "resolved dff")


def router(cfg: dict) -> dict:
    settings = cfg.get("riser_fatigue")
    if not isinstance(settings, dict):
        raise ValueError("riser_fatigue settings must be a mapping")
    design_life_years = _positive_float(settings, "design_life_years")
    dff = resolve_dff(settings)
    finite_number(design_life_years * dff, "required_life_years")
    default_curve, default_env = _curve_settings(settings)

    rows: list[dict[str, Any]] = []
    summaries: list[dict[str, Any]] = []
    for segment in _segments(settings):
        seg_rows, summary = _evaluate_segment(
            segment, design_life_years, dff, default_curve, default_env,
            settings["sn_curve"]["basis"],
        )
        rows.extend(seg_rows)
        summaries.append(summary)

    governing = max(summaries, key=lambda item: item["total_damage"])
    csv_path = _results_csv_path(cfg, settings)
    summary_path = _summary_csv_path(cfg, settings)
    _write_csv(csv_path, rows)
    _write_csv(summary_path, summaries)

    cfg["riser_fatigue"] = {
        "design_life_years": design_life_years,
        "dff": dff,
        "dff_basis": "explicit input" if "dff" in settings else "resolved cited default: DNV-OS-F201",
        "sn_curve": {"curve": default_curve, "environment": default_env,
                     "basis": settings["sn_curve"]["basis"]},
        "governing_segment": governing["segment_id"],
        "governing_damage": governing["total_damage"],
        "governing_wave_damage": governing["wave_damage"],
        "governing_viv_damage": governing["viv_damage"],
        "governing_fatigue_life_years": governing["fatigue_life_years"],
        "governing_dff_margin": governing["dff_margin"],
        # PASS criterion: every segment's fatigue life >= finite design life × DFF.
        # Governing segment has the greatest positive design-life damage.
        "screening_status": "pass" if governing["dff_margin"] >= 1.0 else "fail",
        "segments": summaries,
        "results_csv": _display_path(csv_path),
        "summary_csv": _display_path(summary_path),
    }
    return cfg


def _evaluate_segment(
    segment: dict[str, Any],
    design_life_years: float,
    dff: float,
    default_curve: str,
    default_env: str,
    default_basis: str = "Explicit curve and environment arguments",
) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    if not isinstance(segment, dict):
        raise ValueError("riser_fatigue segment must be a mapping")
    design_life_years = finite_number(design_life_years, "design_life_years")
    dff = finite_number(dff, "dff")
    finite_number(design_life_years * dff, "required_life_years")
    segment_id = str(segment.get("id", segment.get("name", "segment")))
    curve, environment = _curve_settings(segment, default_curve, default_env)

    wave_rows, wave_damage = _wave_damage(
        segment, segment_id, design_life_years, dff, curve, environment,
        segment.get("sn_curve", {}).get("basis", default_basis),
    )
    viv_rows, viv_damage = _viv_damage(
        segment, segment_id, design_life_years, curve, environment
    )
    if not wave_rows and not viv_rows:
        raise ValueError(
            f"riser_fatigue segment '{segment_id}' has neither a wave histogram"
            " nor a VIV case"
        )

    total_damage = positive_damage(wave_damage + viv_damage, "total_damage")
    fatigue_life = _fatigue_life_years(design_life_years, total_damage)
    required_life = design_life_years * dff
    dff_margin = _safe_divide(fatigue_life, required_life)

    summary = {
        "segment_id": segment_id,
        "material": str(segment.get("material", "API 5L X65")),
        "sn_curve": curve,
        "environment": environment,
        "sn_basis": segment.get("sn_curve", {}).get("basis", default_basis),
        "histogram_basis": segment.get("wave", {}).get("histogram_basis", {}).get("evidence", ""),
        "histogram_basis_years": segment.get("wave", {}).get("histogram_basis", {}).get("period_years", ""),
        "scf_basis": segment.get("wave", {}).get("scf_basis", ""),
        "wave_damage": wave_damage,
        "viv_damage": viv_damage,
        "total_damage": total_damage,
        "fatigue_life_years": fatigue_life,
        "required_life_years": required_life,
        "dff_margin": dff_margin,
        # PASS criterion: positive calculated fatigue life >= finite required life.
        "passes_dff": dff_margin >= 1.0,
        # Evaluated criterion: validated finite loading produces positive damage.
        # Exclusion criterion: no contribution data and a reason plus evidence.
        "wave_status": "evaluated" if wave_rows else "not_applicable",
        "viv_status": "evaluated" if viv_rows else "not_applicable",
        "wave_exclusion_reason": segment.get("wave_exclusion", {}).get("reason", ""),
        "wave_exclusion_evidence": segment.get("wave_exclusion", {}).get("evidence", ""),
        "viv_exclusion_reason": segment.get("viv_exclusion", {}).get("reason", ""),
        "viv_exclusion_evidence": segment.get("viv_exclusion", {}).get("evidence", ""),
    }
    return wave_rows + viv_rows, summary


def _wave_damage(
    segment: dict[str, Any],
    segment_id: str,
    design_life_years: float,
    dff: float,
    curve: str,
    environment: str,
    sn_basis: str,
) -> tuple[list[dict[str, Any]], float]:
    if _excluded(segment, "wave", "wave_exclusion"):
        return [], 0.0
    wave = segment.get("wave")
    if not isinstance(wave, dict):
        raise ValueError("riser_fatigue wave must be a mapping or have explicit exclusion evidence")

    stress_ranges = _float_list(wave, "stress_ranges_MPa")
    cycles = _float_list(wave, "cycles")
    if len(stress_ranges) != len(cycles):
        raise ValueError(
            f"riser_fatigue segment '{segment_id}' wave stress_ranges_MPa and"
            " cycles must have equal length"
        )
    section = RiserSection(
        outer_diameter_mm=_positive_float(segment, "outer_diameter_mm"),
        wall_thickness_mm=_positive_float(segment, "wall_thickness_mm"),
        material=str(segment.get("material", "API 5L X65")),
    )
    inp = TouchdownFatigueInput(
        section=section,
        stress_ranges_mpa=stress_ranges,
        cycles=cycles,
        sn_class=curve,
        environment=environment,
        scf=_positive_float(wave, "scf"),
        scf_basis=wave.get("scf_basis"),
        sn_basis=sn_basis,
        histogram_basis=wave.get("histogram_basis"),
        thickness_ref_mm=wave.get("thickness_ref_mm", 25.0),
        thickness_exponent=wave.get("thickness_exponent"),
        thickness_exponent_basis=wave.get("thickness_exponent_basis"),
        design_life_years=design_life_years,
        dff=dff,
        histogram_period_years=_positive_float(wave, "histogram_period_years"),
    )
    result = assess_touchdown_fatigue(inp)
    # Scale the per-period damage to the full design life so wave and VIV
    # damages are accumulated over a common (design-life) basis.
    period_years = inp.histogram_period_years
    life_scale = design_life_years / period_years
    rows: list[dict[str, Any]] = []
    for index, b in enumerate(result.bins):
        rows.append(
            {
                "segment_id": segment_id,
                "contribution": "wave",
                "bin_index": index,
                "frequency_hz": math.nan,
                "stress_range_MPa": b.stress_range_mpa,
                "stress_corrected_MPa": b.stress_corrected_mpa,
                "cycles": b.cycles * life_scale,
                "allowable_cycles": b.allowable_cycles,
                "damage": b.damage * life_scale,
            }
        )
    return rows, result.design_life_damage


def _viv_damage(
    segment: dict[str, Any],
    segment_id: str,
    design_life_years: float,
    curve: str,
    environment: str,
) -> tuple[list[dict[str, Any]], float]:
    if _excluded(segment, "viv_cases", "viv_exclusion"):
        return [], 0.0
    cases = segment.get("viv_cases")
    if not isinstance(cases, list) or not cases:
        raise ValueError("riser_fatigue viv_cases must be a non-empty list or have VIV exclusion evidence")

    bins: list[dict[str, float]] = []
    meta: list[dict[str, float]] = []
    for case in cases:
        if not isinstance(case, dict):
            raise ValueError("riser_fatigue VIV case must be a mapping")
        stress_range = _positive_value(case, ("stress_range_MPa", "stress_range"))
        frequency_hz = _positive_value(case, ("frequency_hz", "frequency"))
        exposure_fraction = _fraction(case)
        cycles = frequency_hz * exposure_fraction * SECONDS_PER_YEAR * design_life_years
        finite_number(cycles, "VIV cycles")
        bins.append({"stress_range": stress_range, "cycles": cycles})
        meta.append({"frequency_hz": frequency_hz, "stress_range": stress_range})

    sn_curve = get_sn_curve(curve, environment)
    damage_df = miner_damage(pd.DataFrame(bins), sn_curve)
    total = positive_damage(damage_df.attrs["total_damage"], "VIV total_damage")

    rows: list[dict[str, Any]] = []
    for index, row in damage_df.iterrows():
        rows.append(
            {
                "segment_id": segment_id,
                "contribution": "viv",
                "bin_index": int(index),
                "frequency_hz": meta[index]["frequency_hz"],
                "stress_range_MPa": float(row["stress_range"]),
                "stress_corrected_MPa": float(row["stress_range"]),
                "cycles": float(row["cycles"]),
                "allowable_cycles": float(row["allowable_cycles"]),
                "damage": float(row["damage"]),
            }
        )
    return rows, total


def _segments(settings: dict[str, Any]) -> list[dict[str, Any]]:
    segments = settings.get("segments")
    if not isinstance(segments, list) or not segments:
        raise ValueError("riser_fatigue segments must be a non-empty list")
    return segments


def _excluded(segment: dict, data_key: str, exclusion_key: str) -> bool:
    """Exclude only absent data with an explicit reason and evidence reference."""
    if exclusion_key not in segment:
        return False
    if data_key in segment:
        raise ValueError(f"{data_key} and {exclusion_key} cannot both be supplied")
    exclusion = segment[exclusion_key]
    if not isinstance(exclusion, dict) or set(exclusion) != {"reason", "evidence"}:
        raise ValueError(f"{exclusion_key} requires reason and evidence")
    evidence_text(exclusion["reason"], exclusion_key + " reason")
    evidence_text(exclusion["evidence"], exclusion_key + " evidence")
    return True


def _curve_settings(
    source: dict[str, Any],
    default_curve: Optional[str] = None,
    default_env: Optional[str] = None,
) -> tuple[str, str]:
    if "sn_curve" not in source and default_curve is not None and default_env is not None:
        get_sn_curve(default_curve, default_env)
        return default_curve, default_env
    curve = source.get("sn_curve")
    if not isinstance(curve, dict):
        raise ValueError("riser_fatigue sn_curve must be an explicit mapping")
    detail = evidence_text(curve.get("curve"), "sn_curve curve")
    environment = evidence_text(curve.get("environment"), "sn_curve environment")
    evidence_text(curve.get("basis"), "sn_curve basis")
    get_sn_curve(detail, environment)
    return detail, environment


def _float_list(source: dict[str, Any], name: str) -> list[float]:
    values = source.get(name)
    if not isinstance(values, list) or not values:
        raise ValueError(f"riser_fatigue {name} must be a non-empty list")
    result = [finite_number(value, name, positive=False) for value in values]
    if any(value < 0 for value in result):
        raise ValueError(f"riser_fatigue {name} must be non-negative")
    return result


def _fraction(case: dict[str, Any]) -> float:
    names = ("exposure_fraction", "exposure_hours_per_year")
    supplied = [name for name in names if name in case]
    if len(supplied) != 1:
        raise ValueError("riser_fatigue VIV exposure requires exactly one representation")
    name = supplied[0]
    value = finite_number(case[name], name)
    if name == "exposure_hours_per_year":
        value /= 365.25 * 24.0
    if not 0.0 < value <= 1.0:
        raise ValueError("riser_fatigue VIV exposure fraction must be in (0, 1]")
    return value


def _positive_value(source: dict[str, Any], aliases: tuple[str, ...]) -> float:
    supplied = [name for name in aliases if name in source]
    if len(supplied) != 1:
        raise ValueError(f"riser_fatigue requires exactly one of {aliases}")
    return finite_number(source[supplied[0]], supplied[0])


def _positive_float(source: dict[str, Any], name: str) -> float:
    return finite_number(source.get(name), name)


def _fatigue_life_years(design_life_years: float, damage: float) -> float:
    life = finite_number(design_life_years, "design_life_years")
    damage = positive_damage(damage, "total_damage")
    return finite_number(life / damage, "fatigue_life_years")


def _safe_divide(numerator: float, denominator: float) -> float:
    numerator = finite_number(numerator, "fatigue_life_years")
    denominator = finite_number(denominator, "required_life_years")
    return finite_number(numerator / denominator, "dff_margin")


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / f"{_input_stem(cfg)}_riser_fatigue.csv"


def _summary_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / f"{_input_stem(cfg)}_riser_fatigue_summary.csv"


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
    return str(cfg.get("basename", "riser_fatigue"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("riser_fatigue cannot write empty results")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="\n") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
