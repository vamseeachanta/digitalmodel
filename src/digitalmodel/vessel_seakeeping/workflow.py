"""Durable workflow: vessel seakeeping operability from motion RAOs + a sea state.

For each degree of freedom (heave, roll, pitch, ...) the motion response
spectrum is formed from the DOF's motion RAO and a parametric wave spectrum
(JONSWAP / Pierson-Moskowitz / Bretschneider) via the standard linear transfer
``S_response(w) = |RAO(w)|^2 * S_wave(w)``; the significant motion amplitude
``s_1/3 = 2*sqrt(m0)`` is compared against an operability criterion across the
sea-state scatter, weighted by occurrence. Each DOF gets an operability
percentage; the vessel passes when every DOF meets its required operability.

This wires the tested ``digitalmodel.hydrodynamics.seakeeping`` engine
(``operability_analysis``, ``compute_response_spectrum``, ``spectral_moments``,
``significant_amplitude``) into a durable workflow rather than reimplementing
it. Reference: PNA Vol III (Motions in Waves); DNV-RP-C205 (wave spectra).
"""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any

REPO_ROOT = Path(__file__).resolve().parents[3]

_SPECTRA = {"jonswap", "pm", "bretschneider"}


def router(cfg: dict) -> dict:
    settings = cfg.get("vessel_seakeeping") or {}
    vessel = str(settings.get("vessel", "vessel"))
    spectrum_type = _spectrum_type(settings)
    gamma = float(settings.get("gamma", 3.3))
    required_operability = _positive(
        settings,
        "required_operability_pct",
        "vessel_seakeeping.required_operability_pct",
    )
    rao_freqs = _rao_frequencies(settings)
    scatter = _scatter(settings)

    from digitalmodel.hydrodynamics.seakeeping import operability_analysis

    rows: list[dict[str, Any]] = []
    summaries: list[dict[str, Any]] = []
    for dof in _dofs(settings, len(rao_freqs)):
        name = dof["name"]
        criterion = dof["criterion"]
        result = operability_analysis(
            rao_freqs,
            dof["rao_amplitude"],
            scatter,
            {name: criterion},
            spectrum_type=spectrum_type,
            gamma=gamma,
        )
        operability_pct = result["operability_pct"]
        for sea_state in result["sea_state_results"]:
            rows.append(
                {
                    "dof": name,
                    "unit": dof["unit"],
                    "hs": sea_state["hs"],
                    "tp": sea_state["tp"],
                    "probability": sea_state["probability"],
                    "significant_amplitude": sea_state["significant_amplitude"],
                    "criterion": criterion,
                    "operable": sea_state["operable"],
                }
            )
        summaries.append(
            {
                "dof": name,
                "unit": dof["unit"],
                "criterion": criterion,
                "operability_pct": operability_pct,
                "required_operability_pct": required_operability,
                "passes": operability_pct >= required_operability,
            }
        )

    governing = min(summaries, key=lambda item: item["operability_pct"])
    csv_path = _results_csv_path(cfg, settings)
    summary_path = _summary_csv_path(cfg, settings)
    _write_csv(csv_path, rows)
    _write_csv(summary_path, summaries)

    cfg["vessel_seakeeping"] = {
        "vessel": vessel,
        "spectrum_type": spectrum_type,
        "required_operability_pct": required_operability,
        "governing_dof": governing["dof"],
        "governing_operability_pct": governing["operability_pct"],
        "response_transfer": "S_response(w) = |RAO(w)|^2 * S_wave(w)",
        # Top-level pass/fail: the vessel is operable only if every DOF meets
        # its required operability. Drives the deckhand report mitigation section.
        "screening_status": "pass" if all(s["passes"] for s in summaries) else "fail",
        "dofs": summaries,
        "results_csv": _display_path(csv_path),
        "summary_csv": _display_path(summary_path),
    }
    cfg["screening_status"] = cfg["vessel_seakeeping"]["screening_status"]
    return cfg


def _spectrum_type(settings: dict[str, Any]) -> str:
    kind = str(settings.get("spectrum_type", "jonswap")).strip().lower()
    if kind not in _SPECTRA:
        raise ValueError(
            f"vessel_seakeeping spectrum_type must be one of {sorted(_SPECTRA)}; got {kind!r}"
        )
    return kind


def _rao_frequencies(settings: dict[str, Any]) -> Any:
    import numpy as np

    freqs = settings.get("rao_frequency_rad_s")
    if not isinstance(freqs, list) or len(freqs) < 2:
        raise ValueError(
            "vessel_seakeeping rao_frequency_rad_s must be a list of >=2 values"
        )
    values = [float(v) for v in freqs]
    if any(b <= a for a, b in zip(values, values[1:])):
        raise ValueError(
            "vessel_seakeeping rao_frequency_rad_s must be strictly increasing"
        )
    if values[0] <= 0.0:
        raise ValueError("vessel_seakeeping rao_frequency_rad_s must be positive")
    return np.asarray(values, dtype=float)


def _scatter(settings: dict[str, Any]) -> list[dict[str, Any]]:
    sea_states = settings.get("sea_states")
    if not isinstance(sea_states, list) or not sea_states:
        raise ValueError("vessel_seakeeping sea_states must be a non-empty list")
    scatter = []
    for entry in sea_states:
        hs = _positive(entry, "hs", "sea_state.hs")
        tp = _positive(entry, "tp", "sea_state.tp")
        prob = float(entry.get("probability", 1.0))
        if prob < 0.0:
            raise ValueError("sea_state.probability must be non-negative")
        scatter.append({"hs": hs, "tp": tp, "probability": prob})
    if sum(e["probability"] for e in scatter) <= 0.0:
        raise ValueError(
            "vessel_seakeeping sea_state probabilities must sum to a positive value"
        )
    return scatter


def _dofs(settings: dict[str, Any], n_freqs: int) -> list[dict[str, Any]]:
    import numpy as np

    dofs = settings.get("dofs")
    if not isinstance(dofs, list) or not dofs:
        raise ValueError("vessel_seakeeping dofs must be a non-empty list")
    parsed = []
    for dof in dofs:
        name = str(dof.get("name", "")).strip()
        if not name:
            raise ValueError("each vessel_seakeeping dof needs a name")
        amplitude = dof.get("rao_amplitude")
        if not isinstance(amplitude, list) or len(amplitude) != n_freqs:
            raise ValueError(
                f"dof {name!r} rao_amplitude must be a list matching rao_frequency_rad_s "
                f"(length {n_freqs})"
            )
        if any(float(v) < 0.0 for v in amplitude):
            raise ValueError(f"dof {name!r} rao_amplitude must be non-negative")
        criterion = _positive(dof, "criterion", f"dof {name!r} criterion")
        parsed.append(
            {
                "name": name,
                "rao_amplitude": np.asarray([float(v) for v in amplitude], dtype=float),
                "criterion": criterion,
                "unit": str(dof.get("unit", "")),
            }
        )
    return parsed


def _positive(source: dict[str, Any], name: str, label: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"{label} is required")
    value = float(value)
    if value <= 0.0 or math.isnan(value):
        raise ValueError(f"{label} must be positive")
    return value


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return _output_dir(cfg, settings) / f"{_input_stem(cfg)}_vessel_seakeeping.csv"


def _summary_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    return (
        _output_dir(cfg, settings) / f"{_input_stem(cfg)}_vessel_seakeeping_summary.csv"
    )


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
    return str(cfg.get("basename", "vessel_seakeeping"))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError("vessel_seakeeping cannot write empty results")
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
