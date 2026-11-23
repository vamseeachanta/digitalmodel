# ABOUTME: Weather-window (workability) screening for marine operations.
# ABOUTME: Persistence of an Hs series vs an operational limit + window-fit verdict.
"""Durable workflow: weather-window (workability) screening.

A marine operation can run only while the sea state is below its operational
limit (Hs limit). This workflow screens a metocean Hs time series against that
limit using persistence analysis:

- **workability** — the fraction of time Hs is below the limit;
- **weather-window durations** — the continuous spells below the limit
  (mean / median / p10 / p90 / max), reused from the tested
  ``digitalmodel.orcaflex.weather_window.analyse_persistence``;
- **window fit** — whether any continuous window is long enough for the
  operation's required duration, and how many such viable windows occur.

The screen passes when at least one weather window is long enough for the
operation (and, if specified, the workability meets a required threshold).

References: DNV-OS-H101 / DNV-ST-N001 (Marine Operations); DNV-RP-H103;
Noble Denton 0027/ND; API RP 2MET.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any


def window_lengths_hours(
    hs_timeseries: list[float], hs_limit: float, time_step_hours: float
) -> list[float]:
    """Durations (hours) of each continuous spell with Hs <= hs_limit."""
    windows: list[float] = []
    run = 0
    for hs in hs_timeseries:
        if hs <= hs_limit:
            run += 1
        else:
            if run > 0:
                windows.append(run * time_step_hours)
            run = 0
    if run > 0:
        windows.append(run * time_step_hours)
    return windows


def assess_weather_window(
    hs_timeseries: list[float],
    hs_limit: float,
    required_duration_hours: float,
    time_step_hours: float = 3.0,
    required_workability_pct: float | None = None,
) -> dict[str, Any]:
    """Workability, persistence stats and the window-fit verdict."""
    import numpy as np

    from digitalmodel.orcaflex.weather_window import analyse_persistence

    if not hs_timeseries:
        raise ValueError("hs_timeseries must be a non-empty list")
    if hs_limit <= 0.0:
        raise ValueError("hs_limit must be positive")
    if required_duration_hours <= 0.0:
        raise ValueError("required_duration_hours must be positive")
    if time_step_hours <= 0.0:
        raise ValueError("time_step_hours must be positive")

    persistence = analyse_persistence(
        np.asarray(hs_timeseries, dtype=float), hs_limit, time_step_hours
    )
    windows = window_lengths_hours(hs_timeseries, hs_limit, time_step_hours)
    viable = [w for w in windows if w >= required_duration_hours]

    total_hours = len(hs_timeseries) * time_step_hours
    workability_pct = 100.0 * persistence.total_hours_below / total_hours
    has_window = persistence.max_window_hours >= required_duration_hours
    workability_ok = (
        required_workability_pct is None or workability_pct >= required_workability_pct
    )
    status = "pass" if has_window and workability_ok else "fail"

    return {
        "standard": "DNV-OS-H101 / Noble Denton 0027 (weather windows)",
        "hs_limit_m": hs_limit,
        "required_duration_hours": required_duration_hours,
        "time_step_hours": time_step_hours,
        "record_hours": total_hours,
        "workability_pct": workability_pct,
        "required_workability_pct": required_workability_pct,
        "longest_window_hours": persistence.max_window_hours,
        "mean_window_hours": persistence.mean_window_hours,
        "p10_window_hours": persistence.p10_window_hours,
        "p90_window_hours": persistence.p90_window_hours,
        "num_windows": persistence.num_windows,
        "num_viable_windows": len(viable),
        "operation_fits_window": has_window,
        "screening_status": status,
    }


def router(cfg: dict) -> dict:
    settings = cfg.get("weather_window") or {}
    operation = settings["operation"]
    metocean = settings["metocean"]

    result = assess_weather_window(
        hs_timeseries=[float(v) for v in metocean["hs_timeseries_m"]],
        hs_limit=float(operation["hs_limit_m"]),
        required_duration_hours=float(operation["required_duration_hours"]),
        time_step_hours=float(metocean.get("time_step_hours", 3.0)),
        required_workability_pct=(
            None
            if operation.get("required_workability_pct") is None
            else float(operation["required_workability_pct"])
        ),
    )

    payload = {
        **settings,
        "result": result,
        "screening_status": result["screening_status"],
    }
    payload["summary_json"] = str(
        _write_summary(cfg, settings.get("outputs", {}), payload)
    )
    cfg["weather_window"] = payload
    cfg["screening_status"] = result["screening_status"]
    return cfg


def _write_summary(cfg: dict, output_cfg: dict, payload: dict[str, Any]) -> Path:
    directory = Path(output_cfg.get("directory", "results/weather_window"))
    if not directory.is_absolute():
        directory = Path(cfg.get("_config_dir_path", Path.cwd())) / directory
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get(
        "summary_json", "weather_window_summary.json"
    )
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return summary_path
