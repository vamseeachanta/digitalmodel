# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Write an offline preload-window sensitivity and Plotly JSON figure.

Usage: PYTHONPATH=src python examples/domains/geotechnical/jackup_preload_window.py OUTPUT
Optional: --observations SERIES.json --sha256 EXPECTED --cadence-h HOURS
Default weather is an assumed scenario, never an ERA5 time series.
"""

from __future__ import annotations

import argparse
import calendar
import hashlib
import json
import math
from dataclasses import asdict
from datetime import datetime, timedelta, timezone
from pathlib import Path

from digitalmodel.marine_ops.preload_sequence import PreloadCriteria
from digitalmodel.marine_ops.preload_weather_window import (
    HullGeometry,
    LegGeometry,
    MonthlyWindow,
    SeaStateLimit,
    WaveSample,
    monthly_windows,
    swell_loads,
)

ASSUMED = "assumed, based on previous experience"
SOURCES = [
    {
        "id": "regional-wave-comparison",
        "source": "https://www.mdpi.com/2673-964x/6/1/8",
        "title": "Assessment of Wave Data in West Africa for the Estimation of Wave Climate (2026)",
        "use": "Regional period and season context; not persistence calibration.",
        "access": "Published indexed text inspected; full-page request returned HTTP 429.",
    },
    {
        "id": "era5-regional-summary",
        "source": "https://www.mdpi.com/2073-4433/17/10/918",
        "title": "Seasonal Variability, Long-Term Trends, and Extreme-Value Statistics of ERA5 Wave Climate Along the West African Coast, 1940–2024 (2026)",
        "use": "Offshore regional context only; no hourly records ingested.",
        "catalog_source_id": "copernicus-cmems",
    },
    {
        "id": "regional-energetic-waves",
        "source": "https://doi.org/10.4236/jep.2019.109071",
        "title": "Physical Forcing Induced Coastal Vulnerability along the Gulf of Guinea (2019), section 1",
        "use": "3–4 m energetic wave context; Hs=4 m remains an assumed stress case, not a return level.",
    },
    {
        "id": "regional-spring-tides",
        "source": "https://digitallibrary.un.org/record/777751",
        "title": "Methodology for assessment and control of coastal erosion in West and Central Africa (1989), section 3.1.5 and Table 3.1",
        "use": "Selected historical spring ranges, 1.6 m and 2.0 m; not a present offshore tide prediction.",
    },
]

# All settings below are synthetic controls, not recovered unit particulars.
INPUT_RANGES = {
    "hold_duration_h": [1, 6],
    "other_operation_allowance_h": [0, 6],
    "in_water_limit_hs_m": [0.5, 1.5],
    "elevated_limit_hs_m": [1.0, 2.0],
    "hull_length_m": [60, 85],
    "hull_breadth_m": [55, 75],
    "hull_depth_m": [6, 10],
    "minimum_draft_at_mean_tide_m": [1, 3],
    "elevated_still_water_gap_m": [0.5, 2],
    "mean_water_depth_m": [20, 60],
    "equivalent_leg_diameter_m": [1.5, 3.5],
    "drag_coefficient": [0.7, 1.5],
    "inertia_coefficient": [1.5, 2.5],
    "regular_height_to_hs_factor": [1.5, 2.0],
    "peak_period_s": [5, 25],
    "density_kg_m3": [1020, 1030],
    "gravity_m_s2": [9.80, 9.83],
    "tide_range_m": [1.6, 2.0],
    "ballast_t": [0, 15000],
    "preload_mode": ["single_leg", "all_legs"],
    "synthetic_weather_cycle_h": [72, 168],
    "synthetic_pulse_exponent": [2, 6],
    "synthetic_background_hs_m": [0.8, 1.4],
    "synthetic_pulse_height_m": [0, 2.4],
}


def assumed_series() -> tuple[WaveSample, ...]:
    """Construct deterministic hourly blocks; no observed persistence is implied.

    A five-day pulse repeats; background height peaks in August. The shape,
    phasing and duration are assumptions. They only exercise the calculation.
    """
    start = datetime(2024, 1, 1, tzinfo=timezone.utc)
    result = []
    for hour in range(366 * 24):
        timestamp = start + timedelta(hours=hour)
        seasonal = (1 + math.cos(2 * math.pi * (timestamp.month - 8) / 12)) / 2
        pulse = max(0, math.cos(2 * math.pi * hour / 120)) ** 4
        height = 0.8 + 0.6 * seasonal + 2.4 * pulse
        period = 12 + 10 * pulse + 3 * seasonal
        result.append(WaveSample(timestamp, height, period))
    return tuple(result)


def read_observations(
    path: Path, expected_sha256: str
) -> tuple[tuple[WaveSample, ...], dict]:
    """Verify bytes before interpreting a supplied series; qualification stays open."""
    raw = path.read_bytes()
    digest = hashlib.sha256(raw).hexdigest()
    if digest != expected_sha256:
        raise ValueError("observation digest mismatch")
    payload = json.loads(raw)
    for key in ("source", "version", "dataset_id"):
        if not isinstance(payload.get(key), str) or not payload[key].strip():
            raise ValueError(f"observations require {key}")
    data = tuple(
        WaveSample(datetime.fromisoformat(row["timestamp"]), row["hs_m"], row["tp_s"])
        for row in payload["samples"]
    )
    return data, {
        "kind": "supplied_unqualified_series",
        "sha256": digest,
        "source": payload["source"],
        "version": payload["version"],
        "dataset_id": payload["dataset_id"],
        "qualification": "Source identity, units, coverage and site applicability require independent review.",
    }


def _monthly_output(month: MonthlyWindow) -> dict:
    data = asdict(month)
    for key in ("probability", "lower_probability", "upper_probability"):
        value = data.pop(key)
        data[key + "_pct"] = None if value is None else round(100 * value, 1)
    return data


def _load_cases(limits: dict[str, SeaStateLimit]) -> list[dict]:
    loads = []
    for state, limit in limits.items():
        bottom = -2 if state == "hull_in_water" else 1
        for wave_case, hs, period in (
            ("state_limit", limit.max_hs_m, 12),
            ("assumed_stress", 4, 25),
        ):
            for tide in (-1, 0, 1):
                result = asdict(
                    swell_loads(
                        wave_height_m=1.8 * hs,
                        period_s=period,
                        mean_depth_m=40,
                        tide_level_m=tide,
                        hull=HullGeometry(72, 66, 8, bottom),
                        leg=LegGeometry(2.5, 1, 2),
                    )
                )
                # Counts and full precision remain in the API; issued figures use one decimal.
                reported = {
                    k: round(v, 1) if isinstance(v, float) else v
                    for k, v in result.items()
                }
                # Wavenumber is retained as wavelength to avoid rounding it to zero.
                reported["wavelength_m"] = round(
                    2 * math.pi / result["wavenumber_m_inv"], 1
                )
                reported.pop("wavenumber_m_inv")
                loads.append(
                    {
                        "state": state,
                        "case": wave_case,
                        "hs_m": hs,
                        "regular_wave_height_m": round(1.8 * hs, 1),
                        "period_s": period,
                        "tide_level_m": tide,
                        "hull_bottom_elevation_m": bottom,
                        "result": reported,
                    }
                )
    return loads


def run(
    output: Path,
    *,
    observations: Path | None = None,
    expected_sha256: str | None = None,
    cadence_h: float = 1,
) -> dict:
    """Write results, chart and a retained digest-bound copy of the input series."""
    source_bytes = None
    if observations is None:
        data = assumed_series()
        basis = {
            "kind": "assumed_scenario",
            "dataset_id": "generic-preload-weather-scenario",
            "version": "1",
            "description": ASSUMED,
            "qualification": "Deterministic constructed weather; no ERA5 persistence estimate.",
        }
        if cadence_h != 1:
            raise ValueError("assumed series has a one-hour cadence")
    else:
        if expected_sha256 is None:
            raise ValueError("observations require an expected SHA-256 digest")
        source_bytes = observations.read_bytes()
        data, basis = read_observations(observations, expected_sha256)
        if hashlib.sha256(source_bytes).hexdigest() != expected_sha256:
            raise ValueError("retained observation digest mismatch")
    limits = {
        "hull_in_water": SeaStateLimit(1.0, 5, 25),
        "hull_elevated": SeaStateLimit(1.5, 5, 25),
    }
    holds = (PreloadCriteria().minimum_hold_h, 3, 6)
    allowance_h = 2
    monthly, traces = {}, []
    for state, limit in limits.items():
        for hold in holds:
            label = f"{state}: hold {hold:g} h + allowance {allowance_h} h"
            rows = monthly_windows(
                data, duration_h=hold + allowance_h, cadence_h=cadence_h, limit=limit
            )
            monthly[label] = [_monthly_output(row) for row in rows]
            traces.append(
                {
                    "type": "scatter",
                    "mode": "lines+markers",
                    "name": label,
                    "x": list(calendar.month_abbr)[1:],
                    "y": [row["probability_pct"] for row in monthly[label]],
                    "connectgaps": False,
                    "hovertemplate": "%{x}: %{y:.1f}%<extra>%{fullData.name}</extra>",
                }
            )
    title = (
        "ASSUMED weather scenario"
        if observations is None
        else "SUPPLIED UNQUALIFIED weather series"
    )
    figure = {
        "data": traces,
        "layout": {
            "title": {"text": f"{title}: preload windows"},
            "xaxis": {"title": "UTC window start month"},
            "yaxis": {
                "title": "Workable starts with complete data (%)",
                "range": [0, 100],
            },
            "annotations": [
                {
                    "text": "Random sampled start; conditional on complete data. No installation acceptance.",
                    "xref": "paper",
                    "yref": "paper",
                    "x": 0,
                    "y": -0.25,
                    "showarrow": False,
                }
            ],
        },
    }
    retained = {
        "basis": basis,
        "cadence_h": cadence_h,
        "units": {"hs_m": "m", "tp_s": "s", "timestamp": "UTC"},
        "samples": [
            {
                "timestamp": row.timestamp.astimezone(timezone.utc).isoformat(),
                "hs_m": row.hs_m,
                "tp_s": row.tp_s,
            }
            for row in data
        ],
    }
    input_bytes = json.dumps(retained, sort_keys=True, allow_nan=False).encode()
    result = {
        "criteria_revision": "preload-window-1",
        "basis": basis,
        "qualified_for_installation": False,
        "source_register": SOURCES,
        "input_ranges": {"basis": ASSUMED, "ranges": INPUT_RANGES},
        "criteria": {
            "hold_durations_h": holds,
            "other_operation_allowance_h": allowance_h,
            "limits": {key: asdict(value) for key, value in limits.items()},
        },
        "load_inputs": {
            "mean_depth_m": 40,
            "hull_length_m": 72,
            "hull_breadth_m": 66,
            "hull_depth_m": 8,
            "leg_diameter_m": 2.5,
            "drag_coefficient": 1,
            "inertia_coefficient": 2,
            "density_kg_m3": 1025,
            "gravity_m_s2": 9.81,
            "regular_height_to_hs_factor": 1.8,
            "assumed_tide_range_m": 2.0,
        },
        "retained_input": {
            "file": "preload_window_inputs.json",
            "sha256": hashlib.sha256(input_bytes).hexdigest(),
        },
        "monthly": monthly,
        "loads": _load_cases(limits),
        "open": [
            "Site-specific hourly series and site applicability require qualification.",
            "Limit, duration, tide datum, geometry and coefficients require qualification.",
            "Ballast and preload mode remain input ranges; reactions are not calculated.",
            "Nonlinear waves, hull contact, diffraction and dynamic response remain unresolved.",
        ],
    }
    output.mkdir(parents=True, exist_ok=True)
    if source_bytes is not None:
        (output / "preload_window_source.json").write_bytes(source_bytes)
    (output / "preload_window_inputs.json").write_bytes(input_bytes)
    for name, payload in (
        ("preload_window_results.json", result),
        ("monthly_preload_windows.plotly.json", figure),
    ):
        (output / name).write_text(
            json.dumps(payload, indent=2, allow_nan=False) + "\n"
        )
    return result


def main() -> None:
    """Parse the offline example arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("--observations", type=Path)
    parser.add_argument("--sha256")
    parser.add_argument("--cadence-h", type=float, default=1)
    args = parser.parse_args()
    run(
        args.output,
        observations=args.observations,
        expected_sha256=args.sha256,
        cadence_h=args.cadence_h,
    )


if __name__ == "__main__":
    main()
