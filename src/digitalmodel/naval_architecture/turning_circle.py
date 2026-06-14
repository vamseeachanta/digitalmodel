# ABOUTME: Preliminary Nomoto turning-circle sweeps, artifacts, and charts.
# ABOUTME: Keeps the public API and packaged YAML used by the estimator tests.
"""Preliminary first-order Nomoto turning-circle estimator.

This module is a bounded estimating tool. It integrates
``r_dot = (K * delta - r) / T`` for constant speed and rudder angle, then
derives advance, transfer, and tactical-diameter estimates from heading
crossings. It is not a full MMG model or class/IMO/ABS compliance proof.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from importlib import resources
from typing import Any, Iterable

import yaml

from digitalmodel.naval_architecture.turning_circle_artifacts import (
    METRICS_HEADERS as METRICS_HEADERS,
    REQUIRED_CHARTS,
    SUPPORTED_CHART_FORMATS,
    SUPPORTED_TABLE_FORMATS,
    TIME_HISTORY_HEADERS as TIME_HISTORY_HEADERS,
    provenance_metadata,
    units_metadata,
    write_turning_circle_results as write_turning_circle_results,
)

KNOT_TO_M_PER_S = 0.514444
M_PER_S_TO_KNOT = 1.0 / KNOT_TO_M_PER_S


@dataclass(frozen=True)
class TurningCircleInput:
    """Validated packaged turning-circle sweep input."""

    case_id: str
    case_description: str
    vessel: dict[str, Any]
    speeds_m_s: list[float]
    speeds_kn: list[float]
    rudder_angles_deg: list[float]
    K_per_s: float
    T_s: float
    duration_s: float
    dt_s: float
    output_directory: str
    table_formats: tuple[str, ...]
    provenance_filename: str
    artifact_manifest_filename: str
    chart_enabled: bool
    chart_formats: tuple[str, ...]
    required_charts: tuple[str, ...]
    raw: dict[str, Any]


def _finite(name: str, value: float) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite, got {value}")
    return value


def _positive(name: str, value: float) -> float:
    value = _finite(name, value)
    if value <= 0:
        raise ValueError(f"{name} must be > 0, got {value}")
    return value


def _nonnegative(name: str, value: float) -> float:
    value = _finite(name, value)
    if value < 0:
        raise ValueError(f"{name} must be >= 0, got {value}")
    return value


def simulate_nomoto_turning_circle(
    *,
    speed_m_s: float,
    rudder_angle_deg: float,
    K_per_s: float,
    T_s: float,
    duration_s: float,
    dt_s: float,
    case_id: str = "direct_nomoto_turning_circle",
) -> dict[str, Any]:
    """Simulate a constant-rudder first-order Nomoto turning circle."""

    speed_m_s = _positive("speed_m_s", speed_m_s)
    rudder_angle_deg = _finite("rudder_angle_deg", rudder_angle_deg)
    K_per_s = _nonnegative("K_per_s", K_per_s)
    T_s = _positive("T_s", T_s)
    duration_s = _positive("duration_s", duration_s)
    dt_s = _positive("dt_s", dt_s)
    if duration_s <= dt_s:
        raise ValueError("duration_s must be greater than dt_s")

    rows = _integrate_nomoto(
        case_id, speed_m_s, rudder_angle_deg, K_per_s, T_s, duration_s, dt_s
    )
    metrics, warnings = _turning_metrics(rows, rudder_angle_deg)
    return {
        "metadata": {
            "case_id": case_id,
            "speed_m_s": speed_m_s,
            "speed_kn": speed_m_s * M_PER_S_TO_KNOT,
            "rudder_angle_deg": rudder_angle_deg,
            "nomoto": {"K_per_s": K_per_s, "T_s": T_s},
            "duration_s": duration_s,
            "dt_s": dt_s,
            "equation": "r_dot = (K * delta - r) / T",
        },
        "metrics": metrics,
        "time_history": rows,
        "warnings": warnings,
    }


def _integrate_nomoto(
    case_id: str,
    speed_m_s: float,
    rudder_angle_deg: float,
    K_per_s: float,
    T_s: float,
    duration_s: float,
    dt_s: float,
) -> list[dict[str, float | str]]:
    time_s = 0.0
    x_m = 0.0
    y_m = 0.0
    heading_rad = 0.0
    yaw_rate_rad_s = 0.0
    delta_rad = math.radians(rudder_angle_deg)
    steady_yaw_rate = K_per_s * delta_rad
    speed_kn = speed_m_s * M_PER_S_TO_KNOT
    rows = [
        _history_row(
            case_id,
            speed_m_s,
            speed_kn,
            rudder_angle_deg,
            time_s,
            x_m,
            y_m,
            heading_rad,
            yaw_rate_rad_s,
        )
    ]
    while time_s < duration_s - 1e-12:
        step_s = min(dt_s, duration_s - time_s)
        decay = math.exp(-step_s / T_s)
        next_yaw_rate = steady_yaw_rate + (yaw_rate_rad_s - steady_yaw_rate) * decay
        delta_heading = steady_yaw_rate * step_s + (
            yaw_rate_rad_s - steady_yaw_rate
        ) * T_s * (1.0 - decay)
        midpoint_heading = heading_rad + 0.5 * delta_heading
        x_m += speed_m_s * math.cos(midpoint_heading) * step_s
        y_m += speed_m_s * math.sin(midpoint_heading) * step_s
        heading_rad += delta_heading
        yaw_rate_rad_s = next_yaw_rate
        time_s = round(time_s + step_s, 10)
        rows.append(
            _history_row(
                case_id,
                speed_m_s,
                speed_kn,
                rudder_angle_deg,
                time_s,
                x_m,
                y_m,
                heading_rad,
                yaw_rate_rad_s,
            )
        )
    return rows


def _history_row(
    case_id: str,
    speed_m_s: float,
    speed_kn: float,
    rudder_angle_deg: float,
    time_s: float,
    x_m: float,
    y_m: float,
    heading_rad: float,
    yaw_rate_rad_s: float,
) -> dict[str, float | str]:
    return {
        "case_id": case_id,
        "speed_m_s": speed_m_s,
        "speed_kn": speed_kn,
        "rudder_angle_deg": rudder_angle_deg,
        "time_s": time_s,
        "x_m": x_m,
        "y_m": y_m,
        "heading_deg": math.degrees(heading_rad),
        "yaw_rate_rad_s": yaw_rate_rad_s,
        "yaw_rate_deg_s": math.degrees(yaw_rate_rad_s),
    }


def _turning_metrics(
    rows: list[dict[str, Any]], rudder_angle_deg: float
) -> tuple[dict[str, Any], list[str]]:
    sign = 1.0 if rudder_angle_deg >= 0 else -1.0
    advance_transfer = _heading_crossing(rows, sign * 90.0)
    tactical = _heading_crossing(rows, sign * 180.0)
    warnings: list[str] = []
    if advance_transfer is None:
        warnings.append(
            "Heading target 90 deg was not reached; metrics are incomplete."
        )
    if tactical is None:
        warnings.append(
            "Heading target 180 deg was not reached; tactical diameter is incomplete."
        )
    return {
        "case_id": rows[0]["case_id"],
        "speed_m_s": rows[0]["speed_m_s"],
        "speed_kn": rows[0]["speed_kn"],
        "rudder_angle_deg": rudder_angle_deg,
        "advance_transfer_time_s": (
            None if advance_transfer is None else advance_transfer["time_s"]
        ),
        "advance_m": None if advance_transfer is None else advance_transfer["x_m"],
        "transfer_m": None if advance_transfer is None else advance_transfer["y_m"],
        "tactical_diameter_time_s": None if tactical is None else tactical["time_s"],
        "tactical_diameter_m": None if tactical is None else tactical["y_m"],
        "metric_status": "ok" if not warnings else "warning",
    }, warnings


def _heading_crossing(
    rows: list[dict[str, Any]], target_heading_deg: float
) -> dict[str, float] | None:
    for previous, current in zip(rows, rows[1:]):
        prev_heading = float(previous["heading_deg"])
        curr_heading = float(current["heading_deg"])
        if target_heading_deg >= 0:
            crossed = prev_heading < target_heading_deg <= curr_heading
        else:
            crossed = prev_heading > target_heading_deg >= curr_heading
        if not crossed or curr_heading == prev_heading:
            continue
        fraction = (target_heading_deg - prev_heading) / (curr_heading - prev_heading)
        return {
            key: float(previous[key])
            + fraction * (float(current[key]) - float(previous[key]))
            for key in ("time_s", "x_m", "y_m", "yaw_rate_rad_s", "yaw_rate_deg_s")
        } | {"heading_deg": target_heading_deg}
    return None


def load_packaged_turning_circle_yaml() -> TurningCircleInput:
    """Load the packaged typical-ship turning-circle YAML."""

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "turning_circle_typical_ship.yml"
    )
    with resource.open("r", encoding="utf-8") as stream:
        return validate_turning_circle_input(yaml.safe_load(stream))


def validate_turning_circle_input(payload: dict[str, Any]) -> TurningCircleInput:
    """Validate the documented turning-circle input contract."""

    if not isinstance(payload, dict):
        raise ValueError("turning circle input must be a YAML mapping")
    required = {"case", "nomoto", "sweep", "outputs"}
    missing = sorted(required - set(payload))
    if missing:
        raise ValueError(f"missing required top-level sections: {missing}")

    speeds = payload["sweep"]["speeds"]
    speed_values = [_positive("speed", value) for value in speeds["values"]]
    if not speed_values:
        raise ValueError("sweep.speeds.values must contain at least one value")
    if speeds.get("units") == "kn":
        speeds_kn = speed_values
        speeds_m_s = [value * KNOT_TO_M_PER_S for value in speed_values]
    elif speeds.get("units") == "m/s":
        speeds_m_s = speed_values
        speeds_kn = [value * M_PER_S_TO_KNOT for value in speed_values]
    else:
        raise ValueError("sweep.speeds.units must be 'kn' or 'm/s'")

    outputs = payload["outputs"]
    table_formats = tuple(outputs.get("tables", ("csv", "json")))
    chart_cfg = outputs.get("charts", {})
    chart_formats = tuple(chart_cfg.get("formats", ("png",)))
    required_charts = tuple(chart_cfg.get("required", ()))
    _validate_formats(table_formats, SUPPORTED_TABLE_FORMATS, "table")
    _validate_formats(chart_formats, SUPPORTED_CHART_FORMATS, "chart")
    _validate_formats(required_charts, set(REQUIRED_CHARTS), "required chart")
    if required_charts and not bool(chart_cfg.get("enabled", False)):
        raise ValueError(
            "outputs.charts.required is invalid when charts.enabled is false"
        )

    sidecar_cfg = outputs.get("sidecars", {})
    duration_s = _positive("simulation.duration_s", payload["simulation"]["duration_s"])
    dt_s = _positive("simulation.dt_s", payload["simulation"]["dt_s"])
    if duration_s <= dt_s:
        raise ValueError("simulation.duration_s must be greater than simulation.dt_s")
    rudder_angles_deg = [
        _finite("rudder_angle_deg", value)
        for value in payload["sweep"]["rudder_angles_deg"]
    ]
    if not rudder_angles_deg:
        raise ValueError("sweep.rudder_angles_deg must contain at least one value")

    return TurningCircleInput(
        case_id=str(payload["case"]["id"]),
        case_description=str(payload["case"].get("description", "")),
        vessel=dict(payload.get("vessel", {})),
        speeds_m_s=speeds_m_s,
        speeds_kn=speeds_kn,
        rudder_angles_deg=rudder_angles_deg,
        K_per_s=_nonnegative("nomoto.K_per_s", payload["nomoto"]["K_per_s"]),
        T_s=_positive("nomoto.T_s", payload["nomoto"]["T_s"]),
        duration_s=duration_s,
        dt_s=dt_s,
        output_directory=str(outputs.get("directory", "results/turning_circle")),
        table_formats=table_formats,
        provenance_filename=str(
            sidecar_cfg.get("provenance", "turning_circle_provenance.json")
        ),
        artifact_manifest_filename=str(
            sidecar_cfg.get("artifact_manifest", "artifact_manifest.json")
        ),
        chart_enabled=bool(chart_cfg.get("enabled", False)),
        chart_formats=chart_formats,
        required_charts=required_charts,
        raw=payload,
    )


def _validate_formats(values: Iterable[str], supported: set[str], label: str) -> None:
    unsupported = sorted(set(values) - supported)
    if unsupported:
        raise ValueError(f"unsupported {label} formats: {unsupported}")


def run_turning_circle_sweep(config: TurningCircleInput) -> dict[str, Any]:
    """Run every speed/rudder-angle combination and return rows + metadata."""

    cases: list[dict[str, Any]] = []
    metrics: list[dict[str, Any]] = []
    time_history: list[dict[str, Any]] = []
    for speed_m_s, speed_kn in zip(config.speeds_m_s, config.speeds_kn):
        for rudder_angle_deg in config.rudder_angles_deg:
            case_id = f"{config.case_id}_{speed_kn:g}kn_{rudder_angle_deg:g}deg"
            result = simulate_nomoto_turning_circle(
                speed_m_s=speed_m_s,
                rudder_angle_deg=rudder_angle_deg,
                K_per_s=config.K_per_s,
                T_s=config.T_s,
                duration_s=config.duration_s,
                dt_s=config.dt_s,
                case_id=case_id,
            )
            cases.append(result["metadata"])
            metrics.append(result["metrics"])
            time_history.extend(result["time_history"])
    return {
        "metadata": {
            "case_id": config.case_id,
            "case_description": config.case_description,
            "units": units_metadata(),
            "required_charts": list(config.required_charts),
            "sidecars": {
                "provenance": config.provenance_filename,
                "artifact_manifest": config.artifact_manifest_filename,
            },
        },
        "provenance": provenance_metadata(),
        "cases": cases,
        "metrics": metrics,
        "time_history": time_history,
    }
