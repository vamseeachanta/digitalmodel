# ABOUTME: Artifact and chart writers for preliminary turning-circle sweeps.
# ABOUTME: Split from turning_circle.py to keep the public estimator module small.
"""Writers for preliminary Nomoto turning-circle tables and charts."""

from __future__ import annotations

import csv
import json
import math
from pathlib import Path
from typing import Any, Iterable

TIME_HISTORY_HEADERS = [
    "case_id",
    "speed_m_s",
    "speed_kn",
    "rudder_angle_deg",
    "time_s",
    "x_m",
    "y_m",
    "heading_deg",
    "yaw_rate_rad_s",
    "yaw_rate_deg_s",
]

METRICS_HEADERS = [
    "case_id",
    "speed_m_s",
    "speed_kn",
    "rudder_angle_deg",
    "advance_transfer_time_s",
    "advance_m",
    "transfer_m",
    "tactical_diameter_time_s",
    "tactical_diameter_m",
    "metric_status",
]

REQUIRED_CHARTS = [
    "trajectory_by_case",
    "yaw_rate_vs_time",
    "heading_vs_time",
    "turning_metrics_vs_rudder_angle",
]

SUPPORTED_TABLE_FORMATS = {"csv", "json"}
SUPPORTED_CHART_FORMATS = {"png", "html"}


def units_metadata() -> dict[str, str]:
    return {
        "speed_m_s": "m/s",
        "speed_kn": "kn",
        "rudder_angle_deg": "deg",
        "time_s": "s",
        "x_m": "m",
        "y_m": "m",
        "heading_deg": "deg",
        "yaw_rate_rad_s": "rad/s",
        "yaw_rate_deg_s": "deg/s",
        "advance_m": "m",
        "transfer_m": "m",
        "tactical_diameter_m": "m",
    }


def provenance_metadata() -> dict[str, Any]:
    return {
        "calculation": "preliminary first-order Nomoto turning-circle estimate",
        "nomoto_equation": "r_dot = (K * delta - r) / T",
        "metric_basis": (
            "advance and transfer use the interpolated 90 deg heading crossing; "
            "tactical diameter uses the interpolated 180 deg heading crossing"
        ),
        "scope_limitations": [
            "Preliminary constant-speed first-order Nomoto estimate only.",
            "Not MMG and not a full MMG maneuvering model.",
            "Not an IMO maneuvering compliance assessment.",
            "No ABS or class compliance conclusion is made.",
        ],
    }


def write_turning_circle_results(
    result: dict[str, Any],
    output_dir: str | Path,
    table_formats: Iterable[str] = ("csv", "json"),
    chart_formats: Iterable[str] = ("png",),
) -> dict[str, Any]:
    """Write CSV/JSON tables, provenance, manifest, and charts."""

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    table_formats = tuple(table_formats)
    chart_formats = tuple(chart_formats)
    _validate_formats(table_formats, SUPPORTED_TABLE_FORMATS, "table")
    _validate_formats(chart_formats, SUPPORTED_CHART_FORMATS, "chart")

    manifest: dict[str, Any] = {"tables": {}, "charts": {}, "sidecars": {}}
    if "csv" in table_formats:
        manifest["tables"].update(_write_csv_tables(result, output_path))
    if "json" in table_formats:
        manifest["tables"]["json"] = _write_json_table(result, output_path)

    sidecar_names = result.get("metadata", {}).get("sidecars", {})
    provenance_path = output_path / sidecar_names.get(
        "provenance", "turning_circle_provenance.json"
    )
    provenance_path.write_text(
        json.dumps(result["provenance"], indent=2), encoding="utf-8"
    )
    manifest["sidecars"]["provenance"] = provenance_path

    manifest["charts"] = write_turning_circle_charts(result, output_path, chart_formats)
    manifest_path = output_path / sidecar_names.get(
        "artifact_manifest", "artifact_manifest.json"
    )
    manifest["sidecars"]["artifact_manifest"] = manifest_path
    manifest_path.write_text(
        json.dumps(_stringify(manifest), indent=2), encoding="utf-8"
    )

    if "json" in manifest["tables"]:
        _attach_manifest_to_json(manifest["tables"]["json"], manifest)
    return manifest


def _write_csv_tables(result: dict[str, Any], output_path: Path) -> dict[str, Path]:
    time_history_path = output_path / "turning_circle_time_history.csv"
    metrics_path = output_path / "turning_circle_metrics.csv"
    _write_csv(time_history_path, TIME_HISTORY_HEADERS, result["time_history"])
    _write_csv(metrics_path, METRICS_HEADERS, result["metrics"])
    return {"time_history_csv": time_history_path, "metrics_csv": metrics_path}


def _write_csv(path: Path, headers: list[str], rows: list[dict[str, Any]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=headers)
        writer.writeheader()
        for row in rows:
            writer.writerow({header: row[header] for header in headers})


def _write_json_table(result: dict[str, Any], output_path: Path) -> Path:
    path = output_path / "turning_circle_results.json"
    payload = {
        "metadata": result["metadata"],
        "provenance": result["provenance"],
        "cases": result["cases"],
        "metrics": result["metrics"],
        "time_history": result["time_history"],
        "artifacts": {},
    }
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    return path


def write_turning_circle_charts(
    result: dict[str, Any],
    output_dir: Path,
    chart_formats: Iterable[str] = ("png",),
) -> dict[str, dict[str, Path]]:
    """Write required PNG/HTML charts and return artifact paths."""

    chart_formats = tuple(chart_formats)
    _validate_formats(chart_formats, SUPPORTED_CHART_FORMATS, "chart")
    if not chart_formats:
        return {}

    charts: dict[str, dict[str, Path]] = {}
    required_charts = (
        result.get("metadata", {}).get("required_charts") or REQUIRED_CHARTS
    )
    for chart_name in required_charts:
        title = chart_name.replace("_", " ").title()
        charts[chart_name] = {}
        if "png" in chart_formats:
            path = output_dir / f"{chart_name}.png"
            _write_png_chart(result, chart_name, title, path)
            charts[chart_name]["png"] = path
        if "html" in chart_formats:
            path = output_dir / f"{chart_name}.html"
            _write_html_chart(result, chart_name, title, path)
            charts[chart_name]["html"] = path
    return charts


def _write_png_chart(
    result: dict[str, Any], chart_name: str, title: str, path: Path
) -> None:
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(8, 5))
    if chart_name == "trajectory_by_case":
        _plot_time_history(ax, result["time_history"], "x_m", "y_m", by_case=True)
        ax.set_xlabel("Advance x (m)")
        ax.set_ylabel("Transfer y (m)")
    elif chart_name == "yaw_rate_vs_time":
        _plot_time_history(ax, result["time_history"], "time_s", "yaw_rate_deg_s")
        ax.set_xlabel("Time (s)")
        ax.set_ylabel("Yaw rate (deg/s)")
    elif chart_name == "heading_vs_time":
        _plot_time_history(ax, result["time_history"], "time_s", "heading_deg")
        ax.set_xlabel("Time (s)")
        ax.set_ylabel("Heading (deg)")
    elif chart_name == "turning_metrics_vs_rudder_angle":
        _plot_metrics(ax, result["metrics"])
        ax.set_xlabel("Rudder angle (deg)")
        ax.set_ylabel("Distance (m)")
    else:
        raise ValueError(f"unsupported chart name: {chart_name}")
    ax.set_title(title)
    ax.grid(True, alpha=0.3)
    ax.legend(loc="best", fontsize="x-small")
    fig.tight_layout()
    fig.savefig(path)
    plt.close(fig)


def _write_html_chart(
    result: dict[str, Any], chart_name: str, title: str, path: Path
) -> None:
    import plotly.graph_objects as go

    fig = go.Figure()
    if chart_name == "trajectory_by_case":
        _add_time_history_traces(fig, result["time_history"], "x_m", "y_m")
        x_title, y_title = "Advance x (m)", "Transfer y (m)"
    elif chart_name == "yaw_rate_vs_time":
        _add_time_history_traces(
            fig, result["time_history"], "time_s", "yaw_rate_deg_s"
        )
        x_title, y_title = "Time (s)", "Yaw rate (deg/s)"
    elif chart_name == "heading_vs_time":
        _add_time_history_traces(fig, result["time_history"], "time_s", "heading_deg")
        x_title, y_title = "Time (s)", "Heading (deg)"
    elif chart_name == "turning_metrics_vs_rudder_angle":
        _add_metric_traces(fig, result["metrics"])
        x_title, y_title = "Rudder angle (deg)", "Distance (m)"
    else:
        raise ValueError(f"unsupported chart name: {chart_name}")
    fig.update_layout(title_text=title)
    fig.update_xaxes(title_text=x_title)
    fig.update_yaxes(title_text=y_title)
    fig.write_html(path, include_plotlyjs=True, full_html=True)


def _plot_time_history(
    ax: Any, rows: list[dict[str, Any]], x_key: str, y_key: str, by_case: bool = False
) -> None:
    group_key = "case_id" if by_case else "rudder_angle_deg"
    for label, grouped in _group_rows(rows, group_key).items():
        ordered = sorted(grouped, key=lambda row: row["time_s"])
        ax.plot(
            [row[x_key] for row in ordered],
            [row[y_key] for row in ordered],
            label=str(label),
        )


def _plot_metrics(ax: Any, rows: list[dict[str, Any]]) -> None:
    for speed, grouped in _group_rows(rows, "speed_kn").items():
        ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
        x_values = [row["rudder_angle_deg"] for row in ordered]
        ax.plot(
            x_values,
            [_chart_value(row["advance_m"]) for row in ordered],
            marker="o",
            label=f"{speed:g} kn advance",
        )
        ax.plot(
            x_values,
            [_chart_value(row["tactical_diameter_m"]) for row in ordered],
            marker="s",
            label=f"{speed:g} kn tactical",
        )


def _add_time_history_traces(
    fig: Any, rows: list[dict[str, Any]], x_key: str, y_key: str
) -> None:
    import plotly.graph_objects as go

    for case_id, grouped in _group_rows(rows, "case_id").items():
        ordered = sorted(grouped, key=lambda row: row["time_s"])
        fig.add_trace(
            go.Scatter(
                x=[row[x_key] for row in ordered],
                y=[row[y_key] for row in ordered],
                mode="lines",
                name=str(case_id),
            )
        )


def _add_metric_traces(fig: Any, rows: list[dict[str, Any]]) -> None:
    import plotly.graph_objects as go

    for speed, grouped in _group_rows(rows, "speed_kn").items():
        ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
        x_values = [row["rudder_angle_deg"] for row in ordered]
        fig.add_trace(
            go.Scatter(
                x=x_values,
                y=[row["advance_m"] for row in ordered],
                mode="lines+markers",
                name=f"{speed:g} kn advance",
            )
        )
        fig.add_trace(
            go.Scatter(
                x=x_values,
                y=[row["tactical_diameter_m"] for row in ordered],
                mode="lines+markers",
                name=f"{speed:g} kn tactical",
            )
        )


def _group_rows(
    rows: list[dict[str, Any]], key: str
) -> dict[Any, list[dict[str, Any]]]:
    grouped: dict[Any, list[dict[str, Any]]] = {}
    for row in rows:
        grouped.setdefault(row[key], []).append(row)
    return grouped


def _chart_value(value: Any) -> float:
    return math.nan if value is None else float(value)


def _validate_formats(values: Iterable[str], supported: set[str], label: str) -> None:
    unsupported = sorted(set(values) - supported)
    if unsupported:
        raise ValueError(f"unsupported {label} formats: {unsupported}")


def _attach_manifest_to_json(path: Path, manifest: dict[str, Any]) -> None:
    payload = json.loads(path.read_text(encoding="utf-8"))
    payload["artifacts"] = _stringify(manifest)
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


def _stringify(value: Any) -> Any:
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, dict):
        return {key: _stringify(item) for key, item in value.items()}
    return value
