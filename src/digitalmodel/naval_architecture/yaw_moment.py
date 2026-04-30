# ABOUTME: Rudder-induced yaw moment sweeps for typical ship maneuvering cases.
# ABOUTME: Loads YAML inputs, reuses maneuverability rudder force, and writes tables/charts.
"""Preliminary rudder-induced yaw moment sweep utilities.

This module intentionally implements a bounded first-cut calculation:
``M_z = x_rudder_from_cg_m * transverse_force_N``. It is not a full MMG,
turning-circle, dynamic yaw-response, or class-rule compliance model.
"""

from __future__ import annotations

import csv
import json
import math
from dataclasses import dataclass
from importlib import resources
from pathlib import Path
from typing import Any, Iterable

import yaml

from digitalmodel.naval_architecture.maneuverability import rudder_normal_force

KNOT_TO_M_PER_S = 0.514444
M_PER_S_TO_KNOT = 1.0 / KNOT_TO_M_PER_S

CSV_HEADERS = [
    "case_id",
    "speed_m_s",
    "speed_kn",
    "rudder_angle_deg",
    "rho_kg_m3",
    "rudder_area_m2",
    "rudder_span_m",
    "x_rudder_from_cg_m",
    "behind_hull",
    "scalar_normal_force_N",
    "transverse_force_N",
    "yaw_moment_Nm",
    "sign_convention",
]

REQUIRED_CHARTS = [
    "yaw_moment_vs_rudder_angle_by_speed",
    "yaw_moment_vs_speed_by_rudder_angle",
    "transverse_force_vs_rudder_angle_by_speed",
    "yaw_moment_speed_angle_heatmap",
]

SUPPORTED_TABLE_FORMATS = {"csv", "json"}
SUPPORTED_CHART_FORMATS = {"png", "html"}
SUPPORTED_FORCE_DIRECTIONS = {"port", "starboard"}


@dataclass(frozen=True)
class YawMomentResult:
    """Single rudder yaw-moment result."""

    scalar_normal_force_N: float
    transverse_force_N: float
    yaw_moment_Nm: float
    metadata: dict[str, Any]


@dataclass(frozen=True)
class YawMomentInput:
    """Validated yaw-moment sweep input."""

    case_id: str
    case_description: str
    vessel: dict[str, Any]
    rudder_area_m2: float
    rudder_span_m: float
    x_rudder_from_cg_m: float
    behind_hull: bool
    rho_kg_m3: float
    speeds_m_s: list[float]
    speeds_kn: list[float]
    rudder_angles_deg: list[float]
    positive_force_direction: str
    output_directory: str
    table_formats: tuple[str, ...]
    chart_enabled: bool
    chart_formats: tuple[str, ...]
    required_charts: tuple[str, ...]
    raw: dict[str, Any]


def _validate_finite(name: str, value: float) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite, got {value}")
    return value


def _validate_positive(name: str, value: float) -> float:
    value = _validate_finite(name, value)
    if value <= 0:
        raise ValueError(f"{name} must be > 0, got {value}")
    return value


def _validate_nonnegative(name: str, value: float) -> float:
    value = _validate_finite(name, value)
    if value < 0:
        raise ValueError(f"{name} must be >= 0, got {value}")
    return value


def rudder_yaw_moment(
    *,
    velocity_m_s: float,
    rho_kg_m3: float,
    rudder_area_m2: float,
    rudder_span_m: float,
    rudder_angle_deg: float,
    x_rudder_from_cg_m: float,
    behind_hull: bool = True,
    positive_force_direction: str = "port",
) -> YawMomentResult:
    """Calculate preliminary rudder-induced yaw moment about CG.

    Positive axes convention is ``+x`` forward, ``+y`` port, ``+z`` up, and
    positive yaw moment turns the bow to port. The existing maneuverability
    helper returns a signed scalar rudder normal force, so this wrapper makes
    the scalar-to-transverse direction mapping explicit.
    """

    velocity_m_s = _validate_nonnegative("velocity_m_s", velocity_m_s)
    rho_kg_m3 = _validate_positive("rho_kg_m3", rho_kg_m3)
    rudder_area_m2 = _validate_positive("rudder_area_m2", rudder_area_m2)
    rudder_span_m = _validate_positive("rudder_span_m", rudder_span_m)
    rudder_angle_deg = _validate_finite("rudder_angle_deg", rudder_angle_deg)
    x_rudder_from_cg_m = _validate_finite(
        "x_rudder_from_cg_m", x_rudder_from_cg_m
    )
    if positive_force_direction not in SUPPORTED_FORCE_DIRECTIONS:
        raise ValueError(
            "positive_force_direction must be 'port' or 'starboard', "
            f"got {positive_force_direction!r}"
        )

    scalar_normal_force_N = rudder_normal_force(
        velocity_m_s=velocity_m_s,
        rho_kg_m3=rho_kg_m3,
        rudder_area_m2=rudder_area_m2,
        rudder_span_m=rudder_span_m,
        rudder_angle_deg=rudder_angle_deg,
        behind_hull=behind_hull,
    )
    transverse_force_N = (
        scalar_normal_force_N
        if positive_force_direction == "port"
        else -scalar_normal_force_N
    )
    yaw_moment_Nm = x_rudder_from_cg_m * transverse_force_N
    return YawMomentResult(
        scalar_normal_force_N=scalar_normal_force_N,
        transverse_force_N=transverse_force_N,
        yaw_moment_Nm=yaw_moment_Nm,
        metadata={
            "sign_convention": _sign_convention_metadata(positive_force_direction),
            "units": _units_metadata(),
        },
    )


def _sign_convention_metadata(positive_force_direction: str) -> dict[str, str]:
    return {
        "axes": "+x forward, +y port, +z up",
        "positive_yaw_moment": "bow_to_port",
        "positive_force_direction": positive_force_direction,
    }


def _units_metadata() -> dict[str, str]:
    return {
        "speed_m_s": "m/s",
        "speed_kn": "kn",
        "rudder_angle_deg": "deg",
        "rho_kg_m3": "kg/m^3",
        "rudder_area_m2": "m^2",
        "rudder_span_m": "m",
        "x_rudder_from_cg_m": "m",
        "scalar_normal_force_N": "N",
        "transverse_force_N": "N",
        "yaw_moment_Nm": "N*m",
    }


def _provenance_metadata(positive_force_direction: str) -> dict[str, str]:
    return {
        "calculation": "rudder-induced yaw moment about CG",
        "yaw_moment_relation": "M_z = x_rudder_from_cg_m * transverse_force_N",
        "force_source_module": (
            "digitalmodel.naval_architecture.maneuverability.rudder_normal_force"
        ),
        "force_source_note": (
            "Existing maneuverability module documents Whicker & Fehlner rudder "
            "lift model; no new standards-derived constants introduced here."
        ),
        "positive_force_direction": positive_force_direction,
        "scope_limitations": (
            "Preliminary rudder-force lever-arm sweep; excludes hull/propeller/"
            "rudder interaction, drift, yaw inertia, MMG derivatives, and "
            "class-rule/IMO compliance."
        ),
    }


def load_packaged_typical_ship_yaml() -> YawMomentInput:
    """Load packaged typical-ship yaw-moment YAML via importlib.resources."""

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "yaw_moment_typical_ship.yml"
    )
    with resource.open("r", encoding="utf-8") as stream:
        payload = yaml.safe_load(stream)
    return validate_yaw_moment_input(payload)


def load_yaw_moment_input(path: str | Path) -> YawMomentInput:
    """Load and validate a user-provided yaw-moment YAML file."""

    with Path(path).open("r", encoding="utf-8") as stream:
        payload = yaml.safe_load(stream)
    return validate_yaw_moment_input(payload)


def validate_yaw_moment_input(payload: dict[str, Any]) -> YawMomentInput:
    """Validate the documented yaw-moment input contract."""

    if not isinstance(payload, dict):
        raise ValueError("yaw moment input must be a YAML mapping")
    required = {"case", "rudder", "environment", "sweep", "outputs"}
    missing = sorted(required - set(payload))
    if missing:
        raise ValueError(f"missing required top-level sections: {missing}")

    case = payload["case"]
    rudder = payload["rudder"]
    environment = payload["environment"]
    sweep = payload["sweep"]
    outputs = payload["outputs"]
    sign_convention = payload.get("sign_convention", {})

    case_id = str(case["id"])
    positive_force_direction = str(
        sign_convention.get("positive_force_direction", "port")
    )
    if positive_force_direction not in SUPPORTED_FORCE_DIRECTIONS:
        raise ValueError("positive_force_direction must be 'port' or 'starboard'")

    speeds = sweep["speeds"]
    speed_units = speeds.get("units")
    speed_values = [_validate_nonnegative("speed", v) for v in speeds["values"]]
    if speed_units == "kn":
        speeds_kn = speed_values
        speeds_m_s = [v * KNOT_TO_M_PER_S for v in speed_values]
    elif speed_units == "m/s":
        speeds_m_s = speed_values
        speeds_kn = [v * M_PER_S_TO_KNOT for v in speed_values]
    else:
        raise ValueError("sweep.speeds.units must be 'kn' or 'm/s'")

    table_formats = tuple(outputs.get("tables", ("csv", "json")))
    unsupported_tables = sorted(set(table_formats) - SUPPORTED_TABLE_FORMATS)
    if unsupported_tables:
        raise ValueError(f"unsupported output table formats: {unsupported_tables}")

    chart_cfg = outputs.get("charts", {})
    chart_enabled = bool(chart_cfg.get("enabled", False))
    chart_formats = tuple(chart_cfg.get("formats", ("png",)))
    unsupported_charts = sorted(set(chart_formats) - SUPPORTED_CHART_FORMATS)
    if unsupported_charts:
        raise ValueError(f"unsupported chart formats: {unsupported_charts}")
    required_charts = tuple(chart_cfg.get("required", ()))
    unknown_required = sorted(set(required_charts) - set(REQUIRED_CHARTS))
    if unknown_required:
        raise ValueError(f"unsupported required charts: {unknown_required}")
    if required_charts and not chart_enabled:
        raise ValueError("outputs.charts.required is invalid when charts.enabled is false")

    return YawMomentInput(
        case_id=case_id,
        case_description=str(case.get("description", "")),
        vessel=dict(payload.get("vessel", {})),
        rudder_area_m2=_validate_positive("rudder.area_m2", rudder["area_m2"]),
        rudder_span_m=_validate_positive("rudder.span_m", rudder["span_m"]),
        x_rudder_from_cg_m=_validate_finite(
            "rudder.x_from_cg_m", rudder["x_from_cg_m"]
        ),
        behind_hull=bool(rudder.get("behind_hull", True)),
        rho_kg_m3=_validate_positive(
            "environment.rho_kg_m3", environment["rho_kg_m3"]
        ),
        speeds_m_s=speeds_m_s,
        speeds_kn=speeds_kn,
        rudder_angles_deg=[
            _validate_finite("rudder_angle_deg", value)
            for value in sweep["rudder_angles_deg"]
        ],
        positive_force_direction=positive_force_direction,
        output_directory=str(outputs.get("directory", "results/yaw_moment")),
        table_formats=table_formats,
        chart_enabled=chart_enabled,
        chart_formats=chart_formats,
        required_charts=required_charts,
        raw=payload,
    )


def run_yaw_moment_sweep(config: YawMomentInput) -> dict[str, Any]:
    """Run every speed/rudder-angle combination and return rows + metadata."""

    rows: list[dict[str, Any]] = []
    for speed_m_s, speed_kn in zip(config.speeds_m_s, config.speeds_kn):
        for rudder_angle_deg in config.rudder_angles_deg:
            result = rudder_yaw_moment(
                velocity_m_s=speed_m_s,
                rho_kg_m3=config.rho_kg_m3,
                rudder_area_m2=config.rudder_area_m2,
                rudder_span_m=config.rudder_span_m,
                rudder_angle_deg=rudder_angle_deg,
                x_rudder_from_cg_m=config.x_rudder_from_cg_m,
                behind_hull=config.behind_hull,
                positive_force_direction=config.positive_force_direction,
            )
            rows.append(
                {
                    "case_id": config.case_id,
                    "speed_m_s": speed_m_s,
                    "speed_kn": speed_kn,
                    "rudder_angle_deg": rudder_angle_deg,
                    "rho_kg_m3": config.rho_kg_m3,
                    "rudder_area_m2": config.rudder_area_m2,
                    "rudder_span_m": config.rudder_span_m,
                    "x_rudder_from_cg_m": config.x_rudder_from_cg_m,
                    "behind_hull": config.behind_hull,
                    "scalar_normal_force_N": result.scalar_normal_force_N,
                    "transverse_force_N": result.transverse_force_N,
                    "yaw_moment_Nm": result.yaw_moment_Nm,
                    "sign_convention": config.positive_force_direction,
                }
            )

    return {
        "metadata": {
            "case_id": config.case_id,
            "case_description": config.case_description,
            "units": _units_metadata(),
            "sign_convention": _sign_convention_metadata(
                config.positive_force_direction
            ),
            "required_charts": list(config.required_charts),
        },
        "provenance": _provenance_metadata(config.positive_force_direction),
        "rows": rows,
    }


def write_yaw_moment_results(
    result: dict[str, Any],
    output_dir: str | Path,
    table_formats: Iterable[str] = ("csv", "json"),
    chart_formats: Iterable[str] = ("png",),
) -> dict[str, Any]:
    """Write CSV/JSON tables and required chart files."""

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    table_formats = tuple(table_formats)
    chart_formats = tuple(chart_formats)
    manifest: dict[str, Any] = {"tables": {}, "charts": {}}

    if "csv" in table_formats:
        csv_path = output_path / "yaw_moment_sweep.csv"
        with csv_path.open("w", newline="", encoding="utf-8") as stream:
            writer = csv.DictWriter(stream, fieldnames=CSV_HEADERS)
            writer.writeheader()
            for row in result["rows"]:
                writer.writerow({header: row[header] for header in CSV_HEADERS})
        manifest["tables"]["csv"] = csv_path

    if "json" in table_formats:
        json_path = output_path / "yaw_moment_sweep.json"
        payload = {
            "metadata": result["metadata"],
            "provenance": result["provenance"],
            "rows": result["rows"],
            "artifacts": {},
        }
        with json_path.open("w", encoding="utf-8") as stream:
            json.dump(payload, stream, indent=2)
        manifest["tables"]["json"] = json_path

    manifest["charts"] = write_yaw_moment_charts(result, output_path, chart_formats)

    if "json" in manifest["tables"]:
        json_path = manifest["tables"]["json"]
        payload = json.loads(json_path.read_text(encoding="utf-8"))
        payload["artifacts"] = _stringify_manifest(manifest)
        json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    return manifest


def write_yaw_moment_charts(
    result: dict[str, Any],
    output_dir: Path,
    chart_formats: Iterable[str] = ("png",),
) -> dict[str, dict[str, Path]]:
    """Write required PNG/HTML charts and return artifact paths."""

    chart_formats = tuple(chart_formats)
    for fmt in chart_formats:
        if fmt not in SUPPORTED_CHART_FORMATS:
            raise ValueError(f"unsupported chart format: {fmt}")

    charts: dict[str, dict[str, Path]] = {}
    for chart_name in REQUIRED_CHARTS:
        title = chart_name.replace("_", " ").title()
        charts[chart_name] = {}
        if "png" in chart_formats:
            path = output_dir / f"{chart_name}.png"
            _write_png_chart(result["rows"], chart_name, title, path)
            charts[chart_name]["png"] = path
        if "html" in chart_formats:
            path = output_dir / f"{chart_name}.html"
            _write_html_chart(result["rows"], chart_name, title, path)
            charts[chart_name]["html"] = path
    return charts


def build_yaw_moment_heatmap_grid(rows: list[dict[str, Any]]) -> dict[str, Any]:
    """Build speed x rudder-angle yaw-moment matrix for heatmap output."""

    speeds = sorted({row["speed_kn"] for row in rows})
    angles = sorted({row["rudder_angle_deg"] for row in rows})
    lookup = {
        (row["speed_kn"], row["rudder_angle_deg"]): row["yaw_moment_Nm"]
        for row in rows
    }
    return {
        "x": angles,
        "y": speeds,
        "z": [[lookup[(speed, angle)] for angle in angles] for speed in speeds],
    }


def _group_rows(rows: list[dict[str, Any]], key: str) -> dict[float, list[dict[str, Any]]]:
    grouped: dict[float, list[dict[str, Any]]] = {}
    for row in rows:
        grouped.setdefault(row[key], []).append(row)
    return grouped


def _write_png_chart(rows: list[dict[str, Any]], chart_name: str, title: str, path: Path) -> None:
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(8, 5))
    if chart_name == "yaw_moment_vs_rudder_angle_by_speed":
        for speed, grouped in _group_rows(rows, "speed_kn").items():
            ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
            ax.plot(
                [row["rudder_angle_deg"] for row in ordered],
                [row["yaw_moment_Nm"] for row in ordered],
                marker="o",
                label=f"{speed:g} kn",
            )
        ax.set_xlabel("Rudder angle (deg)")
        ax.set_ylabel("Yaw moment (N*m)")
    elif chart_name == "yaw_moment_vs_speed_by_rudder_angle":
        for angle, grouped in _group_rows(rows, "rudder_angle_deg").items():
            ordered = sorted(grouped, key=lambda row: row["speed_kn"])
            ax.plot(
                [row["speed_kn"] for row in ordered],
                [row["yaw_moment_Nm"] for row in ordered],
                marker="o",
                label=f"{angle:g} deg",
            )
        ax.set_xlabel("Speed (kn)")
        ax.set_ylabel("Yaw moment (N*m)")
    elif chart_name == "transverse_force_vs_rudder_angle_by_speed":
        for speed, grouped in _group_rows(rows, "speed_kn").items():
            ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
            ax.plot(
                [row["rudder_angle_deg"] for row in ordered],
                [row["transverse_force_N"] for row in ordered],
                marker="o",
                label=f"{speed:g} kn",
            )
        ax.set_xlabel("Rudder angle (deg)")
        ax.set_ylabel("Transverse force (N)")
    elif chart_name == "yaw_moment_speed_angle_heatmap":
        grid = build_yaw_moment_heatmap_grid(rows)
        mesh = ax.imshow(grid["z"], aspect="auto", origin="lower")
        ax.set_xticks(range(len(grid["x"])), [f"{v:g}" for v in grid["x"]])
        ax.set_yticks(range(len(grid["y"])), [f"{v:g}" for v in grid["y"]])
        ax.set_xlabel("Rudder angle (deg)")
        ax.set_ylabel("Speed (kn)")
        fig.colorbar(mesh, ax=ax, label="Yaw moment (N*m)")
    else:
        raise ValueError(f"unknown chart: {chart_name}")
    ax.set_title(title)
    ax.grid(True, alpha=0.3)
    if chart_name != "yaw_moment_speed_angle_heatmap":
        ax.legend(loc="best", fontsize="small")
    fig.tight_layout()
    fig.savefig(path)
    plt.close(fig)


def _write_html_chart(rows: list[dict[str, Any]], chart_name: str, title: str, path: Path) -> None:
    import plotly.graph_objects as go

    fig = go.Figure()
    if chart_name == "yaw_moment_vs_rudder_angle_by_speed":
        for speed, grouped in _group_rows(rows, "speed_kn").items():
            ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
            fig.add_trace(
                go.Scatter(
                    x=[row["rudder_angle_deg"] for row in ordered],
                    y=[row["yaw_moment_Nm"] for row in ordered],
                    mode="lines+markers",
                    name=f"{speed:g} kn",
                )
            )
        fig.update_xaxes(title_text="Rudder angle (deg)")
        fig.update_yaxes(title_text="Yaw moment (N*m)")
    elif chart_name == "yaw_moment_vs_speed_by_rudder_angle":
        for angle, grouped in _group_rows(rows, "rudder_angle_deg").items():
            ordered = sorted(grouped, key=lambda row: row["speed_kn"])
            fig.add_trace(
                go.Scatter(
                    x=[row["speed_kn"] for row in ordered],
                    y=[row["yaw_moment_Nm"] for row in ordered],
                    mode="lines+markers",
                    name=f"{angle:g} deg",
                )
            )
        fig.update_xaxes(title_text="Speed (kn)")
        fig.update_yaxes(title_text="Yaw moment (N*m)")
    elif chart_name == "transverse_force_vs_rudder_angle_by_speed":
        for speed, grouped in _group_rows(rows, "speed_kn").items():
            ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
            fig.add_trace(
                go.Scatter(
                    x=[row["rudder_angle_deg"] for row in ordered],
                    y=[row["transverse_force_N"] for row in ordered],
                    mode="lines+markers",
                    name=f"{speed:g} kn",
                )
            )
        fig.update_xaxes(title_text="Rudder angle (deg)")
        fig.update_yaxes(title_text="Transverse force (N)")
    elif chart_name == "yaw_moment_speed_angle_heatmap":
        grid = build_yaw_moment_heatmap_grid(rows)
        fig.add_trace(go.Heatmap(x=grid["x"], y=grid["y"], z=grid["z"]))
        fig.update_xaxes(title_text="Rudder angle (deg)")
        fig.update_yaxes(title_text="Speed (kn)")
    else:
        raise ValueError(f"unknown chart: {chart_name}")
    fig.update_layout(title_text=title)
    fig.write_html(path, include_plotlyjs=True, full_html=True)


def _stringify_manifest(manifest: dict[str, Any]) -> dict[str, Any]:
    if isinstance(manifest, Path):
        return str(manifest)
    if isinstance(manifest, dict):
        return {key: _stringify_manifest(value) for key, value in manifest.items()}
    if isinstance(manifest, list):
        return [_stringify_manifest(value) for value in manifest]
    return manifest
