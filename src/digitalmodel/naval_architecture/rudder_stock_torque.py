# ABOUTME: Preliminary rudder-stock torque sweeps for typical ship steering cases.
# ABOUTME: Loads YAML inputs, reuses rudder normal force, and writes tables/charts.
"""Preliminary rudder-stock torque sweep utilities.

This module intentionally implements a bounded first-cut calculation:
``T_stock = scalar_normal_force_N * stock_to_center_of_pressure_arm_m``.
The equal/opposite steering-gear holding torque is reported separately. This is
not rudder stock scantling, steering gear machinery sizing, or class/SOLAS
compliance proof.
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
from digitalmodel.naval_architecture.yaw_moment import KNOT_TO_M_PER_S, M_PER_S_TO_KNOT

CSV_HEADERS = [
    "case_id",
    "speed_m_s",
    "speed_kn",
    "rudder_angle_deg",
    "rho_kg_m3",
    "rudder_area_m2",
    "rudder_span_m",
    "behind_hull",
    "stock_to_center_of_pressure_arm_m",
    "scalar_normal_force_N",
    "hydrodynamic_rudder_stock_torque_Nm",
    "required_steering_gear_holding_torque_Nm",
    "rudder_stock_torque_abs_Nm",
    "rudder_stock_torque_abs_kNm",
]

REQUIRED_CHARTS = [
    "rudder_stock_torque_vs_rudder_angle_by_speed",
    "rudder_stock_torque_vs_speed_by_rudder_angle",
    "scalar_normal_force_vs_rudder_angle_by_speed",
    "rudder_stock_torque_speed_angle_heatmap",
]

SUPPORTED_TABLE_FORMATS = {"csv", "json"}
SUPPORTED_CHART_FORMATS = {"png", "html"}


@dataclass(frozen=True)
class RudderStockTorqueResult:
    """Single preliminary rudder-stock torque result."""

    scalar_normal_force_N: float
    hydrodynamic_rudder_stock_torque_Nm: float
    required_steering_gear_holding_torque_Nm: float
    rudder_stock_torque_abs_Nm: float
    rudder_stock_torque_abs_kNm: float
    metadata: dict[str, Any]


@dataclass(frozen=True)
class RudderStockTorqueInput:
    """Validated rudder-stock torque sweep input."""

    case_id: str
    case_description: str
    vessel: dict[str, Any]
    rudder_area_m2: float
    rudder_span_m: float
    stock_to_center_of_pressure_arm_m: float
    behind_hull: bool
    rho_kg_m3: float
    speeds_m_s: list[float]
    speeds_kn: list[float]
    rudder_angles_deg: list[float]
    output_directory: str
    table_formats: tuple[str, ...]
    provenance_filename: str
    artifact_manifest_filename: str
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


def rudder_stock_torque(
    *,
    velocity_m_s: float,
    rho_kg_m3: float,
    rudder_area_m2: float,
    rudder_span_m: float,
    rudder_angle_deg: float,
    stock_to_center_of_pressure_arm_m: float,
    behind_hull: bool = True,
) -> RudderStockTorqueResult:
    """Calculate preliminary rudder-stock torque from rudder normal force.

    ``stock_to_center_of_pressure_arm_m`` is the user-supplied perpendicular
    force-line moment arm from the rudder stock axis to the resultant normal
    force line of action. Positive hydrodynamic torque follows the existing
    signed ``rudder_normal_force`` scalar and a positive arm. The holding torque
    is the equal/opposite counteracting torque.
    """

    velocity_m_s = _validate_nonnegative("velocity_m_s", velocity_m_s)
    rho_kg_m3 = _validate_positive("rho_kg_m3", rho_kg_m3)
    rudder_area_m2 = _validate_positive("rudder_area_m2", rudder_area_m2)
    rudder_span_m = _validate_positive("rudder_span_m", rudder_span_m)
    rudder_angle_deg = _validate_finite("rudder_angle_deg", rudder_angle_deg)
    stock_to_center_of_pressure_arm_m = _validate_nonnegative(
        "stock_to_center_of_pressure_arm_m", stock_to_center_of_pressure_arm_m
    )

    scalar_normal_force_N = rudder_normal_force(
        velocity_m_s=velocity_m_s,
        rho_kg_m3=rho_kg_m3,
        rudder_area_m2=rudder_area_m2,
        rudder_span_m=rudder_span_m,
        rudder_angle_deg=rudder_angle_deg,
        behind_hull=behind_hull,
    )
    hydrodynamic_torque_Nm = (
        scalar_normal_force_N * stock_to_center_of_pressure_arm_m
    )
    holding_torque_Nm = -hydrodynamic_torque_Nm
    abs_torque_Nm = abs(hydrodynamic_torque_Nm)
    return RudderStockTorqueResult(
        scalar_normal_force_N=scalar_normal_force_N,
        hydrodynamic_rudder_stock_torque_Nm=hydrodynamic_torque_Nm,
        required_steering_gear_holding_torque_Nm=holding_torque_Nm,
        rudder_stock_torque_abs_Nm=abs_torque_Nm,
        rudder_stock_torque_abs_kNm=abs_torque_Nm / 1000.0,
        metadata={
            "sign_convention": _sign_convention_metadata(),
            "units": _units_metadata(),
        },
    )


def _sign_convention_metadata() -> dict[str, str]:
    return {
        "force_source": "signed scalar_normal_force_N from rudder_normal_force",
        "positive_hydrodynamic_torque": (
            "right-hand-rule torque about the +z/up rudder stock axis; "
            "counterclockwise viewed from above for positive scalar normal force "
            "and positive stock arm"
        ),
        "holding_torque": "equal and opposite sign to hydrodynamic torque",
    }


def _units_metadata() -> dict[str, str]:
    return {
        "speed_m_s": "m/s",
        "speed_kn": "kn",
        "rudder_angle_deg": "deg",
        "rho_kg_m3": "kg/m^3",
        "rudder_area_m2": "m^2",
        "rudder_span_m": "m",
        "stock_to_center_of_pressure_arm_m": "m",
        "scalar_normal_force_N": "N",
        "hydrodynamic_rudder_stock_torque_Nm": "N*m",
        "required_steering_gear_holding_torque_Nm": "N*m",
        "rudder_stock_torque_abs_Nm": "N*m",
        "rudder_stock_torque_abs_kNm": "kN*m",
    }


def _provenance_metadata() -> dict[str, str]:
    return {
        "calculation": "preliminary rudder stock and steering gear holding torque",
        "torque_relation": (
            "hydrodynamic_rudder_stock_torque_Nm = "
            "scalar_normal_force_N * stock_to_center_of_pressure_arm_m"
        ),
        "holding_torque_relation": (
            "required_steering_gear_holding_torque_Nm = "
            "-hydrodynamic_rudder_stock_torque_Nm"
        ),
        "force_source_module": (
            "digitalmodel.naval_architecture.maneuverability.rudder_normal_force"
        ),
        "force_source_note": (
            "Existing maneuverability module documents Whicker & Fehlner rudder "
            "lift model; no new standards-derived constants introduced here."
        ),
        "stock_arm_source": "user_supplied_constant_perpendicular_arm",
        "stock_arm_note": (
            "Input arm is not a standards-derived coefficient; it is a constant "
            "user-supplied perpendicular force-line moment arm from rudder stock "
            "axis to resultant normal-force line of action."
        ),
        "scope_limitations": (
            "Preliminary constant-arm hydrodynamic torque sweep only; not steering "
            "gear machinery sizing, actuator sizing, rudder stock scantling, or "
            "class/SOLAS compliance proof."
        ),
    }


def load_packaged_rudder_stock_torque_yaml() -> RudderStockTorqueInput:
    """Load packaged typical-ship rudder-stock torque YAML."""

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "rudder_stock_torque_typical_ship.yml"
    )
    with resource.open("r", encoding="utf-8") as stream:
        payload = yaml.safe_load(stream)
    return validate_rudder_stock_torque_input(payload)


def load_rudder_stock_torque_input(path: str | Path) -> RudderStockTorqueInput:
    """Load and validate a user-provided rudder-stock torque YAML file."""

    with Path(path).open("r", encoding="utf-8") as stream:
        payload = yaml.safe_load(stream)
    return validate_rudder_stock_torque_input(payload)


def validate_rudder_stock_torque_input(payload: dict[str, Any]) -> RudderStockTorqueInput:
    """Validate the documented rudder-stock torque input contract."""

    if not isinstance(payload, dict):
        raise ValueError("rudder stock torque input must be a YAML mapping")
    required = {"case", "rudder", "stock", "environment", "sweep", "outputs"}
    missing = sorted(required - set(payload))
    if missing:
        raise ValueError(f"missing required top-level sections: {missing}")

    case = payload["case"]
    rudder = payload["rudder"]
    stock = payload["stock"]
    environment = payload["environment"]
    sweep = payload["sweep"]
    outputs = payload["outputs"]

    rudder_angles_deg = [
        _validate_finite("rudder_angle_deg", value)
        for value in sweep["rudder_angles_deg"]
    ]
    if not rudder_angles_deg:
        raise ValueError("sweep.rudder_angles_deg must contain at least one value")

    speeds = sweep["speeds"]
    speed_units = speeds.get("units")
    speed_values = [_validate_nonnegative("speed", v) for v in speeds["values"]]
    if not speed_values:
        raise ValueError("sweep.speeds.values must contain at least one value")
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
    sidecar_cfg = outputs.get("sidecars", {})
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

    return RudderStockTorqueInput(
        case_id=str(case["id"]),
        case_description=str(case.get("description", "")),
        vessel=dict(payload.get("vessel", {})),
        rudder_area_m2=_validate_positive("rudder.area_m2", rudder["area_m2"]),
        rudder_span_m=_validate_positive("rudder.span_m", rudder["span_m"]),
        stock_to_center_of_pressure_arm_m=_validate_nonnegative(
            "stock.stock_to_center_of_pressure_arm_m",
            stock["stock_to_center_of_pressure_arm_m"],
        ),
        behind_hull=bool(rudder.get("behind_hull", True)),
        rho_kg_m3=_validate_positive(
            "environment.rho_kg_m3", environment["rho_kg_m3"]
        ),
        speeds_m_s=speeds_m_s,
        speeds_kn=speeds_kn,
        rudder_angles_deg=rudder_angles_deg,
        output_directory=str(outputs.get("directory", "results/rudder_stock_torque")),
        table_formats=table_formats,
        provenance_filename=str(
            sidecar_cfg.get("provenance", "rudder_stock_torque_provenance.json")
        ),
        artifact_manifest_filename=str(
            sidecar_cfg.get("artifact_manifest", "artifact_manifest.json")
        ),
        chart_enabled=chart_enabled,
        chart_formats=chart_formats,
        required_charts=required_charts,
        raw=payload,
    )


def run_rudder_stock_torque_sweep(config: RudderStockTorqueInput) -> dict[str, Any]:
    """Run every speed/rudder-angle combination and return rows + metadata."""

    rows: list[dict[str, Any]] = []
    for speed_m_s, speed_kn in zip(config.speeds_m_s, config.speeds_kn):
        for rudder_angle_deg in config.rudder_angles_deg:
            result = rudder_stock_torque(
                velocity_m_s=speed_m_s,
                rho_kg_m3=config.rho_kg_m3,
                rudder_area_m2=config.rudder_area_m2,
                rudder_span_m=config.rudder_span_m,
                rudder_angle_deg=rudder_angle_deg,
                stock_to_center_of_pressure_arm_m=(
                    config.stock_to_center_of_pressure_arm_m
                ),
                behind_hull=config.behind_hull,
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
                    "behind_hull": config.behind_hull,
                    "stock_to_center_of_pressure_arm_m": (
                        config.stock_to_center_of_pressure_arm_m
                    ),
                    "scalar_normal_force_N": result.scalar_normal_force_N,
                    "hydrodynamic_rudder_stock_torque_Nm": (
                        result.hydrodynamic_rudder_stock_torque_Nm
                    ),
                    "required_steering_gear_holding_torque_Nm": (
                        result.required_steering_gear_holding_torque_Nm
                    ),
                    "rudder_stock_torque_abs_Nm": result.rudder_stock_torque_abs_Nm,
                    "rudder_stock_torque_abs_kNm": result.rudder_stock_torque_abs_kNm,
                }
            )

    return {
        "metadata": {
            "case_id": config.case_id,
            "case_description": config.case_description,
            "units": _units_metadata(),
            "sign_convention": _sign_convention_metadata(),
            "required_charts": list(config.required_charts),
            "sidecars": {
                "provenance": config.provenance_filename,
                "artifact_manifest": config.artifact_manifest_filename,
            },
        },
        "provenance": _provenance_metadata(),
        "rows": rows,
    }


def write_rudder_stock_torque_results(
    result: dict[str, Any],
    output_dir: str | Path,
    table_formats: Iterable[str] = ("csv", "json"),
    chart_formats: Iterable[str] = ("png",),
) -> dict[str, Any]:
    """Write CSV/JSON tables, provenance sidecar, manifest, and required charts."""

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)
    table_formats = tuple(table_formats)
    chart_formats = tuple(chart_formats)
    manifest: dict[str, Any] = {"tables": {}, "charts": {}, "sidecars": {}}

    if "csv" in table_formats:
        csv_path = output_path / "rudder_stock_torque_sweep.csv"
        with csv_path.open("w", newline="", encoding="utf-8") as stream:
            writer = csv.DictWriter(stream, fieldnames=CSV_HEADERS)
            writer.writeheader()
            for row in result["rows"]:
                writer.writerow({header: row[header] for header in CSV_HEADERS})
        manifest["tables"]["csv"] = csv_path

    if "json" in table_formats:
        json_path = output_path / "rudder_stock_torque_sweep.json"
        payload = {
            "metadata": result["metadata"],
            "provenance": result["provenance"],
            "rows": result["rows"],
            "artifacts": {},
        }
        json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
        manifest["tables"]["json"] = json_path

    provenance_name = result.get("metadata", {}).get("sidecars", {}).get(
        "provenance", "rudder_stock_torque_provenance.json"
    )
    manifest_name = result.get("metadata", {}).get("sidecars", {}).get(
        "artifact_manifest", "artifact_manifest.json"
    )
    provenance_path = output_path / provenance_name
    provenance_path.write_text(
        json.dumps(result["provenance"], indent=2), encoding="utf-8"
    )
    manifest["sidecars"]["provenance"] = provenance_path
    manifest["charts"] = write_rudder_stock_torque_charts(
        result, output_path, chart_formats
    )

    manifest_path = output_path / manifest_name
    manifest["sidecars"]["artifact_manifest"] = manifest_path
    manifest_path.write_text(
        json.dumps(_stringify_manifest(manifest), indent=2), encoding="utf-8"
    )

    if "json" in manifest["tables"]:
        json_path = manifest["tables"]["json"]
        payload = json.loads(json_path.read_text(encoding="utf-8"))
        payload["artifacts"] = _stringify_manifest(manifest)
        json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    return manifest


def write_rudder_stock_torque_charts(
    result: dict[str, Any],
    output_dir: Path,
    chart_formats: Iterable[str] = ("png",),
) -> dict[str, dict[str, Path]]:
    """Write required PNG/HTML charts and return artifact paths."""

    chart_formats = tuple(chart_formats)
    for fmt in chart_formats:
        if fmt not in SUPPORTED_CHART_FORMATS:
            raise ValueError(f"unsupported chart format: {fmt}")

    required_charts = result.get("metadata", {}).get("required_charts") or REQUIRED_CHARTS
    charts: dict[str, dict[str, Path]] = {}
    if not chart_formats:
        return charts
    for chart_name in required_charts:
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


def build_rudder_stock_torque_heatmap_grid(rows: list[dict[str, Any]]) -> dict[str, Any]:
    """Build speed x rudder-angle absolute-stock-torque matrix for heatmaps."""

    speeds = sorted({row["speed_kn"] for row in rows})
    angles = sorted({row["rudder_angle_deg"] for row in rows})
    lookup = {
        (row["speed_kn"], row["rudder_angle_deg"]): row["rudder_stock_torque_abs_Nm"]
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
    if chart_name == "rudder_stock_torque_vs_rudder_angle_by_speed":
        for speed, grouped in _group_rows(rows, "speed_kn").items():
            ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
            ax.plot(
                [row["rudder_angle_deg"] for row in ordered],
                [row["rudder_stock_torque_abs_kNm"] for row in ordered],
                marker="o",
                label=f"{speed:g} kn",
            )
        ax.set_xlabel("Rudder angle (deg)")
        ax.set_ylabel("Absolute rudder stock torque (kN*m)")
    elif chart_name == "rudder_stock_torque_vs_speed_by_rudder_angle":
        for angle, grouped in _group_rows(rows, "rudder_angle_deg").items():
            ordered = sorted(grouped, key=lambda row: row["speed_kn"])
            ax.plot(
                [row["speed_kn"] for row in ordered],
                [row["rudder_stock_torque_abs_kNm"] for row in ordered],
                marker="o",
                label=f"{angle:g} deg",
            )
        ax.set_xlabel("Speed (kn)")
        ax.set_ylabel("Absolute rudder stock torque (kN*m)")
    elif chart_name == "scalar_normal_force_vs_rudder_angle_by_speed":
        for speed, grouped in _group_rows(rows, "speed_kn").items():
            ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
            ax.plot(
                [row["rudder_angle_deg"] for row in ordered],
                [row["scalar_normal_force_N"] for row in ordered],
                marker="o",
                label=f"{speed:g} kn",
            )
        ax.set_xlabel("Rudder angle (deg)")
        ax.set_ylabel("Scalar normal force (N)")
    elif chart_name == "rudder_stock_torque_speed_angle_heatmap":
        grid = build_rudder_stock_torque_heatmap_grid(rows)
        image = ax.imshow(grid["z"], aspect="auto", origin="lower")
        ax.set_xticks(range(len(grid["x"])), labels=[f"{v:g}" for v in grid["x"]])
        ax.set_yticks(range(len(grid["y"])), labels=[f"{v:g}" for v in grid["y"]])
        ax.set_xlabel("Rudder angle (deg)")
        ax.set_ylabel("Speed (kn)")
        fig.colorbar(image, ax=ax, label="Absolute rudder stock torque (N*m)")
    else:
        raise ValueError(f"unsupported chart name: {chart_name}")
    ax.set_title(title)
    if chart_name != "rudder_stock_torque_speed_angle_heatmap":
        ax.legend(loc="best")
        ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(path)
    plt.close(fig)


def _write_html_chart(rows: list[dict[str, Any]], chart_name: str, title: str, path: Path) -> None:
    import plotly.graph_objects as go

    fig = go.Figure()
    if chart_name == "rudder_stock_torque_vs_rudder_angle_by_speed":
        for speed, grouped in _group_rows(rows, "speed_kn").items():
            ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
            fig.add_trace(
                go.Scatter(
                    x=[row["rudder_angle_deg"] for row in ordered],
                    y=[row["rudder_stock_torque_abs_kNm"] for row in ordered],
                    mode="lines+markers",
                    name=f"{speed:g} kn",
                )
            )
        fig.update_xaxes(title="Rudder angle (deg)")
        fig.update_yaxes(title="Absolute rudder stock torque (kN*m)")
    elif chart_name == "rudder_stock_torque_vs_speed_by_rudder_angle":
        for angle, grouped in _group_rows(rows, "rudder_angle_deg").items():
            ordered = sorted(grouped, key=lambda row: row["speed_kn"])
            fig.add_trace(
                go.Scatter(
                    x=[row["speed_kn"] for row in ordered],
                    y=[row["rudder_stock_torque_abs_kNm"] for row in ordered],
                    mode="lines+markers",
                    name=f"{angle:g} deg",
                )
            )
        fig.update_xaxes(title="Speed (kn)")
        fig.update_yaxes(title="Absolute rudder stock torque (kN*m)")
    elif chart_name == "scalar_normal_force_vs_rudder_angle_by_speed":
        for speed, grouped in _group_rows(rows, "speed_kn").items():
            ordered = sorted(grouped, key=lambda row: row["rudder_angle_deg"])
            fig.add_trace(
                go.Scatter(
                    x=[row["rudder_angle_deg"] for row in ordered],
                    y=[row["scalar_normal_force_N"] for row in ordered],
                    mode="lines+markers",
                    name=f"{speed:g} kn",
                )
            )
        fig.update_xaxes(title="Rudder angle (deg)")
        fig.update_yaxes(title="Scalar normal force (N)")
    elif chart_name == "rudder_stock_torque_speed_angle_heatmap":
        grid = build_rudder_stock_torque_heatmap_grid(rows)
        fig.add_trace(go.Heatmap(x=grid["x"], y=grid["y"], z=grid["z"]))
        fig.update_xaxes(title="Rudder angle (deg)")
        fig.update_yaxes(title="Speed (kn)")
    else:
        raise ValueError(f"unsupported chart name: {chart_name}")
    fig.update_layout(title=title)
    fig.write_html(path)


def _stringify_manifest(value: Any) -> Any:
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, dict):
        return {key: _stringify_manifest(item) for key, item in value.items()}
    return value
