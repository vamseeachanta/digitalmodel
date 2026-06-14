# ABOUTME: Engine-routed well and drilling workflow adapters for registry examples.
# ABOUTME: Wraps existing wellbore, hydraulics, ROP, and tubular envelope logic.
"""Engine-routed well and drilling workflow adapters."""

from __future__ import annotations

import csv
import json
from dataclasses import asdict
from pathlib import Path
from typing import Any

from digitalmodel.well.drilling.hydraulics import WellboreHydraulics
from digitalmodel.well.drilling.rop_models import BourgoineYoungROP, WarrenROP
from digitalmodel.well.drilling.well_bore_design import WellDesign
from digitalmodel.well.tubulars.design_envelope import (
    TubularGeometry,
    design_envelope_points,
)


def _resolve_dir(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _write_summary(
    cfg: dict[str, Any],
    output_cfg: dict[str, Any],
    payload: dict[str, Any],
    default_directory: str,
    default_filename: str,
) -> Path:
    directory = _resolve_dir(cfg, output_cfg.get("directory", default_directory))
    directory.mkdir(parents=True, exist_ok=True)
    summary_path = directory / output_cfg.get("summary_json", default_filename)
    summary_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    cfg.setdefault("outputs", {})["directory"] = str(directory)
    cfg["outputs"]["summary_json"] = str(summary_path)
    return summary_path


def run_well_bore_design(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run the configured wellbore design screen."""
    params = cfg.get("well_bore_design", {})
    design = WellDesign(
        hole_type=str(params["hole_type"]),
        water_depth_m=float(params["water_depth_m"]),
    )
    casing_program = [asdict(casing) for casing in design.casing_program()]
    cost = asdict(design.estimate_cost())
    risk = design.risk_assessment()
    objective = str(params.get("objective", "development"))
    recommended = design.recommend_hole_type(
        water_depth_m=float(params["water_depth_m"]),
        objective=objective,
    )

    summary = {
        "calculation": "well_bore_design",
        "hole_type": design.hole_type,
        "water_depth_m": design.water_depth_m,
        "objective": objective,
        "casing_count": len(casing_program),
        "total_cost_usd": cost["total_usd"],
        "production_potential_bopd": design.production_potential(),
        "risk": risk,
        "recommended_hole_type": recommended,
    }
    summary["summary_json"] = str(
        _write_summary(
            cfg,
            params.get("outputs", {}),
            summary,
            "results/well_bore_design",
            "well_bore_design_summary.json",
        )
    )
    cfg["well_bore_design"] = {
        **params,
        "casing_program": casing_program,
        "cost": cost,
        "risk": risk,
        "summary": summary,
    }
    return cfg


def run_well_hydraulics(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run the configured annular hydraulics screen."""
    params = cfg.get("well_hydraulics", {})
    section = params["section"]
    hydraulics = WellboreHydraulics(
        pipe_od=float(section["pipe_od"]),
        pipe_id=float(section["pipe_id"]),
        hole_diameter=float(section["hole_diameter"]),
        mud_weight=float(section["mud_weight"]),
        flow_rate=float(section["flow_rate"]),
        tvd=float(section["tvd"]),
        plastic_viscosity=float(section["plastic_viscosity"]),
        yield_point=float(section["yield_point"]),
    )
    pressure_drop = hydraulics.pressure_drop_annulus()
    result = {
        "annular_velocity_ft_min": hydraulics.annular_velocity(),
        "pressure_drop_annulus_psi": pressure_drop,
        "ecd_ppg": hydraulics.ecd(pressure_drop),
        "cuttings_transport_ratio": hydraulics.cuttings_transport_ratio(),
    }
    summary = {
        "calculation": "well_hydraulics",
        "section": dict(section),
        "result": result,
    }
    summary["summary_json"] = str(
        _write_summary(
            cfg,
            params.get("outputs", {}),
            summary,
            "results/well_hydraulics",
            "well_hydraulics_summary.json",
        )
    )
    cfg["well_hydraulics"] = {**params, "result": result, "summary": summary}
    return cfg


def run_rop_analysis(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run configured Bourgoyne-Young and Warren ROP predictions."""
    params = cfg.get("rop_analysis", {})

    by_cfg = params["bourgoyne_young"]
    by_params = {key: float(value) for key, value in by_cfg["parameters"].items()}
    by_model = BourgoineYoungROP(**by_params)
    by_inputs = {key: float(value) for key, value in by_cfg["inputs"].items()}
    by_prediction = by_model.predict(**by_inputs)

    warren_cfg = params["warren"]
    warren_model = WarrenROP(
        K=float(warren_cfg["parameters"]["K"]),
        a=float(warren_cfg["parameters"]["a"]),
        b=float(warren_cfg["parameters"]["b"]),
    )
    warren_inputs = {key: float(value) for key, value in warren_cfg["inputs"].items()}
    warren_prediction = warren_model.predict(**warren_inputs)

    result = {
        "bourgoyne_young": {
            "rop_ft_hr": by_prediction.rop_ft_hr,
            "model": by_prediction.model,
            "sensitivity_wob_ft_hr_per_klb": by_model.sensitivity_wob(**by_inputs),
            "sensitivity_rpm_ft_hr_per_rpm": by_model.sensitivity_rpm(**by_inputs),
        },
        "warren": {
            "rop_ft_hr": warren_prediction.rop_ft_hr,
            "model": warren_prediction.model,
        },
    }
    summary = {"calculation": "rop_analysis", "result": result}
    summary["summary_json"] = str(
        _write_summary(
            cfg,
            params.get("outputs", {}),
            summary,
            "results/rop_analysis",
            "rop_analysis_summary.json",
        )
    )
    cfg["rop_analysis"] = {**params, "result": result, "summary": summary}
    return cfg


def run_tubular_design(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run the configured tubular design-envelope sweep."""
    params = cfg.get("tubular_design", {})
    geometry = TubularGeometry(
        od_in=float(params["geometry"]["od_in"]),
        id_in=float(params["geometry"]["id_in"]),
        smys_psi=float(params["geometry"]["smys_psi"]),
        burst_rating_psi=float(params["geometry"]["burst_rating_psi"]),
        collapse_rating_psi=float(params["geometry"]["collapse_rating_psi"]),
    )
    n_points = int(params.get("n_points", 100))
    design_factor = float(params.get("design_factor", 1.0))
    yield_hoop_ratio = params.get("yield_hoop_ratio")
    if yield_hoop_ratio is not None:
        yield_hoop_ratio = float(yield_hoop_ratio)

    envelopes = design_envelope_points(
        geometry=geometry,
        design_factor=design_factor,
        n_points=n_points,
        yield_hoop_ratio=yield_hoop_ratio,
    )

    output_cfg = params.get("outputs", {})
    directory = _resolve_dir(cfg, output_cfg.get("directory", "results/tubular_design"))
    directory.mkdir(parents=True, exist_ok=True)
    csv_path = directory / output_cfg.get("envelope_csv", "tubular_design_envelope.csv")
    with csv_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(
            ["envelope", "point_index", "axial_force_lbf", "differential_pressure_psi"]
        )
        for envelope_name, points in envelopes.items():
            for index, point in enumerate(points):
                writer.writerow(
                    [envelope_name, index, float(point[0]), float(point[1])]
                )

    summary = {
        "calculation": "tubular_design",
        "design_factor": design_factor,
        "n_points": n_points,
        "geometry": {
            **params["geometry"],
            "wall_thickness_in": geometry.wall_thickness_in,
            "cross_section_area_in2": geometry.cross_section_area_in2,
            "yield_force_lbf": geometry.yield_force_lbf,
        },
        "envelopes": {
            name: {
                "point_count": int(points.shape[0]),
                "min_axial_force_lbf": float(points[:, 0].min()),
                "max_axial_force_lbf": float(points[:, 0].max()),
                "min_pressure_psi": float(points[:, 1].min()),
                "max_pressure_psi": float(points[:, 1].max()),
            }
            for name, points in envelopes.items()
        },
    }
    summary["summary_json"] = str(
        _write_summary(
            cfg,
            output_cfg,
            summary,
            "results/tubular_design",
            "tubular_design_summary.json",
        )
    )

    cfg.setdefault("outputs", {})["directory"] = str(directory)
    cfg["outputs"]["envelope_csv"] = str(csv_path)
    cfg["tubular_design"] = {**params, "summary": summary}
    return cfg
