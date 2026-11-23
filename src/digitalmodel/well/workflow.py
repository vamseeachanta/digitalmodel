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
from digitalmodel.well.drilling.swab_surge import SwabSurge
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


def run_swab_surge(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run the swab/surge tripping-pressure screen."""
    params = cfg.get("swab_surge", {})
    section = params["section"]
    trip = params.get("trip", {})

    calc = SwabSurge(
        pipe_od=float(section["pipe_od"]),
        pipe_id=float(section["pipe_id"]),
        hole_diameter=float(section["hole_diameter"]),
        mud_weight=float(section["mud_weight"]),
        tvd=float(section["tvd"]),
        plastic_viscosity=float(section["plastic_viscosity"]),
        yield_point=float(section["yield_point"]),
        clinging_constant=float(section.get("clinging_constant", 0.45)),
        pipe_config=section.get("pipe_config", "closed"),
    )

    trip_speed = float(trip.get("trip_speed_ft_min", 60.0))
    evaluated = calc.evaluate(trip_speed).as_dict()

    result: dict[str, Any] = {"at_trip_speed": evaluated}
    window = params.get("window")
    if window is not None:
        limit = calc.max_safe_trip_speed(
            pore_pressure_emw_ppg=float(window["pore_pressure_emw_ppg"]),
            fracture_emw_ppg=float(window["fracture_emw_ppg"]),
        )
        result["safe_trip_speed"] = limit.as_dict()
        result["kick_at_trip_speed"] = (
            evaluated["swab_emw_ppg"] < float(window["pore_pressure_emw_ppg"])
        )
        result["loss_at_trip_speed"] = (
            evaluated["surge_emw_ppg"] > float(window["fracture_emw_ppg"])
        )

    summary = {
        "calculation": "swab_surge",
        "section": dict(section),
        "trip": dict(trip),
        "result": result,
    }
    summary["summary_json"] = str(
        _write_summary(
            cfg,
            params.get("outputs", {}),
            summary,
            "results/swab_surge",
            "swab_surge_summary.json",
        )
    )
    cfg["swab_surge"] = {**params, "result": result, "summary": summary}
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


def run_casing_design(cfg: dict[str, Any]) -> dict[str, Any]:
    """Run the production casing design-check screen over candidate products.

    Config: a well (depths, gradients, reservoir, frac plan), a list of
    candidate casing products (grade + OD + nominal weight) and optional
    design-factor overrides.  Each product is checked for burst / collapse /
    tension / triaxial design factors plus the maximum allowable frac surface
    pressure, with an optional NACE MR0175 sour-service screen.

    Source: Hansen (Devon Energy), EPA Hydraulic Fracturing Technical
    Workshop Session 2, 2011; API 5C3 / API 5CT / NACE MR0175 (#1290, #1305).
    """
    from digitalmodel.well.tubulars.casing import casing as build_casing
    from digitalmodel.well.tubulars.casing_design import (
        DesignFactors,
        ProductionCasingWell,
        api_round_force_lbf,
        api_round_pressure_psi,
        burst_external_profile,
        check_production_casing,
        max_frac_surface_pressure,
        sour_service_screen,
    )

    params = cfg.get("casing_design", {})
    well = ProductionCasingWell(**params["well"])
    factors = DesignFactors(**params.get("design_factors", {}))

    external = burst_external_profile(
        well.mud_ppg,
        well.toc_ft,
        well.outer_shoe_ft,
        well.td_ft,
        mix_water_ppg=well.mix_water_ppg,
        pore_ppg=well.pore_ppg,
    )

    rows: list[dict[str, Any]] = []
    for product_cfg in params["products"]:
        product = build_casing(
            product_cfg["grade"],
            od_in=float(product_cfg["od_in"]),
            weight_ppf=float(product_cfg["weight_ppf"]),
        )
        checks = check_production_casing(
            product, float(product_cfg["weight_ppf"]), well, factors)
        p_frac_max = max_frac_surface_pressure(
            product, well.frac_fluid_ppg, external, well.td_ft, factors)
        row: dict[str, Any] = {
            "label": (f"{product.od_in:g}\" {product_cfg['weight_ppf']:g}# "
                      f"{product.grade.name}"),
            "grade": product.grade.name,
            "od_in": product.od_in,
            "weight_ppf": float(product_cfg["weight_ppf"]),
            "wall_in": product.wt_in,
            "burst_rating_psi": api_round_pressure_psi(product.burst_psi),
            "collapse_rating_psi": api_round_pressure_psi(
                product.collapse_psi),
            "collapse_regime": product.collapse_regime,
            "body_yield_lbf": api_round_force_lbf(product.body_yield_lbf),
            "max_frac_surface_pressure_psi": round(p_frac_max, 1),
            "passes_all": all(res.passes for res in checks.values()),
        }
        for mode, res in checks.items():
            row[f"{mode}_df"] = (round(res.min_design_factor, 3)
                                 if res.min_design_factor != float("inf")
                                 else None)
            row[f"{mode}_required_df"] = res.required_design_factor
            row[f"{mode}_governing_depth_ft"] = res.governing_depth_ft
            row[f"{mode}_passes"] = res.passes
        rows.append(row)

    summary: dict[str, Any] = {
        "calculation": "casing_design",
        "reference": ("Hansen (Devon Energy), EPA HF workshop 2011; "
                      "API 5C3 / API 5CT / NACE MR0175"),
        "well": params["well"],
        "design_factors": {
            "burst": factors.burst,
            "burst_low_sicp": factors.burst_low_sicp,
            "collapse": factors.collapse,
            "tension": factors.tension,
            "compression": factors.compression,
            "triaxial": factors.triaxial,
        },
        "product_count": len(rows),
        "passing_products": [r["label"] for r in rows if r["passes_all"]],
        "products": rows,
    }

    sour_cfg = params.get("sour_service")
    if sour_cfg:
        assessment = sour_service_screen(
            float(sour_cfg["h2s_ppm"]),
            float(sour_cfg["total_pressure_psia"]),
            well_type=sour_cfg.get("well_type", "gas"),
            service_temp_f=sour_cfg.get("service_temp_f"),
        )
        summary["sour_service"] = {
            "is_sour": assessment.is_sour,
            "h2s_partial_psia": round(assessment.h2s_partial_psia, 4),
            "acceptable_grades": list(assessment.acceptable_grades),
            "reference": assessment.reference,
        }

    output_cfg = params.get("outputs", {})
    summary["summary_json"] = str(
        _write_summary(
            cfg,
            output_cfg,
            summary,
            "results/casing_design",
            "casing_design_summary.json",
        )
    )

    directory = Path(cfg["outputs"]["directory"])
    csv_path = directory / output_cfg.get(
        "products_csv", "casing_design_products.csv")
    with csv_path.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)
    cfg["outputs"]["products_csv"] = str(csv_path)

    cfg["casing_design"] = {**params, "summary": summary}
    return cfg
