# ABOUTME: Engine-routed production-engineering nodal analysis (IPR/VLP operating point).
# ABOUTME: Routes the `production` basename to the existing nodal solver and IPR/VLP correlations.
"""Engine-routed production-engineering screeners.

Supported ``production.calculation`` values:
- ``nodal_analysis`` -> solve the IPR/VLP intersection (well operating point) using the
  Vogel or Linear IPR model and the Hagedorn-Brown VLP correlation.

The result is the operating point (rate, flowing bottom-hole pressure) with
confidence bounds derived from an optional well-test quality score. A JSON
summary sidecar is written for offline delivery.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from digitalmodel.production_engineering.ipr_models import (
    LinearIpr,
    ReservoirConditions,
    VogelIpr,
)
from digitalmodel.production_engineering.nodal_solver import NodalSolver
from digitalmodel.production_engineering.vlp_correlations import (
    FluidProperties,
    TubingConfig,
    vlp_curve,
)


def _resolve_dir(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _build_ipr(reservoir_cfg: dict[str, Any]):
    reservoir = ReservoirConditions(
        reservoir_pressure_psi=float(reservoir_cfg["reservoir_pressure_psi"]),
        bubble_point_psi=float(reservoir_cfg["bubble_point_psi"]),
        productivity_index_bopd_psi=float(reservoir_cfg["productivity_index_bopd_psi"]),
    )
    model = str(reservoir_cfg.get("ipr_model", "vogel")).lower()
    pi = reservoir.productivity_index_bopd_psi
    if model == "vogel":
        return VogelIpr.from_productivity_index(reservoir, pi), model
    if model == "linear":
        return LinearIpr(reservoir, pi), model
    raise ValueError(f"Unsupported ipr_model: {reservoir_cfg.get('ipr_model')}")


class ProductionEngineeringWorkflow:
    """Route selected production-engineering calculations through existing helpers."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        basename = cfg.get("basename")
        if basename == "nodal_analysis":
            workflow_cfg = {"nodal_analysis": cfg["nodal_analysis"]}
            return self._run_nodal_analysis(cfg, workflow_cfg)
        if basename == "vlp_correlations":
            workflow_cfg = {"vlp_correlations": cfg["vlp_correlations"]}
            return self._run_vlp_correlations(cfg, workflow_cfg)

        workflow_cfg = cfg.get("production", {})
        calculation = workflow_cfg.get("calculation")
        if calculation == "nodal_analysis":
            return self._run_nodal_analysis(cfg, workflow_cfg)
        raise ValueError(f"Unsupported production calculation: {calculation}")

    def _run_nodal_analysis(
        self, cfg: dict[str, Any], workflow_cfg: dict[str, Any]
    ) -> dict[str, Any]:
        params = workflow_cfg["nodal_analysis"]
        ipr, ipr_model = _build_ipr(params["reservoir"])

        tubing_cfg = params["tubing"]
        tubing = TubingConfig(
            depth_ft=float(tubing_cfg["depth_ft"]),
            tubing_id_in=float(tubing_cfg["tubing_id_in"]),
            roughness_in=float(tubing_cfg.get("roughness_in", 0.0006)),
        )
        fluid_cfg = params.get("fluid", {})
        fluid = FluidProperties(
            oil_api=float(fluid_cfg.get("oil_api", 35.0)),
            gas_gravity=float(fluid_cfg.get("gas_gravity", 0.65)),
            temperature_f=float(fluid_cfg.get("temperature_f", 150.0)),
        )
        surface = params["surface"]

        operating_point = NodalSolver().solve(
            ipr=ipr,
            tubing=tubing,
            fluid=fluid,
            watercut=float(surface.get("watercut", 0.0)),
            gor_scf_per_bbl=float(surface["gor_scf_per_bbl"]),
            whp_psi=float(surface["whp_psi"]),
            test_quality_score=surface.get("test_quality_score"),
        )

        summary = {
            "calculation": "nodal_analysis",
            "ipr_model": ipr_model,
            "operating_point": {
                "q_bopd": operating_point.q_bopd,
                "pwf_psi": operating_point.pwf_psi,
                "confidence": operating_point.confidence.value,
                "q_uncertainty_fraction": operating_point.q_uncertainty_fraction,
                "q_low_bopd": operating_point.q_low_bopd,
                "q_high_bopd": operating_point.q_high_bopd,
            },
        }

        output_cfg = params.get("outputs", {})
        directory = _resolve_dir(
            cfg, output_cfg.get("directory", "results/nodal_analysis")
        )
        directory.mkdir(parents=True, exist_ok=True)
        summary_path = directory / output_cfg.get(
            "summary_json", "nodal_analysis_summary.json"
        )
        summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")

        summary["summary_json"] = str(summary_path)
        if cfg.get("basename") == "nodal_analysis":
            cfg["nodal_analysis"] = {**params, **summary}
        else:
            cfg["production"] = {
                "calculation": "nodal_analysis",
                "nodal_analysis": {**params, **summary},
            }
        cfg.setdefault("outputs", {})["directory"] = str(directory)
        cfg["outputs"]["summary_json"] = str(summary_path)
        return cfg

    def _run_vlp_correlations(
        self, cfg: dict[str, Any], workflow_cfg: dict[str, Any]
    ) -> dict[str, Any]:
        params = workflow_cfg["vlp_correlations"]
        tubing_cfg = params["tubing"]
        fluid_cfg = params.get("fluid", {})
        surface = params["surface"]

        rates = [float(rate) for rate in params["rates_bopd"]]
        tubing = TubingConfig(
            depth_ft=float(tubing_cfg["depth_ft"]),
            tubing_id_in=float(tubing_cfg["tubing_id_in"]),
            roughness_in=float(tubing_cfg.get("roughness_in", 0.0006)),
        )
        fluid = FluidProperties(
            oil_api=float(fluid_cfg.get("oil_api", 35.0)),
            gas_gravity=float(fluid_cfg.get("gas_gravity", 0.65)),
            temperature_f=float(fluid_cfg.get("temperature_f", 150.0)),
        )
        correlation = str(params.get("correlation", "hagedorn_brown"))
        pressures = vlp_curve(
            rates_bopd=rates,
            watercut=float(surface.get("watercut", 0.0)),
            gor_scf_per_bbl=float(surface["gor_scf_per_bbl"]),
            tubing=tubing,
            fluid=fluid,
            whp_psi=float(surface["whp_psi"]),
            correlation=correlation,
        )
        curve = [
            {"q_bopd": rate, "pwf_psi": pressure}
            for rate, pressure in zip(rates, pressures)
        ]
        summary = {
            "calculation": "vlp_correlations",
            "correlation": correlation,
            "curve": curve,
            "monotonic_increasing": all(
                pressures[index] <= pressures[index + 1]
                for index in range(len(pressures) - 1)
            ),
        }

        output_cfg = params.get("outputs", {})
        directory = _resolve_dir(
            cfg, output_cfg.get("directory", "results/vlp_correlations")
        )
        directory.mkdir(parents=True, exist_ok=True)
        summary_path = directory / output_cfg.get(
            "summary_json", "vlp_correlations_summary.json"
        )
        summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")

        summary["summary_json"] = str(summary_path)
        cfg["vlp_correlations"] = {**params, **summary}
        cfg.setdefault("outputs", {})["directory"] = str(directory)
        cfg["outputs"]["summary_json"] = str(summary_path)
        return cfg
