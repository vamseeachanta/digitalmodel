"""UV-workflow router for elastic beam-column buckling analysis."""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any

from digitalmodel.infrastructure.base_solvers.structural.buckling.elastic_buckling import (
    ElasticBucklingSolver,
)
from digitalmodel.structural.fe.model_builder import (
    BeamModel,
    build_beam_model,
    display_path,
    results_csv_path,
)


def router(cfg: dict) -> dict:
    settings = cfg.get("elastic_buckling") or {}
    model = build_beam_model(settings, "elastic_buckling")
    axial_force = _reference_axial_force(settings)
    for element in model.elements:
        element.axial_force = axial_force

    solver = ElasticBucklingSolver()
    solver.set_num_modes(int(settings.get("num_modes", 5)))
    solver.set_nodes(model.nodes)
    solver.set_elements(model.elements)
    solver.set_boundary_conditions(model.boundary_conditions)
    solver.set_loads(model.loads)
    solver.set_material_properties(model.material)
    results = solver.solve()
    if results.get("status") != "completed":
        raise ValueError(f"elastic_buckling solver failed: {results.get('error')}")

    load_factors = [float(value) for value in results["eigenvalues"]]
    critical_loads = [factor * axial_force for factor in load_factors]
    _validate_distinct_modes(critical_loads)

    euler_reference = _euler_reference_load(settings, model)
    relative_error = abs(critical_loads[0] - euler_reference) / euler_reference
    tolerance = settings.get("reference", {}).get("euler_tolerance")
    if tolerance is not None and relative_error > float(tolerance):
        raise ValueError(
            "elastic_buckling first critical load "
            f"{critical_loads[0]:.6g} differs from Euler reference "
            f"{euler_reference:.6g} by {relative_error:.3%}"
        )

    csv_path = results_csv_path(cfg, settings, "elastic_buckling")
    _write_mode_results_csv(csv_path, load_factors, critical_loads)

    cfg["elastic_buckling"] = {
        "status": results["status"],
        "num_elements": results["num_elements"],
        "num_nodes": results["num_nodes"],
        "num_modes": len(critical_loads),
        "axial_reference_load": axial_force,
        "first_load_factor": load_factors[0],
        "first_critical_load": critical_loads[0],
        "euler_reference_load": euler_reference,
        "euler_relative_error": relative_error,
        "results_csv": display_path(csv_path),
    }
    return cfg


def _reference_axial_force(settings: dict[str, Any]) -> float:
    axial_force = float(settings.get("axial_force", 0.0))
    if axial_force <= 0.0:
        raise ValueError("elastic_buckling axial_force must be positive compression")
    return axial_force


def _validate_distinct_modes(critical_loads: list[float]) -> None:
    if not critical_loads or critical_loads[0] <= 0.0:
        raise ValueError("elastic_buckling produced no positive critical loads")
    if len(critical_loads) > 1 and critical_loads[1] <= critical_loads[0]:
        raise ValueError("elastic_buckling modes are not distinct and increasing")


def _euler_reference_load(settings: dict[str, Any], model: BeamModel) -> float:
    reference = settings.get("reference", {})
    length = float(reference.get("length", sum(element.L for element in model.elements)))
    effective_length_factor = float(reference.get("effective_length_factor", 1.0))
    if length <= 0.0 or effective_length_factor <= 0.0:
        raise ValueError("Euler reference length and effective length factor must be positive")

    element = model.elements[0]
    return math.pi**2 * element.E * element.I / (effective_length_factor * length) ** 2


def _write_mode_results_csv(
    path: Path,
    load_factors: list[float],
    critical_loads: list[float],
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["mode", "eigenvalue", "critical_load"])
        for mode_index, (load_factor, critical_load) in enumerate(
            zip(load_factors, critical_loads),
            start=1,
        ):
            writer.writerow([mode_index, load_factor, critical_load])
