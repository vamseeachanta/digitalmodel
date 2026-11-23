"""UV-workflow router for Von Mises beam stress analysis."""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

from digitalmodel.infrastructure.base_solvers.structural.stress.von_mises import (
    VonMisesSolver,
)
from digitalmodel.structural.fe.model_builder import (
    BeamModel,
    build_beam_model,
    display_path,
    results_csv_path,
)


def router(cfg: dict) -> dict:
    settings = cfg.get("von_mises") or {}
    model = build_beam_model(settings, "von_mises")

    solver = VonMisesSolver()
    solver.set_nodes(model.nodes)
    solver.set_elements(model.elements)
    solver.set_boundary_conditions(model.boundary_conditions)
    solver.set_loads(model.loads)
    solver.set_material_properties(model.material)
    results = solver.solve()
    if results.get("status") != "completed":
        raise ValueError(f"von_mises solver failed: {results.get('error')}")

    summary = solver.get_results_summary()
    csv_path = results_csv_path(cfg, settings, "von_mises")
    _write_element_results_csv(csv_path, model, results)

    cfg["von_mises"] = {
        "status": results["status"],
        "num_elements": results["num_elements"],
        "num_nodes": results["num_nodes"],
        "max_stress": float(results["max_stress"]),
        "min_safety_factor": float(results["min_safety_factor"]),
        "n_critical": len(results["critical_elements"]),
        "max_displacement": summary["max_displacement"],
        "yield_strength": results["yield_strength"],
        "results_csv": display_path(csv_path),
    }
    return cfg


def _write_element_results_csv(
    path: Path, model: BeamModel, results: dict[str, Any]
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(
            [
                "element_id",
                "von_mises_stress",
                "safety_factor",
                "axial_stress",
                "bending_stress_top",
                "bending_stress_bottom",
            ]
        )
        for index, stresses in sorted(results["stresses"].items()):
            writer.writerow(
                [
                    model.elements[int(index)].elem_id,
                    stresses["von_mises"],
                    results["safety_factors"][index],
                    stresses["axial_stress"],
                    stresses["bending_stress_top"],
                    stresses["bending_stress_bottom"],
                ]
            )
