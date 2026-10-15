# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the resistance study's criteria before native computation starts."""

import json
from pathlib import Path


CRITERIA = Path(__file__).resolve().parents[3] / "config/cfd/calm_water_study.json"


def test_meshes_have_one_constant_refinement_ratio():
    criteria = json.loads(CRITERIA.read_text())
    scales = criteria["mesh_scales"]
    assert len(scales) == 3
    assert scales[1] / scales[0] == scales[2] / scales[1]
    assert criteria["grid_uncertainty"]["required_completed_meshes"] == 3


def test_comparison_requires_opened_tabulated_source():
    comparison = json.loads(CRITERIA.read_text())["comparison"]
    assert comparison["require_opened_source"] is True
    assert comparison["require_table_and_page"] is True
    assert comparison["plot_digitisation"] is False
    assert comparison["missing_source_result"] == "not available"


def test_native_runs_require_resource_and_convergence_evidence():
    criteria = json.loads(CRITERIA.read_text())
    assert criteria["ranks"] == 8
    assert criteria["resource_limits"]["memory_reserve_gb"] == 6
    assert criteria["resource_limits"]["minimum_disk_free_gb"] == 15
    assert criteria["resource_limits"]["total_projected_solver_hours"] == 80
    assert criteria["full_iterations"] == 25000
    assert criteria["averaging_iterations"] == 4000
