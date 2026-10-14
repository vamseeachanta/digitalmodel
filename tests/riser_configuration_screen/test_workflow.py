# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Ranked-table and validity-boundary contracts."""

import importlib
import json

import pytest


def workflow():
    assert importlib.util.find_spec("digitalmodel.riser_configuration_screen.workflow") is not None
    return importlib.import_module("digitalmodel.riser_configuration_screen.workflow")


def settings(kind="SCR"):
    return {
        "riser_type": kind, "water_depth_m": 1500.0,
        "submerged_weight_n_m": 1000.0, "grounded_length_m": 500.0,
        "offsets_m": [-50.0, 0.0, 50.0],
        "pipe": {"outer_diameter_m": 0.3, "wall_thickness_m": 0.025,
                 "youngs_modulus_pa": 2.07e11, "yield_strength_pa": 4.5e8,
                 "internal_pressure_pa": 0.0, "external_pressure_pa": 0.0},
        "constraints": {"max_top_tension_n": 1e7, "min_bend_radius_m": 1.0,
                        "min_hog_clearance_m": 1.0, "max_static_utilisation": 10.0},
        "axes": {"hangoff_angle_deg": [8.0, 20.0]},
        "provisional": True,
    }


def test_two_configuration_ranking():
    result = workflow().screen(settings())
    assert [row["configuration"]["hangoff_angle_deg"] for row in result["ranked_candidates"]] == [20.0, 8.0]
    assert result["ranked_candidates"][0]["curvature_range_per_m"] < result["ranked_candidates"][1]["curvature_range_per_m"]


def test_violation_is_retained_with_margin():
    data = settings("SLWR")
    data["axes"] = {"hangoff_angle_deg": [10.0], "buoyancy_factor": [0.5, 2.0],
                    "sag_fraction": [0.1], "hog_fraction": [0.4]}
    result = workflow().screen(data)
    assert len(result["ranked_candidates"]) == 2
    bad = next(row for row in result["ranked_candidates"] if not row["feasible"])
    assert bad["constraints"]["net_buoyancy"]["margin"] == -0.5
    assert bad["curvature_range_per_m"] is None
    assert len(result["solver_candidates"]) == 1


def test_tables_keep_screen_boundary(tmp_path):
    cfg = {"riser_configuration_screen": settings(), "Analysis": {"result_folder": str(tmp_path)}}
    workflow().router(cfg)
    result = json.loads((tmp_path / "ranked_candidates.json").read_text())
    assert result["tier"] == "SCREEN"
    assert "not computed" in result["fatigue_statement"]
    assert result["provisional"] is True
    assert len(result["solver_candidates"]) == 2
    csv = (tmp_path / "ranked_candidates.csv").read_text()
    assert "SCREEN" in csv and "not computed" in csv
    assert "constraints" in csv
    assert result["selection_rule"]


def test_zero_excursion_flat_objective_stops():
    data = settings()
    data["offsets_m"] = [0.0]
    with pytest.raises(ValueError, match="flat"):
        workflow().screen(data)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -1.0])
def test_nonfinite_or_negative_depth_is_rejected(value):
    data = settings()
    data["water_depth_m"] = value
    with pytest.raises(ValueError, match="water_depth"):
        workflow().screen(data)


def test_cli_avoids_licensed_interface(tmp_path):
    import subprocess
    import sys
    import yaml
    cfg = {"basename": "riser_configuration_screen", "riser_configuration_screen": settings(),
           "Analysis": {"result_folder": str(tmp_path / "output")}}
    source = tmp_path / "input.yml"
    source.write_text(yaml.safe_dump(cfg))
    script = """
import runpy, sys

def guard(event, arguments):
    if event == 'import' and arguments[0].startswith('OrcFxAPI'):
        raise AssertionError('licensed interface import attempted')
sys.addaudithook(guard)
sys.argv = ['digitalmodel', sys.argv[1]]
runpy.run_module('digitalmodel', run_name='__main__')
"""
    result = subprocess.run([sys.executable, "-c", script, str(source)], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    assert (tmp_path / "output" / "ranked_candidates.json").exists()


def test_neutral_buoyancy_is_reported_as_a_violation():
    data = settings("SLWR")
    data["axes"] = {"hangoff_angle_deg": [10.0], "buoyancy_factor": [1.0],
                    "sag_fraction": [0.1], "hog_fraction": [0.4]}
    row = workflow().screen(data)["ranked_candidates"][0]
    assert row["constraints"]["net_buoyancy"]["status"] == "violation"
    assert row["constraints"]["net_buoyancy"]["margin"] == 0.0
