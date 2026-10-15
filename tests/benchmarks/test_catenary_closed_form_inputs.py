# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline contracts for the closed-form catenary inputs."""

import hashlib
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys

import yaml

from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
)
from digitalmodel.solvers.orcaflex.yaml_utils import orcaflex_load

ROOT = Path(__file__).resolve().parents[2]
DATA = Path(os.environ.get(
    "CATENARY_BENCHMARK_DIR", ROOT / "docs/benchmarks/catenary_closed_form"
))
QUANTITIES = {
    "top_tension_kN", "top_angle_deg_from_vertical",
    "touchdown_horizontal_distance_from_top_m",
}


def benchmark():
    """Read the machine-readable reference."""
    return json.loads((DATA / "benchmark.json").read_text())


def models():
    """Read each model using the repository loader."""
    for case in benchmark()["cases"]:
        path = DATA / "inputs" / Path(case["model_file"]).name
        yield case, path, orcaflex_load(path)[0]


def test_benchmark_schema_and_band():
    """Keep the inputs, exported quantities and acceptance band explicit."""
    data = benchmark()
    assert set(data) == {
        "benchmark", "units", "cases", "calibrates", "results_to_export", "sources"
    }
    assert data["benchmark"] == "catenary_closed_form"
    assert data["units"] == {"force": "kN", "length": "m", "angle": "degrees"}
    assert [c["case"] for c in data["cases"]] == ["C1", "C2"]
    common = {
        "case", "description", "inputs", "governing_form", "top_strain_percent",
        "model_file", "expected",
    }
    for c in data["cases"]:
        extra = {"plain_form_top_tension_kN", "plain_form_excess_percent"}
        assert set(c) == (common | extra if c["case"] == "C2" else common)
        assert set(c["inputs"]) == {
            "w_N_per_m", "EA_N", "L_m", "h_m", "X_m", "outer_diameter_m",
            "mass_te_per_m", "EA_kN", "EI_kN_m2",
        }
        assert set(c["expected"]) == QUANTITIES
        assert all(isinstance(v, (int, float)) for v in c["inputs"].values())
        assert all(isinstance(v, (int, float)) for v in c["expected"].values())
    assert data["calibrates"] == {
        "description": "static hand-calculation check of a single line against the closed-form catenary",
        "band": {
            "top_tension": {"kind": "relative", "percent": 5.0},
            "top_angle": {"kind": "absolute", "degrees": 1.0},
            "touchdown_horizontal_distance": {"kind": "relative", "percent": 5.0},
        },
        "applies_to": "each case, program result against the expected value",
    }
    assert len(data["results_to_export"]) == 3
    assert len(data["sources"]) == 2
    assert "03143504bb05ed7ad4a71a3288b3826b8a521046" in str(data["sources"])


def test_hand_values_and_self_checks():
    """Recompute both references and the independent integration checks."""
    output = subprocess.check_output(
        [sys.executable, str(DATA / "catenary_hand_values.py")], text=True
    )
    values = json.loads(output)
    assert values["benchmark"] == benchmark()
    assert [c["governing_form"] for c in values["benchmark"]["cases"]] == [
        "plain", "elastic"
    ]
    for c in values["computed"]:
        expected = next(v["expected"] for v in benchmark()["cases"] if v["case"] == c["case"])
        for key in QUANTITIES:
            assert round(c[key], 3) == expected[key]
    for check in values["self_checks"]:
        assert check["large_EA_H_relative_difference"] < 1e-9
        assert check["rk4_steps"] >= 20000
        assert check["rk4_tension_relative_difference"] < 1e-9
        assert check["rk4_angle_difference_deg"] < 1e-9


def test_models_parse_with_repository_loader():
    """Require a self-contained mapping with exactly one line and type."""
    parsed = list(models())
    assert len(parsed) == 2
    for _, _, data in parsed:
        assert set(data) == {"General", "Environment", "LineTypes", "Lines"}
        assert len(data["Lines"]) == len(data["LineTypes"]) == 1


def test_text_conventions_and_post_validation():
    """Check empty scalars, mode ordering and generator references."""
    for _, path, data in models():
        text = path.read_text()
        # Structural mappings have no inline value; only blank scalar leaves
        # are rejected. Quoted empty script text is distinct from a default.
        assert not re.search(r"\b(?:null|None)\b", text)
        assert "~" in text

        def check_scalar_nodes(node):
            if isinstance(node, yaml.ScalarNode):
                assert node.value != "" or node.style is not None
            elif isinstance(node, yaml.MappingNode):
                for key, value in node.value:
                    check_scalar_nodes(key)
                    check_scalar_nodes(value)
            elif isinstance(node, yaml.SequenceNode):
                for value in node.value:
                    check_scalar_nodes(value)

        check_scalar_nodes(yaml.compose(text))

        def walk(node):
            if isinstance(node, dict):
                keys = list(node)
                pairs = {
                    "UnitsSystem": ("g",),
                    "SeabedType": ("SeabedOriginDepth", "SeabedSlope"),
                    "SeabedModel": ("SeabedNormalStiffness", "SeabedShearStiffness"),
                    "VerticalCurrentVariationMethod": ("RefCurrentSpeed", "CurrentDepth, CurrentFactor, CurrentRotation"),
                    "WindType": ("WindSpeed",),
                    "WaveType": ("WaveHeight", "WavePeriod"),
                    "Category": ("OD", "EI", "EA", "MassPerUnitLength"),
                    "IncludeTorsion": ("ConnectionxBendingStiffness, ConnectionyBendingStiffness",),
                    "EndAConnection": ("EndAX", "EndAY", "EndAZ"),
                    "EndBConnection": ("EndBX", "EndBY", "EndBZ"),
                    "ContentsMethod": ("ContentsDensity",),
                    "DynamicsSolutionMethod": ("ImplicitConstantTimeStep",),
                }
                for selector, dependents in pairs.items():
                    for dependent in dependents:
                        if dependent in keys:
                            assert selector in keys
                            assert keys.index(selector) < keys.index(dependent)
                for value in node.values():
                    walk(value)
            elif isinstance(node, list):
                for value in node:
                    walk(value)
        walk(data)
        assert PostGenerationValidator().validate_data({path.name: data}) == []


def test_model_settings():
    """Read back geometry, material properties and the still environment."""
    for case, _, data in models():
        env, general = data["Environment"], data["General"]
        line, lt = data["Lines"][0], data["LineTypes"][0]
        inputs = case["inputs"]
        assert general["UnitsSystem"] == "SI"
        assert general["g"] == 9.80665
        assert env["WaterSurfaceZ"] == 0
        assert env["SeabedOriginDepth"] == 150
        assert env["Density"] == 1.025
        assert env["SeabedType"] == "Flat"
        assert env["SeabedSlope"] == 0
        assert env["SeabedNormalStiffness"] == 0
        assert env["RefCurrentSpeed"] == env["WindSpeed"] == 0
        assert all(w["WaveType"] == "No waves" for w in env["WaveTrains"])
        assert line["EndAConnection"] == line["EndBConnection"] == "Fixed"
        assert [line[f"EndA{a}"] for a in "XYZ"] == [0, 0, -150]
        assert [line[f"EndB{a}"] for a in "XYZ"] == [inputs["X_m"], 0, 0]
        assert line["LineType, Length, TargetSegmentLength"] == [[lt["Name"], 600, 1]]
        assert line["IncludeTorsion"] == "No"
        assert line["StaticsStep1"] == "Catenary"
        assert line["StaticsStep2"] == "Full statics"
        assert line["ConnectionxBendingStiffness, ConnectionyBendingStiffness"] == [[0, None], [0, None]]
        assert lt["EA"] == inputs["EA_kN"]
        assert lt["EI"] == inputs["EI_kN_m2"] == 0
        assert lt["OD"] == inputs["outer_diameter_m"]
        assert lt["MassPerUnitLength"] == inputs["mass_te_per_m"]
        assert lt["SeabedLateralFrictionCoefficient"] == 0
        assert lt["SeabedAxialFrictionCoefficient"] == 0


def test_submerged_weight():
    """Check the rounded mass specification against each nominal weight."""
    for case, _, data in models():
        lt = data["LineTypes"][0]
        w = (lt["MassPerUnitLength"] - data["Environment"]["Density"] * math.pi * lt["OD"] ** 2 / 4) * data["General"]["g"]
        nominal = {"C1": 1.0, "C2": 0.2}[case["case"]]
        assert abs(w / nominal - 1) < 1e-4


def test_manifest_digests():
    """Bind the regeneration command to the actual model bytes."""
    manifest = json.loads((DATA / "inputs/MANIFEST.json").read_text())
    paths = {c["model_file"] for c in benchmark()["cases"]}
    assert {m["path"] for m in manifest["files"]} == paths
    for entry in manifest["files"]:
        payload = (DATA / "inputs" / Path(entry["path"]).name).read_bytes()
        assert len(payload) == entry["bytes"]
        assert hashlib.sha256(payload).hexdigest() == entry["sha256"]
        assert entry["command"] == "PYTHONPATH=src uv run --no-sources python scripts/benchmark/build_catenary_closed_form_inputs.py"


def test_benchmark_file_sizes():
    """Keep all reference assets below the text-input size limit."""
    files = [p for p in DATA.rglob("*") if p.is_file()]
    assert files
    assert all(p.stat().st_size <= 1_000_000 for p in files)
