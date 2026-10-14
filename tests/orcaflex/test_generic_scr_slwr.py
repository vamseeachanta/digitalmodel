# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the generic steel-riser basis and its solver-free calibration inputs."""

import importlib.util
import json
import re
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

from digitalmodel.orcaflex.riser_config import RiserPipeProperties
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
)
from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator

ROOT = Path(__file__).resolve().parents[2]
CAMPAIGN = ROOT / "docs/domains/orcaflex/library/campaigns/generic-scr-slwr"


def load_basis():
    """Read the authoritative parameter records."""
    return yaml.safe_load((CAMPAIGN / "basis.yml").read_text())


def load_check():
    """Load the analytical reproducer without any solver interface."""
    path = ROOT / "scripts/orcaflex/check_generic_scr_slwr.py"
    spec = importlib.util.spec_from_file_location("generic_static_check", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_basis_has_provenance_for_every_parameter():
    """Missing sources and unexplained assumptions prevent basis qualification."""
    basis = load_basis()
    assert basis["readiness"] == "prepared; not qualified for design or outward use"
    assert basis["host"]["redistribution_status"] == "NO_VESSEL_TYPE_DATA_COMMITTED"
    for name, record in basis["parameters"].items():
        assert record["unit"], name
        assert record["source"] in basis["sources"], name
        assert record["status"] in ("SOURCED", "ASSUMED"), name
        if record["status"] == "ASSUMED":
            assert record["reason"], name
    assert all(c["code"] and c["title"] for c in basis["criteria"])
    assert all(c["limit"] is None for c in basis["criteria"])


@pytest.mark.parametrize("kind", ["scr", "slwr"])
def test_specs_match_basis_and_static_geometry(kind):
    """Pipe, contents, buoyancy, endpoints and section lengths share one basis."""
    basis = load_basis()
    p = {name: r["value"] for name, r in basis["parameters"].items()}
    spec = yaml.safe_load((CAMPAIGN / kind / "spec.yml").read_text())
    reference = load_check().calculate(basis)[kind]
    pipe = RiserPipeProperties(**{k: p[k] for k in (
        "outer_diameter", "wall_thickness", "coating_thickness",
        "coating_density", "contents_density", "corrosion_allowance",
        "seawater_density",
    )})
    assert spec["environment"]["water"]["depth"] == p["water_depth"]
    assert spec["environment"]["water"]["density"] == p["seawater_density"] / 1000
    assert spec["environment"]["current"]["speed"] == p["static_current_speed"]
    assert spec["environment"]["waves"]["height"] == p["extreme_hs"]
    assert spec["environment"]["waves"]["period"] == p["extreme_tp"]
    host = spec["riser"]["vessel"]
    assert host["type_name"] == basis["host"]["type_name"]
    assert host["length"] == p["host_length"]
    assert host["primary_motion"] == host["superimposed_motion"] == "None"
    line = spec["riser"]["lines"][0]
    assert line["contents"]["density"] == p["contents_density"] / 1000
    assert line["end_a"]["position"] == [0, 0, -p["hangoff_depth"]]
    assert line["end_a"]["declination"] == 180 - p[kind + "_angle"]
    assert line["end_a"]["bending_stiffness"] == p["joint_stiffness"]
    assert line["end_b"]["position"][0] == pytest.approx(
        reference["oo"]["touchdown_horizontal_distance_m"] + p["bottom_length"]
    )
    assert line["end_b"]["position"][2] == pipe.total_od / 2
    assert sum(s["length"] for s in line["sections"]) == pytest.approx(
        reference["oo"]["suspended_length_m"] + p["bottom_length"]
    )
    bare = spec["riser"]["line_types"][0]
    assert bare["outer_diameter"] == pipe.total_od
    assert bare["inner_diameter"] == pipe.inner_diameter
    assert bare["mass_per_length"] == pytest.approx(
        (pipe.steel_mass_per_m + pipe.coating_mass_per_m) / 1000
    )
    assert bare["axial_stiffness"] == pytest.approx(pipe.axial_stiffness / 1000)
    assert bare["bending_stiffness"] == pytest.approx(pipe.bending_stiffness / 1000)
    if kind == "slwr":
        floats = spec["riser"]["line_types"][1]
        volume = reference["buoyancy"]["volume_per_m_m2"]
        assert floats["outer_diameter"] == pytest.approx(reference["buoyancy"]["effective_od_m"])
        assert floats["mass_per_length"] == pytest.approx(
            bare["mass_per_length"] + volume * p["module_density"] / 1000
        )
        assert line["sections"][1]["length"] == pytest.approx(reference["sections_m"][1])


@pytest.mark.parametrize("kind", ["scr", "slwr"])
def test_models_validate_and_reproduce(kind, tmp_path):
    """Fresh and previously loaded processes reproduce the model's YAML values."""
    model = CAMPAIGN / kind / "modular"
    result = OrcaFlexYAMLValidator().validate_file(model / "master.yml")
    assert result.valid, result.errors
    assert not PostGenerationValidator().validate_directory(model)
    environment = yaml.safe_load((model / "includes/03_environment.yml").read_text())
    train = environment["Environment"]["WaveTrains"][0]
    assert train["WaveTp"] == load_basis()["parameters"]["extreme_tp"]["value"]
    assert "WaveTz" not in train
    generated = tmp_path / kind
    command = (
        "import sys; "
        "from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator; "
        "ModularModelGenerator(sys.argv[1]).generate(sys.argv[2])"
    )
    subprocess.run([sys.executable, "-c", command,
                    str(CAMPAIGN / kind / "spec.yml"), str(generated)], check=True)
    expected = {p.relative_to(model) for p in model.rglob("*.yml")}
    assert expected == {p.relative_to(generated) for p in generated.rglob("*.yml")}
    loaded = tmp_path / (kind + "-loaded")
    # A prior import may register PyYAML's incompatible null spelling globally.
    prior_import = (
        "import yaml; "
        "yaml.add_representer(type(None), lambda dumper, value: "
        "dumper.represent_scalar('tag:yaml.org,2002:null', 'null')); "
    )
    subprocess.run([sys.executable, "-c", prior_import + command,
                    str(CAMPAIGN / kind / "spec.yml"), str(loaded)], check=True)
    assert expected == {p.relative_to(loaded) for p in loaded.rglob("*.yml")}
    for path in expected:
        retained = (model / path).read_bytes()
        for directory in (generated, loaded):
            raw = (directory / path).read_bytes()
            assert yaml.safe_load(retained) == yaml.safe_load(raw), path
            assert not re.search(rb"(?<!\w)null(?!\w)", raw), path
            if path == Path("master.yml"):
                # The master is written directly, without a YAML dumper.
                # The EOF hook adds its missing final newline.
                canonical = raw if raw.endswith(b"\n") else raw + b"\n"
                assert retained == canonical


def test_analytical_reference_reproduces_from_basis():
    """Both kernel results and model digests are checked against retained bytes."""
    expected = json.loads((CAMPAIGN / "static-reference.json").read_text())
    assert expected == load_check().reference(CAMPAIGN)
    for kind in ("scr", "slwr"):
        result = expected["models"][kind]
        assert result["oo"]["top_tension_kN"] > 0
        assert result["general"]["top_tension_kN"] > 0
        assert result["top_tension_difference_pct"] <= 2
        assert result["general"]["converged"]


def test_flooded_weight_and_module_net_uplift():
    """The operating/flooded distinction and upward buoyed weight are explicit."""
    result = load_check().calculate(load_basis())
    assert result["slwr"]["buoyancy"]["net_weight_N_per_m"] < 0
    assert result["flooded_weight_N_per_m"] > result["operating_weight_N_per_m"]
