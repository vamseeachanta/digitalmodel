"""Regression tests for the Ballymore spec-to-modular pipeline (#602, #603).

Locks the path:

    domain spec.yml -> jumper calculations -> build_modular_spec()
        -> ProjectInputSpec -> ModularModelGenerator -> master.yml/includes

Runs green on unlicensed Linux: no OrcFxAPI import or license is required
anywhere in this path (generation is pure YAML emission).

CI command:
    uv run pytest tests/solvers/orcaflex/modular_generator/test_ballymore_jumper_pipeline.py -q
"""
from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.marine_ops.installation.jumper_to_modular_spec import (
    LT_BUOY,
    LT_COATED,
    LT_CONNECTOR,
    LT_STRAKE,
    build_line_types,
    build_modular_spec,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema.root import ProjectInputSpec
from digitalmodel.solvers.orcaflex.yaml_validator import validate_orcaflex_yaml

REPO_ROOT = Path(__file__).resolve().parents[4]
SPEC_DIR = REPO_ROOT / "docs/domains/orcaflex/subsea/jumper/installation"
BALLYMORE = ["ballymore_mf_plet", "ballymore_plet_plem"]

# 27 sections totalling 71.6417 m of pipe + 2 x 1.3 m connectors.
EXPECTED_SECTIONS = 27
EXPECTED_TOTAL_M = 74.242


def _spec_path(name: str) -> Path:
    return SPEC_DIR / name / "spec.yml"


def _sections(data: dict) -> list:
    props = data["generic"]["lines"][0]["properties"]
    key = next(k for k in props if k.startswith("LineType,"))
    return props[key]


@pytest.mark.parametrize("name", BALLYMORE)
def test_domain_spec_is_not_directly_generator_ready(name):
    """Negative guard from #602: the domain spec is a calculation spec, not a
    ProjectInputSpec — direct validation must fail with the model-type error."""
    raw = yaml.safe_load(_spec_path(name).read_text())
    with pytest.raises(ValidationError, match="pipeline.*riser.*mooring.*generic"):
        ProjectInputSpec(**raw)


@pytest.mark.parametrize("name", BALLYMORE)
def test_build_modular_spec_is_generator_ready(name):
    data = build_modular_spec(str(_spec_path(name)))

    sections = _sections(data)
    assert len(sections) == EXPECTED_SECTIONS
    assert sum(row[1] for row in sections) == pytest.approx(EXPECTED_TOTAL_M, abs=0.01)

    # every section line type resolves to a definition
    type_names = {lt["name"] for lt in data["generic"]["line_types"]}
    assert {row[0] for row in sections} <= type_names
    assert type_names == {LT_COATED, LT_STRAKE, LT_BUOY, LT_CONNECTOR}

    # environment carried over from the domain spec — assert via the parsed
    # model, NOT the input dict: the Waves before-validator mutates the
    # caller's dict (flat keys are popped into trains[0]). Expected values
    # come from each domain spec itself (plet_plem has no metocean wave
    # block yet, pending #480 workbook verification).
    domain = yaml.safe_load(_spec_path(name).read_text())
    wave_in = domain.get("environment", {}).get("metocean", {}).get("wave", {})
    spec = ProjectInputSpec(**data)  # must not raise
    assert spec.generic is not None
    assert spec.environment.water.depth == domain["environment"]["water"]["depth"]
    assert spec.environment.waves.height == float(wave_in.get("significant", 0))


@pytest.mark.parametrize("name", BALLYMORE)
def test_generation_writes_master_and_includes(name, tmp_path):
    data = build_modular_spec(str(_spec_path(name)))
    spec_file = tmp_path / "spec.yml"
    spec_file.write_text(yaml.safe_dump(data, sort_keys=False))

    out_dir = tmp_path / "model"
    ModularModelGenerator(spec_file).generate(out_dir)

    assert (out_dir / "master.yml").exists()
    assert (out_dir / "inputs/parameters.yml").exists()
    assert (out_dir / "includes/20_generic_objects.yml").exists()

    for f in sorted(out_dir.rglob("*.yml")):
        parsed = yaml.safe_load(f.read_text())  # must parse
        assert parsed is not None
        result = validate_orcaflex_yaml(f)
        errors = [i.message for i in result.errors]
        assert not errors, f"{f.relative_to(out_dir)}: {errors}"

    # master references only includes that exist
    for entry in yaml.safe_load((out_dir / "master.yml").read_text()):
        assert (out_dir / entry["includefile"]).exists()

    # generated model carries the 27 jumper sections
    objects = yaml.safe_load(
        (out_dir / "includes/20_generic_objects.yml").read_text()
    )
    line = objects["Lines"][0]
    key = next(k for k in line if k.startswith("LineType,"))
    assert len(line[key]) == EXPECTED_SECTIONS
    lt_names = {lt["Name"] for lt in objects["LineTypes"]}
    assert {row[0] for row in line[key]} <= lt_names


def test_configurations_currently_identical_pending_workbook():
    """PLET-PLEM segment lengths still mirror MF-PLET (KNOWN_JUMPER_CONFIGS
    TODO pending workbook verification, #480/#481). This test documents the
    sameness per #602 acceptance; when #480 lands distinct lengths, flip the
    assertion to inequality."""
    a = build_modular_spec(str(_spec_path("ballymore_mf_plet")))
    b = build_modular_spec(str(_spec_path("ballymore_plet_plem")))
    assert _sections(a) == _sections(b)
    # names stay distinct even while geometry matches
    assert a["metadata"]["name"] != b["metadata"]["name"]


def test_line_type_properties_traceable_to_jumper_lift():
    """Spot-check the physics chain from jumper_lift into the line types."""
    line_types = {lt["name"]: lt for lt in build_line_types()}

    coated = line_types[LT_COATED]
    assert coated["outer_diameter"] == pytest.approx(0.27305)
    assert coated["properties"]["CoatingThickness"] == pytest.approx(0.0762)
    assert coated["properties"]["CoatingMaterialDensity"] == pytest.approx(
        0.97873, abs=1e-4
    )

    # OCS 200-V mass from connector weight/length (1678.5 kg / 1.3 m)
    connector = line_types[LT_CONNECTOR]
    assert connector["mass_per_length"] == pytest.approx(1.29115, abs=1e-4)

    # smeared buoyancy coating spans pipe OD to module hydro OD
    buoy = line_types[LT_BUOY]
    assert buoy["properties"]["CoatingThickness"] == pytest.approx(
        (1.11117 - 0.27305) / 2, abs=1e-4
    )
    # smeared density must be lighter than insulation alone (buoyant kit)
    assert (
        buoy["properties"]["CoatingMaterialDensity"]
        < coated["properties"]["CoatingMaterialDensity"]
    )
