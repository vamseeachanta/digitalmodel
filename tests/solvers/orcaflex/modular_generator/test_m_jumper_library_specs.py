"""L1 regression tests for the literature-derived M-shaped jumper library
specs (issue #722).

Locks, for each mj* model_library entry:
  - the spec validates against ProjectInputSpec (generic track)
  - generation produces master.yml + includes with no OrcFxAPI/license
  - every generated YAML file is strict-YAML validator clean
  - master.yml references only include files that actually exist
    (regression for the CLI master.yml dangling-includefile bug)
  - the line section chain matches the published dimension chain
    (section count and total centerline length)

Published natural frequencies (the L2/L3 validation targets) are recorded
in each spec header — comparing them against licensed OrcaFlex runs is
#718/#719 scope, deliberately NOT asserted here.

CI command:
    uv run pytest tests/solvers/orcaflex/modular_generator/test_m_jumper_library_specs.py -q
"""
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema.root import ProjectInputSpec
from digitalmodel.solvers.orcaflex.yaml_validator import validate_orcaflex_yaml

REPO_ROOT = Path(__file__).resolve().parents[4]
LIBRARY = REPO_ROOT / "docs/domains/orcaflex/library/model_library"

# (dir name, expected section count, expected centerline length, tolerance)
MJ_CASES = [
    ("mj01_exxonmobil_m_jumper_viv", 13.966, 7),
    ("mj02_zhu2022_m_jumper_fullscale", 34.609, 13),
    ("mj03_li2024_m_jumper_lab", 7.0968, 15),
]
IDS = [c[0] for c in MJ_CASES]


def _load_spec_dict(name: str) -> dict:
    spec_path = LIBRARY / name / "spec.yml"
    assert spec_path.exists(), f"missing spec: {spec_path}"
    return yaml.safe_load(spec_path.read_text())


@pytest.mark.parametrize("name,total_length,n_sections", MJ_CASES, ids=IDS)
def test_spec_validates_as_generic_project_input(name, total_length, n_sections):
    spec = ProjectInputSpec(**_load_spec_dict(name))
    assert spec.generic is not None
    assert len(spec.generic.line_types) == 1
    assert len(spec.generic.lines) == 1


@pytest.mark.parametrize("name,total_length,n_sections", MJ_CASES, ids=IDS)
def test_generation_is_validator_clean(name, total_length, n_sections, tmp_path):
    out_dir = tmp_path / name
    generator = ModularModelGenerator(LIBRARY / name / "spec.yml")
    generator.generate(out_dir)

    master = out_dir / "master.yml"
    assert master.exists()

    for f in sorted(out_dir.rglob("*.yml")):
        result = validate_orcaflex_yaml(f)
        errors = [i.message for i in result.errors]
        assert not errors, f"{f.relative_to(out_dir)}: {errors}"

    # master.yml must reference only include files that exist
    for entry in yaml.safe_load(master.read_text()):
        ref = out_dir / entry["includefile"]
        assert ref.exists(), f"dangling includefile: {entry['includefile']}"


@pytest.mark.parametrize("name,total_length,n_sections", MJ_CASES, ids=IDS)
def test_section_chain_matches_published_dimensions(
    name, total_length, n_sections, tmp_path
):
    out_dir = tmp_path / name
    ModularModelGenerator(LIBRARY / name / "spec.yml").generate(out_dir)

    objects = yaml.safe_load((out_dir / "includes/20_generic_objects.yml").read_text())
    lines = objects["Lines"]
    assert len(lines) == 1
    line = lines[0]

    section_key = next(k for k in line if k.startswith("LineType,"))
    sections = line[section_key]
    assert len(sections) == n_sections
    assert sum(row[1] for row in sections) == pytest.approx(total_length, abs=1e-3)

    # every section's line type resolves to a LineType definition
    type_names = {lt["Name"] for lt in objects["LineTypes"]}
    assert {row[0] for row in sections} <= type_names


def test_cli_generate_master_has_no_dangling_includes(tmp_path):
    """The CLI generate path must list only includes it actually wrote.

    Regression: cmd_generate previously wrote master.yml from the full
    builder registry, so generic-track models referenced ~16 include
    files that were never created — a guaranteed OrcFxAPI load failure.
    (The ModularModelGenerator API path was always correct; the CLI
    duplicated the logic.)
    """
    import argparse

    from digitalmodel.solvers.orcaflex.modular_generator.cli import cmd_generate

    out_dir = tmp_path / "mj01_cli"
    args = argparse.Namespace(
        input=str(LIBRARY / "mj01_exxonmobil_m_jumper_viv/spec.yml"),
        output=str(out_dir),
    )
    assert cmd_generate(args) == 0

    master_entries = yaml.safe_load((out_dir / "master.yml").read_text())
    assert master_entries, "master.yml lists no includes"
    for entry in master_entries:
        ref = out_dir / entry["includefile"]
        assert ref.exists(), f"dangling includefile: {entry['includefile']}"
