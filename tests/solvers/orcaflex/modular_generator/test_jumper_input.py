# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise specification-to-input generation without importing the solver."""

import os
from pathlib import Path
import subprocess
import sys

import pytest
import yaml


REPO_ROOT = Path(__file__).resolve().parents[4]
SPEC_DIR = REPO_ROOT / "docs/domains/orcaflex/subsea/jumper/installation"
SOURCE_SPECS = sorted(SPEC_DIR.glob("*/spec.yml"))


@pytest.mark.parametrize("source", SOURCE_SPECS, ids=["jumper-a", "jumper-b"])
def test_generation_and_validation_without_solver(source, tmp_path):
    """Fail on any solver import attempt in a fresh generation process."""
    script = """
import sys
attempts = []
class SolverImportDetector:
    def find_spec(self, fullname, path=None, target=None):
        if fullname in {"OrcFxAPI", "OrcFxAPIConfig"}:
            attempts.append(fullname)
            raise AssertionError("Licensed solver lookup attempted")
sys.meta_path.insert(0, SolverImportDetector())
def reject_solver_import(event, args):
    if event == "import" and args[0] in {"OrcFxAPI", "OrcFxAPIConfig"}:
        attempts.append(args[0])
        raise AssertionError("Licensed solver import attempted")
sys.addaudithook(reject_solver_import)
from digitalmodel.marine_ops.installation.jumper_input import generate_input
import digitalmodel.solvers.orcaflex as package
assert package.get_version() == package.__version__
assert "run-to-sim" in package.list_cli_commands()
assert "digitalmodel.solvers.orcaflex.run_to_sim" not in sys.modules
assert "digitalmodel.solvers.orcaflex.opp" not in sys.modules
generate_input(sys.argv[1], sys.argv[2], name="jumper-a")
assert not attempts, "Licensed solver import attempted"
assert "OrcFxAPI" not in sys.modules
assert "OrcFxAPIConfig" not in sys.modules
"""
    environment = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(key, None)
    subprocess.run(
        [sys.executable, "-c", script, str(source), str(tmp_path)],
        check=True,
        cwd=REPO_ROOT,
        env=environment,
    )
    master = yaml.safe_load((tmp_path / "master.yml").read_text())
    for entry in master:
        assert (tmp_path / entry["includefile"]).is_file()
    objects = yaml.safe_load(
        (tmp_path / "includes/20_generic_objects.yml").read_text()
    )
    line = objects["Lines"][0]
    rows = line[next(key for key in line if key.startswith("LineType,"))]
    assert len(rows) == 27
    assert all(row[1] > 0 for row in rows)
    assert {row[0] for row in rows} <= {
        item["Name"] for item in objects["LineTypes"]
    }
    parameters = yaml.safe_load((tmp_path / "inputs/parameters.yml").read_text())
    domain = yaml.safe_load(source.read_text())
    assert parameters["water_depth"] == domain["environment"]["water"]["depth"]
    assert line["Name"] == "jumper-a"


def test_validator_rejects_missing_include(tmp_path):
    """Require include existence rather than only parseable master YAML."""
    from digitalmodel.marine_ops.installation.jumper_input import validate_input

    (tmp_path / "master.yml").write_text("- includefile: includes/missing.yml\n")
    with pytest.raises(ValueError, match="Missing include"):
        validate_input(tmp_path)


def test_validator_rejects_invalid_yaml(tmp_path):
    """Reject solver-format errors in an otherwise parseable include."""
    from digitalmodel.marine_ops.installation.jumper_input import validate_input

    (tmp_path / "master.yml").write_text("- includefile: includes/objects.yml\n")
    (tmp_path / "includes").mkdir()
    (tmp_path / "includes/objects.yml").write_text("Lines: wrong\n")
    with pytest.raises(ValueError):
        validate_input(tmp_path)


def test_validator_rejects_include_escape(tmp_path):
    """Reject a master pointing outside its own model directory."""
    from digitalmodel.marine_ops.installation.jumper_input import validate_input

    (tmp_path / "master.yml").write_text("- includefile: ../outside.yml\n")
    with pytest.raises(ValueError, match="within the model directory"):
        validate_input(tmp_path)


@pytest.mark.parametrize("index", [0, 1], ids=["jumper-a", "jumper-b"])
def test_retained_input_matches_source(index, tmp_path):
    """Keep retained solver input consistent with its generation command."""
    from digitalmodel.marine_ops.installation.jumper_input import generate_input

    name = ["jumper-a", "jumper-b"][index]
    generate_input(SOURCE_SPECS[index], tmp_path, name=name)
    retained = SPEC_DIR / "generated" / name
    generated_files = {path.relative_to(tmp_path) for path in tmp_path.rglob("*.yml")}
    assert generated_files == {
        path.relative_to(retained) for path in retained.rglob("*.yml")
    }
    for relative in generated_files:
        assert yaml.safe_load((tmp_path / relative).read_text()) == yaml.safe_load(
            (retained / relative).read_text()
        )
