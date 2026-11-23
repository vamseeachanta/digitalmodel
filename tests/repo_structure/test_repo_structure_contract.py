# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Repository structure rules, including deliberately planted violations."""

import copy
import importlib.util
import os
import shlex
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/maintenance/verify_repo_structure.py"


def git_environment():
    return {key: value for key, value in os.environ.items()
            if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}


def checker():
    spec = importlib.util.spec_from_file_location("structure_checker", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def config():
    return yaml.safe_load((ROOT / "config/repo_structure.yml").read_text())


def test_repo_structure_config_loads():
    assert config()["version"] == 1
    assert checker().validate_config(config()) == []


def test_current_control_files_and_checker_artifacts_pass():
    paths = subprocess.check_output(
        ["git", "ls-tree", "-r", "--name-only", "HEAD"],
        cwd=ROOT, env=git_environment(), text=True,
    ).splitlines()
    paths += [
        "docs/standards/repo-structure.md", "config/repo_structure.yml",
        "scripts/maintenance/verify_repo_structure.py",
        "tests/repo_structure/test_repo_structure_contract.py",
    ]
    assert checker().check_paths(paths, config()) == []


@pytest.mark.parametrize("path", [
    "_count_tests.py", "_coverage_map.py", "orcaflex_maturity_analysis.py",
    "surprise/file.txt", "src/digitalmodel/tests/test_bad.py", "src/tests/bad.py",
    "notebooks/new.ipynb", "outputs/new/result.json", "coverage.xml",
    "reports/quality_gates_results.json", "dist/new.whl", "../escape", "/escape",
])
def test_planted_violations_fail(path):
    assert checker().check_paths([path], config())


def exception():
    return {
        "pattern": "outputs/retained/**", "maintainer": "engineering",
        "reason": "Retained evidence pending a per-file retention decision.",
        "category": "temporary_durable_generated_evidence",
        "follow_up_issue_or_permanent_justification": "#597",
        "reviewed_on": "2026-10-14",
    }


def test_generated_evidence_requires_complete_metadata():
    rules = copy.deepcopy(config())
    rules["durable_exceptions"] = [exception()]
    assert checker().check_paths(["outputs/retained/result.json"], rules) == []
    assert checker().check_paths(["outputs/another/result.json"], rules)
    for field in rules["required_metadata_for_exceptions"]:
        bad = copy.deepcopy(rules)
        del bad["durable_exceptions"][0][field]
        assert checker().check_paths(["outputs/retained/result.json"], bad)


@pytest.mark.parametrize("value", ["TODO", "TBD", "pending", "Create/link follow-up", "", "#0"])
def test_exception_follow_up_rejects_placeholders(value):
    rules = copy.deepcopy(config())
    row = exception()
    row["follow_up_issue_or_permanent_justification"] = value
    rules["durable_exceptions"] = [row]
    assert checker().validate_config(rules)


def test_invalid_review_date_and_permanent_justification():
    rules = copy.deepcopy(config())
    row = exception()
    row["reviewed_on"] = "2026-02-30"
    rules["durable_exceptions"] = [row]
    assert checker().validate_config(rules)
    row["reviewed_on"] = "2026-10-14"
    row["category"] = "permanent_durable_generated_evidence"
    row["follow_up_issue_or_permanent_justification"] = "permanent: TODO"
    assert checker().validate_config(rules)
    row["follow_up_issue_or_permanent_justification"] = (
        "permanent: Small deterministic reference fixture required for regression tests."
    )
    assert checker().validate_config(rules) == []


def test_current_inventory_has_no_generated_exception_to_grandfather():
    assert config()["durable_exceptions"] == []
    assert config()["allowed_tracked_notebooks"] == ['docs/domains/wind/uswtdbApiMapVizExamples.ipynb', 'docs/notebooks/fatigue-sn-library-demo.ipynb', 'docs/notebooks/freespan-viv-demo.ipynb', 'examples/domains/fpso/fpso_mooring_analysis.ipynb', 'examples/domains/hydrodynamics/hydro_coefficients_example.ipynb', 'examples/domains/ocimf/ocimf_visualization_example.ipynb', 'examples/domains/tutorials/01_getting_started.ipynb', 'examples/domains/tutorials/02_catenary_deep_dive.ipynb', 'notebooks/gis/GIS_2013-12-16-UTM_conv.ipynb']


def test_cli_clean_and_violation(tmp_path):
    env = git_environment()
    subprocess.run(["git", "init", "-q", str(tmp_path)], env=env, check=True)
    (tmp_path / "README.md").write_text("fixture\n")
    command = [sys.executable, str(SCRIPT), "--repo", str(tmp_path),
               "--config", str(ROOT / "config/repo_structure.yml")]
    clean = subprocess.run(command, env=env, capture_output=True, text=True)
    assert clean.returncode == 0, clean.stdout + clean.stderr
    (tmp_path / "unexpected.py").write_text("pass\n")
    bad = subprocess.run(command, env=env, capture_output=True, text=True)
    assert bad.returncode == 1
    assert "unexpected.py: root-allowlist" in bad.stdout


def test_structure_tests_run_in_contracts_domain():
    gates = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())["gates"]
    gate = gates["tests-contracts"]
    root = "tests/repo_structure/"
    assert (ROOT / root).is_dir()
    assert gate["domain"] == "contracts"
    assert gate["enabled"] is True
    for entries in (gate["test_roots"], shlex.split(gate["command"])):
        assert entries.count(root) == 1
        assert entries[entries.index("tests/docs/") + 1] == root
    registrations = [name for name, row in gates.items()
                     if root in row.get("test_roots", [])
                     or root in shlex.split(row.get("command", ""))]
    assert registrations == ["tests-contracts"]


@pytest.mark.parametrize("version", [True, 1.0, "1", None])
def test_schema_version_requires_an_integer(version):
    rules = config()
    rules["version"] = version
    assert checker().validate_config(rules) == ["config: schema-version"]


@pytest.mark.parametrize("value", [[{}], [[]], [None], ["maintainer", "maintainer"]])
def test_required_metadata_is_a_unique_list_of_field_names(value):
    rules = config()
    rules["required_metadata_for_exceptions"] = value
    assert checker().validate_config(rules) == ["config: exception-required-fields"]


def test_required_metadata_cannot_repeat_an_otherwise_complete_field():
    rules = config()
    rules["required_metadata_for_exceptions"].append("maintainer")
    assert checker().validate_config(rules) == ["config: exception-required-fields"]


@pytest.mark.parametrize("value", ["none", "N/A", "not applicable", "-", "?"])
@pytest.mark.parametrize("field", ["maintainer", "reason"])
def test_exception_metadata_rejects_empty_meaning(field, value):
    rules = config()
    row = exception()
    row[field] = value
    rules["durable_exceptions"] = [row]
    assert f"exception[1]: {field}" in checker().validate_config(rules)


def test_permanent_justification_length_excludes_padding():
    rules = config()
    row = exception()
    row["category"] = "permanent_durable_generated_evidence"
    row["follow_up_issue_or_permanent_justification"] = "permanent: fixture" + " " * 80
    rules["durable_exceptions"] = [row]
    assert "exception[1]: permanent-justification" in checker().validate_config(rules)


@pytest.mark.parametrize("path", ["C:/escape", "src/", "src/file\nname.py", "src/file\x00.py"])
def test_paths_are_normalized_and_portable(path):
    assert checker().check_paths([path], config()) == ["<invalid-path>: relative-path"]


@pytest.mark.parametrize("case", ["top-level", "nested", "merged", "unhashable"])
def test_cli_rejects_duplicate_yaml_keys(tmp_path, case):
    """A second key must not replace either a rule list or a nested allowlist."""
    env = git_environment()
    subprocess.run(["git", "init", "-q", str(tmp_path)], env=env, check=True)
    contract = tmp_path / "contract.yml"
    text = yaml.safe_dump(config())
    if case == "top-level":
        text += "\ngenerated_patterns: []\n"
    elif case == "nested":
        text = text.replace("root_allowlist:\n", "root_allowlist:\n  files: []\n")
    elif case == "merged":
        text = text.replace("root_allowlist:\n", "root_allowlist:\n  <<: {files: []}\n")
    else:
        text += "\n? [invalid]\n: value\n"
    contract.write_text(text)
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--repo", str(tmp_path), "--config", str(contract)],
        env=env, capture_output=True, text=True,
    )
    assert result.returncode == 2
    assert result.stderr == "structure: cannot read repository paths or contract\n"
    assert "Traceback" not in result.stderr


@pytest.mark.parametrize("case", [
    "top-level", "nested", "merged", "unhashable", "clean-and-violation",
])
def test_cli_fixtures_preserve_caller_repository(tmp_path, monkeypatch, case):
    """Inherited Git bindings must not redirect fixture commands to a caller."""
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    caller = tmp_path / "caller"
    subprocess.run(["git", "init", "-q", str(caller)], env=env, check=True)
    subprocess.run(["git", "config", "core.filemode", "false"],
                   cwd=caller, env=env, check=True)
    (caller / "README.md").write_text("caller file must remain unchanged\n")
    subprocess.run(["git", "read-tree", "--empty"], cwd=caller, env=env, check=True)

    def snapshot():
        return {path.relative_to(caller): path.read_bytes() if path.is_file() else None
                for path in caller.rglob("*")}

    before = snapshot()
    for key, path in {
        "GIT_DIR": caller / ".git",
        "GIT_WORK_TREE": caller,
        "GIT_COMMON_DIR": caller / ".git",
        "GIT_INDEX_FILE": caller / ".git/index",
    }.items():
        monkeypatch.setenv(key, str(path))
    fixture = tmp_path / "fixture"
    fixture.mkdir()
    try:
        if case == "clean-and-violation":
            test_cli_clean_and_violation(fixture)
        else:
            test_cli_rejects_duplicate_yaml_keys(fixture, case)
    finally:
        assert snapshot() == before, "fixture commands changed the caller repository"
    assert (fixture / ".git").is_dir()


def test_worked_path_classification_example():
    """Hand evaluation of the approved plan's Classification Rules (2026-05-07).

    README.md is root-allowlisted; the maintenance script is under scripts/.
    coverage.xml is generated and absent from the root allowlist: two rules.
    src/tests/example.py matches src/**/tests/**, including zero intervening
    directories: one rule. Thus four inputs produce exactly three violations.
    The expected list is derived here by rule membership, without the checker.
    """
    paths = ["README.md", "scripts/maintenance/example.py", "coverage.xml",
             "src/tests/example.py"]
    assert checker().check_paths(paths, config()) == [
        "coverage.xml: generated-tracked-path",
        "coverage.xml: root-allowlist",
        "src/tests/example.py: forbidden-path",
    ]
