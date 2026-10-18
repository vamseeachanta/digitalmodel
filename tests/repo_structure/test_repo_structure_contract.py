# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Repository structure rules, including deliberately planted violations."""

import copy
import importlib.util
import shlex
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/maintenance/verify_repo_structure.py"


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
        ["git", "ls-tree", "-r", "--name-only", "HEAD"], cwd=ROOT, text=True
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
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    (tmp_path / "README.md").write_text("fixture\n")
    command = [sys.executable, str(SCRIPT), "--repo", str(tmp_path),
               "--config", str(ROOT / "config/repo_structure.yml")]
    clean = subprocess.run(command, capture_output=True, text=True)
    assert clean.returncode == 0, clean.stdout + clean.stderr
    (tmp_path / "unexpected.py").write_text("pass\n")
    bad = subprocess.run(command, capture_output=True, text=True)
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
