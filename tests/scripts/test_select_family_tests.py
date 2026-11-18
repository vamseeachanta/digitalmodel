# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Advisory family selection and read-only diff explanations."""

import copy
import json
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

from scripts.ci import detect_touched_domains as detector
from scripts.ci import select_family_tests as selector


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/select_family_tests.py"


@pytest.fixture
def family_map():
    return {
        "schema_version": 1,
        "families": ["ENGINE", "integrity", "hydrodynamics", "platform"],
        "module_to_family": {
            "digitalmodel": "ENGINE",
            "digitalmodel.engine": "ENGINE",
            "digitalmodel.units": "ENGINE",
            "digitalmodel.cathodic_protection": "integrity",
            "digitalmodel.cathodic_protection.solve": "integrity",
            "digitalmodel.hydrodynamics.wave": "hydrodynamics",
        },
        "family_to_engine_deps": {
            "integrity": ["digitalmodel", "digitalmodel.units"],
            "hydrodynamics": ["digitalmodel", "digitalmodel.engine"],
            "platform": [],
        },
        "test_file_to_family": {
            "tests/test_engine.py": "ENGINE",
            "tests/cathodic_protection/test_solve.py": "integrity",
            "tests/hydrodynamics/test_wave.py": "hydrodynamics",
            "tests/mixed/test_platform.py": "platform",
            "tests/mixed/test_integrity.py": "integrity",
        },
    }


@pytest.mark.parametrize(("path", "families", "rule"), [
    ("src/digitalmodel/cathodic_protection/solve.py", ["integrity"], "module-home"),
    ("./src/digitalmodel/cathodic_protection/__init__.py", ["integrity"], "module-home"),
    ("src/digitalmodel/engine.py", ["ENGINE", "hydrodynamics"], "engine-dependents"),
    ("src/digitalmodel/units.py", ["ENGINE", "integrity"], "engine-dependents"),
    ("src/digitalmodel/__init__.py", ["ENGINE", "hydrodynamics", "integrity"], "engine-dependents"),
    ("tests/mixed/test_integrity.py", ["integrity"], "test-home"),
    ("src/digitalmodel/new.py", ["ENGINE", "hydrodynamics", "integrity", "platform"], "unmapped"),
    ("src/digitalmodel/cathodic_protection/new.py", ["ENGINE", "hydrodynamics", "integrity", "platform"], "unmapped"),
    ("tests/new/test_new.py", ["ENGINE", "hydrodynamics", "integrity", "platform"], "unmapped"),
    ("pytest.ini", ["ENGINE", "hydrodynamics", "integrity", "platform"], "unmapped"),
])
def test_path_selection(family_map, path, families, rule):
    result = selector.select_tests([path], family_map)
    assert result["families"] == families
    assert result["reasons"][0]["rule"] == rule
    assert result["reasons"][0]["families"] == families
    assert result["test_roots"] == sorted(
        path for path, family in family_map["test_file_to_family"].items()
        if family in families
    )


def test_union_is_deterministic_and_keeps_mixed_directory_homes(family_map):
    paths = ["tests/mixed/test_integrity.py", "src/digitalmodel/units.py"]
    original = copy.deepcopy(family_map)
    result = selector.select_tests(paths + paths, family_map)
    assert result == selector.select_tests(list(reversed(paths)), family_map)
    assert result["families"] == ["ENGINE", "integrity"]
    assert "tests/mixed/test_integrity.py" in result["test_roots"]
    assert "tests/mixed/test_platform.py" not in result["test_roots"]
    assert family_map == original


def test_empty_diff_selects_nothing(family_map):
    assert selector.select_tests([], family_map) == {
        "families": [], "test_roots": [], "reasons": [], "unmapped_paths": [],
    }


@pytest.mark.parametrize("filename", ["legacy-plot", "legacy plot", "legacy(draft)"])
def test_path_derived_module_names_match_recorded_inventory(family_map, filename):
    family_map["module_to_family"][f"digitalmodel.structural.{filename}"] = "integrity"
    result = selector.select_tests([f"src/digitalmodel/structural/{filename}.py"], family_map)
    assert result["families"] == ["integrity"]
    assert result["reasons"][0]["rule"] == "module-home"


@pytest.mark.parametrize("path", ["../escape.py", "/absolute.py", "a/../b.py", "a\\b.py", "a//b.py", "C:/profile/x.py", ""])
def test_invalid_changed_paths_are_rejected(family_map, path):
    with pytest.raises(ValueError):
        selector.select_tests([path], family_map)


@pytest.mark.parametrize(("field", "value"), [
    ("schema_version", 2),
    ("families", ["ENGINE", "ENGINE"]),
    ("module_to_family", {"digitalmodel.engine": "unknown"}),
    ("module_to_family", {"other.engine": "ENGINE"}),
    ("test_file_to_family", {"../outside.py": "ENGINE"}),
    ("test_file_to_family", {"tests/test_engine.py": "unknown"}),
    ("family_to_engine_deps", {"integrity": ["digitalmodel.missing"]}),
    ("family_to_engine_deps", {"integrity": ["digitalmodel.cathodic_protection"]}),
    ("family_to_engine_deps", {}),
])
def test_invalid_inventory_is_rejected(family_map, field, value):
    family_map[field] = value
    with pytest.raises(ValueError):
        selector.select_tests([], family_map)


def test_duplicate_json_keys_are_rejected(tmp_path):
    path = tmp_path / "families.json"
    path.write_text('{"schema_version": 1, "schema_version": 1}')
    with pytest.raises(ValueError, match="duplicate"):
        selector.load_family_map(path)


def test_explanation_compares_existing_domain_selection(family_map):
    domains = [
        detector.Domain("cathodic-protection", ("tests/cathodic_protection/",)),
        detector.Domain("hydrodynamics", ("tests/hydrodynamics/",)),
    ]
    result = selector.explain(
        ["src/digitalmodel/cathodic_protection/solve.py"], family_map, domains,
    )
    assert result["advisory"] is True
    comparison = result["domain_comparison"]
    assert comparison["domains"] == ["cathodic-protection"]
    assert comparison["family_only_test_files"] == ["tests/mixed/test_integrity.py"]
    assert comparison["domain_only_test_files"] == []


def test_revision_comparison_preserves_record_pins(family_map, monkeypatch):
    domains = [detector.Domain("contracts", ("tests/contracts/",))]
    calls = []

    def pins(ref, supplied_domains):
        calls.append(ref)
        assert supplied_domains == domains
        return {"tests/mixed/test_integrity.py": {"contracts"}} if ref == "old" else {}

    monkeypatch.setattr(detector, "git_pinned_domains", pins)
    result = selector.explain(
        ["tests/mixed/test_integrity.py"], family_map, domains, "old", "new",
    )
    assert calls == ["old", "new"]
    assert result["domain_comparison"]["domains"] == ["contracts"]


def test_cli_explains_revision_diff_without_writes(family_map, tmp_path, monkeypatch, capsys):
    map_path = tmp_path / "families.json"
    map_path.write_text(json.dumps(family_map))
    domains_path = tmp_path / "DOMAINS.md"
    domains_path.write_text("| integrity | `tests/cathodic_protection/` |\n")
    calls = []

    def changed(base, head):
        calls.append((base, head))
        # Git's --no-renames diff supplies both old and new names.
        return ["tests/mixed/test_integrity.py", "tests/new/test_renamed.py"]

    monkeypatch.setattr(detector, "git_changed_files", changed)
    monkeypatch.setattr(detector, "git_pinned_domains", lambda *args: {})
    before = {p.name: p.read_bytes() for p in tmp_path.iterdir()}
    assert selector.main([
        "--families-file", str(map_path), "--domains-file", str(domains_path),
        "--base", "old", "--head", "new",
    ]) == 0
    result = json.loads(capsys.readouterr().out)
    assert result["advisory"] is True
    assert result["unmapped_paths"] == ["tests/new/test_renamed.py"]
    assert calls == [("old", "new")]
    assert before == {p.name: p.read_bytes() for p in tmp_path.iterdir()}


def test_cli_script_accepts_explicit_paths(family_map, tmp_path):
    map_path = tmp_path / "families.json"
    map_path.write_text(json.dumps(family_map))
    result = subprocess.run([
        sys.executable, str(SCRIPT), "--families-file", str(map_path),
        "--changed-path", "src/digitalmodel/engine.py",
    ], cwd=ROOT, text=True, capture_output=True)
    assert result.returncode == 0, result.stderr
    assert json.loads(result.stdout)["families"] == ["ENGINE", "hydrodynamics"]


@pytest.mark.parametrize("args", [[], ["--base", "old"], ["--head", "new"], ["--base", "old", "--head", "new", "--changed-path", "x.py"]])
def test_cli_requires_one_complete_diff_source(args):
    with pytest.raises(SystemExit) as error:
        selector.main(args)
    assert error.value.code == 2


@pytest.mark.parametrize("option", ["--base", "--head"])
def test_cli_rejects_revision_options_before_git(family_map, tmp_path, monkeypatch, option):
    map_path = tmp_path / "families.json"
    map_path.write_text(json.dumps(family_map))

    def unexpected_git(*args):
        pytest.fail("revision options must not reach Git")

    monkeypatch.setattr(detector, "git_changed_files", unexpected_git)
    revisions = {"--base": "old", "--head": "new"}
    revisions[option] = "--output=unexpected.txt"
    with pytest.raises(SystemExit) as error:
        selector.main([
            "--families-file", str(map_path),
            *[f"{key}={value}" for key, value in revisions.items()],
        ])
    assert error.value.code == 2
    monkeypatch.setattr(detector, "git_pinned_domains", unexpected_git)
    with pytest.raises(ValueError):
        selector.explain([], family_map, [], revisions["--base"], revisions["--head"])


def test_missing_map_is_an_explicit_error(tmp_path, capsys):
    assert selector.main([
        "--families-file", str(tmp_path / "missing.json"),
        "--changed-path", "src/digitalmodel/engine.py",
    ]) == 2
    assert "family inventory unavailable or invalid" in capsys.readouterr().err


def test_selector_regressions_run_in_the_existing_ci_harness():
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    steps = workflow["jobs"]["ci-harness-tests"]["steps"]
    commands = [step["run"] for step in steps if step.get("name") == "Run harness tests"]
    assert len(commands) == 1
    assert "tests/scripts/test_select_family_tests.py" in commands[0].split()
