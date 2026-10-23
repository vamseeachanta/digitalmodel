# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check family generation without making inventory freshness a required gate."""

import ast
import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/generate_module_families.py"
CONTRACT = ROOT / "config/ci/module-families.json"


@pytest.fixture(scope="module")
def generator():
    spec = importlib.util.spec_from_file_location("family_generator", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def rules(generator):
    return generator.load_rules(ROOT)


@pytest.fixture(scope="module")
def live(generator, rules):
    sources, tests = generator.read_inventory(ROOT)
    return generator.build_contract(sources, tests, rules)


def test_snapshot_schema_and_internal_consistency(generator):
    contract = json.loads(CONTRACT.read_text(), object_pairs_hook=generator.unique_object)
    assert contract["schema_version"] == 2
    assert contract["coverage_mode"] == "report-only"
    assert set(contract["families"]) == {
        "ENGINE", "data", "energy-production", "hydrodynamics", "integrity",
        "marine-operations", "naval-architecture", "platform", "solvers",
        "structural", "subsea", "workflows",
    }
    assert len(contract["families"]) == 12
    assert set(contract["module_to_family"].values()) <= set(contract["families"])
    assert set(contract["test_file_to_family"].values()) <= set(contract["families"])
    assert contract["engine"]["status"] == "proposed; maintainer confirmation required"
    assert contract["engine"]["modules"] == sorted(
        module for module, family in contract["module_to_family"].items()
        if family == "ENGINE"
    )
    for dependencies in contract["family_to_engine_deps"].values():
        assert dependencies == sorted(set(dependencies))
        assert set(dependencies) <= set(contract["engine"]["modules"])


def test_tracked_inventory_is_exhaustively_assigned(generator, live):
    sources, tests = generator.read_inventory(ROOT)
    assert set(live["module_to_family"]) == generator.source_modules(sources)
    assert set(live["test_file_to_family"]) == set(tests)


@pytest.mark.parametrize("tree", ["tests", "examples", "docs", "scripts", "src", "new-tree"])
def test_new_tests_receive_deterministic_homes(generator, rules, tree):
    paths = [f"{tree}/new/test_new.py", f"{tree}/new/other_test.py"]
    result = generator.build_contract({}, paths, rules)
    assert set(result["test_file_to_family"]) == set(paths)
    assert set(result["test_file_to_family"].values()) <= set(rules["families"])


def test_new_modules_and_namespaces_inherit_prefix_rules(generator, rules):
    result = generator.build_contract({
        "src/digitalmodel/hydrodynamics/new/quality.py": "",
        "src/digitalmodel/new_package/core.py": "",
        "src/digitalmodel/_output_paths.py": "",
    }, [], rules)
    modules = result["module_to_family"]
    assert modules["digitalmodel"] == "ENGINE"
    assert modules["digitalmodel.hydrodynamics.new"] == "hydrodynamics"
    assert modules["digitalmodel.hydrodynamics.new.quality"] == "hydrodynamics"
    assert modules["digitalmodel.new_package.core"] == "platform"
    assert modules["digitalmodel._output_paths"] == "ENGINE"


def test_prefixes_match_components_and_root_facade_is_exact(generator, rules):
    assert generator.module_family("digitalmodel.reporting.spec", rules) == "ENGINE"
    assert generator.module_family("digitalmodel.reporting_extra", rules) == "platform"
    assert generator.module_family("digitalmodel.materials", rules) == "integrity"
    assert generator.module_family("digitalmodel.solvers.new", rules) == "solvers"
    assert set(rules["engine_exact_modules"]) == {"digitalmodel"}
    assert {module for module, family in rules["module_prefix_to_family"].items()
            if family == "ENGINE"} == {
        "digitalmodel.__main__", "digitalmodel._compat", "digitalmodel._output_paths",
        "digitalmodel.citations", "digitalmodel.citations.registry",
        "digitalmodel.citations.schema", "digitalmodel.common.assumption_ledger",
        "digitalmodel.engine", "digitalmodel.infrastructure.utils.private_paths",
        "digitalmodel.reporting", "digitalmodel.run_contract", "digitalmodel.units",
    }


@pytest.mark.parametrize("directory", ["venv", ".tox", ".nox", ".claude/worktrees/copy"])
def test_untracked_environments_and_checkouts_are_not_inventoried(
    generator, tmp_path, monkeypatch, directory,
):
    tracked = "tests/test_tracked.py"
    (tmp_path / "tests").mkdir()
    (tmp_path / tracked).write_text("")
    excluded = tmp_path / directory
    excluded.mkdir(parents=True)
    (excluded / "pyvenv.cfg").write_text("include-system-site-packages = false\n")
    (excluded / "test_noise.py").write_text("")
    calls = []

    def git_read(root, *args):
        calls.append(args)
        return f"100644 {'0' * 40} 0\t{tracked}\0".encode()

    monkeypatch.setattr(generator, "git_read", git_read)
    assert generator.read_inventory(tmp_path) == ({}, [tracked])
    assert calls == [("ls-files", "--stage", "-z")]


def test_symlink_and_submodule_entries_are_excluded(generator, tmp_path, monkeypatch):
    records = "".join(
        f"{mode} {'0' * 40} 0\t{path}\0" for mode, path in [
            ("120000", "tests/test_link.py"), ("160000", "checkout"),
        ]
    )
    monkeypatch.setattr(generator, "git_read", lambda *args: records.encode())
    assert generator.read_inventory(tmp_path) == ({}, [])


def test_engine_dependency_measurement_is_exact(generator, rules):
    sources = {
        "src/digitalmodel/__init__.py": "",
        "src/digitalmodel/reporting/spec.py": "",
        "src/digitalmodel/reporting/thickness.py": "",
        "src/digitalmodel/solvers/new.py": "import digitalmodel\n",
        "src/digitalmodel/code_checks/new.py": "from digitalmodel.reporting.thickness import Result\n",
        "src/digitalmodel/ansys/new.py": (
            "from digitalmodel.reporting import spec, thickness\n"
            "import digitalmodel.reporting.spec as report\n"
        ),
    }
    result = generator.build_contract(sources, [], rules)
    expected = {family: [] for family in rules["families"] if family != "ENGINE"}
    expected.update({
        "solvers": ["digitalmodel"],
        "integrity": ["digitalmodel.reporting.thickness"],
        "structural": ["digitalmodel.reporting.spec", "digitalmodel.reporting.thickness"],
    })
    assert result["family_to_engine_deps"] == expected


def test_measured_dependencies_equal_declared_for_every_family(generator, live):
    # Independent absolute-import scan: both missing and surplus edges fail.
    sources, _ = generator.read_inventory(ROOT)
    homes = live["module_to_family"]
    expected = {family: set() for family in live["families"] if family != "ENGINE"}
    for path, source in sources.items():
        family = homes[generator.module_for_path(path)]
        if family == "ENGINE":
            continue
        for node in ast.walk(ast.parse(source)):
            if isinstance(node, ast.Import):
                imports = [alias.name for alias in node.names]
            elif isinstance(node, ast.ImportFrom) and node.level == 0:
                imports = [f"{node.module}.{alias.name}" for alias in node.names]
            else:
                continue
            for name in imports:
                while name and name not in homes:
                    name = name.rpartition(".")[0]
                if homes.get(name) == "ENGINE":
                    expected[family].add(name)
    assert live["family_to_engine_deps"] == {
        family: sorted(edges) for family, edges in expected.items()
    }


def _assert_registry_families(generator, rules, registry):
    for entry in registry["modules"]:
        records = [(entry["entry_point"], entry.get("ci_family"))]
        records.extend((sub["agent_pack"], sub.get("ci_family"))
                       for sub in entry.get("submodules", {}).values())
        for path, declared in records:
            # New registry records inherit rules without needing an annotation.
            if declared is None:
                continue
            if path.endswith("agent-pack.md"):
                path = str(Path(path).parent.parent / "__init__.py")
            assert generator.module_family(generator.module_for_path(path), rules) == declared


def test_registry_families_agree_with_live_rules(generator, rules):
    registry = yaml.safe_load((ROOT / "docs/registry/module-routing.yaml").read_text())
    _assert_registry_families(generator, rules, registry)


def test_new_registry_record_does_not_introduce_a_family_gate(generator, rules):
    _assert_registry_families(generator, rules, {
        "modules": [{"entry_point": "src/digitalmodel/new_module"}],
    })


@pytest.mark.parametrize("record", [
    {"entry_point": "src/digitalmodel/solvers", "ci_family": "platform"},
    {"entry_point": "src/digitalmodel/solvers", "submodules": {
        "new": {"agent_pack": "src/digitalmodel/solvers/new/docs/agent-pack.md",
                "ci_family": "platform"},
    }},
])
def test_wrong_registry_annotations_still_fail(generator, rules, record):
    with pytest.raises(AssertionError):
        _assert_registry_families(generator, rules, {"modules": [record]})


def test_duplicate_rules_are_rejected(generator):
    with pytest.raises(ValueError, match="Duplicate assignment"):
        json.loads('{"test.py":"platform","test.py":"ENGINE"}',
                   object_pairs_hook=generator.unique_object)


def test_unknown_families_are_rejected(generator, rules):
    altered = copy.deepcopy(rules)
    altered["module_prefix_to_family"]["digitalmodel.new"] = "unknown"
    with pytest.raises(ValueError, match="Unknown family"):
        generator.validate_rules(altered)


def test_rendering_is_deterministic(generator, rules):
    sources = {"src/digitalmodel/units.py": "", "src/digitalmodel/engine.py": ""}
    first = generator.build_contract(sources, ["tests/test_b.py", "tests/test_a.py"], rules)
    second = generator.build_contract(dict(reversed(list(sources.items()))),
                                      ["tests/test_a.py", "tests/test_b.py"], rules)
    assert generator.render(first) == generator.render(second)


def test_stale_snapshot_is_report_only_but_check_detects_it(tmp_path):
    output = tmp_path / "families.json"
    output.write_text("{}\n")
    command = [sys.executable, str(SCRIPT), "--output", str(output)]
    report = subprocess.run([*command, "--report"], capture_output=True, text=True)
    assert report.returncode == 0, report.stderr
    assert "out of date" in report.stdout
    check = subprocess.run([*command, "--check"], capture_output=True, text=True)
    assert check.returncode == 1, check.stderr
    write = subprocess.run([*command, "--write"], capture_output=True, text=True)
    assert write.returncode == 0, write.stderr
    check = subprocess.run([*command, "--check"], capture_output=True, text=True)
    assert check.returncode == 0, check.stderr
