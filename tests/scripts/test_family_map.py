# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Static family-contract checks; no application or solver imports/collection."""

import copy
import json
import importlib.util
from types import SimpleNamespace
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
CONTRACT = ROOT / "config/ci/module-families.json"
TEST_TREES = ("tests", "examples", "docs", "scripts", "src")
FAMILIES = {
    "ENGINE",
    "data",
    "energy-production",
    "hydrodynamics",
    "integrity",
    "marine-operations",
    "naval-architecture",
    "platform",
    "solvers",
    "structural",
    "subsea",
    "workflows",
}
ENGINE_BOUNDARIES = {
    "digitalmodel",
    "digitalmodel.__main__",
    "digitalmodel._compat",
    "digitalmodel.citations",
    "digitalmodel.citations.registry",
    "digitalmodel.citations.schema",
    "digitalmodel.common.assumption_ledger",
    "digitalmodel.engine",
    "digitalmodel.infrastructure.utils.private_paths",
    "digitalmodel.reporting",
    "digitalmodel.run_contract",
    "digitalmodel.units",
}


def _unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"Duplicate assignment: {key}")
        result[key] = value
    return result


@pytest.fixture(scope="module")
def contract():
    return json.loads(
        CONTRACT.read_text(encoding="utf-8"), object_pairs_hook=_unique_object
    )


_spec = importlib.util.spec_from_file_location(
    "family_generator", ROOT / "scripts/ci/generate_family_map.py"
)
generator = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(generator)
_source_modules = generator.source_modules
_test_files = generator.test_files


def _report_inventory(assignments, expected):
    assert set(assignments.values()) <= FAMILIES, "Unknown family"
    if set(assignments) != expected:
        print(
            "Family inventory drift is report-only; run "
            "python scripts/ci/generate_family_map.py --write"
        )


def _assert_generated_inventory(assignments, expected):
    actual = set(assignments)
    assert expected - actual == set(), f"Unassigned: {sorted(expected - actual)}"
    assert actual - expected == set(), f"Stale: {sorted(actual - expected)}"
    assert set(assignments.values()) <= FAMILIES, "Unknown family"


def test_schema_and_families(contract):
    assert contract["schema_version"] == 1
    assert set(contract["families"]) == FAMILIES
    assert len(contract["families"]) == len(FAMILIES)
    assert contract["engine"]["status"] == "proposed; product manager confirms"


def test_every_source_module_and_namespace_has_one_family(contract):
    _report_inventory(contract["module_to_family"], _source_modules(ROOT))


def test_application_subpackages_inherit_their_top_level_family(contract):
    modules = contract["module_to_family"]
    for module, family in modules.items():
        if family != "ENGINE" and module.count(".") > 1:
            assert family == modules[".".join(module.split(".")[:2])], module


def test_every_test_file_has_one_home_including_excluded_files(contract):
    # File homes also give each function/class/parameterized node ID one home.
    # This does not claim collection parity or introduce CI test selection.
    _report_inventory(contract["test_file_to_family"], _test_files(ROOT))


def test_engine_modules_and_package_boundaries_are_explicit(contract):
    engine = contract["engine"]
    assert set(engine["boundaries"]) == ENGINE_BOUNDARIES
    assert len(engine["boundaries"]) == len(ENGINE_BOUNDARIES)
    expected = {
        module
        for module in contract["module_to_family"]
        if module in ENGINE_BOUNDARIES
        or any(
            module.startswith(boundary + ".")
            for boundary in ENGINE_BOUNDARIES - {"digitalmodel"}
        )
    }
    assert set(engine["modules"]) == expected
    assert len(engine["modules"]) == len(expected)
    actual = {m for m, f in contract["module_to_family"].items() if f == "ENGINE"}
    assert actual == expected
    assert contract["module_to_family"]["digitalmodel.materials"] == "integrity"
    assert contract["module_to_family"]["digitalmodel.orcaflex"] == "solvers"
    assert (
        contract["module_to_family"]["digitalmodel.marine_ops"] == "marine-operations"
    )


def test_family_engine_dependencies_reference_only_engine_modules(contract):
    dependencies = contract["family_to_engine_deps"]
    assert set(dependencies) == FAMILIES - {"ENGINE"}
    for family, modules in dependencies.items():
        assert modules == sorted(set(modules)), family
        assert set(modules) <= set(contract["engine"]["modules"]), family


def _module_for_source_path(source_path):
    path = Path(source_path)
    if path.name == "agent-pack.md":
        path = path.parent.parent
    parts = path.relative_to("src").with_suffix("").parts
    if parts[-1] == "__init__":
        parts = parts[:-1]
    return ".".join(parts)


def test_registry_families_agree_with_contract(contract):
    registry = yaml.safe_load(
        (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    )
    for entry in registry["modules"]:
        if "ci_family" not in entry:
            print("Registry family annotation pending (report-only)")
            continue
        module = _module_for_source_path(entry["entry_point"])
        assert entry["ci_family"] == generator.prefix_family(
            module, contract["module_to_family"], "."
        ), module
        for submodule in entry.get("submodules", {}).values():
            if "ci_family" not in submodule:
                print("Registry family annotation pending (report-only)")
                continue
            module = _module_for_source_path(submodule["agent_pack"])
            assert submodule["ci_family"] == generator.prefix_family(
                module, contract["module_to_family"], "."
            ), module


@pytest.mark.parametrize(
    "missing",
    [
        "digitalmodel",
        "digitalmodel.example",
        "digitalmodel.example.child",
    ],
)
def test_generated_modules_include_every_namespace(tmp_path, missing, tracked_fixture):
    source = tmp_path / "src/digitalmodel/example/child.py"
    source.parent.mkdir(parents=True)
    source.write_text("", encoding="utf-8")
    modules = _source_modules(tmp_path)
    assert modules == {
        "digitalmodel",
        "digitalmodel.example",
        "digitalmodel.example.child",
    }
    assignments = dict.fromkeys(modules - {missing}, "platform")
    with pytest.raises(AssertionError, match="Unassigned"):
        _assert_generated_inventory(assignments, modules)


@pytest.mark.parametrize("tree", (*TEST_TREES, "new-test-tree"))
def test_tracked_test_files_are_inventoried(tmp_path, tree, tracked_fixture):
    test = tmp_path / tree / "nested/test_new.py"
    test.parent.mkdir(parents=True)
    test.write_text("", encoding="utf-8")
    other = test.with_name("other_test.py")
    other.write_text("", encoding="utf-8")
    files = _test_files(tmp_path)
    assert files == {f"{tree}/nested/test_new.py", f"{tree}/nested/other_test.py"}
    _report_inventory({}, files)


def test_duplicate_homes_are_rejected():
    with pytest.raises(ValueError, match="Duplicate assignment"):
        json.loads(
            '{"test.py":"platform","test.py":"ENGINE"}',
            object_pairs_hook=_unique_object,
        )


@pytest.mark.parametrize(
    "assignments, expected, error",
    [
        ({"test.py": "unknown"}, {"test.py"}, "Unknown family"),
        ({"deleted.py": "platform"}, set(), "Stale"),
    ],
)
def test_generated_inventory_rejects_unknown_and_stale_entries(assignments, expected, error):
    with pytest.raises(AssertionError, match=error):
        _assert_generated_inventory(assignments, expected)


def test_domain_module_cannot_silently_become_engine(contract):
    altered = copy.deepcopy(contract)
    altered["module_to_family"]["digitalmodel.materials"] = "ENGINE"
    with pytest.raises(AssertionError):
        test_engine_modules_and_package_boundaries_are_explicit(altered)


@pytest.mark.parametrize(
    "directory, marker",
    [
        ("venv", "pyvenv.cfg"),
        (".tox/py", "pyvenv.cfg"),
        (".nox/check", "pyvenv.cfg"),
        (".claude/worktrees/child", ".git"),
    ],
)
def test_inventory_prunes_environments_and_checkouts(tmp_path, directory, marker, tracked_fixture):
    nested = tmp_path / directory
    nested.mkdir(parents=True)
    (nested / marker).write_text("", encoding="utf-8")
    (nested / "test_dependency.py").write_text("", encoding="utf-8")
    (tmp_path / "test_real.py").write_text("", encoding="utf-8")
    assert _test_files(tmp_path) == {"test_real.py"}


def test_measured_engine_edges_include_review_regressions(contract):
    expected = {
        "solvers": {"digitalmodel"},
        "integrity": {"digitalmodel.reporting.thickness"},
        "structural": {
            "digitalmodel.reporting.spec",
            "digitalmodel.reporting.thickness",
        },
    }
    for family, modules in expected.items():
        assert modules <= set(contract["family_to_engine_deps"][family])


def test_generator_measures_declared_dependencies(contract):
    generated = generator.regenerate(ROOT, contract)
    assert generated["family_to_engine_deps"] == contract["family_to_engine_deps"]


def test_inventory_drift_is_advisory(capsys):
    _report_inventory({"old.py": "platform"}, {"new.py"})
    assert "report-only" in capsys.readouterr().out


def test_prefix_rules_cover_new_files(contract):
    rules = contract["assignment_rules"]
    assert (
        generator.prefix_family(
            "digitalmodel.solvers.new", rules["module_prefixes"], "."
        )
        == "solvers"
    )
    assert (
        generator.prefix_family("new-tree/test_new.py", rules["test_prefixes"], "/")
        == "platform"
    )


def test_pruning_without_filesystem_access(monkeypatch):
    root = Path("fixture")
    markers = {root / "venv/pyvenv.cfg", root / "checkout/.git"}
    monkeypatch.setattr(Path, "exists", lambda path: path in markers)
    monkeypatch.setattr(Path, "is_symlink", lambda path: path.name == "linked")
    if hasattr(Path, "is_junction"):
        monkeypatch.setattr(Path, "is_junction", lambda path: False)

    def walk(path, followlinks):
        children = ["venv", "checkout", "linked", "tests"]
        yield str(root), children, []
        for name in children:
            yield str(root / name), [], ["test_example.py"]

    monkeypatch.setattr(generator.os, "walk", walk)
    monkeypatch.setattr(generator, "tracked_python_paths", lambda _: {
        f"{name}/test_example.py" for name in ("venv", "checkout", "linked", "tests")
    })
    assert generator.test_files(root) == {"tests/test_example.py"}


@pytest.mark.parametrize("statement", [
    "from digitalmodel.reporting.spec import value",
    "import digitalmodel.reporting.spec",
    "from ..reporting.spec import value",
    "def local():\n    from digitalmodel.reporting import spec",
    "from typing import TYPE_CHECKING\nif TYPE_CHECKING:\n    from digitalmodel.reporting import spec",
])
def test_static_import_forms_are_measured(monkeypatch, statement):
    root = Path("fixture")
    source = root / "src/digitalmodel/example/child.py"
    monkeypatch.setattr(generator, "python_files", lambda _: iter([source]))
    monkeypatch.setattr(
        Path,
        "read_text",
        lambda *args, **kwargs: statement,
    )
    modules = dict.fromkeys(
        [
            "digitalmodel",
            "digitalmodel.reporting",
            "digitalmodel.reporting.spec",
        ],
        "ENGINE",
    )
    modules["digitalmodel.example.child"] = "platform"
    expected = sorted(m for m, f in modules.items() if f == "ENGINE")
    assert generator.measured_dependencies(root, modules, ["ENGINE", "platform"]) == {
        "platform": expected
    }


def test_assignment_rules_use_known_families(contract):
    rules = contract["assignment_rules"]
    assert rules["default"] == "platform"
    for key in ("module_prefixes", "test_prefixes"):
        assert set(rules[key].values()) <= FAMILIES


def test_unannotated_registry_addition_is_advisory(contract, monkeypatch, capsys):
    monkeypatch.setattr(
        yaml,
        "safe_load",
        lambda _: {"modules": [{"entry_point": "src/digitalmodel/new.py"}]},
    )
    test_registry_families_agree_with_contract(contract)
    assert "report-only" in capsys.readouterr().out


@pytest.fixture
def tracked_fixture(monkeypatch, tmp_path):
    # Model index membership without mutating any repository metadata.
    monkeypatch.setattr(generator, "tracked_python_paths", lambda _: {
        path.relative_to(tmp_path).as_posix() for path in tmp_path.rglob("*.py")
    })


def test_inventory_excludes_ignored_and_untracked_files(tmp_path, monkeypatch):
    tracked = "tests/test_kept.py"
    for name in (tracked, "build/lib/demo/test_ignored.py",
                 "src/digitalmodel/structural/untracked.py"):
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("from digitalmodel import run_contract\n", encoding="utf-8")
    (tmp_path / ".gitignore").write_text("build/\n", encoding="utf-8")

    def listing(args, **kwargs):
        assert args == ["git", "ls-files", "-z", "--cached"]
        assert kwargs["cwd"] == tmp_path
        assert not any(key.startswith("GIT_") for key in kwargs["env"])
        return SimpleNamespace(stdout=tracked.encode() + b"\0")

    monkeypatch.setenv("GIT_DIR", "unrelated")
    monkeypatch.setattr(generator.subprocess, "run", listing)
    assert _test_files(tmp_path) == {tracked}
    assert _source_modules(tmp_path) == set()


def test_index_membership_filters_walk_results(monkeypatch):
    root = Path("fixture")

    def listing(args, **kwargs):
        assert args == ["git", "ls-files", "-z", "--cached"]
        assert kwargs["cwd"] == root
        assert not any(key.upper().startswith("GIT_") for key in kwargs["env"])
        assert kwargs["check"] is True
        return SimpleNamespace(stdout=b"tests/test_kept.py\0")

    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.setenv(key, "unrelated")
    monkeypatch.setattr(generator.subprocess, "run", listing)
    monkeypatch.setattr(Path, "is_symlink", lambda _: False)
    monkeypatch.setattr(generator.os, "walk", lambda *args, **kwargs: iter([
        (str(root / "tests"), [], ["test_kept.py", "test_untracked.py"]),
        (str(root / "build/lib"), [], ["test_ignored.py"]),
    ]))
    assert _test_files(root) == {"tests/test_kept.py"}


@pytest.mark.parametrize("name", ["tests/new_dir/test_x.py", "scripts/new_dir/test_x.py"])
def test_unknown_test_directory_uses_platform_default(contract, name):
    assert generator.prefix_family(
        name, contract["assignment_rules"]["test_prefixes"], "/"
    ) == "platform"
