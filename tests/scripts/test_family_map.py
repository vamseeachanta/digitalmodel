# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Static family-contract checks; no application or solver imports/collection."""

import ast
import copy
import json
import os
from pathlib import Path
import tokenize
import warnings

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


def _source_modules(root):
    modules = set()
    for path in (root / "src/digitalmodel").rglob("*.py"):
        parts = path.relative_to(root / "src").with_suffix("").parts
        if parts[-1] == "__init__":
            parts = parts[:-1]
        modules.add(".".join(parts))
        modules.update(".".join(parts[:i]) for i in range(1, len(parts)))
    return modules


def _test_files(root):
    files = set()
    for directory, subdirs, names in os.walk(root):
        subdirs[:] = [
            name
            for name in subdirs
            if name not in {".git", ".venv", "__pycache__", "node_modules"}
            and not (Path(directory) / name / "pyvenv.cfg").is_file()
        ]
        for name in names:
            if name.endswith(".py") and (
                name.startswith("test_") or name.endswith("_test.py")
            ):
                files.add((Path(directory) / name).relative_to(root).as_posix())
    return files


def _assert_exhaustive(assignments, expected):
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
    _assert_exhaustive(contract["module_to_family"], _source_modules(ROOT))


def test_application_subpackages_inherit_their_top_level_family(contract):
    modules = contract["module_to_family"]
    for module, family in modules.items():
        if family != "ENGINE" and module.count(".") > 1:
            assert family == modules[".".join(module.split(".")[:2])], module


def test_every_test_file_has_one_home_including_excluded_files(contract):
    # File homes also give each function/class/parameterized node ID one home.
    # This does not claim collection parity or introduce CI test selection.
    _assert_exhaustive(contract["test_file_to_family"], _test_files(ROOT))


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
    measured = _measured_engine_dependencies(ROOT, contract["module_to_family"])
    for family, modules in measured.items():
        missing = modules - set(dependencies[family])
        assert not missing, (
            f"Unlisted ENGINE dependencies for {family}: {sorted(missing)}"
        )


def _import_targets(node, module, is_package):
    if isinstance(node, ast.Import):
        return [alias.name for alias in node.names]
    if not isinstance(node, ast.ImportFrom):
        return []
    base = node.module or ""
    if node.level:
        package = module.split(".") if is_package else module.split(".")[:-1]
        prefix = package[: len(package) - node.level + 1]
        base = ".".join(prefix + ([base] if base else []))
    return [base, *(f"{base}.{alias.name}" for alias in node.names)]


def _measured_engine_dependencies(root, modules):
    engine = {module for module, family in modules.items() if family == "ENGINE"}
    measured = {family: set() for family in FAMILIES - {"ENGINE"}}
    for path in (root / "src/digitalmodel").rglob("*.py"):
        module = _module_for_source_path(path.relative_to(root))
        family = modules[module]
        if family == "ENGINE":
            continue
        with tokenize.open(path) as source, warnings.catch_warnings():
            # Legacy escape warnings do not prevent static import measurement.
            warnings.simplefilter("ignore", (SyntaxWarning, DeprecationWarning))
            tree = ast.parse(source.read(), filename=str(path))
        for node in ast.walk(tree):
            targets = _import_targets(node, module, path.name == "__init__.py")
            measured[family].update(engine.intersection(targets))
    return measured


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
        module = _module_for_source_path(entry["entry_point"])
        assert entry["ci_family"] == contract["module_to_family"][module], module
        for submodule in entry.get("submodules", {}).values():
            module = _module_for_source_path(submodule["agent_pack"])
            assert submodule["ci_family"] == contract["module_to_family"][module], (
                module
            )


@pytest.mark.parametrize(
    "missing",
    [
        "digitalmodel",
        "digitalmodel.example",
        "digitalmodel.example.child",
    ],
)
def test_missing_module_or_namespace_is_rejected(tmp_path, missing):
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
        _assert_exhaustive(assignments, modules)


@pytest.mark.parametrize("tree", (*TEST_TREES, "new-test-tree"))
def test_new_test_file_requires_an_explicit_home(tmp_path, tree):
    test = tmp_path / tree / "nested/test_new.py"
    test.parent.mkdir(parents=True)
    test.write_text("", encoding="utf-8")
    other = test.with_name("other_test.py")
    other.write_text("", encoding="utf-8")
    files = _test_files(tmp_path)
    assert files == {f"{tree}/nested/test_new.py", f"{tree}/nested/other_test.py"}
    with pytest.raises(AssertionError, match="Unassigned"):
        _assert_exhaustive({f"{tree}/nested/test_new.py": "platform"}, files)


@pytest.mark.parametrize(
    "environment",
    [".venv", "venv", ".tox/py", ".nox/tests", "custom-environment"],
)
def test_virtual_environment_tests_are_not_repository_tests(tmp_path, environment):
    environment = tmp_path / environment
    dependency_test = environment / "lib/site-packages/dependency/test_vendor.py"
    dependency_test.parent.mkdir(parents=True)
    (environment / "pyvenv.cfg").write_text("include-system-site-packages = false\n")
    dependency_test.write_text("raise RuntimeError('must never be imported')\n")
    repository_test = tmp_path / "tests/test_repository.py"
    repository_test.parent.mkdir()
    repository_test.write_text("", encoding="utf-8")
    assert _test_files(tmp_path) == {"tests/test_repository.py"}


@pytest.mark.parametrize("directory", ["venv", ".tox", ".nox", "new-test-tree"])
def test_environment_names_without_a_marker_still_require_a_home(tmp_path, directory):
    test = tmp_path / directory / "test_repository.py"
    test.parent.mkdir()
    test.write_text("", encoding="utf-8")
    assert _test_files(tmp_path) == {f"{directory}/test_repository.py"}


@pytest.mark.parametrize("family", ["solvers", "hydrodynamics", "structural"])
def test_missing_engine_facade_dependency_is_rejected(
    contract, family, tmp_path, monkeypatch
):
    source = tmp_path / "src/digitalmodel" / family / "__init__.py"
    source.parent.mkdir(parents=True)
    source.write_text("import digitalmodel\n", encoding="utf-8")
    monkeypatch.setitem(globals(), "ROOT", tmp_path)
    altered = copy.deepcopy(contract)
    altered["family_to_engine_deps"][family] = [
        module
        for module in altered["family_to_engine_deps"][family]
        if module != "digitalmodel"
    ]
    with pytest.raises(AssertionError, match="Unlisted ENGINE dependencies"):
        test_family_engine_dependencies_reference_only_engine_modules(altered)


@pytest.mark.parametrize(
    "source, expected",
    [
        ("import digitalmodel\n", {"digitalmodel"}),
        ("if False:\n    import digitalmodel as package\n", {"digitalmodel"}),
        (
            "def function():\n    from digitalmodel import engine\n",
            {"digitalmodel", "digitalmodel.engine"},
        ),
        ("from .. import engine\n", {"digitalmodel", "digitalmodel.engine"}),
        ("from ..engine import run\n", {"digitalmodel.engine"}),
        ("import digitalmodel.engine\n", {"digitalmodel.engine"}),
    ],
)
def test_engine_dependencies_are_measured_without_importing_sources(
    tmp_path, source, expected
):
    source_path = tmp_path / "src/digitalmodel/example/consumer.py"
    source_path.parent.mkdir(parents=True)
    source_path.write_text(source + "raise RuntimeError('must never be imported')\n")
    modules = {
        "digitalmodel": "ENGINE",
        "digitalmodel.engine": "ENGINE",
        "digitalmodel.example.consumer": "platform",
    }
    measured = _measured_engine_dependencies(tmp_path, modules)
    assert measured["platform"] == expected


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
def test_unknown_and_stale_assignments_are_rejected(assignments, expected, error):
    with pytest.raises(AssertionError, match=error):
        _assert_exhaustive(assignments, expected)


def test_domain_module_cannot_silently_become_engine(contract):
    altered = copy.deepcopy(contract)
    altered["module_to_family"]["digitalmodel.materials"] = "ENGINE"
    with pytest.raises(AssertionError):
        test_engine_modules_and_package_boundaries_are_explicit(altered)
