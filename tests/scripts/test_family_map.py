# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Static family-contract checks; no application or solver imports/collection."""

import copy
from functools import lru_cache
import importlib.util
import json
import subprocess
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
    "digitalmodel._output_paths",
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
    generator = _generator()
    return generator.source_modules(generator.tracked_paths(root))


def _test_files(root):
    generator = _generator()
    return generator.test_files(generator.tracked_paths(root))


def _assert_exhaustive(assignments, expected):
    actual = set(assignments)
    assert expected - actual == set(), f"Unassigned: {sorted(expected - actual)}"
    assert actual - expected == set(), f"Stale: {sorted(actual - expected)}"
    assert set(assignments.values()) <= FAMILIES, "Unknown family"


def test_schema_and_families(contract):
    assert contract["schema_version"] == 1
    assert set(contract["families"]) == FAMILIES
    assert len(contract["families"]) == len(FAMILIES)
    assert contract["engine"]["status"] == "proposed; maintainer confirms"


@pytest.fixture(scope="module")
def live_inventory():
    generator = _generator()
    paths, sources, revision = generator.inventory(ROOT)
    rules = generator.read_json(ROOT / generator.RULES)
    return generator.build_contract(paths, sources, rules, revision), sources


def test_every_source_module_and_namespace_gets_a_generated_family(live_inventory):
    generated, _ = live_inventory
    _assert_exhaustive(generated["module_to_family"], _source_modules(ROOT))


def test_application_subpackages_inherit_their_top_level_family(contract):
    modules = contract["module_to_family"]
    for module, family in modules.items():
        if family != "ENGINE" and module.count(".") > 1:
            assert family == modules[".".join(module.split(".")[:2])], module


def test_every_test_file_gets_a_generated_home_including_excluded_files(live_inventory):
    # File homes also give each function/class/parameterized node ID one home.
    # This does not claim collection parity or introduce CI test selection.
    generated, _ = live_inventory
    _assert_exhaustive(generated["test_file_to_family"], _test_files(ROOT))


def test_frozen_inventory_drift_is_report_only(contract, live_inventory):
    generated, _ = live_inventory
    report = _generator().coverage_report(contract, generated)
    print(json.dumps(report, sort_keys=True))
    # New files do not fail the required test path before family activation.
    assert "module_to_family" in report
    assert "test_file_to_family" in report


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


def test_generated_family_engine_dependencies_are_exact(live_inventory):
    generated, sources = live_inventory
    dependencies = generated["family_to_engine_deps"]
    assert set(dependencies) == FAMILIES - {"ENGINE"}
    for family, modules in dependencies.items():
        assert modules == sorted(set(modules)), family
        assert set(modules) <= set(generated["engine"]["modules"]), family
    _generator().validate_engine_dependencies(
        dependencies, sources, generated["module_to_family"]
    )


def _module_for_source_path(source_path):
    path = Path(source_path)
    if path.name == "agent-pack.md":
        path = path.parent.parent
    parts = path.relative_to("src").with_suffix("").parts
    if parts[-1] == "__init__":
        parts = parts[:-1]
    return ".".join(parts)


@pytest.fixture(params=["current", "new-module", "new-submodule"])
def registry_inventory(request, live_inventory, contract):
    registry = yaml.safe_load(
        (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    )
    generated, sources = live_inventory
    if request.param != "current":
        generator = _generator()
        source = "src/digitalmodel/new_domain/child/member.py"
        paths = [*generator.tracked_paths(ROOT), source]
        rules = generator.read_json(ROOT / generator.RULES)
        generated = generator.build_contract(paths, {**sources, source: b""}, rules)
        assert "digitalmodel.new_domain" not in contract["module_to_family"]
        assert "digitalmodel.new_domain.child" not in contract["module_to_family"]
        if request.param == "new-module":
            registry["modules"].append(
                {
                    "entry_point": "src/digitalmodel/new_domain/__init__.py",
                    "ci_family": "platform",
                }
            )
        else:
            registry["modules"][0].setdefault("submodules", {})["new_domain"] = {
                "agent_pack": "src/digitalmodel/new_domain/child/.claude/agent-pack.md",
                "ci_family": "platform",
            }
    return registry, generated


def test_registry_families_agree_with_contract(registry_inventory):
    registry, generated = registry_inventory
    for entry in registry["modules"]:
        module = _module_for_source_path(entry["entry_point"])
        assert entry["ci_family"] == generated["module_to_family"][module], module
        for submodule in entry.get("submodules", {}).values():
            module = _module_for_source_path(submodule["agent_pack"])
            assert (
                submodule["ci_family"] == generated["module_to_family"][module]
            ), module


@pytest.mark.parametrize("kind", ["module", "submodule"])
def test_incorrect_registry_family_is_rejected(live_inventory, kind):
    generated, _ = live_inventory
    if kind == "module":
        entry = {"entry_point": "src/digitalmodel/engine.py", "ci_family": "platform"}
    else:
        entry = {
            "entry_point": "src/digitalmodel/engine.py",
            "ci_family": "ENGINE",
            "submodules": {
                "units": {
                    "agent_pack": "src/digitalmodel/units/.claude/agent-pack.md",
                    "ci_family": "platform",
                }
            },
        }
    expected = "digitalmodel.engine" if kind == "module" else "digitalmodel.units"
    with pytest.raises(AssertionError, match=expected):
        test_registry_families_agree_with_contract(({"modules": [entry]}, generated))


@pytest.mark.parametrize(
    "missing",
    [
        "digitalmodel",
        "digitalmodel.example",
        "digitalmodel.example.child",
    ],
)
def test_missing_module_or_namespace_is_rejected(tmp_path, missing, monkeypatch):
    source = tmp_path / "src/digitalmodel/example/child.py"
    source.parent.mkdir(parents=True)
    source.write_text("", encoding="utf-8")
    monkeypatch.setattr(
        _generator(),
        "tracked_paths",
        lambda root: ["src/digitalmodel/example/child.py"],
    )
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
def test_new_test_file_gets_a_prefix_or_default_home(tmp_path, tree, monkeypatch):
    test = tmp_path / tree / "nested/test_new.py"
    test.parent.mkdir(parents=True)
    test.write_text("", encoding="utf-8")
    other = test.with_name("other_test.py")
    other.write_text("", encoding="utf-8")
    monkeypatch.setattr(
        _generator(),
        "tracked_paths",
        lambda root: [f"{tree}/nested/test_new.py", f"{tree}/nested/other_test.py"],
    )
    files = _test_files(tmp_path)
    assert files == {f"{tree}/nested/test_new.py", f"{tree}/nested/other_test.py"}
    with pytest.raises(AssertionError, match="Unassigned"):
        _assert_exhaustive({f"{tree}/nested/test_new.py": "platform"}, files)
    rules = _generator().read_json(ROOT / _generator().RULES)
    assignments = {path: _generator().family_for_test(path, rules) for path in files}
    _assert_exhaustive(assignments, files)


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


@lru_cache(maxsize=1)
def _generator():
    spec = importlib.util.spec_from_file_location(
        "module_family_generator", ROOT / "scripts/ci/generate_module_families.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_inventory_uses_git_paths_and_ignores_local_environments(tmp_path, monkeypatch):
    tracked = "tests/test_tracked.py"
    extras = (
        "venv/lib/test_dependency.py",
        ".tox/check/lib/test_dependency.py",
        ".nox/check/lib/test_dependency.py",
        ".claude/worktrees/checkout/tests/test_nested.py",
        "src/digitalmodel/local/test_untracked.py",
    )
    for name in (tracked, *extras):
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("", encoding="utf-8")
    (tmp_path / "venv/pyvenv.cfg").write_text("", encoding="utf-8")
    (tmp_path / ".claude/worktrees/checkout/.git").write_text(
        "gitdir: metadata\n", encoding="utf-8"
    )

    def git_paths(args, **kwargs):
        assert args == ["git", "ls-files", "--cached", "-z"]
        assert kwargs["cwd"] == tmp_path
        return subprocess.CompletedProcess(args, 0, stdout=tracked.encode() + b"\0")

    monkeypatch.setattr(subprocess, "run", git_paths)
    assert _test_files(tmp_path) == {tracked}


def test_engine_dependencies_include_bare_and_from_imports():
    generator = _generator()
    sources = {
        "src/digitalmodel/solvers/path_resolver.py": "import digitalmodel\n",
        "src/digitalmodel/code_checks/tank.py": (
            "from digitalmodel.reporting.thickness import render\n"
        ),
        "src/digitalmodel/ansys/report.py": (
            "from digitalmodel.reporting import spec, thickness\n"
        ),
    }
    modules = {
        "digitalmodel": "ENGINE",
        "digitalmodel.reporting": "ENGINE",
        "digitalmodel.reporting.spec": "ENGINE",
        "digitalmodel.reporting.thickness": "ENGINE",
        "digitalmodel.solvers.path_resolver": "solvers",
        "digitalmodel.code_checks.tank": "integrity",
        "digitalmodel.ansys.report": "structural",
    }
    measured = generator.measure_engine_dependencies(sources, modules)
    assert measured["solvers"] == ["digitalmodel"]
    assert measured["integrity"] == ["digitalmodel.reporting.thickness"]
    assert measured["structural"] == [
        "digitalmodel.reporting",
        "digitalmodel.reporting.spec",
        "digitalmodel.reporting.thickness",
    ]
    altered = copy.deepcopy(measured)
    altered["solvers"] = []
    with pytest.raises(ValueError, match="ENGINE dependencies differ"):
        generator.validate_engine_dependencies(altered, sources, modules)
    altered = copy.deepcopy(measured)
    altered["solvers"].append("digitalmodel.reporting")
    with pytest.raises(ValueError, match="ENGINE dependencies differ"):
        generator.validate_engine_dependencies(altered, sources, modules)


def test_absolute_import_measurement_is_static_and_includes_guarded_edges():
    generator = _generator()
    sources = {
        "src/digitalmodel/solvers/example.py": (
            "raise RuntimeError('must never execute')\n"
            "if False:\n"
            "    import digitalmodel.units as units\n"
            "def local():\n"
            "    from digitalmodel.reporting import render\n"
            "from . import units\n"
            "from unrelated import digitalmodel\n"
        )
    }
    modules = {
        "digitalmodel": "ENGINE",
        "digitalmodel.units": "ENGINE",
        "digitalmodel.reporting": "ENGINE",
        "digitalmodel.solvers.example": "solvers",
    }
    assert generator.measure_engine_dependencies(sources, modules) == {
        "solvers": ["digitalmodel.reporting", "digitalmodel.units"]
    }


@pytest.mark.parametrize(
    "path, expected",
    [
        ("tests/hydrodynamics/new/test_added.py", "hydrodynamics"),
        ("tests/power/test_added.py", "energy-production"),
        ("tests/citations/test_added.py", "ENGINE"),
        ("tests/unrecognized/test_added.py", "platform"),
        ("new-test-tree/test_added.py", "platform"),
        ("src/digitalmodel/structural/new/test_added.py", "structural"),
    ],
)
def test_new_files_follow_prefix_rules(path, expected):
    generator = _generator()
    rules = generator.read_json(ROOT / generator.RULES)
    assert generator.family_for_test(path, rules) == expected


def test_engine_facade_does_not_capture_new_domain_modules():
    generator = _generator()
    rules = generator.read_json(ROOT / generator.RULES)
    assert generator.family_for_module("digitalmodel._output_paths", rules) == "ENGINE"
    assert (
        generator.family_for_module("digitalmodel.new_domain.child", rules)
        == "platform"
    )
    assert generator.family_for_module("digitalmodel.reporting.new", rules) == "ENGINE"
    assert (
        generator.family_for_module("digitalmodel.reporting_extra", rules) == "platform"
    )


def test_report_write_and_explicit_check_modes(tmp_path, monkeypatch, capsys):
    generator = _generator()
    config = tmp_path / "config/ci"
    config.mkdir(parents=True)
    rules = generator.read_json(ROOT / generator.RULES)
    (tmp_path / generator.RULES).write_text(json.dumps(rules), encoding="utf-8")
    paths = ["src/digitalmodel/solvers/new.py", "tests/solvers/test_new.py"]
    sources = {paths[0]: b"import digitalmodel\n"}
    monkeypatch.setattr(
        generator, "inventory", lambda root, ref: (paths, sources, "fixture")
    )
    args = ["--repo", str(tmp_path)]
    assert generator.main([*args, "--write"]) == 0
    first = (tmp_path / generator.CONTRACT).read_bytes()
    assert generator.main([*args, "--check"]) == 0
    paths.reverse()
    assert generator.main([*args, "--write"]) == 0
    assert (tmp_path / generator.CONTRACT).read_bytes() == first
    assert generator.main([*args, "--check"]) == 0
    sources["src/digitalmodel/solvers/added.py"] = b"import digitalmodel\n"
    paths.extend(["src/digitalmodel/solvers/added.py", "tests/solvers/test_added.py"])
    assert generator.main(args) == 0
    report = capsys.readouterr().out
    assert "digitalmodel.solvers.added" in report
    assert "tests/solvers/test_added.py" in report
    assert (tmp_path / generator.CONTRACT).read_bytes() == first
    assert generator.main([*args, "--check"]) == 1
    assert generator.main([*args, "--write"]) == 0
    assert generator.main([*args, "--check"]) == 0


def test_unknown_rules_and_duplicate_keys_are_rejected():
    generator = _generator()
    rules = generator.read_json(ROOT / generator.RULES)
    rules["module_prefixes"]["digitalmodel.new"] = "unknown"
    with pytest.raises(ValueError, match="Unknown module family"):
        generator.build_contract([], {}, rules)
    with pytest.raises(ValueError, match="Duplicate assignment"):
        json.loads('{"same": 1, "same": 2}', object_pairs_hook=generator.unique_object)


def test_revision_inventory_uses_blob_ids_without_a_worktree(tmp_path, monkeypatch):
    generator = _generator()
    oid = b"a" * 40
    source = b"import digitalmodel\n"
    calls = []

    def read_git(root, *args, input_data=None):
        assert root == tmp_path
        calls.append(args[0])
        if args[0] == "rev-parse":
            return b"b" * 40 + b"\n"
        if args[0] == "ls-tree":
            return b"100644 blob " + oid + b"\tsrc/digitalmodel/solvers/example.py\0"
        assert args == ("cat-file", "--batch")
        assert input_data == oid + b"\n"
        return oid + b" blob " + str(len(source)).encode() + b"\n" + source + b"\n"

    monkeypatch.setattr(generator, "git", read_git)
    paths, sources, revision = generator.inventory(tmp_path, "main")
    assert paths == ["src/digitalmodel/solvers/example.py"]
    assert sources == {paths[0]: source}
    assert revision == "b" * 40
    assert calls == ["rev-parse", "ls-tree", "cat-file"]
