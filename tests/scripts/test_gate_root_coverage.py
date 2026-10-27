# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execution assignments must cover declared roots and every pytest file."""

import ast
import configparser
import glob
import os
import re
import shlex
import subprocess
import sys
from fnmatch import fnmatch
from functools import lru_cache
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
GATES = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())["gates"]
SHARDS = {name: gate for name, gate in GATES.items() if gate.get("domain")}


# These exceptions describe existing scheduling and collection controls. They
# neither exclude tests nor permit new gaps. Removing sweep-only entries is
# allowed; adding an unlisted sweep-only path fails the guard (issue #2083).
SWEEP_ONLY = {
    Path("tests/data/test_fetch.py"): "Content-store fetch checks run in the scheduled sweep; #2083.",
    Path("tests/data/test_manifest.py"): "Content-store manifest checks run in the scheduled sweep; #2083.",
    Path("tests/data/test_slwr_store_references.py"): "Store-reference checks run in the scheduled sweep; #2083.",
    Path("tests/test_fetch_steel_shapes.py"): "Steel-shape fetch checks run in the scheduled sweep; #2083.",
    Path("tests/test_steel_shapes.py"): "Steel-shape data checks run in the scheduled sweep; #2083.",
    Path("tests/test_steel_shapes_import.py"): "Steel-shape import checks run in the scheduled sweep; #2083.",
}
COLLECTION_EXCEPTIONS = {
    "conftest.py:collect_ignore:scripts": "Standalone scripts are outside the test suite; #1983.",
    "conftest.py:collect_ignore:docs": "Documentation utilities are outside the test suite; #1977.",
    "conftest.py:collect_ignore:.codex": "External skill tests are outside this repository; #1983.",
    "conftest.py:collect_ignore:.gemini": "External skill tests are outside this repository; #1983.",
    "pytest.ini:norecursedirs:.venv": "Environment files are outside the test suite.",
    "pytest.ini:norecursedirs:.git": "Git metadata is outside the test suite.",
    "pytest.ini:norecursedirs:__pycache__": "Bytecode caches are outside the test suite.",
    "pytest.ini:norecursedirs:node_modules": "Installed JavaScript dependencies are outside the test suite.",
    "pytest.ini:norecursedirs:htmlcov": "Generated coverage pages are outside the test suite.",
    "pytest.ini:norecursedirs:tests/data_systems/data_scraping": "Existing targeted scraper exclusion; original rationale is unrecorded; #2083.",
    "pytest.ini:norecursedirs:tests/marine_ops/artificial_lift/dynacard/benchmark": "Existing targeted benchmark exclusion; original rationale is unrecorded; #2083.",
    "pytest.ini:norecursedirs:tests/workflows/integration": "Existing targeted integration exclusion; original rationale is unrecorded; #2083.",
    **{
        "tests/conftest.py:collect_ignore:tests/" + path: reason
        for path, reason in {
            "solvers/orcaflex/examples_integration/test_converter.py": "Standalone-script local imports are absent; #2083.",
            "solvers/orcaflex/examples_integration/test_single_download.py": "Standalone-script local imports are absent; #2083.",
            "structural/fatigue_analysis/test_reference_seastate_scaling.py": "Local reference module is absent; #2083.",
            "visualization/design_tools/pilot_program/test_case_1_separator.py": "Local design-tool imports are absent; #2083.",
            "solvers/orcaflex/test_orcaflex_unit.py": "Required source module is absent; #2083.",
            "structural/fatigue_apps/test_load_scaling.py": "Required source module is absent; #2083.",
            "subsea/pipeline/test_on_bottom_stability.py": "Required source module is absent; #2083.",
            "test_plate_capacity.py": "Required source module is absent; #2083.",
            "workflows/orcawave/test_com_connection.py": "Patch targets reference removed top-level modules; #1923, #2083.",
            "workflows/orcawave/test_end_to_end.py": "Test bodies import the removed source tree; #1923, #2083.",
            "workflows/orcawave/test_integration.py": "Collection imports the removed source tree; #1923, #2083.",
            "workflows/standalone/markitdown/test_converter.py": "Dependency is undeclared and patch targets are stale; #1923, #2083.",
            "hydrodynamics/hull_library/test_hull_library_expansion.py": "Hull-profile data files are absent; #1923, #2083.",
        }.items()
    },
}


def validate_sweep_only(paths, reasons):
    """Permit only named historical sweep-only files with recorded reasons."""
    return [str(path) for path in sorted(paths) if not reasons.get(path, "").strip()]


def validate_collection_exceptions(controls, reasons):
    """Require exact declarations and reasons for every collection control."""
    return sorted(set(controls) ^ set(reasons)) + [
        key for key in controls if key in reasons and not reasons[key].strip()
    ]


def collection_controls(root):
    """Read collection exclusions without importing executable conftests."""
    files = candidate_test_files(root)
    controls = {}
    config = configparser.ConfigParser(interpolation=None)
    config.read(root / "pytest.ini")
    for pattern in config.get("pytest", "norecursedirs", fallback="").split():
        controls[f"pytest.ini:norecursedirs:{pattern}"] = {
            path for path in files if any(
                fnmatch(parent.name, pattern) if "/" not in pattern
                else fnmatch(str(parent), pattern) or fnmatch(str(parent), "*/" + pattern)
                for parent in path.parents if parent != Path(".")
            )
        }
    conftests = sorted((root / "tests").rglob("conftest.py"))
    if (root / "conftest.py").exists():
        conftests.insert(0, root / "conftest.py")
    for conftest in conftests:
        tree = ast.parse(conftest.read_text())
        local_tests_dir = any(
            isinstance(node, ast.Assign)
            and any(isinstance(target, ast.Name) and target.id == "_tests_dir"
                    for target in node.targets)
            and ast.unparse(node.value) == "Path(__file__).parent"
            for node in tree.body
        )
        for node in ast.walk(tree):
            names = {item.id for item in ast.walk(node) if isinstance(item, ast.Name)
                     and isinstance(item.ctx, ast.Store)} if isinstance(
                         node, (ast.Assign, ast.AnnAssign, ast.AugAssign)
                     ) else set()
            if (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
                    and isinstance(node.func.value, ast.Name)
                    and node.func.value.id in {"collect_ignore", "collect_ignore_glob"}):
                raise ValueError("Unsupported collection control")
            if (isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
                    and node.name == "pytest_ignore_collect") or "collect_ignore_glob" in names:
                raise ValueError("Unsupported collection control")
            if "collect_ignore" not in names:
                continue
            if not isinstance(node, ast.Assign) or not isinstance(node.value, (ast.List, ast.Tuple)):
                raise ValueError("Unsupported collection control")
            for value in node.value.elts:
                if isinstance(value, ast.Constant) and isinstance(value.value, str):
                    relative = value.value
                elif (local_tests_dir and isinstance(value, ast.Call) and isinstance(value.func, ast.Name)
                      and value.func.id == "str" and len(value.args) == 1
                      and isinstance(value.args[0], ast.BinOp)
                      and isinstance(value.args[0].op, ast.Div)
                      and isinstance(value.args[0].left, ast.Name)
                      and value.args[0].left.id == "_tests_dir"
                      and isinstance(value.args[0].right, ast.Constant)
                      and isinstance(value.args[0].right.value, str)):
                    relative = value.args[0].right.value
                else:
                    raise ValueError("Unsupported collection control")
                path = conftest.parent.relative_to(root) / relative
                if path.is_absolute() or ".." in path.parts:
                    raise ValueError("Unsupported collection control")
                key = f"{conftest.relative_to(root)}:collect_ignore:{path}"
                controls[key] = {file for file in files if file == path or path in file.parents}
    return controls


def candidate_test_files(root):
    """Inventory filename candidates, including files excluded at collection."""
    return sorted(
        path.relative_to(root) for path in (root / "tests").rglob("*.py")
        if fnmatch(path.name, "test_*.py") or fnmatch(path.name, "*_test.py")
    )


@lru_cache(maxsize=None)
def pytest_paths(command, root=ROOT):
    """Read roots and literal ignores; reject unsupported selection options."""
    args = shlex.split(command)
    args = args[args.index("pytest") + 1 :]
    roots, ignored = [], []
    while args:
        arg = args.pop(0)
        if arg == "--ignore":
            if not args or not args[0].startswith("tests/"):
                raise ValueError(f"Unsupported pytest argument: {arg}")
            ignored.append(Path(args.pop(0)))
        elif arg.startswith("--ignore=tests/"):
            ignored.append(Path(arg.split("=", 1)[1]))
        elif arg.startswith("tests/") and "::" not in arg:
            paths = glob.glob(arg, root_dir=root) if glob.has_magic(arg) else [arg]
            roots.extend(Path(path) for path in paths)
        elif arg.startswith("tests/") and "::" in arg:
            # A selected node cannot establish execution of its whole file.
            continue
        elif arg in {"-p", "--tb", "--color", "--junitxml", "--basetemp"}:
            if not args or args[0].startswith("-"):
                raise ValueError(f"Unsupported pytest argument: {arg}")
            args.pop(0)
        elif arg == "-o":
            if not args or not args[0].startswith("junit_family="):
                raise ValueError(f"Unsupported pytest argument: {arg}")
            args.pop(0)
        elif arg in {"-q", "-v", "--no-header", "--no-summary", "--strict-markers"}:
            continue
        elif re.fullmatch(r"-r[aAfEsxXpPwN]+", arg):
            continue
        elif arg.startswith(("--tb=", "--color=", "--junitxml=", "--basetemp=")):
            continue
        else:
            raise ValueError(f"Unsupported pytest argument: {arg}")
    return roots, ignored


def covers(path, command):
    roots, ignored = pytest_paths(command)
    return any(path == root or root in path.parents for root in roots) and not any(
        path == root or root in path.parents for root in ignored
    )


def test_declared_test_roots_are_executed():
    """Existing routed tests need matching pytest arguments, not metadata alone."""
    missing = []
    for line in (ROOT / "tests/DOMAINS.md").read_text().splitlines():
        cells = line.split("|")
        if len(cells) < 4 or (name := "tests-" + cells[1].strip()) not in SHARDS:
            continue
        roots, _ = pytest_paths(SHARDS[name]["command"])
        for root in re.findall(r"`(tests/[^`]+)`", cells[2]):
            if (ROOT / root).exists() and root.endswith(("/", ".py")) and Path(root) not in roots:
                missing.append(f"{name}: {root}")
    assert not missing, f"{len(missing)} missing root assignments:\n" + "\n".join(missing)


def test_every_test_file_has_an_execution_assignment():
    """Exercise the sweep's real command reader, including sweep-only paths."""
    sweep = yaml.safe_load((ROOT / ".github/workflows/full-matrix-sweep.yml").read_text())
    steps = sweep["jobs"]["sweep-tests"]["steps"]
    reader = next(step for step in steps if step.get("id") == "gate")
    script = reader["run"].split("<<'PY'\n", 1)[1].split("\nPY\n", 1)[0]
    commands = []
    for name, gate in SHARDS.items():
        assert gate["enabled"], name
        env = {**os.environ, "GATE_NAME": name}
        result = subprocess.run(
            [sys.executable, "-c", script], cwd=ROOT, env=env,
            check=True, capture_output=True, text=True,
        )
        commands.extend((gate["command"], result.stdout.strip()))
    files = candidate_test_files(ROOT)
    assert files, "No test files found"
    pr_commands = [gate["command"] for gate in SHARDS.values()]
    missing = [str(path) for path in files if not any(covers(path, cmd) for cmd in commands)]
    assert not missing, f"{len(missing)} unassigned test files:\n" + "\n".join(missing)
    sweep_only = {path for path in files if not any(covers(path, cmd) for cmd in pr_commands)}
    assert not (unexpected := validate_sweep_only(sweep_only, SWEEP_ONLY)), unexpected
    print(f"Static assignments: {len(files)} files; {len(sweep_only)} sweep-only")
    for path in sorted(sweep_only):
        print(f"Sweep-only: {path}: {SWEEP_ONLY[path]}")


def test_collection_exclusions_are_explicit_and_reasoned():
    controls = collection_controls(ROOT)
    assert not (errors := validate_collection_exceptions(controls, COLLECTION_EXCEPTIONS)), errors
    for key, paths in controls.items():
        print(f"Collection exception: {key}: {len(paths)} candidate files: {COLLECTION_EXCEPTIONS[key]}")


@pytest.mark.parametrize(
    ("path", "arguments", "expected"),
    [
        ("tests/a/test_one.py", "tests/a/", True),
        ("tests/ab/test_one.py", "tests/a/", False),
        ("tests/a/test_one.py", "--ignore tests/a/ tests/", False),
        ("tests/a/test_one.py", "--ignore=tests/a/test_one.py tests/a/", False),
        ("tests/a/test_one.py", "tests/a/test_one.py::test_one", False),
        ("tests/a/test_one.py", "tests/a/test_one.py", True),
    ],
)
def test_execution_path_matching(path, arguments, expected):
    assert covers(Path(path), f"python -m pytest {arguments} -q") is expected


def test_pytest_paths_expands_shell_globs(tmp_path):
    directory = tmp_path / "tests" / "module"
    directory.mkdir(parents=True)
    (directory / "test_one.py").write_text("")
    (directory / "helper.py").write_text("")
    roots, _ = pytest_paths("python -m pytest tests/module/test_*.py", root=tmp_path)
    assert roots == [Path("tests/module/test_one.py")]


@pytest.mark.parametrize("selector", [
    "--ignore-glob=tests/a/*", "--ignore-glob tests/a/*",
    "--deselect=tests/a/test_one.py::test_one",
    "--deselect tests/a/test_one.py::test_one",
    "-k one", "-kone", "-m unit", "-munit",
    "--pyargs", "--confcutdir=tests/a", "--unknown-selection",
])
def test_pytest_paths_rejects_unsupported_selection(selector):
    with pytest.raises(ValueError, match="Unsupported pytest argument"):
        pytest_paths(f"python -m pytest tests/a/ {selector}")


def test_sweep_only_inventory_is_explicit_and_shrink_only():
    expected = {Path("tests/data/test_fetch.py"), Path("tests/test_steel_shapes.py")}
    reasons = {path: "Scheduled data checks; tracked by #2083." for path in expected}
    assert validate_sweep_only(expected, reasons) == []
    assert validate_sweep_only(expected | {Path("tests/new.py")}, reasons)
    assert validate_sweep_only(expected - {Path("tests/data/test_fetch.py")}, reasons) == []
    assert validate_sweep_only(expected, {path: "" for path in expected})


def test_collection_controls_require_reasoned_exceptions(tmp_path):
    tests = tmp_path / "tests"
    tests.mkdir()
    hidden = tests / "hidden" / "test_one.py"
    hidden.parent.mkdir()
    hidden.write_text("")
    (tests / "conftest.py").write_text('collect_ignore = ["hidden/test_one.py"]')
    (tmp_path / "pytest.ini").write_text("[pytest]\nnorecursedirs = hidden\n")
    controls = collection_controls(tmp_path)
    assert controls == {
        "tests/conftest.py:collect_ignore:tests/hidden/test_one.py": {Path("tests/hidden/test_one.py")},
        "pytest.ini:norecursedirs:hidden": {Path("tests/hidden/test_one.py")},
    }
    reasons = {key: "Fixture dependency is absent; tracked by #2083." for key in controls}
    assert validate_collection_exceptions(controls, reasons) == []
    assert validate_collection_exceptions(controls, {})
    assert validate_collection_exceptions(controls, {key: "" for key in controls})
    (tests / "conftest.py").write_text('collect_ignore = make_ignores()')
    with pytest.raises(ValueError, match="Unsupported collection control"):
        collection_controls(tmp_path)


def test_historical_result_table_has_all_columns():
    from html.parser import HTMLParser

    class Tables(HTMLParser):
        def __init__(self):
            super().__init__()
            self.rows = []
            self.row = []
            self.in_cell = False

        def handle_starttag(self, tag, attrs):
            if tag == "tr":
                self.row = []
            elif tag in {"td", "th"}:
                self.in_cell = True
                self.row.append("")

        def handle_data(self, data):
            if self.in_cell:
                self.row[-1] += data

        def handle_endtag(self, tag):
            if tag in {"td", "th"}:
                self.in_cell = False
            elif tag == "tr":
                self.rows.append(self.row)

    parser = Tables()
    parser.feed((ROOT / "docs/ci/module-family-coverage.html").read_text())
    header = next(i for i, row in enumerate(parser.rows) if row[0] == "Shard")
    results = parser.rows[header + 1:header + 6]
    assert all(len(row) == 5 for row in results), results
    assert next(row for row in results if row[0] == "misc")[1:] == ["100", "8", "1", "0"]
    assert [sum(int(row[column]) for row in results[:-1]) for column in range(1, 5)] == [
        int(value) for value in results[-1][1:]
    ]


@pytest.mark.parametrize("control", [
    "collect_ignore = make_ignores()",
    "collect_ignore_glob = ['test_*.py']",
    "collect_ignore = []; collect_ignore.append('test_one.py')",
    "collect_ignore = []; collect_ignore += ['test_one.py']",
    "def pytest_ignore_collect(collection_path, config): return True",
    "_tests_dir = elsewhere; collect_ignore = [str(_tests_dir / 'test_one.py')]",
])
def test_dynamic_collection_controls_fail_closed(tmp_path, control):
    (tmp_path / "tests").mkdir()
    (tmp_path / "tests/conftest.py").write_text(control)
    with pytest.raises(ValueError, match="Unsupported collection control"):
        collection_controls(tmp_path)


def test_repository_guard_rejects_unlisted_sweep_only_file(tmp_path, monkeypatch):
    (tmp_path / "tests/new").mkdir(parents=True)
    (tmp_path / "tests/new/test_one.py").write_text("")
    workflow = tmp_path / ".github/workflows/full-matrix-sweep.yml"
    workflow.parent.mkdir(parents=True)
    workflow.write_text(yaml.safe_dump({"jobs": {"sweep-tests": {"steps": [{
        "id": "gate", "run": "command=$(python - <<'PY'\nprint('python -m pytest tests/')\nPY\n)"
    }]}}}))
    monkeypatch.setattr(sys.modules[__name__], "ROOT", tmp_path)
    monkeypatch.setattr(sys.modules[__name__], "SHARDS", {
        "tests-fixture": {"enabled": True, "command": "python -m pytest tests/owned/"}
    })
    with pytest.raises(AssertionError, match="tests/new/test_one.py"):
        test_every_test_file_has_an_execution_assignment()


def test_root_collection_exclusion_is_reported(tmp_path):
    (tmp_path / "tests").mkdir()
    (tmp_path / "tests/test_one.py").write_text("")
    (tmp_path / "conftest.py").write_text('collect_ignore = ["tests"]')
    assert collection_controls(tmp_path) == {
        "conftest.py:collect_ignore:tests": {Path("tests/test_one.py")}
    }


@pytest.mark.parametrize("arguments", [
    "--ignore", "-p", "-o norecursedirs=tests", "-o addopts=-kone",
    "--override-ini=norecursedirs=tests", "-c other.ini",
])
def test_pytest_paths_rejects_missing_values_and_collection_overrides(arguments):
    with pytest.raises(ValueError, match="Unsupported pytest argument"):
        pytest_paths(f"python -m pytest tests/ {arguments}")


def test_pytest_paths_consumes_junit_suffix_values():
    roots, ignored = pytest_paths(
        "python -m pytest tests/a/ --junitxml tests/report.xml -o junit_family=xunit2"
    )
    assert roots == [Path("tests/a")]
    assert ignored == []
