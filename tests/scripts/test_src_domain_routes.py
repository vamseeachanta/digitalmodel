# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Source-package routing: the measured table, its derivation and the read trace."""

import importlib.util
import json
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
CI = ROOT / "scripts/ci"
TABLE = CI / "src_domain_routes.json"


def load(name: str):
    spec = importlib.util.spec_from_file_location(name, CI / f"{name}.py")
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def detector():
    return load("detect_touched_domains")


@pytest.fixture(scope="module")
def derive():
    return load("derive_src_routes")


@pytest.fixture(scope="module")
def trace():
    return load("trace_src_reads")


def domains_of(detector, *names: str):
    return [detector.Domain(name, (f"tests/{name}/",)) for name in names]


def write_table(path: Path, routes: dict, unrouted: dict | None = None) -> Path:
    path.write_text(json.dumps({"routes": routes, "unrouted": unrouted or {}}), encoding="utf-8")
    return path


TABLE_ROWS = {
    "src/digitalmodel/solvers/": ["orcaflex", "smoke", "subsea"],
    "src/digitalmodel/solvers/orcaflex/": ["orcaflex", "subsea"],
    "src/digitalmodel/engine.py": ["misc"],
}
UNROUTED_ROWS = {
    "src/digitalmodel/infrastructure/": "shared core: every shard reads it",
    "src/digitalmodel/solvers/unread/": "no shard opened it during the trace",
}
NAMES = ("orcaflex", "smoke", "subsea", "misc", "other")


# --- detector: table lookup -------------------------------------------------


@pytest.mark.parametrize(("path", "expected"), [
    ("src/digitalmodel/solvers/orcaflex/model/line.py", {"orcaflex", "subsea"}),
    ("src/digitalmodel/solvers/orcaflex/data/curve.csv", {"orcaflex", "subsea"}),
    # A file directly in the group folder takes the group row: the union of its readers.
    ("src/digitalmodel/solvers/__init__.py", {"orcaflex", "smoke", "subsea"}),
    # A subfolder added after the trace has no row of its own and takes the group row.
    ("src/digitalmodel/solvers/added_later/x.py", {"orcaflex", "smoke", "subsea"}),
    ("src/digitalmodel/engine.py", {"misc"}),
    # Listed as unrouted: no route.
    ("src/digitalmodel/infrastructure/utils/x.py", None),
    ("src/digitalmodel/solvers/unread/x.py", None),
    # Not in the table at all: no route.
    ("src/digitalmodel/new_package/x.py", None),
    ("src/digitalmodel/new_module.py", None),
    ("src/digitalmodel/enginex.py", None),
    ("tests/solvers/orcaflex/test_x.py", None),
    ("src/other/solvers/orcaflex/x.py", None),
])
def test_route_lookup_uses_the_deepest_known_unit(detector, tmp_path, path, expected) -> None:
    table = detector.load_src_routes(write_table(tmp_path / "t.json", TABLE_ROWS, UNROUTED_ROWS))
    assert detector.src_route_names(path, table, set(NAMES)) == expected


def test_route_naming_an_unknown_shard_is_no_route(detector, tmp_path) -> None:
    """A row that names a shard the domains file lacks cannot be trusted to be complete."""
    table = detector.load_src_routes(write_table(tmp_path / "t.json", TABLE_ROWS))
    path = "src/digitalmodel/solvers/orcaflex/x.py"
    assert detector.src_route_names(path, table, {"orcaflex"}) is None


@pytest.mark.parametrize("text", [
    "", "not json", "[]", '{"routes": []}', '{"routes": {"src/digitalmodel/a/": "x"}}',
    '{"routes": {"src/digitalmodel/a/": []}}', '{"routes": {"src/digitalmodel/a/": [1]}}',
    '{"routes": {"a/": ["x"]}}', '{"routes": {}, "unrouted": []}',
])
def test_unreadable_table_routes_nothing(detector, tmp_path, text) -> None:
    path = tmp_path / "t.json"
    path.write_text(text, encoding="utf-8")
    assert detector.load_src_routes(path) == {}
    assert detector.load_src_routes(tmp_path / "missing.json") == {}


# --- detector: selection ----------------------------------------------------


def selected(detector, tmp_path, changed: list[str], use_table: bool = True) -> set[str]:
    table = detector.load_src_routes(write_table(tmp_path / "t.json", TABLE_ROWS, UNROUTED_ROWS))
    domains = domains_of(detector, *NAMES)
    chosen = detector.touched_domains(changed, domains, src_routes=table if use_table else None)
    return {domain.name for domain in chosen}


def test_routed_source_change_selects_only_its_readers(detector, tmp_path) -> None:
    assert selected(detector, tmp_path, ["src/digitalmodel/solvers/orcaflex/x.py"]) == {
        "orcaflex", "subsea"}


def test_routed_changes_are_unioned_with_test_changes(detector, tmp_path) -> None:
    changed = ["src/digitalmodel/engine.py", "src/digitalmodel/solvers/orcaflex/x.py",
               "tests/other/test_x.py"]
    assert selected(detector, tmp_path, changed) == {"misc", "orcaflex", "subsea", "other"}


@pytest.mark.parametrize("unrouted", [
    "src/digitalmodel/infrastructure/x.py", "src/digitalmodel/new_package/x.py",
    "src/digitalmodel/solvers/unread/x.py", "src/digitalmodel/__init__.py",
])
def test_one_unrouted_source_change_keeps_the_full_matrix(detector, tmp_path, unrouted) -> None:
    changed = ["src/digitalmodel/solvers/orcaflex/x.py", unrouted]
    assert selected(detector, tmp_path, changed) == set(NAMES)


def test_without_a_table_every_source_change_keeps_the_full_matrix(detector, tmp_path) -> None:
    changed = ["src/digitalmodel/solvers/orcaflex/x.py"]
    assert selected(detector, tmp_path, changed, use_table=False) == set(NAMES)


def test_a_change_to_the_table_itself_runs_every_shard(detector, tmp_path) -> None:
    assert selected(detector, tmp_path, ["scripts/ci/src_domain_routes.json"]) == set(NAMES)


def test_hand_written_routes_still_win(detector, tmp_path) -> None:
    """DOMAIN_PATHS rows are applied before the measured table, as before."""
    table = {"src/digitalmodel/citations/": ["misc"]}
    loaded = detector.load_src_routes(write_table(tmp_path / "t.json", table))
    domains = domains_of(detector, "citations", "misc")
    chosen = detector.touched_domains(["src/digitalmodel/citations/x.py"], domains, src_routes=loaded)
    assert {domain.name for domain in chosen} == {"citations"}


def run_cli(detector_path: Path, repo: Path, *extra: str) -> dict:
    result = subprocess.run(
        [sys.executable, str(detector_path), "--mode", "touched", "--base", "HEAD~1", "--head", "HEAD",
         "--domains-file", "DOMAINS.md", "--output-format", "json-matrix", *extra],
        cwd=repo, check=True, text=True, capture_output=True, encoding="utf-8")
    return json.loads(result.stdout.strip().splitlines()[-1])


def git(repo: Path, *args: str) -> None:
    subprocess.run(["git", "-c", "user.name=t", "-c", "user.email=t@example.invalid", *args],
                   cwd=repo, check=True, capture_output=True)


def test_command_line_reads_the_table_named_by_the_option(tmp_path) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    git(repo, "init", "-q")
    rows = "\n".join(f"| {name} | `tests/{name}/` | x |" for name in NAMES)
    (repo / "DOMAINS.md").write_text(f"| Domain | Test roots | Notes |\n| --- | --- | --- |\n{rows}\n")
    git(repo, "add", "DOMAINS.md")
    git(repo, "commit", "-q", "-m", "base")
    target = repo / "src/digitalmodel/solvers/orcaflex/x.py"
    target.parent.mkdir(parents=True)
    target.write_text("x = 1\n")
    git(repo, "add", "src")
    git(repo, "commit", "-q", "-m", "change")
    table = write_table(tmp_path / "t.json", TABLE_ROWS, UNROUTED_ROWS)
    script = CI / "detect_touched_domains.py"
    routed = run_cli(script, repo, "--src-routes", str(table))
    assert [row["domain"] for row in routed["include"]] == ["orcaflex", "subsea"]
    absent = run_cli(script, repo, "--src-routes", str(tmp_path / "missing.json"))
    assert [row["domain"] for row in absent["include"]] == list(NAMES)


# --- derivation -------------------------------------------------------------


@pytest.mark.parametrize(("record", "expected"), [
    ("/", []),
    ("__init__.py", ["__init__.py"]),
    ("engine.py", ["engine.py"]),
    ("__pycache__/engine.cpython-311.pyc", ["engine.py"]),
    ("__pycache__/engine.cpython-311.opt-1.pyc", ["engine.py"]),
    ("__pycache__/", []),
    ("solvers/", ["solvers/"]),
    ("solvers/__init__.py", ["solvers/"]),
    ("solvers/__pycache__/__init__.cpython-311.pyc", ["solvers/"]),
    ("solvers/__pycache__/", ["solvers/"]),
    ("solvers/orcaflex/", ["solvers/", "solvers/orcaflex/"]),
    ("solvers/orcaflex/model/line.py", ["solvers/", "solvers/orcaflex/"]),
    ("solvers/orcaflex/__pycache__/x.cpython-311.pyc", ["solvers/", "solvers/orcaflex/"]),
    ("solvers/orcaflex/model/", ["solvers/", "solvers/orcaflex/"]),
])
def test_record_to_unit_keys(derive, record, expected) -> None:
    assert derive.keys_for(record) == expected


def make_tree(root: Path) -> None:
    files = {
        "src/digitalmodel/__init__.py": "",
        "src/digitalmodel/engine.py": "",
        "src/digitalmodel/core/__init__.py": "",
        "src/digitalmodel/solvers/__init__.py": "",
        "src/digitalmodel/solvers/orcaflex/__init__.py": "",
        "src/digitalmodel/solvers/mesh/__init__.py": "",
        "src/digitalmodel/solvers/unread/__init__.py": "",
        "src/digitalmodel/lonely/__init__.py": "",
        "tests/DOMAINS.md": "| Domain | Test roots | Notes |\n| --- | --- | --- |\n"
                            "| alpha | `tests/alpha/` | x |\n| beta | `tests/beta/` | x |\n"
                            "| gamma | `tests/gamma/` | x |\n",
        "tests/alpha/test_a.py": "from digitalmodel.solvers.orcaflex import x\n",
        "tests/beta/test_b.py": "import digitalmodel.engine\n",
        "tests/gamma/test_c.py": "",
    }
    for name, text in files.items():
        target = root / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8")


READS = {
    "alpha": {"/", "__init__.py", "core/__init__.py", "solvers/__init__.py",
              "solvers/orcaflex/__init__.py", "gone/__init__.py"},
    "beta": {"__init__.py", "core/__init__.py", "engine.py", "solvers/__init__.py",
             "solvers/mesh/__init__.py"},
    "gamma": {"__init__.py", "core/"},
}


COMPLETED = {"alpha": "pass", "beta": "fail", "gamma": "pass"}


def test_globbed_roots_read_every_matching_test(derive, tmp_path) -> None:
    make_tree(tmp_path)
    folder = tmp_path / "tests/alpha"
    (folder / "test_b.py").write_text("import digitalmodel.engine")
    domain = derive.detector.Domain("alpha", ("tests/alpha/test_*.py",))
    assert derive.named_units(tmp_path, [domain])["alpha"] == {
        "solvers/", "solvers/orcaflex/", "engine.py"}


@pytest.mark.parametrize("pattern", ["tests/missing_*.py", "tests/missing.py"])
def test_unmatched_root_is_an_error(derive, tmp_path, pattern) -> None:
    make_tree(tmp_path)
    domain = derive.detector.Domain("alpha", (pattern,))
    with pytest.raises(ValueError) as exc:
        derive.named_units(tmp_path, [domain])
    assert pattern in str(exc.value) and "alpha" in str(exc.value)


def test_trace_without_a_result_reads_everything(derive, tmp_path) -> None:
    make_tree(tmp_path)
    table = derive.build(tmp_path, READS, commit="c" * 40, shards={"alpha": "pass", "beta": "pass"})
    assert table["routes"]["src/digitalmodel/engine.py"] == ["beta", "gamma"]
    assert table["routes"]["src/digitalmodel/solvers/orcaflex/"] == ["alpha", "gamma"]


def test_build_routes_from_recorded_reads(derive, tmp_path) -> None:
    make_tree(tmp_path)
    table = derive.build(tmp_path, READS, commit="c" * 40, shards=COMPLETED)
    assert table["routes"] == {
        "src/digitalmodel/engine.py": ["beta"],
        "src/digitalmodel/solvers/": ["alpha", "beta"],
        "src/digitalmodel/solvers/mesh/": ["beta"],
        "src/digitalmodel/solvers/orcaflex/": ["alpha"],
    }
    assert table["unrouted"] == {
        "src/digitalmodel/__init__.py": derive.EVERY,
        "src/digitalmodel/core/": derive.EVERY,
        "src/digitalmodel/lonely/": derive.NOBODY,
        "src/digitalmodel/solvers/unread/": derive.NOBODY,
    }
    assert table["trace"] == {"commit": "c" * 40, "shards": COMPLETED}
    assert derive.drift(tmp_path, table) == []


def test_a_shard_that_did_not_finish_reads_everything(derive, tmp_path) -> None:
    make_tree(tmp_path)
    reads = {name: set(paths) for name, paths in READS.items()}
    reads["gamma"] = set()
    table = derive.build(tmp_path, reads, commit="c" * 40,
                         shards={"alpha": "pass", "beta": "fail", "gamma": "timeout"})
    assert table["routes"]["src/digitalmodel/solvers/orcaflex/"] == ["alpha", "gamma"]
    assert table["routes"]["src/digitalmodel/engine.py"] == ["beta", "gamma"]
    assert table["unrouted"]["src/digitalmodel/lonely/"] == derive.NOBODY


def test_a_shard_missing_from_the_trace_reads_everything(derive, tmp_path) -> None:
    make_tree(tmp_path)
    reads = {name: paths for name, paths in READS.items() if name != "gamma"}
    table = derive.build(tmp_path, reads, commit="c" * 40, shards=COMPLETED)
    assert table["routes"]["src/digitalmodel/solvers/orcaflex/"] == ["alpha", "gamma"]


def test_empty_trace_with_a_result_reads_everything(derive, tmp_path) -> None:
    make_tree(tmp_path)
    table = derive.build(tmp_path, {**READS, "gamma": set()}, commit="c" * 40, shards=COMPLETED)
    assert table["routes"]["src/digitalmodel/engine.py"] == ["beta", "gamma"]


def test_test_file_naming_an_unread_unit_is_drift_and_write_repairs_it(derive, tmp_path) -> None:
    make_tree(tmp_path)
    table = derive.build(tmp_path, READS, commit="c" * 40, shards=COMPLETED)
    (tmp_path / "tests/gamma/test_c.py").write_text(
        "from digitalmodel.solvers import mesh\nROOT / 'src/digitalmodel/engine.py'\n", encoding="utf-8")
    found = derive.drift(tmp_path, table)
    assert found == [
        "gamma names src/digitalmodel/engine.py, whose route lacks it",
        "gamma names src/digitalmodel/solvers/, whose route lacks it",
        "gamma names src/digitalmodel/solvers/mesh/, whose route lacks it",
    ]
    repaired = derive.with_named_units(tmp_path, table)
    assert repaired["routes"]["src/digitalmodel/engine.py"] == ["beta", "gamma"]
    assert repaired["routes"]["src/digitalmodel/solvers/mesh/"] == ["beta", "gamma"]
    # The group row stays the union of its subfolders; all three shards now read it.
    assert "src/digitalmodel/solvers/" not in repaired["routes"]
    assert repaired["unrouted"]["src/digitalmodel/solvers/"] == derive.EVERY
    assert derive.drift(tmp_path, repaired) == []


def test_naming_an_unrouted_or_unknown_unit_is_not_drift(derive, tmp_path) -> None:
    make_tree(tmp_path)
    table = derive.build(tmp_path, READS, commit="c" * 40, shards=COMPLETED)
    (tmp_path / "tests/gamma/test_c.py").write_text(
        "import digitalmodel.lonely\nimport digitalmodel.core\nimport digitalmodel.absent\n",
        encoding="utf-8")
    assert derive.drift(tmp_path, table) == []


# --- the committed table ----------------------------------------------------


def test_committed_table_names_real_shards_and_real_paths(detector) -> None:
    payload = json.loads(TABLE.read_text(encoding="utf-8"))
    names = {domain.name for domain in detector.parse_domains(ROOT / "tests/DOMAINS.md")}
    assert payload["routes"], "the table routes nothing"
    assert not set(payload["routes"]) & set(payload["unrouted"])
    for key, route in payload["routes"].items():
        assert route == sorted(set(route)) and set(route) < names, key
        assert (ROOT / key).is_dir() if key.endswith("/") else (ROOT / key).is_file(), key
    assert len(payload["trace"]["commit"]) == 40
    assert set(payload["trace"]["shards"]) == names
    assert detector.load_src_routes(TABLE)


def test_committed_table_covers_every_unit_the_tests_name(derive) -> None:
    payload = json.loads(TABLE.read_text(encoding="utf-8"))
    assert derive.drift(ROOT, payload) == []


def test_group_rows_contain_their_subfolder_rows() -> None:
    payload = json.loads(TABLE.read_text(encoding="utf-8"))
    routes = payload["routes"]
    for key, route in routes.items():
        parent = "/".join(key.split("/")[:3]) + "/"
        if key.count("/") == 4 and parent in routes:
            assert set(route) <= set(routes[parent]), key


# --- the read trace ---------------------------------------------------------


def test_hook_records_opens_and_listings_under_the_source_root(trace, tmp_path) -> None:
    src = tmp_path / "repo/src/digitalmodel"
    (src / "pkg/sub").mkdir(parents=True)
    (src / "pkg/sub/data.txt").write_text("x", encoding="utf-8")
    (tmp_path / "repo/elsewhere.txt").write_text("x", encoding="utf-8")
    site = tmp_path / "site"
    site.mkdir()
    out = tmp_path / "out/shard"
    trace.install(out, src, site)
    code = (
        "import os, site, subprocess, sys\n"
        f"site.addsitedir({str(site)!r})\n"
        f"os.chdir({str(tmp_path / 'repo')!r})\n"
        "open('src/digitalmodel/pkg/sub/data.txt').close()\n"
        "open('elsewhere.txt').close()\n"
        "os.listdir('src/digitalmodel/pkg')\n"
        "list(os.scandir('src/digitalmodel'))\n"
        "child = 'import site, pathlib; site.addsitedir(%r); pathlib.Path(%r).read_text()'\n"
        f"subprocess.run([sys.executable, '-c', child % ({str(site)!r}, "
        f"{str(src / 'pkg/sub/data.txt')!r})], check=True)\n"
    )
    subprocess.run([sys.executable, "-c", code], check=True)
    records = trace.read_records(tmp_path / "out")
    assert records == {"shard": {"pkg/sub/data.txt", "pkg/", "/"}}
    assert len(list(out.glob("*.txt"))) == 2, "the child process writes its own record"
