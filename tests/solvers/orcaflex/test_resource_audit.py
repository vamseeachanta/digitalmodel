# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Source-only resource audit tests; fixtures are synthetic Python snippets."""

import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

from digitalmodel.solvers.orcaflex.resource_audit import (
    audit_source,
    audit_tree,
    evaluate_budget,
)


@pytest.mark.parametrize(
    "expression,state,argument",
    [
        ("api.Model()", "DEFAULT_CORES", None),
        ("api.Model('case.dat')", "DEFAULT_CORES", None),
        ("api.Model(threadCount=None)", "DEFAULT_CORES", "None"),
        ("api.Model(threadCount=3)", "EXPLICIT_LITERAL", "3"),
        ("api.Model(None, 3)", "EXPLICIT_LITERAL", "3"),
        ("api.Model(threadCount=config.threads)", "NOT_EVALUATED", "config.threads"),
        ("api.Model(**options)", "NOT_EVALUATED", None),
        ("api.Model(*args)", "NOT_EVALUATED", None),
        ("api.Model(threadCount=0)", "NOT_EVALUATED", "0"),
        ("api.Model(threadCount=True)", "NOT_EVALUATED", "True"),
        ("api.Model(threadCount=-2)", "NOT_EVALUATED", "-2"),
        ("api.Model(threadCount=3, **options)", "NOT_EVALUATED", "3"),
        ("api.Model(None, None)", "DEFAULT_CORES", "None"),
        ("api.Model(handle=existing)", "NOT_EVALUATED", None),
        ("api.Model(None, 3, existing)", "NOT_EVALUATED", "3"),
    ],
)
def test_constructor_arguments(expression, state, argument):
    """Orcina Model construction, online edition retrieved 2026: second arg is threads.

    Positive integer literals are syntax observations only; fixture value three
    is an explicit test input, not a recommended native solver configuration.
    """
    (row,) = audit_source(f"model = {expression}", "example.py")
    assert row["kind"] == "model_candidate"
    assert row["thread_argument"] == argument
    assert row["thread_declaration"] == state
    assert row["budget_status"] == "NOT_EVALUATED"


def test_direct_import_alias_and_no_execution():
    """An import declaration supplies the direct constructor alias without import."""
    rows = audit_source(
        "from OrcFxAPI import Model as NativeModel\n"
        "raise RuntimeError('source must never execute')\n"
        "model = NativeModel(threadCount=2)\n",
        "example.py",
    )
    assert len(rows) == 1
    assert rows[0]["call"] == "NativeModel"
    assert rows[0]["line"] == 3


def test_scope_evidence_separates_operations_and_nested_functions():
    """Operations are lexical evidence, not a call-graph or receiver proof."""
    rows = audit_source(
        "class Runner:\n"
        "    def extract(self):\n"
        "        model = api.Model()\n"
        "        model.TimeHistory('X')\n"
        "        def convert():\n"
        "            other = api.Model()\n"
        "            other.SaveData('copy.yml')\n"
        "    def solve(self):\n"
        "        model = api.Model()\n"
        "        model.CalculateStatics()\n"
        "        model.RunSimulation()\n",
        "example.py",
    )
    assert [row["scope"] for row in rows] == [
        "Runner.extract",
        "Runner.extract.convert",
        "Runner.solve",
    ]
    assert [row["scope_operations"] for row in rows] == [
        ["extraction"],
        ["conversion"],
        ["simulation"],
    ]


@pytest.mark.parametrize(
    "definition,body_scope",
    [
        (
            "def extract(m=api.Model(), ready=model.RunSimulation()):\n"
            "    m.TimeHistory('X')\n"
            "    api.Model()\n",
            "extract",
        ),
        (
            "async def extract(*, m=api.Model(), ready=model.RunSimulation()):\n"
            "    m.TimeHistory('X')\n"
            "    api.Model()\n",
            "extract",
        ),
        (
            "@decorate(api.Model(), model.RunSimulation())\n"
            "def extract():\n"
            "    model.TimeHistory('X')\n"
            "    api.Model()\n",
            "extract",
        ),
        (
            "extract = lambda m=api.Model(), ready=model.RunSimulation(): "
            "(m.TimeHistory('X'), api.Model())\n",
            "<lambda>",
        ),
        (
            "@decorate(api.Model(), model.RunSimulation())\n"
            "class Extract:\n"
            "    model.TimeHistory('X')\n"
            "    api.Model()\n",
            "Extract",
        ),
        (
            "class Extract(base(api.Model(), model.RunSimulation())):\n"
            "    model.TimeHistory('X')\n"
            "    api.Model()\n",
            "Extract",
        ),
        (
            "def extract(m: annotate(api.Model(), model.RunSimulation())):\n"
            "    m.TimeHistory('X')\n"
            "    api.Model()\n",
            "extract",
        ),
    ],
)
@pytest.mark.parametrize("nested", [False, True])
def test_definition_expressions_belong_to_enclosing_scope(
    definition, body_scope, nested
):
    """Definition expressions and body calls share identical inventory boundaries."""
    source = "model = api.Model()\n" + definition
    parent_scope = "<module>"
    if nested:
        source = "def outer():\n" + "".join(
            "    " + line for line in source.splitlines(keepends=True)
        )
        parent_scope = "outer"
        body_scope = "outer." + body_scope
    rows = audit_source(source, "example.py")
    assert len(rows) == 3
    assert [row["scope"] for row in rows] == [
        parent_scope,
        parent_scope,
        body_scope,
    ]
    assert [row["scope_operations"] for row in rows] == [
        ["simulation"],
        ["simulation"],
        ["extraction"],
    ]


def test_tree_rejects_fifo_without_waiting_for_a_writer(tmp_path):
    """A Python-suffixed FIFO is reported without opening its blocking stream."""
    os.mkfifo(tmp_path / "stream.py")
    script = (
        Path(__file__).resolve().parents[3]
        / "src/digitalmodel/solvers/orcaflex/resource_audit.py"
    )
    result = subprocess.run(
        [sys.executable, str(script), str(tmp_path)],
        capture_output=True,
        text=True,
        timeout=3,
        check=False,
    )
    assert result.returncode == 1, result.stderr
    report = json.loads(result.stdout)
    assert report["errors"] == [{"path": "stream.py", "type": "UnsupportedFileType"}]
    assert report["files_scanned"] == []
    assert report["source_sha256"] == {}
    assert report["findings"] == []


def test_worker_declarations_are_not_associated_with_models():
    """An unrelated pool cannot establish a model's concurrent worker count."""
    rows = audit_source(
        "model = api.Model(threadCount=2)\n"
        "with ExecutorClass(max_workers=self.num_threads) as executor:\n"
        "    pass\n"
        "with concurrent.futures.ProcessPoolExecutor(4) as executor:\n"
        "    pass\n",
        "example.py",
    )
    assert [row["kind"] for row in rows] == [
        "model_candidate",
        "pool_candidate",
        "pool_candidate",
    ]
    assert [row["worker_argument"] for row in rows[1:]] == ["self.num_threads", "4"]
    assert all(row["budget_status"] == "NOT_EVALUATED" for row in rows)


def test_tree_reports_coverage_and_does_not_follow_symlinks(tmp_path):
    """Only regular Python source within the selected root is claimed covered."""
    source = tmp_path / "source"
    source.mkdir()
    (source / "b.py").write_text("m = api.Model()\n", encoding="utf-8")
    (source / "a.py").write_text("# no constructor\n", encoding="utf-8")
    outside = tmp_path / "outside.py"
    outside.write_text("raise AssertionError('outside root')", encoding="utf-8")
    (source / "linked.py").symlink_to(outside)
    report = audit_tree(source)
    assert report["files_scanned"] == ["a.py", "b.py"]
    assert report["skipped_symlinks"] == ["linked.py"]
    assert report["findings"][0]["path"] == "b.py"
    assert report["budget_status"] == "NOT_EVALUATED"


def test_tree_records_parse_errors_instead_of_silent_coverage(tmp_path):
    """A syntax error prevents a complete inventory and remains visible."""
    (tmp_path / "broken.py").write_text("def broken(\n", encoding="utf-8")
    report = audit_tree(tmp_path)
    assert report["files_scanned"] == []
    assert report["errors"][0]["path"] == "broken.py"
    assert report["errors"][0]["type"] == "SyntaxError"


@pytest.mark.parametrize(
    "workers,threads,cpu,status,demand",
    [
        (4, 3, 8, "EXCEEDS_BUDGET", 12),
        (4, 2, 8, "WITHIN_BUDGET", 8),
        (2, 3, 8, "WITHIN_BUDGET", 6),
        (4, None, 8, "NOT_EVALUATED", None),
        (None, 2, 8, "NOT_EVALUATED", None),
        (4, 2, None, "NOT_EVALUATED", None),
    ],
)
def test_budget_uses_explicit_inputs(workers, threads, cpu, status, demand):
    """Independent hand calculation, with required synthetic input counts.

    Four workers each reserving three threads enumerate as [3, 3, 3, 3],
    whose sum is twelve, exceeding eight allocated CPU slots. Replacing three
    by two gives [2, 2, 2, 2], summing to eight. Two workers reserving three
    each give [3, 3], summing to six. Missing terms cannot establish demand.
    """
    result = evaluate_budget(workers, threads, cpu)
    assert result["status"] == status
    assert result["requested_threads"] == demand


@pytest.mark.parametrize("bad", [0, -1, True, 1.5, "2"])
@pytest.mark.parametrize("field", range(3))
def test_budget_rejects_invalid_inputs_even_if_another_is_missing(bad, field):
    """Counts must be positive integers; missing values do not mask bad inputs."""
    values = [None, None, None]
    values[field] = bad
    with pytest.raises(ValueError, match="positive integer"):
        evaluate_budget(*values)


def test_cli_does_not_import_solver_and_emits_relative_paths(tmp_path):
    """A subprocess import fence rejects any attempt to load the native API."""
    target = tmp_path / "source"
    target.mkdir()
    (target / "example.py").write_text(
        "import OrcFxAPI\nmodel = OrcFxAPI.Model()\n", encoding="utf-8"
    )
    command = (
        "import sys, runpy\n"
        "sys.path.insert(0, sys.argv.pop(1))\n"
        "class Fence:\n"
        "    def find_spec(self, fullname, *args):\n"
        "        if fullname == 'OrcFxAPI' or fullname.endswith('.orcaflex_api'):\n"
        "            raise AssertionError('native API import attempted')\n"
        "sys.meta_path.insert(0, Fence())\n"
        "sys.argv = ['resource_audit', sys.argv[1]]\n"
        "runpy.run_module('digitalmodel.solvers.orcaflex.resource_audit', "
        "run_name='__main__')\n"
    )
    result = subprocess.run(
        [
            sys.executable,
            "-c",
            command,
            str(Path(__file__).resolve().parents[3] / "src"),
            str(target),
        ],
        text=True,
        capture_output=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    report = json.loads(result.stdout)
    assert report["files_scanned"] == ["example.py"]
    assert report["findings"][0]["thread_declaration"] == "DEFAULT_CORES"
    assert str(tmp_path) not in result.stdout


def test_current_parallel_implementations_are_in_inventory():
    """Repository-source oracles: named constructors and pools are read as text."""
    root = Path(__file__).resolve().parents[3] / "src/digitalmodel/solvers/orcaflex"
    report = audit_tree(root)
    rows = report["findings"]
    for name in ("orcaflex_optimized_parallel.py", "orcaflex_optimized_parallel_v2.py"):
        (constructor,) = [
            r for r in rows if r["path"] == name and r["kind"] == "model_candidate"
        ]
        assert constructor["thread_declaration"] == "DEFAULT_CORES"
        assert constructor["scope_operations"] == ["conversion", "simulation"]
        assert any(r["path"] == name and r["kind"] == "pool_candidate" for r in rows)


def test_pool_import_alias_with_positional_workers():
    """Python concurrent.futures Executor API: first argument is max_workers."""
    rows = audit_source(
        "from concurrent.futures import ProcessPoolExecutor as Processes\n"
        "with Processes(4):\n"
        "    pass\n",
        "example.py",
    )
    assert len(rows) == 1
    assert rows[0]["worker_argument"] == "4"


def test_pool_alias_named_pool_keeps_keyword_argument():
    """An executor alias called Pool still uses its max_workers keyword."""
    (row,) = audit_source("Pool(max_workers=7)", "example.py")
    assert row["worker_argument"] == "7"


def test_tree_has_repeatable_digests_and_records_read_errors(tmp_path, monkeypatch):
    """SHA-256 covers the exact bytes inspected, including the encoding header."""
    import hashlib

    data = b"# coding: latin-1\n# \xe9\nm = api.Model()\n"
    path = tmp_path / "example.py"
    path.write_bytes(data)
    report = audit_tree(tmp_path)
    assert report == audit_tree(tmp_path)
    assert report["source_sha256"] == {"example.py": hashlib.sha256(data).hexdigest()}
    assert report["errors"] == []

    def denied(self):
        raise PermissionError("read denied")

    monkeypatch.setattr(Path, "read_bytes", denied)
    report = audit_tree(tmp_path)
    assert report["errors"] == [{"path": "example.py", "type": "PermissionError"}]
    assert report["files_scanned"] == []


def test_tree_rejects_missing_root(tmp_path):
    """A missing root cannot be reported as a complete empty inventory."""
    with pytest.raises(ValueError, match="regular directory"):
        audit_tree(tmp_path / "missing")


def test_cli_returns_nonzero_with_parse_error(tmp_path):
    """An incomplete inventory cannot exit successfully merely because JSON exists."""
    from digitalmodel.solvers.orcaflex.resource_audit import main

    (tmp_path / "broken.py").write_text("def broken(\n", encoding="utf-8")
    assert main([str(tmp_path)]) == 1


def test_scope_with_no_recognized_operation_remains_unclassified():
    """A bare constructor does not imply a simulation, extraction or conversion."""
    (row,) = audit_source("model = api.Model()", "example.py")
    assert row["scope_operations"] == []
