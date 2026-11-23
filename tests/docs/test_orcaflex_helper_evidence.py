"""Solver-free documentation and source contracts for parallel helpers.

Expected outcomes are derived by hand from the constructor assignments and
the early-return dictionaries in orcaflex_optimized_parallel_v2.py at revision
fb8a214bdb622ed04f0b6473a9311695c9a5a314. No engineering constants are used.
Only the selected class body is compiled; module imports, workers, resource
probes and executors are never evaluated. This is not native runtime evidence.
"""

import ast
import inspect
import logging
import re
from html.parser import HTMLParser
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock

import pytest

ROOT = Path(__file__).resolve().parents[2]
MODULE = ROOT / "src/digitalmodel/solvers/orcaflex"
CLASS_NAME = "OrcaFlexOptimizedParallelAnalysis"


@pytest.fixture
def analysis_class():
    """Evaluate the class without the module-level licensed-interface import."""
    tree = ast.parse(
        (MODULE / "orcaflex_optimized_parallel_v2.py").read_text(encoding="utf-8")
    )
    selected = next(
        node
        for node in tree.body
        if isinstance(node, ast.ClassDef) and node.name == CLASS_NAME
    )
    future = ast.ImportFrom(
        module="__future__", names=[ast.alias(name="annotations")], level=0
    )
    isolated = ast.fix_missing_locations(
        ast.Module(body=[future, selected], type_ignores=[])
    )
    namespace = {
        "Path": Path,
        "os": SimpleNamespace(name="posix"),
        "logger": logging.getLogger(__name__),
    }
    exec(compile(isolated, "<parallel-helper-source-contract>", "exec"), namespace)
    return namespace[CLASS_NAME]


@pytest.mark.parametrize("use_processes", [True, False])
def test_constructor_selects_dynamic_allocation(analysis_class, use_processes):
    """By assignment: omitted count stays None and enables dynamic_threads."""
    analysis = analysis_class(use_processes=use_processes)
    assert analysis.num_threads is None
    assert analysis.dynamic_threads is True
    assert analysis.use_processes is use_processes
    assert analysis.results == []
    assert analysis.failed_files == []


def test_constructor_preserves_explicit_count(analysis_class):
    """Hand trace: caller's synthetic count 3 is retained; 3 is not None."""
    requested_count = 3  # Synthetic input, not a resource recommendation.
    analysis = analysis_class(num_threads=requested_count)
    assert analysis.num_threads == requested_count
    assert analysis.dynamic_threads is False


def test_empty_batch_returns_before_resource_probes(analysis_class):
    """Hand trace: the first branch returns no_files and an empty result list."""
    assert analysis_class().process_files_parallel([]) == {
        "status": "no_files",
        "results": [],
    }


@pytest.mark.parametrize("exists", [True, False])
def test_directory_early_returns(analysis_class, tmp_path, exists):
    """Hand trace: missing directory and unmatched glob have distinct statuses."""
    directory = tmp_path if exists else tmp_path / "absent"
    expected = "no_matching_files" if exists else "directory_not_found"
    assert analysis_class().process_directory(str(directory)) == {
        "status": expected,
        "results": [],
    }


@pytest.mark.parametrize("pattern", [None, "**/*.dat"])
def test_directory_delegates_matching_paths_and_config(analysis_class, tmp_path, pattern):
    """Default glob stays local; a recursive pattern includes a nested file."""
    included = tmp_path / "included.dat"
    included.write_text("synthetic input; not a solver model", encoding="utf-8")
    (tmp_path / "excluded.txt").write_text("excluded", encoding="utf-8")
    nested = tmp_path / "nested" / "included.dat"
    nested.parent.mkdir()
    nested.write_text("synthetic nested input", encoding="utf-8")
    analysis = analysis_class()
    expected = object()
    analysis.process_files_parallel = Mock(return_value=expected)
    config = {"static": False}
    kwargs = {} if pattern is None else {"pattern": pattern}
    assert analysis.process_directory(str(tmp_path), config=config, **kwargs) is expected
    analysis.process_files_parallel.assert_called_once()
    paths, received_config = analysis.process_files_parallel.call_args.args
    expected_paths = [included] if pattern is None else [included, nested]
    assert sorted(paths) == sorted(str(path) for path in expected_paths)
    assert received_config is config
    guide = (MODULE / "THREAD_OPTIMIZATION_GUIDELINES.md").read_text(encoding="utf-8")
    assert "without recursion" not in guide
    assert "a caller pattern containing `**` recurses" in guide


def test_readme_maturity_matches_parent_registry():
    registry = (ROOT / "docs/registry/module-routing.yaml").read_text(encoding="utf-8")
    group = re.search(r"(?ms)^  - module: solvers\n.*?(?=^  - module:|\Z)", registry)[0]
    maturity = re.search(r"^    maturity: (\w+)$", group, re.MULTILINE)[1]
    readme = (MODULE / "MODULE_README.md").read_text(encoding="utf-8")
    assert f"**Status**: {maturity}" in readme
    assert re.search(r"(?i)production[- ]ready", readme) is None


def test_guideline_examples_match_constructor_and_methods(analysis_class):
    text = (MODULE / "THREAD_OPTIMIZATION_GUIDELINES.md").read_text(encoding="utf-8")
    calls = []
    for block in re.findall(r"```python\n(.*?)```", text, re.DOTALL):
        calls.extend(
            node for node in ast.walk(ast.parse(block)) if isinstance(node, ast.Call)
        )
    constructors = [
        node
        for node in calls
        if isinstance(node.func, ast.Name) and node.func.id == CLASS_NAME
    ]
    assert constructors, "The guide needs a source-checked constructor example"
    for call in constructors:
        inspect.signature(analysis_class).bind(
            *[object() for _ in call.args],
            **{keyword.arg: object() for keyword in call.keywords},
        )
    for call in calls:
        if isinstance(call.func, ast.Attribute) and isinstance(
            call.func.value, ast.Name
        ):
            if call.func.value.id == "analysis":
                method = getattr(analysis_class, call.func.attr)
                inspect.signature(method).bind(
                    object(),
                    *[object() for _ in call.args],
                    **{keyword.arg: object() for keyword in call.keywords},
                )


def test_performance_guidance_exposes_evidence_boundary():
    text = (MODULE / "THREAD_OPTIMIZATION_GUIDELINES.md").read_text(encoding="utf-8")
    assert "UNVERIFIED" in text
    assert "NOT_EVALUATED" in text
    assert "num_threads=None" in text
    assert "independent benchmark" in text
    assert "helper-evidence.html" in text


def test_readme_parallel_claims_expose_evidence_boundary():
    readme = (MODULE / "MODULE_README.md").read_text(encoding="utf-8")
    performance = readme.split("## Performance\n", 1)[1].split("\n---", 1)[0]
    recommendations, benchmarks = performance.split("### Benchmarks\n", 1)
    assert "UNVERIFIED" in recommendations
    assert "UNVERIFIED" in benchmarks
    assert "helper-evidence.html" in performance
    parallel = re.search(r"^\- \*\*Parallel Processing\*\*: (.+)$", readme, re.M)[1]
    assert "NOT_EVALUATED" in parallel
    assert "Thread-safe" not in parallel


def test_evidence_page_classifies_every_new_test():
    """Every test entry in this focused slice must carry a dependency status."""
    page = (MODULE / "docs/helper-evidence.html").read_text(encoding="utf-8")
    entries = re.findall(r'data-test="([^"]+)" data-status="([^"]+)"', page)
    source = ast.parse(Path(__file__).read_text(encoding="utf-8"))
    expected = {
        node.name
        for node in source.body
        if isinstance(node, ast.FunctionDef) and node.name.startswith("test_")
    }
    assert len(entries) == len(expected)
    assert {name for name, _ in entries} == expected
    assert all(
        status in {"offline", "interface-dependent", "external-service-dependent"}
        for _, status in entries
    )
    assert "Conditional skips: none" in page


def test_evidence_page_links_resolve():
    """Evidence locators are repository files, not unresolved documentation links."""
    page = MODULE / "docs/helper-evidence.html"
    for target in re.findall(r'href="([^"]+)"', page.read_text(encoding="utf-8")):
        assert (page.parent / target).is_file(), target


def test_evidence_table_captions_are_first_children():
    """Captions precede row groups in markup; CSS controls visual placement."""

    class TableParser(HTMLParser):
        def __init__(self):
            super().__init__()
            self.tables = []
            self.stack = []

        def handle_starttag(self, tag, attrs):
            if tag == "table":
                self.tables.append([])
            elif self.stack and self.stack[-1] == "table":
                self.tables[-1].append(tag)
            if tag == "table" or self.stack:
                self.stack.append(tag)

        def handle_endtag(self, tag):
            if self.stack:
                assert self.stack.pop() == tag

    parser = TableParser()
    parser.feed((MODULE / "docs/helper-evidence.html").read_text(encoding="utf-8"))
    parser.close()
    assert parser.tables
    for children in parser.tables:
        assert children[0] == "caption"
        assert children.count("caption") == 1
