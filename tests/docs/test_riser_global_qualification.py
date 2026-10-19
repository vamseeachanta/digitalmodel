# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline documentation checks; no model, binding or solver is imported.

Expected requirements come from issue 2530, Detection and Acceptance, and the
global-model knowledge pack, sections 5 and 8 (revision 2, 2026). These tests
check evidence completeness statements, not engineering acceptance values.
Test coverage is read as syntax rather than collected: a solver marker cannot
prevent a module-level availability probe during collection.
"""

import ast
import hashlib
import json
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REGISTER = ROOT / "docs/domains/risers/global-model-qualification.json"
REPORT = REGISTER.with_suffix(".html")
TESTS = ROOT / "tests/drilling_riser/global_model"

# Source: knowledge pack revision 2, section 5, AQG applicability paragraph.
GATES = {"G0", "G1", "G2", "G3", "G4", "G5"}
# Source: issue 2530, Detection and Acceptance; pack sections 5 and 8.
REQUIREMENTS = {
    "appointments", "target-version", "gate-reconciliation", "input-fingerprint",
    "native-load", "independent-hand-comparison", "offset-sweep",
    "component-inventory", "short-dynamics", "diffraction-provenance",
    "fingerprint-appendix", "directional-stroke", "joint-angle", "clearance",
}


@pytest.fixture
def register():
    return json.loads(REGISTER.read_text(encoding="utf-8"))


def test_all_required_evidence_stays_unqualified(register):
    """Missing evidence has no native record, appointment or qualification PASS."""
    assert register["qualification_status"] == "NOT_EVALUATED"
    assert register["snapshot_revision"] == "6476218e8f5af3fa7d16c3dafc60b6a9084c6e6c"
    rows = register["requirements"]
    assert {row["id"] for row in rows} == REQUIREMENTS
    assert len(rows) == len(REQUIREMENTS)
    assert {gate for row in rows for gate in row["gates"]} == GATES
    for row in rows:
        assert row["status"] == "NOT_EVALUATED"
        assert row["evidence_records"] == []
        assert row["missing_evidence"].strip()
        assert row["required_record"].strip()
        assert row["basis"] and row["responsible_role"].strip()
        assert set(row["gates"]) <= GATES
    assert register["appointments"] == {
        "independent_quality_manager": "UNCONFIRMED",
        "deputy": "UNCONFIRMED",
    }
    assert register["aqg_source_status"] == "REFERENCED_ONLY"
    assert register["numeric_acceptance_limits"] == {}


def test_every_source_is_a_readable_fingerprinted_repository_file(register):
    """A fingerprint ties this gap snapshot to source bytes, not solver results."""
    sources = register["sources"]
    assert sources
    for source in sources.values():
        path = ROOT / source["path"]
        assert path.is_relative_to(ROOT)
        assert path.is_file()
        assert hashlib.sha256(path.read_bytes()).hexdigest() == source["sha256"]
    for row in register["requirements"]:
        assert all(basis["source"] in sources for basis in row["basis"])
        assert all(basis["locator"].strip() for basis in row["basis"])


class ReportParser(HTMLParser):
    """Read requirement identifiers and repository links without a browser."""

    def __init__(self):
        super().__init__()
        self.requirements = []
        self.links = []
        self.rows = {}
        self.current = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "data-requirement" in attrs:
            self.current = attrs["data-requirement"]
            self.requirements.append(self.current)
            self.rows[self.current] = []
        if tag == "a":
            self.links.append(attrs["href"])

    def handle_data(self, data):
        if self.current:
            self.rows[self.current].append(data)

    def handle_endtag(self, tag):
        if tag == "tr":
            self.current = None


def test_report_exposes_every_gap_and_resolves_repository_links(register):
    parser = ReportParser()
    text = REPORT.read_text(encoding="utf-8")
    parser.feed(text)
    assert set(parser.requirements) == REQUIREMENTS
    assert len(parser.requirements) == len(REQUIREMENTS)
    assert "Revision history" in text
    assert text.index("Summary") < text.index("Revision history") < text.index("Evidence gaps")
    assert "NOT_EVALUATED" in text and "REFERENCED_ONLY" in text
    for row in register["requirements"]:
        rendered = " ".join(parser.rows[row["id"]])
        assert row["status"] in rendered
        assert row["missing_evidence"] in rendered
        assert row["required_record"] in rendered
    for link in parser.links:
        assert not link.startswith(("http:", "https:"))
        assert (REPORT.parent / link.split("#")[0]).resolve().is_file()
    index = (REPORT.parent / "risers.md").read_text(encoding="utf-8")
    assert f"]({REPORT.name})" in index


def _solver_marked(node):
    return any(
        isinstance(part, ast.Attribute) and part.attr == "solver"
        for decorator in node.decorator_list for part in ast.walk(decorator)
    )


def _test_functions(tree):
    return [node for node in ast.walk(tree)
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
            and node.name.startswith("test_")]


def test_static_coverage_lists_every_file_without_collecting_it(register):
    """Expected membership is the live file set, not a hard-coded test count."""
    entries = register["test_coverage"]
    assert {row["file"] for row in entries} == {
        path.relative_to(ROOT).as_posix() for path in TESTS.glob("test_*.py")
    }
    assert len(entries) == len({row["file"] for row in entries})
    for row in entries:
        path = ROOT / row["file"]
        raw = path.read_bytes()
        assert hashlib.sha256(raw).hexdigest() == row["sha256"]
        tree = ast.parse(raw)
        functions = _test_functions(tree)
        # A module-level pytestmark.solver applies to every test in that file.
        module_solver = any(
            isinstance(node, ast.Assign)
            and any(isinstance(t, ast.Name) and t.id == "pytestmark" for t in node.targets)
            and any(isinstance(p, ast.Attribute) and p.attr == "solver"
                    for p in ast.walk(node.value))
            for node in tree.body
        )
        native = sorted(n.name for n in functions if module_solver or _solver_marked(n))
        offline = sorted(n.name for n in functions if n.name not in native)
        assert row["solver_marked_tests"] == native
        assert row["other_tests"] == offline
        assert row["execution_status"] == "NOT_RUN"


def test_collection_probe_flags_are_visible_without_executing_calls(register):
    """Availability expressions outside test bodies are collection-time hazards."""
    for row in register["test_coverage"]:
        tree = ast.parse((ROOT / row["file"]).read_bytes())
        outside_bodies = []
        for node in tree.body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                outside_bodies.extend(node.decorator_list)
            else:
                outside_bodies.append(node)
        probes = any(
            isinstance(part, ast.Call) and isinstance(part.func, ast.Attribute)
            and part.func.attr == "available"
            for node in outside_bodies for part in ast.walk(node)
        )
        assert row["collection_availability_probe"] is probes
        if probes and row["other_tests"]:
            assert row["classification"] == "MIXED_WITH_COLLECTION_PROBE"
    assert any(row["classification"] == "MIXED_WITH_COLLECTION_PROBE"
               for row in register["test_coverage"])
