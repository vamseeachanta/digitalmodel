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
import importlib.util
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REGISTER = ROOT / "docs/domains/risers/global-model-qualification.json"
REPORT = REGISTER.with_suffix(".html")
TESTS = ROOT / "tests/drilling_riser/global_model"


def load_script(relative, name):
    spec = importlib.util.spec_from_file_location(name, ROOT / relative)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def generator_module():
    return load_script("scripts/docs/update_riser_qualification.py", "riser_generator")


def test_generator_changes_select_contracts():
    detector = load_script("scripts/ci/detect_touched_domains.py", "riser_detector")
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    selected = detector.touched_domains(["scripts/docs/update_riser_qualification.py"], domains)
    assert "contracts" in {domain.name for domain in selected}


def test_snapshot_checks_do_not_read_live_sources(register, monkeypatch):
    def reject_live_read(*args, **kwargs):
        pytest.fail("Historical verification consulted working-tree bytes or membership")
    monkeypatch.setattr(Path, "read_bytes", reject_live_read)
    monkeypatch.setattr(Path, "glob", reject_live_read)
    test_every_source_is_a_readable_fingerprinted_repository_file(register)
    test_static_coverage_lists_every_file_without_collecting_it(register)
    test_collection_probe_flags_are_visible_without_executing_calls(register)


@pytest.mark.parametrize("relative", ["../outside.py", "tests/../../../outside.py"])
def test_generator_rejects_escaping_paths(relative):
    with pytest.raises(ValueError, match="repository"):
        generator_module().repository_file(ROOT, relative)


def test_generator_check_is_current_and_read_only(monkeypatch):
    generator = generator_module()
    def no_write(*args, **kwargs):
        pytest.fail("check mode wrote a file")
    monkeypatch.setattr(Path, "write_text", no_write)
    assert generator.reconcile(ROOT, write=False) == 0


def test_generator_detects_inventory_drift_and_updates_both_outputs(monkeypatch):
    generator = generator_module()
    original = generator.snapshot_files
    def added_file(root, revision):
        files = original(root, revision)
        files[generator.TESTS + "/test_added.py"] = b"def test_added():\n    pass\n"
        return files
    monkeypatch.setattr(generator, "snapshot_files", added_file)
    written = {}
    monkeypatch.setattr(Path, "write_text", lambda path, text, **kw: written.update({path: text}))
    assert generator.reconcile(ROOT, write=False) == 1
    assert not written
    assert generator.reconcile(ROOT, write=True) == 0
    updated = json.loads(written[ROOT / generator.REGISTER])
    original_register = json.loads(REGISTER.read_text(encoding="utf-8"))
    assert updated["requirements"] == original_register["requirements"]
    assert updated["sources"] == original_register["sources"]
    assert any(row["other_tests"] == ["test_added"] for row in updated["test_coverage"])
    assert "test_added.py" in written[ROOT / generator.REPORT]


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
    assert len(register["snapshot_revision"]) == 40
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
        assert path.resolve().is_relative_to(ROOT)
        assert hashlib.sha256(snapshot_blob(register, source["path"])).hexdigest() == source["sha256"]
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
        self.inventory = []
        self.in_inventory = False
        self.cells = None
        self.cell = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "table" and attrs.get("id") == "test-inventory":
            self.in_inventory = True
        if self.in_inventory and tag == "tr":
            self.cells = []
        if self.in_inventory and tag == "td":
            self.cell = []
        if "data-requirement" in attrs:
            self.current = attrs["data-requirement"]
            self.requirements.append(self.current)
            self.rows[self.current] = []
        if tag == "a":
            self.links.append(attrs["href"])

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)
        if self.current:
            self.rows[self.current].append(data)

    def handle_endtag(self, tag):
        if self.in_inventory and tag == "td":
            self.cells.append("".join(self.cell).strip())
            self.cell = None
        if self.in_inventory and tag == "tr" and self.cells:
            self.inventory.append(self.cells)
        if tag == "table":
            self.in_inventory = False
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
        target = (REPORT.parent / link.split("#")[0]).resolve()
        assert target.is_relative_to(ROOT)
        relative = target.relative_to(ROOT).as_posix()
        if relative.startswith(("src/", "tests/")):
            assert snapshot_blob(register, relative)
        else:
            assert target.is_file()
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
    """Membership and digests describe the retained revision, not later edits."""
    entries = register["test_coverage"]
    assert {row["file"] for row in entries} == {
        name for name in snapshot_names(register)
        if Path(name).name.startswith("test_") and name.endswith(".py")
    }
    assert len(entries) == len({row["file"] for row in entries})
    for row in entries:
        raw = snapshot_blob(register, row["file"])
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
        tree = ast.parse(snapshot_blob(register, row["file"]))
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


def snapshot_blob(register, relative):
    assert (ROOT / relative).resolve().is_relative_to(ROOT)
    return generator_module().git_read(
        ROOT, "cat-file", "blob", f"{register['snapshot_revision']}:{relative}"
    )


def snapshot_names(register):
    raw = generator_module().git_read(
        ROOT, "ls-tree", "--name-only", "-z", register["snapshot_revision"],
        "tests/drilling_riser/global_model/",
    )
    return [name.decode("utf-8") for name in raw.split(b"\0") if name]


def assert_inventory_table(text, register):
    parser = ReportParser()
    parser.feed(text)
    expected = {
        Path(row["file"]).name: [Path(row["file"]).name,
            str(len(row["solver_marked_tests"])), str(len(row["other_tests"])),
            row["classification"], row["execution_status"]]
        for row in register["test_coverage"]
    }
    assert len(parser.inventory) == len(expected)
    assert {row[0]: row for row in parser.inventory} == expected


def test_inventory_table_matches_register(register):
    assert_inventory_table(REPORT.read_text(encoding="utf-8"), register)


@pytest.mark.parametrize("old,new", [
    ("<td>8</td>", "<td>999</td>"),
    ("<td>0</td>", "<td>999</td>"),
    ("<td>NO_DIRECT_COLLECTION_PROBE_FOUND</td>", "<td>STALE</td>"),
])
def test_inventory_table_rejects_stale_cells(register, old, new):
    text = REPORT.read_text(encoding="utf-8").replace(old, new, 1)
    with pytest.raises(AssertionError):
        assert_inventory_table(text, register)
