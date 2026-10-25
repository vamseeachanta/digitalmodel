# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic report calculations, traceability and reproducible issued files."""

from html.parser import HTMLParser
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
from xml.etree import ElementTree

import pytest

from digitalmodel.lifting_lug.padeye_report import (
    REVIEW_SCRIPT,
    SCHEMATIC,
    SECTIONS,
    render_report,
    sample_case,
    write_report,
)


class ReportElements(HTMLParser):
    def __init__(self):
        super().__init__()
        self.sections = []
        self.external_assets = []
        self.svg_titles = 0

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "section":
            self.sections.append(attrs.get("id"))
        if tag in {"script", "img", "link", "iframe"} and (
            attrs.get("src") or attrs.get("href")
        ):
            self.external_assets.append(attrs)
        if tag == "title":
            self.svg_titles += 1


def test_synthetic_sample_has_independent_results():
    """P=100*1.2*1.5=180 kN; F/A (OpenStax, 2016, 12.3) gives
    180, 90, 450/7 and 75 MPa, with throat=180000/(300*60)=10 mm.
    The comparison limits are synthetic inputs, not class-rule values.
    """
    result = sample_case()
    assert [c.demand_mpa for c in result.checks] == pytest.approx(
        [180, 90, 450 / 7, 75]
    )
    assert result.checks[-1].required_throat_mm == pytest.approx(10)
    assert result.screening_status.value == "FAIL"
    assert result.abs_acceptance.value == "NOT_EVALUATED"


def test_single_file_report_has_reviewable_basis_and_ten_sections():
    page = render_report()
    parsed = ReportElements()
    parsed.feed(page)
    assert len(parsed.sections) == 10
    assert len(set(parsed.sections)) == 10
    assert not parsed.external_assets
    assert parsed.svg_titles >= 6
    for phrase in (
        "Synthetic example - not for issue",
        "Revision history",
        "ASSUMED - to be confirmed",
        "NOT_EVALUATED",
        "Synthetic comparison",
        "9.2 Source locators",
        "60.0 MPa",
        "10.000 mm",
        "Pin bending",
        "Save comments beside report",
        "showSaveFilePicker",
        "Load comments",
        "Change panel",
        "Pre-flight evidence",
    ):
        assert phrase in page
    assert page.index("Revision history") < page.index('id="basis"')
    assert "<b>" not in page and "<strong>" not in page


def test_report_identifies_net_ligament_convention_and_source():
    page = render_report()
    assert "https://mechanicalc.com/reference/lug-analysis#simplified-shear-tear-out" in page
    assert "Lsp = e − h/2" in page
    assert "effective-area markers, not physical tangent fracture paths" in page
    assert "sections 9.3.1 and 9.3.2, equations 9-3a and 9-6a" in page
    assert "Net ligament: 70.0 mm" in page
    assert "Draft for client review" not in page


def test_effective_area_markers_match_the_axial_clear_ligament():
    """Two 70 mm markers share the scale of the 100 mm centre-to-edge distance."""
    diagram = ElementTree.fromstring(SCHEMATIC)
    hole = diagram.find("circle")
    centre_x, centre_y, radius = (float(hole.attrib[k]) for k in ("cx", "cy", "r"))
    markers = next(
        p for p in diagram.findall("path") if p.get("stroke-dasharray") == "6 4"
    )
    segments = re.findall(r"M([\d.]+) ([\d.]+)V([\d.]+)", markers.attrib["d"])
    assert len(segments) == 2
    for x, start_y, edge_y in segments:
        x, start_y, edge_y = float(x), float(start_y), float(edge_y)
        assert start_y == pytest.approx(centre_y - radius)
        assert abs(x - centre_x) == pytest.approx(radius)
        assert (start_y - edge_y) / (centre_y - edge_y) == pytest.approx(70 / 100)


@pytest.mark.parametrize("platform_newline", ["\n", "\r\n"])
def test_report_files_and_digest_are_independent_of_platform_newline(
    tmp_path, monkeypatch, platform_newline
):
    """Exercise the writer with both native text-file newline translations."""
    native_open = Path.open

    def platform_open(
        path, mode="r", buffering=-1, encoding=None, errors=None, newline=None
    ):
        if "b" not in mode and newline is None:
            newline = platform_newline
        return native_open(path, mode, buffering, encoding, errors, newline)

    monkeypatch.setattr(Path, "open", platform_open)
    write_report(tmp_path)
    page = (tmp_path / "report.html").read_bytes()
    panel = (tmp_path / "changes.json").read_bytes()
    assert b"\r" not in page
    assert b"\r" not in panel
    assert page == render_report().encode("utf-8")
    assert json.loads(panel)["report_digest"] == hashlib.sha256(page).hexdigest()


def test_committed_report_and_change_panel_are_reproducible(tmp_path):
    """A freshly rendered report must exactly match the issued synthetic example."""
    write_report(tmp_path)
    root = Path(__file__).resolve().parents[2] / "docs/reports/padeye-lift-point"
    for name in ("report.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (root / name).read_bytes()
    changes = json.loads((tmp_path / "changes.json").read_text())
    assert (
        changes["report_digest"]
        == hashlib.sha256((tmp_path / "report.html").read_bytes()).hexdigest()
    )
    assert changes["revision"] == 1
    assert changes["changes"]


def test_comment_controls_save_reload_and_reject_other_reports():
    """Exercise actual embedded controls with a DOM/file-picker substitute.

    This covers saved content, read-back, mismatch rejection, duplicate merge,
    text-only display and the download fallback. It establishes control logic,
    not browser rendering or browser file-system permission behaviour.
    """
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the comment-control check")
    harness = r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
class Element {
  constructor() { this.value = ''; this.textContent = ''; this.files = []; this.children = []; }
  appendChild(child) { this.children.push(child); if (!this.value) this.value = child.value; }
  click() { this.clicked = true; }
}
const elements = new Map();
const el = id => { if (!elements.has(id)) elements.set(id, new Element()); return elements.get(id); };
el('review-config').textContent = JSON.stringify(CONFIG);
let saved = '', filename = '', download = null;
const window = {showSaveFilePicker: async options => {
  filename = options.suggestedName;
  return {createWritable: async () => ({write: async data => {saved = data;}, close: async () => {}}),
          getFile: async () => ({text: async () => saved})};
}};
const document = {getElementById: el, createElement: tag => {
  const element = new Element(); if (tag === 'a') download = element; return element;
}};
vm.runInNewContext(SCRIPT, {document, window, Date, Set, JSON, Blob,
  URL: {createObjectURL: () => 'blob:example', revokeObjectURL: () => {}},
  setTimeout: fn => fn()});
(async () => {
  assert.equal(el('comment-section').children.length, 10);
  el('comment-text').value = '<img src=x onerror=example()> treated as text';
  el('add-comment').onclick();
  assert.match(el('comment-list').children[0].textContent, /<img/);
  await el('save-comments').onclick();
  assert.equal(filename, 'report.comments.json');
  assert.match(el('comment-status').textContent, /saved and read back/);
  const first = JSON.parse(saved);
  assert.equal(first.comments.length, 1);
  assert.equal(first.revision, CONFIG.revision);
  assert.equal(first.content_digest, CONFIG.content_digest);
  const incoming = JSON.parse(saved);
  incoming.comments.push({section: 'basis', text: 'Review synthetic targets', recorded_at: '2026-10-19'});
  el('load-comments').files = [{text: async () => JSON.stringify(incoming)}];
  await el('load-comments').onchange();
  await el('save-comments').onclick();
  assert.equal(JSON.parse(saved).comments.length, 2);
  incoming.content_digest = 'another report';
  el('load-comments').files = [{text: async () => JSON.stringify(incoming)}];
  await el('load-comments').onchange();
  assert.match(el('comment-status').textContent, /do not match/);
  await el('save-comments').onclick();
  assert.equal(JSON.parse(saved).comments.length, 2);
  window.showSaveFilePicker = async () => { throw new Error('Save cancelled'); };
  await el('save-comments').onclick();
  assert.equal(el('comment-status').textContent, 'Save cancelled');
  window.showSaveFilePicker = undefined;
  await el('save-comments').onclick();
  assert.equal(download.download, 'report.comments.json');
  assert.equal(download.clicked, true);
  assert.match(el('comment-status').textContent, /Download requested/);
})().catch(error => { console.error(error.message); process.exitCode = 1; });
"""
    setup = (
        "const CONFIG = "
        + json.dumps(
            {"content_digest": "synthetic-content", "revision": 1, "sections": SECTIONS}
        )
        + ";\nconst SCRIPT = "
        + json.dumps(REVIEW_SCRIPT)
        + ";\n"
    )
    result = subprocess.run(
        [node, "-"],
        input=setup + harness,
        text=True,
        capture_output=True,
        timeout=15,
    )
    assert result.returncode == 0, result.stderr
