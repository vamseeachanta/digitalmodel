# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Report data, analytical anchors and portable rebuild contracts."""

import base64
import importlib.util
import json
import math
import re
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
FOLDER = ROOT / "docs/reports/modules/riserconfigurationscreen"
REBUILD = "python scripts/reporting/build_riser_configuration_report.py"


def builder():
    spec = importlib.util.spec_from_file_location(
        "riser_report", ROOT / "scripts/reporting/build_riser_configuration_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_report_rebuild():
    rebuilt = builder().render_report()
    assert rebuilt == (FOLDER / "report.html").read_text(
        encoding="utf-8"), f"Report is stale; rebuild with: {REBUILD}"
    assert b"\r\n" not in rebuilt.encode("utf-8")


def test_all_example_numbers_match_module():
    module = builder()
    data = module.calculate()
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    embedded = json.loads(re.search(
        r'<script id="report-data" type="application/json">(.*?)</script>',
        text, re.S).group(1))
    assert embedded == module.display_data(data), f"Rebuild with: {REBUILD}"
    for path, digits, shown in re.findall(
            r'data-number="([^"]+)" data-digits="(\d+)">([^<]+)', text):
        value = data
        for key in path.split("."):
            value = value[int(key)] if isinstance(value, list) else value[key]
        assert shown == module.number(value, int(digits)), path


def test_hand_calculation_anchors():
    row = builder().calculate()["anchor"]
    # Assumed depth 100 m and angle 30 degrees: sin(angle)=1/2;
    # a = depth*sin(angle)/(1-sin(angle)) = 100 m.
    assert row["touchdown_curvature_per_m"] == pytest.approx(0.01, rel=1e-7)
    # Weight 10 N/m gives H=w*a = 1000 N.
    assert row["horizontal_tension_n"] == pytest.approx(1000.0, rel=1e-7)
    # T_top = w*(a+depth) = 2000 N, from vertical equilibrium.
    assert row["top_tension_n"] == pytest.approx(2000.0, rel=1e-7)


def test_structure_and_public_wording():
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    assert 'class="hs-doc"' in text
    assert len(re.findall(r'<h2 id="s\d+">', text)) == 10
    assert 'id="comments"' in text and "showSaveFilePicker" in text
    assert "Draft for maintainer review" in text
    assert "ASSUMED - to be confirmed" in text
    assert "<script src=" not in text
    assert all(href.startswith("data:text/css;base64,") for href in re.findall(
        r'<link rel="stylesheet" href="([^"]+)"', text))
    visible = re.sub(r"<(script|style)\b.*?</\1>", "", text, flags=re.S)
    visible = re.sub(r"<[^>]+>", " ", visible)
    assert not re.search(r"\b(owner|client|lane|seat|session|could|should|would)\b", visible, re.I)
    assert "<strong" not in text and "<b>" not in text


def test_style_and_changes_are_embedded():
    text = (FOLDER / "report.html").read_text(encoding="utf-8")
    changes = json.loads((FOLDER / "changes.json").read_text(encoding="utf-8"))
    assert changes["changes"][0]["description"] in text
    encoded = re.search(r'href="data:text/css;base64,([^"]+)"', text)
    assert encoded is not None
    assert base64.b64decode(encoded[1]) == (ROOT / "docs/api/_assets/brand.css").read_bytes()
    assert "Limit" in text and "<title>" in text


def test_report_generator_is_registered():
    from scripts import check_generated_html as checker

    entries = [entry for entry in checker.GENERATORS
               if entry.script.endswith("build_riser_configuration_report.py")]
    assert len(entries) == 1
    assert entries[0].script == "scripts/reporting/build_riser_configuration_report.py"
    assert entries[0].outputs == (
        "docs/reports/modules/riserconfigurationscreen/report.html",)
    assert set(entries[0].inputs) >= {
        "examples/workflows/riser-configuration-screen/scr.yml",
        "docs/reports/modules/riserconfigurationscreen/report.template.html",
        "docs/reports/modules/riserconfigurationscreen/changes.json",
    }


@pytest.mark.parametrize("direction", [-math.inf, math.inf])
@pytest.mark.parametrize("source", ["anchor", "screen", "closure"])
def test_report_ignores_ulp_perturbations(monkeypatch, direction, source):
    module = builder()
    expected = module.render_report()
    original = module.scr_offsets if source == "anchor" else module.screen

    def perturbed(settings, *args):
        settings = dict(settings)
        if source != "closure":
            settings["water_depth_m"] = math.nextafter(
                settings["water_depth_m"], direction)
        result = original(settings, *args)
        if source == "closure":
            # Platform residuals of a few tens of picometres are not results.
            for index in (2, 6):
                result["ranked_candidates"][index]["constraints"]["shape_closure"]["margin"] += (
                    math.copysign(2e-11, direction))
        return result

    monkeypatch.setattr(module, "scr_offsets" if source == "anchor" else "screen", perturbed)
    assert module.render_report() == expected


def test_chart_payload_excludes_solver_diagnostics():
    module = builder()
    data = module.display_data(module.calculate())
    assert set(data) == {"settings", "result", "anchor", "reference"}
    assert set(data["result"]) == {"ranked_candidates"}
    for candidate in data["result"]["ranked_candidates"]:
        assert set(candidate) == {"configuration", "offset_results"}
        for row in candidate["offset_results"]:
            assert set(row) == {
                "offset_m", "top_tension_n", "grounded_length_m",
                "touchdown_curvature_per_m", "static_von_mises_utilisation",
                "horizontal_span_m", "shape",
            }
    assert "shape_closure" not in json.dumps(data)


def test_anchor_agreement_uses_explicit_tolerance(monkeypatch):
    module = builder()
    assert "agree within a relative tolerance of 1e-7" in module.render_report()
    original = module.scr_offsets

    def disagree(settings, offsets):
        rows = original(settings, offsets)
        rows[0]["top_tension_n"] *= 1.01
        return rows

    monkeypatch.setattr(module, "scr_offsets", disagree)
    assert "do not agree within a relative tolerance of 1e-7" in module.render_report()


def test_example_headline_values_have_independent_anchors():
    # Independently solved fixed-length cable equations at the three radial
    # separations; literal references deliberately do not call report helpers.
    candidates = builder().calculate()["result"]["ranked_candidates"]
    first = candidates[0]
    assert first["configuration"]["hangoff_angle_deg"] == 20.0
    assert first["curvature_range_per_m"] == pytest.approx(4.9461e-4, abs=5e-9)
    assert all(candidate["feasible"] for candidate in candidates)
    assert [c["configuration"]["hangoff_angle_deg"] for c in candidates] == [
        20.0, 18.0, 16.0, 14.0, 12.0, 10.0, 8.0]
    radial = [row for row in first["offset_results"]
              if isinstance(row["offset_m"], (int, float))]
    for row, offset, tension, grounded in zip(
            radial, [-50.0, 0.0, 50.0],
            [2143957.6, 2279705.0, 2444926.8], [597.26, 500.00, 387.28], strict=True):
        assert row["offset_m"] == offset
        assert row["top_tension_n"] == pytest.approx(tension, abs=0.05, rel=0)
        assert row["grounded_length_m"] == pytest.approx(grounded, abs=0.005, rel=0)
    # Far-excursion hangoff: T/A + E*(D/2)*kappa_top, divided by yield.
    assert radial[2]["static_von_mises_utilisation"] == pytest.approx(
        0.2625, abs=0.00005, rel=0)


@pytest.mark.parametrize("revision", ["B", "C"])
@pytest.mark.parametrize("save_method", ["picker", "download"])
def test_feedback_revision_round_trip(tmp_path, monkeypatch, revision, save_method):
    """Execute shipped handlers with a small DOM and both save transports."""
    module = builder()
    changes = json.loads((FOLDER / "changes.json").read_text(encoding="utf-8"))
    changes["revision"] = revision
    (tmp_path / "changes.json").write_text(json.dumps(changes), encoding="utf-8")
    (tmp_path / "report.template.html").write_text(
        (FOLDER / "report.template.html").read_text(encoding="utf-8"), encoding="utf-8")
    monkeypatch.setattr(module, "FOLDER", tmp_path)
    page = module.render_report()
    payloads = dict(re.findall(
        r'<script id="([^"]+)" type="application/json">(.*?)</script>', page, re.S))
    script = re.search(r"<script>(.*?)</script>", page, re.S)[1]
    setup = r"""
const assert = require('node:assert/strict');
class Element {
  constructor() { this.textContent = ''; this.value = ''; this.children = []; }
  append(...children) { this.children.push(...children); }
  replaceChildren(...children) { this.children = children; }
  setAttribute() {}
  addEventListener() {}
  get selectedOptions() { return this.children.slice(0, 1); }
  click() { downloaded = true; }
}
const nodes = new Map();
const getNode = id => {
  if (!nodes.has(id)) nodes.set(id, new Element());
  return nodes.get(id);
};
for (const [id, text] of Object.entries(PAYLOADS)) getNode(id).textContent = text;
getNode('quantity').value = 'top_tension_n';
const heading = new Element(); heading.id = 's1'; heading.textContent = '1. Executive summary';
const document = {
  getElementById: getNode, createElement: () => new Element(),
  createElementNS: () => new Element(), querySelectorAll: () => [heading]
};
let savedText, savedBlob, downloaded = false, closed = false;
const window = {};
if (SAVE_METHOD === 'picker') window.showSaveFilePicker = async () => ({
  createWritable: async () => ({write: async text => { savedText = text; },
    close: async () => { closed = true; }})
});
const URL = {createObjectURL: blob => { savedBlob = blob; return 'blob:comments'; },
  revokeObjectURL() {}};
const setTimeout = callback => callback();
"""
    checks = r"""
async function checkFeedback() {
  const original = [{section: heading.textContent, text: '<tag> & comment'}];
  getNode('comment-text').value = '  ' + original[0].text + '  ';
  getNode('add-comment').onclick();
  await getNode('save-comments').onclick();
  if (SAVE_METHOD === 'download') {
    assert.ok(downloaded); savedText = await savedBlob.text();
  } else assert.ok(closed);
  const saved = JSON.parse(savedText);
  assert.equal(saved.revision, REVISION);
  assert.deepEqual(saved.comments, original);
  const load = async value => getNode('load-comments').onchange({
    target: {files: [{text: async () => JSON.stringify(value)}]}
  });
  const rendered = () => getNode('comment-list').children.map(li => li.textContent);
  const expected = original.map(c => `${c.section}: ${c.text}`);
  // Reload after changing the list, so a no-op load cannot pass.
  getNode('comment-text').value = 'Unsaved extra'; getNode('add-comment').onclick();
  assert.equal(rendered().length, 2);
  await load(saved);
  assert.equal(getNode('comment-status').textContent, 'Saved comments loaded.');
  assert.deepEqual(rendered(), expected);
  // Old, missing and future revisions must not replace current comments.
  for (const revision of ['A', undefined, 'future']) {
    await load({revision, comments: [{section: 'Other', text: 'Wrong revision'}]});
    assert.match(getNode('comment-status').textContent, /not a compatible comments file/);
    assert.deepEqual(rendered(), expected);
  }
  for (const comments of [null, [{}], [null], [{section: 's1', text: 42}]]) {
    await load({revision: REVISION, comments});
    assert.match(getNode('comment-status').textContent, /not a compatible comments file/);
    assert.deepEqual(rendered(), expected);
  }
  await load({revision: REVISION, comments: []});
  assert.equal(getNode('comment-status').textContent, 'Saved comments loaded.');
  assert.deepEqual(rendered(), []);
}
checkFeedback().catch(error => { console.error(error); process.exitCode = 1; });
"""
    result = subprocess.run(
        ["node", "-"], input=(f"const PAYLOADS = {json.dumps(payloads)};\n"
                             f"const REVISION = {json.dumps(revision)};\n"
                             f"const SAVE_METHOD = {json.dumps(save_method)};\n"
                             + setup + script + checks),
        text=True, capture_output=True, timeout=30, check=False)
    assert result.returncode == 0, result.stderr
