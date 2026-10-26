"""Check report consistency and an independent grounded-catenary reference."""

import copy
import csv
import importlib.util
import json
import math
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urlsplit

import pytest

from digitalmodel.dynmoor_mooring.workflow import router
from digitalmodel.subsea.mooring_analysis import dynmoor

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/dynmoormooring"


def load_builder():
    spec = importlib.util.spec_from_file_location("dynmoor_report", REPORT / "generate.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_report_rebuild_and_all_values(tmp_path):
    builder = load_builder()
    document, changes = builder.build(tmp_path / "build")
    assert document == (REPORT / "report.html").read_text(encoding="utf-8")
    assert changes == json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    payload = json.loads(re.search(
        r'<script id="results" type="application/json">(.*?)</script>',
        document, re.S,
    ).group(1))
    cfg = json.loads((REPORT / "example.json").read_text(encoding="utf-8"))
    cfg["dynmoor_mooring"]["output_dir"] = str(tmp_path / "independent")
    expected = router(copy.deepcopy(cfg))["dynmoor_mooring"]
    assert payload["lines"] == expected["lines"]
    assert payload["offsets"] == expected["offsets_evaluated"]
    with Path(expected["line_tables_csv"]).open(encoding="utf-8") as stream:
        table = [{key: float(value) if key != "model" else value
                  for key, value in row.items()} for row in csv.DictReader(stream)]
    assert payload["table"] == table
    assert payload["inputs"] == json.loads((REPORT / "example.json").read_text(encoding="utf-8"))["dynmoor_mooring"]
    assert payload["table_rows"] == dynmoor.N_TABLE
    assert payload["water_weight_factor"] == dynmoor.WATER_WEIGHT_FACTORS["chain"]
    for row in payload["benchmark"]:
        assert row["calculated_tension_kips"] == table[row["row"] - 1]["total_tension_kips"]
        assert row["difference_percent"] == 100 * (
            row["calculated_tension_kips"] / row["reference_tension_kips"] - 1)
    for key, value in builder.numeric_values(payload).items():
        assert f'data-value="{key}">{builder.number(value)}</span>' in document


def test_structure_and_offline_controls(tmp_path):
    builder = load_builder()
    document, _ = builder.build(tmp_path)
    assert '<body class="hs-doc">' in document
    assert len(re.findall(r'<h2>\d+\.', document)) == 10
    assert 'src="http' not in document
    assert "showSaveFilePicker" in document
    assert "comments.json" in document
    assert 'id="change-panel"' in document
    head = document.split("</head>", 1)[0]
    assert "<title>Static spread-mooring module assessment</title>" in head
    assert "Breaking strength reference" in document
    assert '<strong' not in document and '<b>' not in document


def test_public_example_matches_existing_fixture(tmp_path):
    spec = importlib.util.spec_from_file_location(
        "existing_workflow_tests", Path(__file__).with_name("test_dynmoor_workflow.py")
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    expected = module._base_config(tmp_path)
    expected.pop("_config_dir_path")
    assert json.loads((REPORT / "example.json").read_text(encoding="utf-8")) == expected


def test_saved_curve_against_public_static_function():
    document = (REPORT / "report.html").read_text(encoding="utf-8")
    data = json.loads(re.search(
        r'<script id="results" type="application/json">(.*?)</script>',
        document, re.S,
    ).group(1))
    cfg = data["inputs"]["line_types"][0]
    line_type = dynmoor.DynmoorLineType(
        segment1=dynmoor.DynmoorSegment(**cfg["segments"][0]),
        water_depth_ft=cfg["vertical_distance_anchor_to_fairlead_ft"],
        bottom_friction_percent=cfg["bottom_friction_percent"],
    )
    native = dynmoor.build_line_table(line_type)
    for index, row in enumerate(data["table"]):
        assert row["distance_to_anchor_ft"] == native.u[index]
        assert row["total_tension_kips"] == native.t[index] / 1000
        assert row["horizontal_tension_kips"] == native.h[index] / 1000
        assert row["suspended_length_ft"] == native.total_length_ft - native.z[index]
        assert row["vertical_anchor_pull_kips"] == native.p[index] / 1000


def test_saved_html_has_no_unresolved_or_external_resources():
    document = (REPORT / "report.html").read_text(encoding="utf-8")
    assert not re.search(r"\{\{[A-Z_]+\}\}", document)
    assert 'src="http' not in document
    assert '<body class="hs-doc">' in document
    assert len(re.findall(r'<h2>\d+\.', document)) == 10
    class Links(HTMLParser):
        def __init__(self):
            super().__init__()
            self.links = []

        def handle_starttag(self, tag, attrs):
            self.links.extend((tag, value) for key, value in attrs if key == "href")

    parser = Links()
    parser.feed(document)
    for tag, href in parser.links:
        link = urlsplit(href)
        if link.scheme:
            assert tag == "a", "External citations must not become runtime resources"
            assert link.scheme == "https"
            assert link.netloc in {"cmoe-tudelft.github.io", "openstax.org", "www.orcina.com"}
        elif href.startswith("#"):
            assert f'id="{href[1:]}"' in document
        else:
            assert (REPORT / href).resolve().is_file()


def test_saved_offsets_against_public_statics():
    document = (REPORT / "report.html").read_text(encoding="utf-8")
    data = json.loads(re.search(
        r'<script id="results" type="application/json">(.*?)</script>', document, re.S
    ).group(1))
    cfg = data["inputs"]["line_types"][0]
    line_type = dynmoor.DynmoorLineType(
        segment1=dynmoor.DynmoorSegment(**cfg["segments"][0]),
        water_depth_ft=cfg["vertical_distance_anchor_to_fairlead_ft"],
        bottom_friction_percent=cfg["bottom_friction_percent"],
    )
    table = dynmoor.build_line_table(line_type)
    lines = [dynmoor.DynmoorLine(
        item["type"], item["fairlead_x_ft"], item["fairlead_y_ft"],
        item["angle_deg"], item["pretension_percent"]
    ) for item in data["inputs"]["lines"]]
    solutions = [dynmoor.solve_pretension(line, line_type, table) for line in lines]
    anchors = [(item.anchor_x_ft, item.anchor_y_ft) for item in solutions]
    for actual, expected in zip(data["lines"], solutions):
        assert actual["pretension_kips"] == expected.pretension_lb / 1000
        assert actual["horizontal_distance_to_anchor_ft"] == expected.horizontal_distance_ft
        assert actual["anchor_x_ft"] == expected.anchor_x_ft
        assert actual["anchor_y_ft"] == expected.anchor_y_ft
        assert actual["fairlead_angle_deg"] == expected.fairlead_angle_deg
    for row in data["offsets"]:
        native = dynmoor.restoring_force(
            lines, [table], anchors, row["surge_ft"], row["sway_ft"],
            row["yaw_deg"] * math.pi / 180,
        )
        assert row["restoring_surge_kips"] == native.surge_lb / 1000
        assert row["restoring_sway_kips"] == native.sway_lb / 1000
        assert row["restoring_yaw_kip_ft"] == native.yaw_lb_ft / 1000
        assert row["max_tension_line"] == native.max_tension_line
        for index, tension in enumerate(native.total_tensions_lb, start=1):
            assert row[f"line_{index}_tension_kips"] == tension / 1000
            assert row[f"line_{index}_horizontal_kips"] == native.horizontal_tensions_lb[index - 1] / 1000


def reference_span(horizontal_lb):
    """Integrate a grounded catenary and horizontal strain, without lookup tables.

    The fixture uses w = 0.87 * 60 lb/ft, d = 100 ft, L = 1000 ft,
    EA = 30000000 lb and friction coefficient 0.02. Vertical elastic
    displacement is omitted to match the stated legacy approximation.
    """
    weight, depth, length, stiffness, friction = 0.87 * 60, 100, 1000, 30000000, 0.02
    suspended = math.sqrt(depth**2 + 2 * depth * horizontal_lb / weight)
    assert suspended < length, "Reference only covers grounded chain"
    grounded = length - suspended
    loaded = min(grounded, horizontal_lb / (friction * weight))
    extension = (horizontal_lb * suspended + horizontal_lb * loaded
                 - friction * weight * loaded**2 / 2) / stiffness
    return horizontal_lb / weight * math.asinh(weight * suspended / horizontal_lb) + grounded + extension


def reference_horizontal(span_ft):
    """Bisect force balance in the grounded branch, independently of the port."""
    lower, upper = 1.0, 200000.0
    assert reference_span(lower) < span_ft < reference_span(upper)
    for _ in range(80):
        middle = (lower + upper) / 2
        if reference_span(middle) < span_ft:
            lower = middle
        else:
            upper = middle
    return (lower + upper) / 2


def test_saved_headlines_against_first_principles():
    """Allow 0.002 ft, 0.05 kip and 1 kip-ft for table interpolation."""
    document = (REPORT / "report.html").read_text(encoding="utf-8")
    data = json.loads(re.search(
        r'<script id="results" type="application/json">(.*?)</script>', document, re.S
    ).group(1))
    span = reference_span(75000 - 0.87 * 60 * 100)
    assert span == pytest.approx(989.571, abs=0.001, rel=0)
    for line in data["lines"]:
        assert line["horizontal_distance_to_anchor_ft"] == pytest.approx(span, abs=0.002, rel=0)
    yaw = math.radians(1)
    cosine, sine = math.cos(yaw), math.sin(yaw)
    surge = sway = moment = 0.0
    geometry = [(200, -50, 300), (200, 50, 60), (-200, 50, 120), (-200, -50, 240)]
    offset = data["offsets"][1]
    assert (offset["surge_ft"], offset["sway_ft"], offset["yaw_deg"]) == (10, 5, 1)
    for index, (x, y, angle) in enumerate(geometry, start=1):
        arm_x, arm_y = x * cosine - y * sine, x * sine + y * cosine
        dx = x + span * math.cos(math.radians(angle)) - (10 + arm_x)
        dy = y + span * math.sin(math.radians(angle)) - (5 + arm_y)
        distance = math.hypot(dx, dy)
        horizontal = reference_horizontal(distance)
        fx, fy = horizontal * dx / distance, horizontal * dy / distance
        surge += fx * cosine + fy * sine
        sway += -fx * sine + fy * cosine
        moment += arm_x * fy - arm_y * fx
        assert offset[f"line_{index}_horizontal_kips"] == pytest.approx(horizontal / 1000, abs=0.05, rel=0)
    assert offset["restoring_surge_kips"] == pytest.approx(surge / 1000, abs=0.05, rel=0)
    assert offset["restoring_sway_kips"] == pytest.approx(sway / 1000, abs=0.05, rel=0)
    assert offset["restoring_yaw_kip_ft"] == pytest.approx(moment / 1000, abs=1, rel=0)


def test_generator_outputs_utf8_lf_with_non_utf8_defaults(tmp_path, monkeypatch):
    """Exercise real output with ASCII defaults and Windows newline translation."""
    builder = load_builder()
    cfg = json.loads((REPORT / "example.json").read_text(encoding="utf-8"))
    cfg["description"] = "Synthetic chain — public fixture"
    (tmp_path / "example.json").write_text(json.dumps(cfg, ensure_ascii=False), encoding="utf-8", newline="\n")
    template = (REPORT / "report.template.html").read_text(encoding="utf-8")
    (tmp_path / "report.template.html").write_text(template, encoding="utf-8", newline="\n")
    monkeypatch.setattr(builder, "HERE", tmp_path)
    monkeypatch.setattr(sys, "argv", ["generate.py", "--work-dir", str(tmp_path / "results")])
    original_open = Path.open

    def platform_open(path, mode="r", buffering=-1, encoding=None, errors=None, newline=None):
        if "b" not in mode:
            encoding = "ascii" if encoding in (None, "locale") else encoding
            newline = "\r\n" if newline is None else newline
        return original_open(path, mode, buffering, encoding, errors, newline)

    monkeypatch.setattr(Path, "open", platform_open)
    builder.main()
    for name in ("report.html", "changes.json"):
        raw = (tmp_path / name).read_bytes()
        assert b"\r\n" not in raw
        assert raw.endswith(b"\n")
        raw.decode("utf-8")


def test_source_locators_do_not_embed_unrelated_content_digests():
    sources = load_builder().source_table()
    assert "Normalized content digest" not in sources
    assert not re.search(r"\b[0-9a-f]{64}\b", sources)


@pytest.mark.parametrize("rebuild", [False, True], ids=["saved", "rebuilt"])
def test_report_revision_matches_current_history(tmp_path, rebuild):
    document = (load_builder().build(tmp_path)[0] if rebuild else
                (REPORT / "report.html").read_text(encoding="utf-8"))
    history = re.search(r'<section id="revision-history">(.*?)</section>',
                        document, re.S).group(1)
    revisions = re.findall(r"<tr><td>(.*?)</td>", history)
    changes = json.loads(re.search(
        r'<script id="change-data" type="application/json">(.*?)</script>',
        document, re.S).group(1))
    assert revisions == ["Initial", "1", "2", "3"]
    assert changes["revision"] == revisions[-1]
    assert json.loads((REPORT / "changes.json").read_text(encoding="utf-8")) == changes


COMMENT_CONTROLS_SCRIPT = r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const input = JSON.parse(require('node:fs').readFileSync(0, 'utf8'));
const elements = new Map();
let downloaded, revoked = false, saved = '', closed = false;
const element = tag => ({tag, children: [], value: '', textContent: '',
  append(child) { this.children.push(child); },
  replaceChildren() { this.children = []; },
  click() { downloaded = this.download; }
});
const byId = id => {
  if (!input.ids.includes(id)) return null;
  if (!elements.has(id)) elements.set(id, element(id));
  return elements.get(id);
};
byId('change-data').textContent = input.changes;
const context = {document: {
  getElementById: byId, createElement: element,
  querySelectorAll(selector) {
    assert.equal(selector, 'main > section');
    return input.sections.map(section => ({id: section.id,
      querySelector(selector) {
        const heading = selector.split(',').map(tag => section.headings[tag.trim()])
          .find(text => text !== undefined);
        return heading === undefined ? null : {textContent: heading};
      }
    }));
  }
}, window: {}, Blob, URL: {
  createObjectURL(blob) { context.downloadBlob = blob; return 'blob:comments'; },
  revokeObjectURL(url) { assert.equal(url, 'blob:comments'); revoked = true; }
}, setTimeout(callback) { callback(); }};
vm.runInNewContext(input.script, context);
async function checkControls() {
  assert.deepEqual(byId('comment-section').children.map(option => option.value),
    input.sections.map(section => section.id));
  assert.equal(byId('comment-section').children.find(option =>
    option.value === 'revision-history').textContent, 'Revision history');
  assert.deepEqual(byId('changes').children.map(item => item.textContent),
    JSON.parse(input.changes).changes);
  byId('comment-section').value = 'revision-history';
  byId('comment-text').value = '  Check revision entry.  ';
  byId('add-comment').onclick();
  assert.equal(byId('comment-list').children[0].textContent,
    'revision-history: Check revision entry.');
  assert.equal(byId('comment-text').value, '');
  assert.equal(byId('comment-status').textContent, 'Unsaved comments.');
  byId('add-comment').onclick();
  assert.equal(byId('comment-list').children.length, 1);
  context.window.showSaveFilePicker = async options => {
    assert.equal(options.suggestedName, 'report.comments.json');
    return {createWritable: async () => ({
      write: async text => { saved = text; }, close: async () => { closed = true; }
    })};
  };
  await byId('save-comments').onclick();
  const expected = {revision: input.revision, comments: [
    {section: 'revision-history', text: 'Check revision entry.'}]};
  assert.deepEqual(JSON.parse(saved), expected);
  assert.equal(closed, true);
  assert.equal(byId('comment-status').textContent, 'Comments saved to the selected file.');
  delete context.window.showSaveFilePicker;
  await byId('save-comments').onclick();
  assert.equal(downloaded, 'report.comments.json');
  assert.deepEqual(JSON.parse(await context.downloadBlob.text()), expected);
  assert.equal(revoked, true);
  const load = text => byId('load-comments').onchange({target: {files: [
    {text: async () => text}]}});
  await load(JSON.stringify({revision: input.revision, comments: [
    {section: 'summary', text: 'Loaded review note.'}]}));
  assert.equal(byId('comment-list').children.length, 1);
  assert.equal(byId('comment-list').children[0].textContent, 'summary: Loaded review note.');
  assert.equal(byId('comment-status').textContent, 'Comments loaded.');
  for (const revision of ['Initial', '1', '2', '99']) {
    await load(JSON.stringify({revision, comments: [
      {section: 'summary', text: 'Earlier review note.'}]}));
    assert.equal(byId('comment-list').children[0].textContent, 'summary: Earlier review note.');
    const status = byId('comment-status').textContent;
    assert.match(status, /warning/i);
    assert.ok(status.includes(`comments revision ${revision}`));
    assert.ok(status.includes(`report revision ${input.revision}`));
    assert.notEqual(status, 'Comments loaded.');
  }
  for (const invalid of [
    {revision: input.revision, comments: [{}]},
    {revision: input.revision, comments: [null]},
    {revision: input.revision, comments: 'invalid'},
    {comments: []}, {revision: 3, comments: []}, {revision: '', comments: []}
  ]) {
    await load(JSON.stringify(invalid));
    assert.match(byId('comment-status').textContent, /^Comments were not loaded:/);
    assert.equal(byId('comment-list').children[0].textContent, 'summary: Earlier review note.');
  }
  await load(JSON.stringify({revision: input.revision, comments: []}));
  assert.equal(byId('comment-list').children.length, 0);
  assert.equal(byId('comment-status').textContent, 'Comments loaded.');
}
checkControls().catch(error => { console.error(error); process.exitCode = 1; });
"""


@pytest.mark.parametrize("rebuild", [False, True], ids=["saved", "rebuilt"])
def test_comment_controls_initialize_with_revision_history(tmp_path, rebuild):
    """Execute report JavaScript against its section headings and comment controls.

    Node supplies the JavaScript runtime; a small DOM adapter supplies only the
    interfaces used here. Browser layout and native file dialogs are not tested.
    """
    document = (load_builder().build(tmp_path)[0] if rebuild else
                (REPORT / "report.html").read_text(encoding="utf-8"))
    sections = [{"id": section_id, "headings": dict(re.findall(
        r"<(h[23])>(.*?)</h[23]>", body, re.S))}
        for section_id, body in re.findall(
            r'<section id="([^"]+)">(.*?)</section>', document, re.S)]
    assert any(section["id"] == "revision-history" for section in sections)
    changes = re.search(r'<script id="change-data" type="application/json">(.*?)</script>',
                        document, re.S).group(1)
    script = re.search(r"<script>\s*(.*?)</script>", document, re.S).group(1)
    result = subprocess.run(
        ["node", "-e", COMMENT_CONTROLS_SCRIPT], input=json.dumps({
            "sections": sections, "changes": changes, "script": script,
            "revision": "3",
            "ids": re.findall(r'\bid="([^"]+)"', document)}),
        capture_output=True, text=True, encoding="utf-8", check=False, timeout=10,
    )
    assert result.returncode == 0, result.stderr
