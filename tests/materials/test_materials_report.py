# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the client report and compare its numerical surfaces to public calls."""

import importlib.util
import json
import math
import re
from dataclasses import replace
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel.materials import line_pipe, smts_mpa

ROOT = Path(__file__).resolve().parents[2]
GENERATOR = ROOT / "scripts/generate_materials_report.py"
REPORT = ROOT / "docs/reports/modules/materials/materials-module-report.html"
CHANGES = REPORT.with_name("changes.json")


@pytest.fixture(autouse=True)
def legacy_text_locale(monkeypatch):
    """Exercise report helpers with a Windows legacy text-file default."""
    original_open = Path.open

    def open_with_legacy_encoding(path, mode="r", buffering=-1, encoding=None,
                                  errors=None, newline=None):
        if "b" not in mode and encoding in (None, "locale"):
            encoding = "cp1252"
        return original_open(path, mode, buffering, encoding, errors, newline)

    monkeypatch.setattr(Path, "open", open_with_legacy_encoding)


class ReportParser(HTMLParser):
    """Collect numerical cells, local references and visible report content."""

    def __init__(self):
        super().__init__()
        self.quantities = []
        self.ids = set()
        self.links = []
        self.levels = []
        self.visible = []
        self.current = None
        self.hidden = 0

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        self.ids.add(attrs.get("id"))
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag == "h2":
            self.levels.append(tag)
        if tag in ("script", "style"):
            self.hidden += 1
        if "data-quantity" in attrs:
            self.current = [attrs["data-quantity"], attrs["data-format"], ""]

    def handle_data(self, data):
        if not self.hidden:
            self.visible.append(data)
        if self.current is not None:
            self.current[2] += data

    def handle_endtag(self, tag):
        if tag in ("script", "style"):
            self.hidden -= 1
        if tag == "span" and self.current is not None:
            self.quantities.append(self.current)
            self.current = None


def load_generator():
    assert GENERATOR.is_file(), "the report generator has not been implemented"
    spec = importlib.util.spec_from_file_location("materials_report", GENERATOR)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def parse(document):
    parser = ReportParser()
    parser.feed(document)
    return parser


def expected_quantities():
    pipe = line_pipe("X52", nps=8.0, schedule="SCH 40")
    return {
        "nps": pipe.nps,
        "od_in": pipe.od_in, "wt_in": pipe.wt_in, "id_in": pipe.id_in,
        "od_mm": pipe.od_mm, "wt_mm": pipe.wt_mm, "id_mm": pipe.id_mm,
        "d_over_t": pipe.d_over_t, "metal_area_in2": pipe.metal_area_in2,
        "bore_area_in2": pipe.bore_area_in2,
        "weight_kg_m": pipe.weight_kg_m, "weight_lb_ft": pipe.weight_lb_ft,
        "volume_l_m": pipe.internal_volume_m3_per_m * 1000,
        "smys_mpa": pipe.grade.smys_mpa,
        "smts_psl1_mpa": smts_mpa(pipe.grade.name, psl="PSL1"),
        "smts_psl2_mpa": smts_mpa(pipe.grade.name, psl="PSL2"),
        "rho_kg_m3": pipe.grade.rho_kg_m3,
        "wall_limit_mm": pipe.od_mm / 2,
    }


def test_rebuild_is_deterministic_and_matches_committed_report(tmp_path):
    generator = load_generator()
    generator.build(tmp_path)
    assert (tmp_path / REPORT.name).read_bytes() == REPORT.read_bytes()
    assert (tmp_path / CHANGES.name).read_bytes() == CHANGES.read_bytes()
    assert sorted(p.name for p in tmp_path.iterdir()) == sorted([REPORT.name, CHANGES.name])


def test_all_worked_example_numbers_equal_fresh_module_calls():
    document = load_generator().render_report()
    expected = expected_quantities()
    quantities = parse(document).quantities
    assert quantities
    assert {key for key, _, _ in quantities} == set(expected)
    for key, precision, displayed in quantities:
        assert displayed == format(expected[key], precision), key
    section = re.search(r'<section id="worked-example">(.*?)</section>', document, re.S)[1]
    assert 'data-quantity="weight_kg_m"' in section
    assert "computed" in section.lower()


def test_rendered_example_matches_independent_cited_values():
    """Pin the supplier strength transcription and an annular-area hand check.

    Strength basis: UNIASEN, API 5L Pipe Specification, edition 46 (2018),
    Mechanical Requirement PSL1/PSL2, L360/X52; report reference ref-public-grade.
    Original-table qualification remains pending. Dimensions: Continental Steel,
    WSP40-800-322, Specifications; report reference ref-public-dimensions.
    Mass = pi * t * (D - t) * density, with inches converted to metres;
    7850 kg/m3 is the assumed implementation basis, not a measured density.
    """
    expected = {
        "smys_mpa": "360.0", "smts_psl1_mpa": "460.0",
        "smts_psl2_mpa": "460.0", "weight_kg_m": "42.5",
    }
    hand_mass_kg_m = math.pi * 0.322 * (8.625 - 0.322) * 0.0254 ** 2 * 7850
    assert format(hand_mass_kg_m, ".1f") == expected["weight_kg_m"]
    parsed = parse(load_generator().render_report())
    for key, value in expected.items():
        displayed = [text for name, _, text in parsed.quantities if name == key]
        assert displayed and all(text == value for text in displayed), key
    assert {"ref-public-grade", "ref-public-dimensions", "ref-method"} <= parsed.ids


def test_report_refuses_density_inconsistent_with_mass_basis(monkeypatch):
    generator = load_generator()
    pipe = line_pipe("X52", nps=8.0, schedule="SCH 40")
    changed = replace(pipe, grade=replace(pipe.grade, rho_kg_m3=8000))
    monkeypatch.setattr(generator, "line_pipe", lambda *args, **kwargs: changed)
    with pytest.raises(ValueError, match="density"):
        generator.render_report()


@pytest.mark.parametrize("relative_difference", [-0.004, 0.004])
def test_public_mass_guard_rejects_both_out_of_tolerance_directions(
    monkeypatch, relative_difference,
):
    from digitalmodel.materials import LinePipe

    generator = load_generator()
    changed = generator.PUBLIC_REFERENCE["weight_lb_ft"] * (1 + relative_difference)
    monkeypatch.setattr(LinePipe, "weight_lb_ft", property(lambda self: changed))
    with pytest.raises(ValueError, match="public plain-end mass check"):
        generator.render_report()


def test_report_sources_have_one_license_header():
    for path in (GENERATOR, Path(__file__)):
        assert path.read_bytes().count(b"Copyright" + b" (c)") == 1, path.name


def test_example_changes_when_module_output_changes(monkeypatch):
    generator = load_generator()
    from digitalmodel.materials import LinePipe
    original = line_pipe("X52", nps=8.0, schedule="SCH 40").weight_kg_m
    changed = original * 1.001
    monkeypatch.setattr(LinePipe, "weight_kg_m", property(lambda self: changed))
    values = [text for key, _, text in parse(generator.render_report()).quantities
              if key == "weight_kg_m"]
    assert values and all(value == format(changed, ".1f") for value in values)
    assert format(changed, ".1f") != format(original, ".1f")


def test_chart_points_and_open_boundary_follow_module():
    generator = load_generator()
    document = generator.render_report()
    payload = json.loads(re.search(
        r'<script id="chart-data" type="application/json">(.*?)</script>', document, re.S
    )[1])
    pipe = line_pipe("X52", nps=8.0, schedule="SCH 40")
    assert payload["wall_limit_mm"] == format(pipe.od_mm / 2, ".3f")
    for point in payload["points"]:
        sample = line_pipe("X52", od_in=pipe.od_in, wt_in=point["wt_in"])
        assert point["bore_mm"] == format(sample.id_mm, ".1f")
        assert point["wall_mm"] == format(sample.wt_mm, ".3f")
        assert sample.wt_in < pipe.od_in / 2
    assert 'id="geometry-limit"' in document
    assert 'id="zero-wall-limit"' in document
    assert "outside the hollow-section domain" in document
    with pytest.raises(ValueError):
        line_pipe("X52", od_in=pipe.od_in, wt_in=pipe.od_in / 2)
    with pytest.raises(ValueError):
        line_pipe("X52", od_in=pipe.od_in, wt_in=0)


def test_style_sources_sections_links_and_offline_delivery():
    generator = load_generator()
    document = generator.render_report()
    parsed = parse(document)
    assert '<body class="hs-doc">' in document
    assert (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8") in document
    assert len(parsed.levels) == 10
    assert all(link[1:] in parsed.ids for link in parsed.links if link.startswith("#"))
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)="https?://', document)
    assert "showSaveFilePicker" in document and "textContent" in document
    assert "materials-module-report.comments.json" in document
    assert 'id="changes-data"' in document
    assert "Draft for client review" in document
    assert "Not covered" in document
    assert "NOT_EVALUATED" in document
    assert "ASSUMED - to be confirmed" in document
    assert document.index('id="revision-history"') < document.index('id="introduction"')


def test_built_visible_text_uses_engineering_register():
    visible = " ".join(parse(load_generator().render_report()).visible)
    assert not re.search(r'\b(owner|agent|lane|session|job|sha|could|should|would)\b', visible, re.I)
    assert not re.search(r'<(?:b|strong)(?:\s|>)', load_generator().render_report())
    assert not re.search(r'\b\d+\.\d{4,}\b', visible)


def test_generator_refuses_unqualified_source_digest(monkeypatch):
    generator = load_generator()
    from digitalmodel.materials import database
    monkeypatch.setattr(database, "_SOURCE_SHA256", "0" * 64)
    with pytest.raises(ValueError, match="digest"):
        generator.render_report()


def test_embedded_interactions_save_roundtrip_and_handle_untrusted_comments():
    """Execute shipped JavaScript with DOM/file doubles, without a browser."""
    import subprocess

    document = load_generator().render_report()
    script = re.findall(r'<script>(.*?)</script>', document, re.S)[-1]
    chart = json.loads(re.search(
        r'<script id="chart-data" type="application/json">(.*?)</script>', document, re.S
    )[1])
    changes = json.loads(CHANGES.read_text(encoding="utf-8"))
    program = r'''
const assert = require("node:assert/strict");
const vm = require("node:vm");
const fs = require("node:fs");
const input = JSON.parse(fs.readFileSync(0, "utf8"));
function element() {
  return {textContent: "", value: "", children: [], events: {}, dataset: {},
    addEventListener(type, handler) { this.events[type] = handler; },
    appendChild(child) { this.children.push(child); },
    replaceChildren() { this.children = []; }, click() {}};
}
async function check(nativeSave) {
  const ids = new Map();
  const get = id => { if (!ids.has(id)) ids.set(id, element()); return ids.get(id); };
  get("chart-data").textContent = JSON.stringify(input.chart);
  get("changes-data").textContent = JSON.stringify(input.changes);
  get("comment-section").value = "6";
  let written, downloaded, stored;
  const context = {JSON, Number, Array, Error, String, setTimeout: callback => callback(),
    Blob, URL: {createObjectURL(blob) { downloaded = blob; return "blob:comments"; }, revokeObjectURL() {}},
    localStorage: {getItem() {return stored || null;}, setItem(key, value) {stored = value;}},
    document: {getElementById: get, querySelectorAll() {return [];}, createElement: element},
    window: nativeSave ? {showSaveFilePicker: async () => ({createWritable: async () => ({
      async write(text) { written = text; }, async close() {}})})} : {}};
  vm.runInNewContext(input.script, context);
  assert.match(get("chart-readout").textContent, /component: pipe wall/);
  assert.match(get("chart-readout").textContent, /wall demand:/);
  get("comment-text").value = "<img src=x onerror=alert(1)>";
  get("add-comment").events.click();
  assert.equal(get("comment-list").children.length, 1);
  assert.match(get("comment-list").children[0].textContent, /<img/);
  await get("save-comments").events.click();
  const saved = JSON.parse(nativeSave ? written : await downloaded.text());
  assert.equal(saved.comments[0].section, 6);
  assert.equal(saved.comments[0].text, "<img src=x onerror=alert(1)>");
  assert.equal(saved.revision, input.changes.revision);
  const load = value => get("import-comments").events.change({target: {files: [{size: 100,
    text: async () => JSON.stringify(value)}]}});
  await load({...saved, revision: "different"});
  assert.match(get("comment-status").textContent, /not loaded/);
  assert.equal(get("comment-list").children.length, 1);
  await load({...saved, comments: [{section: 11, text: "invalid"}]});
  assert.match(get("comment-status").textContent, /not loaded/);
  await load(saved);
  assert.match(get("comment-status").textContent, /loaded for this revision/);
  assert.equal(JSON.parse(stored).comments[0].text, saved.comments[0].text);
}
(async () => { await check(true); await check(false); })().catch(error => {
  process.stderr.write(String(error.stack)); process.exitCode = 1;
});
'''
    result = subprocess.run(
        ["node", "-e", program], input=json.dumps({
            "script": script, "chart": chart, "changes": changes,
        }), capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stderr


def test_deliverables_are_not_hidden_by_output_ignore_rules():
    import subprocess

    paths = [GENERATOR, REPORT, CHANGES]
    result = subprocess.run(
        ["git", "check-ignore", *[str(path.relative_to(ROOT)) for path in paths]],
        cwd=ROOT, text=True, capture_output=True, check=False,
    )
    assert result.returncode == 1, result.stdout


def test_missing_house_style_fails_explicitly(tmp_path, monkeypatch):
    generator = load_generator()
    exemplar = tmp_path / "docs/api/cfd/turbulent-flat-plate-verification.html"
    exemplar.parent.mkdir(parents=True)
    exemplar.write_text("<html><body>Style missing</body></html>", encoding="utf-8")
    brand = tmp_path / "docs/api/_assets/brand.css"
    brand.parent.mkdir(parents=True)
    brand.write_bytes((ROOT / "docs/api/_assets/brand.css").read_bytes())
    monkeypatch.setattr(generator, "ROOT", tmp_path)
    with pytest.raises(ValueError, match="house stylesheet"):
        generator.render_report()
