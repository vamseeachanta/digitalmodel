# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the module analysis and check the numerical and reader contracts."""

from copy import deepcopy
from html.parser import HTMLParser
import importlib.util
import json
from pathlib import Path
import re

import pytest
import yaml

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/reporting/build_cp_module_report.py"
REPORT = ROOT / "docs/reports/modules/cp"
EXAMPLE = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/published_jacket_84m.yml"


def builder():
    """Load the command line builder without changing the import environment."""
    spec = importlib.util.spec_from_file_location("cp_module_report", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Reader(HTMLParser):
    """Collect numerical annotations, headings, links and visible strings."""

    def __init__(self):
        super().__init__()
        self.values = []
        self.headings = []
        self.links = []
        self.ids = []
        self.visible = []
        self.skip = 0
        self.current = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in {"script", "style"}:
            self.skip += 1
        if tag in {"h2", "h3", "h4", "h5", "h6"}:
            self.headings.append(tag)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if "data-value" in attrs:
            self.current = attrs

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.skip -= 1
        if tag in {"span", "text"}:
            self.current = None

    def handle_data(self, data):
        if not self.skip:
            self.visible.append(data)
        if self.current is not None and data.strip():
            self.values.append((self.current.copy(), data))


@pytest.fixture(scope="module")
def built(tmp_path_factory):
    target = tmp_path_factory.mktemp("cp-module-analysis")
    mod = builder()
    mod.build(target)
    return mod, target


def test_report_rebuild_is_deterministic_and_committed(built):
    _, target = built
    for name in ("index.html", "changes.json"):
        assert (target / name).read_bytes() == (REPORT / name).read_bytes()


def test_every_worked_quantity_matches_the_actual_module(built):
    mod, target = built
    cfg = yaml.safe_load(EXAMPLE.read_text())
    expected = run_cathodic_protection(deepcopy(cfg))["results"]
    actual = mod.calculate()
    assert actual["results"] == expected
    recommended = expected["current_output_verification"]["recommended_anode_count"]
    cfg["inputs"]["anode"]["count"] = recommended
    assert actual["rechecked"] == run_cathodic_protection(cfg)["results"]
    assert actual["rechecked"]["status"]["result"] == "PASS"
    html = (target / "index.html").read_text()
    payload = json.loads(re.search(r'<script id="calculation-data" type="application/json">(.*?)</script>', html, re.S)[1])
    assert payload == mod.quantities(actual)
    for zone in cfg["inputs"]["structure"]["zones"]:
        for phase, density in zone["effective_current_density_A_m2"].items():
            assert payload[f"density_mA_m2.{zone['zone']}.{phase}"] == density * 1000
    reader = Reader()
    reader.feed(html)
    assert len(reader.values) >= 50
    for attrs, shown in reader.values:
        value = payload[attrs["data-key"]]
        assert json.loads(attrs["data-value"]) == value
        assert shown == mod.format_number(value, int(attrs["data-places"]))
    expected_paths = {
        "results.current_output_verification." + key for key in (
            "anode_count", "count_by_mass", "count_by_initial_current",
            "count_by_final_current", "recommended_anode_count",
            "anode_current_output_initial_A", "anode_current_output_final_A",
        )
    }
    assert expected_paths <= {attrs["data-key"] for attrs, _ in reader.values}
    # Quantities in the worked narrative are rendered through annotations only.
    worked = re.search(r'<section id="worked-example">(.*?)</section>', html, re.S)[1]
    worked = re.sub(r'<span\b.*?</span>', '', worked, flags=re.S)
    worked = re.sub(r'<h2>.*?</h2>', '', worked, flags=re.S)
    text_reader = Reader()
    text_reader.feed(worked)
    assert not re.search(r'\d', ' '.join(text_reader.visible))


def test_house_style_structure_sources_and_self_containment(built):
    _, target = built
    html = (target / "index.html").read_text()
    reader = Reader()
    reader.feed(html)
    assert reader.headings.count("h2") == 10
    assert set(reader.headings) <= {"h2", "h3"}
    assert len(reader.ids) == len(set(reader.ids))
    assert '<body class="doc hs-doc">' in html
    shared = (ROOT / "src/digitalmodel/reporting/assets/calc_report_format.html").read_text()
    assert re.search(r'<style>.*?</style>', shared, re.S)[0] in html
    assert '<script src=' not in html and '<link rel="stylesheet"' not in html
    for href in reader.links:
        if href.startswith("#"):
            assert href[1:] in reader.ids
        elif not href.startswith("https:"):
            assert (REPORT / href).resolve().is_file()
    text = " ".join(reader.visible)
    assert "Draft for client review" in text
    assert "ASSUMED - to be confirmed" in text
    assert "NOT_EVALUATED" in text
    assert "supplied member schedule" in text
    assert "Different structures are represented" in text
    assert "locally saved comments" in text
    assert "showSaveFilePicker" in html and "createWritable" in html
    assert "comments.json" in html and "textContent" in html
    assert 'id="changes-data"' in html
    changes = json.loads((target / "changes.json").read_text())
    assert changes["revision"] == 0
    assert changes["changes"]


def test_chart_limits_and_client_text_preflight(built):
    _, target = built
    html = (target / "index.html").read_text()
    reader = Reader()
    reader.feed(html)
    assert 'id="count-chart"' in html
    assert 'class="limit"' in html
    assert 'data-tooltip=' in html and 'tabindex="0"' in html
    assert "Demand:" in html and "Limit:" in html and "Units: anodes" in html
    visible = ' '.join(reader.visible)
    forbidden = (
        r'\b(?:[o]wner|[a]gent|[l]ane|[s]ession|[d]ispatcher|[f]leet|'
        r'could|should|would|validated|certified|compliant|confirmed|traceable)\b'
    )
    # The required assumption label is the sole use of "confirmed".
    visible = visible.replace("ASSUMED - to be confirmed", "")
    assert not re.search(forbidden, visible, re.I)
    assert not re.search(r'<(?:b|strong)\b', html)
    without_doi = re.sub(r'\b10\.\d+/[\w./-]+', '', visible)
    assert not re.search(r'(?<!\d)\d+\.\d{4,}', without_doi)
    prohibited_roots = '|'.join('/' + name + '/' for name in ('home', 'mnt'))
    assert not re.search(prohibited_roots + r'|[A-Za-z]:\\Users\\|\bws-[0-9a-f]{2}\b', html)
    assert not re.search(r'[\w.+-]+@[\w.-]+\.[A-Za-z]{2,}', html)


def test_changed_material_assumption_stops_publication(monkeypatch):
    mod = builder()
    monkeypatch.setattr(mod.kernel, "ANODE_DENSITY_ALZNI", mod.kernel.ANODE_DENSITY_ALZNI + 1)
    with pytest.raises(ValueError, match="material defaults differ"):
        mod.calculate()


def test_comment_save_load_stale_rejection_and_chart_events(built):
    """Exercise the delivered script with DOM doubles, without a browser."""
    import shutil
    import subprocess

    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable for the script contract")
    _, target = built
    html = (target / "index.html").read_text()
    script = re.findall(r'<script>(.*?)</script>', html, re.S)[-1]
    changes = json.loads((target / "changes.json").read_text())
    harness = r'''
const assert = require("node:assert/strict");
(async () => {
  let saved, downloaded, blocked = false;
  class Element {
    constructor() { this.handlers = {}; this.children = []; this.value = "summary"; }
    addEventListener(name, handler) { this.handlers[name] = handler; }
    append(child) { this.children.push(child); }
    replaceChildren() { this.children = []; }
    click() { downloaded = this.download; }
  }
  const elements = new Map(), get = id => {
    if (!elements.has(id)) elements.set(id, new Element()); return elements.get(id);
  };
  get("document-key").content = "current-document";
  get("changes-data").textContent = JSON.stringify(CHANGES);
  get("comment-section").options = ["summary", "scope"].map(value => ({value}));
  const point = new Element(); point.dataset = {tooltip:"Demand: 575; Limit: 569; Units: anodes"};
  const handlers = {};
  global.document = {
    getElementById:get, createElement:() => new Element(),
    querySelectorAll:() => [point], addEventListener:(name, handler) => { handlers[name] = handler; }
  };
  const store = new Map();
  global.localStorage = {
    getItem:key => store.get(key) || null,
    setItem:(key, value) => { if (blocked) throw new Error("unavailable"); store.set(key, value); }
  };
  global.window = {
    getSelection:() => ({rangeCount:1, toString:() => "selected quotation",
      anchorNode:{parentElement:{closest:() => ({id:"scope"})}}}),
    showSaveFilePicker:async () => ({createWritable:async () => ({
      write:async blob => { saved = JSON.parse(await blob.text()); }, close:async () => {}
    })})
  };
  global.setTimeout = callback => callback();
  URL.createObjectURL = () => "blob:comments"; URL.revokeObjectURL = () => {};
  SCRIPT
  point.handlers.focus(); assert.equal(get("chart-readout").textContent, point.dataset.tooltip);
  handlers.selectionchange(); get("comment-text").value = "Review the distribution.";
  get("add-comment").handlers.click();
  assert.equal(get("comment-list").children.length, 1);
  assert.match(get("comment-list").children[0].textContent, /selected quotation/);
  await get("save-comments").handlers.click();
  assert.equal(saved.document, "current-document"); assert.equal(saved.comments.length, 1);
  const load = async data => get("load-comments").handlers.change({target:{files:[{text:async () => JSON.stringify(data)}]}});
  await load(saved); assert.equal(get("comment-list").children.length, 1);
  await load({document:"another-document", comments:saved.comments});
  assert.match(get("comment-status").textContent, /does not match/);
  assert.equal(get("comment-list").children.length, 1);
  await load({document:"current-document", comments:[{section:"scope", text:7, quote:""}]});
  assert.match(get("comment-status").textContent, /invalid content/);
  await load({document:"current-document", comments:[{section:"scope", text:"<img onerror=alert(1)>", quote:""}]});
  assert.equal(get("comment-list").children.length, 2);
  assert.match(get("comment-list").children[1].textContent, /<img/);
  window.showSaveFilePicker = undefined;
  await get("save-comments").handlers.click(); assert.equal(downloaded, "comments.json");
  blocked = true; get("comment-text").value = "Retain this additional note.";
  get("add-comment").handlers.click();
  assert.match(get("comment-status").textContent, /storage is unavailable/);
})().catch(error => { console.error(error); process.exitCode = 1; });
'''
    harness = harness.replace("JSON.stringify(CHANGES)", "JSON.stringify(" + json.dumps(changes) + ")")
    harness = harness.replace("  SCRIPT\n", script + "\n")
    result = subprocess.run([node], input=harness, capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stderr
