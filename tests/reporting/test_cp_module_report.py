# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the module analysis and check the numerical and reader contracts."""

from copy import deepcopy
import hashlib
from html.parser import HTMLParser
import importlib.util
import json
import math
from pathlib import Path
import re
import subprocess

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


def assert_html_matches(actual, expected):
    """Compare numerical evidence with tolerance and retain text checks."""
    command = "PYTHONPATH=src uv run --no-sources python scripts/reporting/build_cp_module_report.py"
    pattern = r'(<script id="calculation-data" type="application/json">)(.*?)(</script>)'
    actual_values = json.loads(re.search(pattern, actual, re.S)[2])
    expected_values = json.loads(re.search(pattern, expected, re.S)[2])
    assert actual_values == pytest.approx(expected_values, rel=1e-9, abs=1e-9), command
    annotations = r'data-value="([^"]+)"'
    assert [float(v) for v in re.findall(annotations, actual)] == pytest.approx(
        [float(v) for v in re.findall(annotations, expected)], rel=1e-9, abs=1e-9
    ), command
    actual = re.sub(pattern, r'\1\3', actual, flags=re.S)
    expected = re.sub(pattern, r'\1\3', expected, flags=re.S)
    assert re.sub(annotations, 'data-value=""', actual) == re.sub(
        annotations, 'data-value=""', expected
    ), command


@pytest.mark.parametrize("artifact", ["index.html", "changes.json"])
def test_committed_report_artifacts_are_not_ignored(artifact):
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "--",
         f"docs/reports/modules/cp/{artifact}"],
        cwd=ROOT, capture_output=True, text=True,
    )
    assert result.returncode == 1, result.stdout + result.stderr
    assert not result.stdout


def test_report_rebuild_is_deterministic_and_committed(built):
    _, target = built
    command = "PYTHONPATH=src uv run --no-sources python scripts/reporting/build_cp_module_report.py"
    assert_html_matches((target / "index.html").read_text(encoding="utf-8"),
                        (REPORT / "index.html").read_text(encoding="utf-8"))
    assert json.loads((target / "changes.json").read_text(encoding="utf-8")) == json.loads(
        (REPORT / "changes.json").read_text(encoding="utf-8")
    ), command


def test_committed_rebuild_accepts_crlf_checkout(built, tmp_path, monkeypatch):
    for name in ("index.html", "changes.json"):
        text = (REPORT / name).read_text(encoding="utf-8")
        (tmp_path / name).write_bytes(text.replace("\n", "\r\n").encode("utf-8"))
    monkeypatch.setitem(globals(), "REPORT", tmp_path)
    test_report_rebuild_is_deterministic_and_committed(built)


@pytest.mark.parametrize("delta", [1e-11, 1.0])
def test_committed_rebuild_uses_numeric_tolerance(built, tmp_path, monkeypatch, delta):
    for name in ("index.html", "changes.json"):
        text = (REPORT / name).read_text(encoding="utf-8")
        if name == "index.html":
            pattern = r'(<script id="calculation-data" type="application/json">)(.*?)(</script>)'
            match = re.search(pattern, text, re.S)
            payload = json.loads(match[2])
            payload["results.current_demand_A.total_initial_A"] += delta
            text = text[:match.start(2)] + json.dumps(payload, sort_keys=True) + text[match.end(2):]
        (tmp_path / name).write_text(text, encoding="utf-8", newline="\n")
    monkeypatch.setitem(globals(), "REPORT", tmp_path)
    if delta < 1e-9:
        test_report_rebuild_is_deterministic_and_committed(built)
    else:
        with pytest.raises(AssertionError, match="build_cp_module_report.py"):
            test_report_rebuild_is_deterministic_and_committed(built)


@pytest.mark.parametrize("material_change", [False, True])
def test_full_render_distinguishes_roundoff_from_material_change(monkeypatch, material_change):
    """Recompute the complete report and its identity after a math perturbation."""
    mod = builder()
    expected_data = mod.calculate()
    expected = mod.render(expected_data)
    real_hypot = math.hypot

    def perturbed_hypot(*args):
        value = real_hypot(*args)
        return value * 1.01 if material_change else math.nextafter(value, math.inf)

    monkeypatch.setattr(mod.math, "hypot", perturbed_hypot)
    actual_data = mod.calculate()
    assert actual_data["section_periphery_m"] != expected_data["section_periphery_m"]
    actual = mod.render(actual_data)
    key_pattern = r'id="document-key"[^>]+content="([^"]+)"'
    actual_key = re.search(key_pattern, actual)[1]
    expected_key = re.search(key_pattern, expected)[1]
    if material_change:
        assert actual_key != expected_key
        with pytest.raises(AssertionError, match="build_cp_module_report.py"):
            assert_html_matches(actual, expected)
    else:
        assert_html_matches(actual, expected)
        assert actual_key == expected_key


def test_rebuild_rejects_an_isolated_document_key_change(built):
    _, target = built
    expected = (target / "index.html").read_text(encoding="utf-8")
    actual = re.sub(r'(?<=name="document-key" content=")[^"]+', "stale-document", expected)
    with pytest.raises(AssertionError, match="build_cp_module_report.py"):
        assert_html_matches(actual, expected)


def test_access_metadata_preserves_issued_input_digest():
    mod = builder()
    issued = ROOT / "docs/domains/cathodic_protection/reports/reference-jacket/cathodic-protection-analysis.citations.json"
    retained = json.loads(issued.read_text(encoding="utf-8"))
    canonical_input = mod.GEOMETRY.read_text(encoding="utf-8").encode("utf-8")
    assert hashlib.sha256(canonical_input).hexdigest() == retained["input_sha256"], (
        "Restore the issued input before rebuilding with PYTHONPATH=src uv run "
        "--no-sources python scripts/reporting/build_reference_jacket_analysis.py"
    )
    for fixture in (EXAMPLE, mod.GEOMETRY):
        assert "source_access" not in yaml.safe_load(fixture.read_text(encoding="utf-8"))


def test_every_worked_quantity_matches_the_actual_module(built):
    mod, target = built
    cfg = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    # Published symmetric trapezoid: c = b + B + 2 hypot(h, (B-b)/2).
    cfg["inputs"]["anode"]["radius_m"] = 0.1672452171860739
    expected = run_cathodic_protection(deepcopy(cfg))["results"]
    actual = mod.calculate()
    assert actual["results"] == expected
    recommended = expected["current_output_verification"]["recommended_anode_count"]
    cfg["inputs"]["anode"]["count"] = recommended
    assert actual["rechecked"] == run_cathodic_protection(cfg)["results"]
    assert actual["rechecked"]["status"]["result"] == "PASS"
    html = (target / "index.html").read_text(encoding="utf-8")
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
    html = (target / "index.html").read_text(encoding="utf-8")
    reader = Reader()
    reader.feed(html)
    assert reader.headings.count("h2") == 10
    assert set(reader.headings) <= {"h2", "h3"}
    assert len(reader.ids) == len(set(reader.ids))
    assert '<body class="doc hs-doc">' in html
    shared = (ROOT / "src/digitalmodel/reporting/assets/calc_report_format.html").read_text(encoding="utf-8")
    assert re.search(r'<style>.*?</style>', shared, re.S)[0] in html
    assert '<script src=' not in html and '<link rel="stylesheet"' not in html
    for href in reader.links:
        if href.startswith("#"):
            assert href[1:] in reader.ids
        elif not href.startswith("https:"):
            assert (REPORT / href).resolve().is_file()
    text = " ".join(reader.visible)
    assert "Draft for maintainer review" in text
    assert "ASSUMED - to be confirmed" in text
    assert "NOT_EVALUATED" in text
    assert "supplied member schedule" in text
    assert "Different structures are represented" in text
    assert "locally saved comments" in text
    assert "showSaveFilePicker" in html and "createWritable" in html
    assert "comments.json" in html and "textContent" in html
    assert 'id="changes-data"' in html
    changes = json.loads((target / "changes.json").read_text(encoding="utf-8"))
    assert changes["revision"] == 3
    assert changes["date"] == "2026-10-23"
    assert changes["changes"]


def test_chart_limits_and_client_text_preflight(built):
    _, target = built
    html = (target / "index.html").read_text(encoding="utf-8")
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
    html = (target / "index.html").read_text(encoding="utf-8")
    script = re.findall(r'<script>(.*?)</script>', html, re.S)[-1]
    changes = json.loads((target / "changes.json").read_text(encoding="utf-8"))
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
  const point = new Element(); point.dataset = {tooltip:"Demand: 547; Limit: 569; Units: anodes"};
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


def test_published_section_basis_and_independent_arithmetic():
    """Pin results to public inputs and hand arithmetic, without a solver oracle."""
    data = builder().calculate()
    results = data["results"]
    demand = results["current_demand_A"]
    # Table 1 areas times Table 2 phase densities, plus 9 x 2 A:
    # initial = 652.9*.01 + 3726.8*.15 + 15737*.12 + 10814.8*.02 + 18.
    # mean and final use (.02,.07,.06,.02) and (.04,.10,.08,.02).
    assert demand["total_initial_A"] == pytest.approx(2688.285)
    assert demand["total_mean_A"] == pytest.approx(1452.450)
    assert demand["total_final_A"] == pytest.approx(1892.052)
    # M = 1452.450 x 25 x 8760 / (2000 x .90); ceil(M / 323.4) = 547.
    assert results["anode_requirements"]["total_mass_kg"] == pytest.approx(176714.75)
    ver = results["current_output_verification"]
    # Published section (mm): 240 + 270 + 2*hypot(270, (270-240)/2).
    # c = 1.0508326913 m; r = c/(2*pi). R = .22*(ln(4*2.020/r)-1)/(2*pi*2.020).
    # Final radius = r*sqrt(1-.90), at fixed length; I = .25/R.
    pins = {
        "equivalent_radius_initial_m": 0.1672452172,
        "equivalent_radius_final_m": 0.0528875814,
        "anode_resistance_initial_ohm": 0.0498809659,
        "anode_resistance_final_ohm": 0.0698371332,
        "anode_current_output_initial_A": 5.0119318175,
        "anode_current_output_final_A": 3.5797574817,
        "total_anode_current_output_initial_A": 2851.7892042,
        "total_anode_current_output_final_A": 2036.8820071,
    }
    for key, value in pins.items():
        # The adapter rounds radii/resistances to six, output to four, totals
        # to three decimal places. Compare independent values at that precision.
        places = 3 if key.startswith("total_") else (4 if "current_output" in key else 6)
        assert ver[key] == pytest.approx(value, abs=0.5 * 10 ** -places, rel=0), key
    # ceil(2688.285 / 5.0119318175) = 537; ceil(1892.052 / 3.5797574817) = 529.
    assert (ver["count_by_mass"], ver["count_by_initial_current"],
            ver["count_by_final_current"]) == (547, 537, 529)
    assert ver["recommended_anode_count"] == 547
    assert ver["governing_case"] == "mass"
    assert results["status"]["result"] == "PASS"
    assert data["rechecked"]["status"]["result"] == "PASS"
    text = builder().render(data)
    assert "install 575" not in text.lower()
    assert "fails the modelled initial-current" not in text
    assert "No aggregate increase is required" in text


def test_fingerprint_and_rebuild_ignore_checkout_line_endings(tmp_path, monkeypatch):
    """CRLF inputs keep report bytes and saved-comment identity unchanged."""
    mod = builder()
    expected = mod.render(mod.calculate())
    for name, original in (("__file__", BUILDER), ("STYLE", mod.STYLE),
                           ("EXAMPLE", mod.EXAMPLE), ("GEOMETRY", mod.GEOMETRY),
                           ("SOURCE_ACCESS", mod.SOURCE_ACCESS)):
        path = tmp_path / original.name
        path.write_bytes(original.read_bytes().replace(b"\r\n", b"\n").replace(b"\n", b"\r\n"))
        monkeypatch.setattr(mod, name, path)
    actual = mod.render(mod.calculate())
    assert_html_matches(actual, expected)
    assert re.search(r'id="document-key"[^>]+content="([^"]+)"', actual)[1] == re.search(
        r'id="document-key"[^>]+content="([^"]+)"', expected
    )[1], "PYTHONPATH=src uv run --no-sources python scripts/reporting/build_cp_module_report.py"
    # A substantive edit must still invalidate saved comments.
    source = Path(mod.__file__)
    source.write_bytes(source.read_bytes() + b"# Content change\r\n")
    assert mod.render(mod.calculate()) != expected


def test_output_newlines_are_explicit(tmp_path, monkeypatch):
    """Emulate Windows text output even when the test runs on Linux."""
    real_write = Path.write_text

    def windows_write(path, text, *args, **kwargs):
        if kwargs.get("newline") is None:
            kwargs["newline"] = "\r\n"
        return real_write(path, text, *args, **kwargs)

    monkeypatch.setattr(Path, "write_text", windows_write)
    builder().build(tmp_path)
    for name in ("index.html", "changes.json"):
        assert b"\r" not in (tmp_path / name).read_bytes()


def test_comparison_precision_labels_and_signed_differences(built):
    mod, target = built
    reader = Reader()
    reader.feed((target / "index.html").read_text(encoding="utf-8"))
    for attrs, shown in reader.values:
        if attrs["data-key"] == "comparison.initial_output.reference":
            assert shown == "5.01" and attrs["data-places"] == "2"
        if attrs["data-key"] == "comparison.final_output.reference":
            assert shown == "3.63" and attrs["data-places"] == "2"
    visible = " ".join(reader.visible)
    assert "Installed/required count" in visible
    assert "Signed difference" in visible
    values = mod.quantities(mod.calculate())
    assert values["comparison.mean_demand.difference_percent"] < 0
    assert values["comparison.initial_output.difference_percent"] > 0
    assert values["comparison.final_output.difference_percent"] < 0
    assert "−1.4" in visible


def test_source_access_is_recorded_separately_from_calculation_inputs(built):
    mod, target = built
    html = (target / "index.html").read_text(encoding="utf-8")
    records = json.loads(mod.SOURCE_ACCESS.read_text(encoding="utf-8"))["source_access"]
    assert {row["input"] for row in records} == {EXAMPLE.name, mod.GEOMETRY.name}
    assert mod.calculate()["source_access"] == records
    for access in records:
        assert access["url"].startswith("https://")
        assert access["http_status"] == 200
        assert re.fullmatch(r"[0-9a-f]{64}", access["response_sha256"])
        assert re.fullmatch(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z", access["accessed_utc"])
        assert access["accessed_utc"] in html
    assert "Both open papers were read on" not in html


@pytest.mark.parametrize("bad_record", [
    None, {}, {"http_status": 404}, {"http_status": None},
    {"accessed_utc": "2026-99-99T00:00:00Z"},
    {"response_sha256": "invalid"}, {"url": ""}, {"input": "other.yml"},
])
def test_source_access_rejects_incomplete_evidence(tmp_path, monkeypatch, bad_record):
    mod = builder()
    records = deepcopy(mod.calculate()["source_access"])
    for record, fixture in zip(records, (EXAMPLE, mod.GEOMETRY)):
        record["input"] = fixture.name
    if bad_record is None or bad_record == {}:
        records[0] = bad_record
    else:
        records[0].update(bad_record)
    path = tmp_path / "source_access.json"
    path.write_text(json.dumps({"source_access": records}), encoding="utf-8", newline="\n")
    monkeypatch.setattr(mod, "SOURCE_ACCESS", path, raising=False)
    with pytest.raises(ValueError, match="source-access"):
        mod.calculate()
