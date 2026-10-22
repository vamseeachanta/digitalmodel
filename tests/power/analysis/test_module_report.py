# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand anchors, consistency checks and portable report rebuild checks."""

import importlib.util
import json
import math
import re
import shlex
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/power"
REBUILD = "PYTHONPATH=src uv run --locked python docs/reports/modules/power/build_report.py"
spec = importlib.util.spec_from_file_location("power_report", REPORT / "build_report.py")
builder = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = builder
spec.loader.exec_module(builder)


def read_utf8(path):
    """Normalize source line endings on every supported platform."""
    with path.open(encoding="utf-8", newline=None) as stream:
        return stream.read()


def calculation_data(html):
    return json.loads(re.search(
        r'<script id="calculation-data" type="application/json">(.*?)</script>', html, re.S,
    ).group(1))


class Numbers(HTMLParser):
    """Collect displayed numerical bindings rather than CPU-specific float text."""

    def __init__(self):
        super().__init__()
        self.values = []
        self.active = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "span" and "data-value" in attrs:
            self.active = (attrs["data-value"], int(attrs["data-digits"]))

    def handle_data(self, data):
        if self.active:
            self.values.append((*self.active, data))

    def handle_endtag(self, tag):
        if tag == "span":
            self.active = None


def test_hand_anchors():
    result = builder.hand_case()
    # V1=1+j0, I1=(1-(.96+j.28))/(j*.5)=-.56-j.08;
    # S1=V1*conj(I1)=-.56+j.08, hence Q1=.08 pu. Absolute tol 1e-8 pu.
    assert result.bus_results[0]["q_net_pu"] == pytest.approx(0.08, rel=0, abs=1e-8)
    # I2=2.16-j.16; S2=(.96+j.28)*(2.16+j.16);
    # Q2=.96*.16+.28*2.16=.1536+.6048=.7584 pu. Absolute tol 1e-8 pu.
    assert result.bus_results[1]["q_net_pu"] == pytest.approx(0.7584, rel=0, abs=1e-8)
    # |V3|=sqrt(.90**2+(-.12)**2)=sqrt(.8244)=.9079647570 pu;
    # the literal is rounded to ten decimals; absolute tolerance 1e-8 pu.
    assert result.bus_results[2]["v_pu"] == pytest.approx(0.9079647570, rel=0, abs=1e-8)


def test_public_case_mapping_and_headline():
    case = builder.read_case()
    net = builder.public_network(case)
    # Bus 4 generation minus demand: (318-80)/100=238/100=2.38 pu.
    assert net.buses[3].p_gen_pu - net.buses[3].p_load_pu == pytest.approx(2.38)
    assert net.buses[3].voltage_pu == 1.02  # generator setpoint overrides bus initial guess
    assert all(branch.tap_ratio == 1 for branch in net.branches)
    record = builder.calculate()
    # Headline Qg is recomputed from displayed bus injection and the public load/base.
    q_net = float(builder.format_number(record["public.bus.3.q_net_pu"], 6))
    qg = q_net * case["base_mva"] + case["buses"][3]["q_mvar"]
    assert qg == pytest.approx(record["public.qg4_mvar"], rel=0, abs=0.000051)
    q_reference = float(builder.format_number(record["public.bus.0.q_net_pu"], 6))
    assert q_reference * case["base_mva"] + case["buses"][0]["q_mvar"] == pytest.approx(
        record["public.qg1_mvar"], rel=0, abs=0.000051,
    )
    assert record["public.qg4_mvar"] > case["generators"][0]["q_max_mvar"]
    assert record["public.qg1_mvar"] > case["generators"][1]["q_max_mvar"]
    assert record["public.converged"] is True


@pytest.mark.parametrize(("key", "expected", "tolerance"), [
    ("public.qg4_mvar", 181.4296, 0.00005),
    ("public.qg1_mvar", 114.5008, 0.00005),
    ("public.total_loss_mw", 4.8091, 0.00005),
    ("public.bus.1.v_pu", 0.982421, 0.0000005),
    ("public.bus.2.v_pu", 0.969005, 0.0000005),
])
def test_four_bus_independent_literals(key, expected, tolerance):
    # Independent pi-model Newton calculation, cross-checked by Gauss-Seidel,
    # with inputs typed from MATPOWER 8.1 case4gs. These literals are prescribed
    # reference results, never obtained by calling the builder or solver here.
    assert builder.calculate()[key] == pytest.approx(expected, rel=0, abs=tolerance)


@pytest.mark.parametrize("violates", [False, True])
def test_limit_dispositions_follow_inputs(monkeypatch, tmp_path, violates):
    case = builder.read_case()
    if violates:
        for bus in case["buses"]:
            bus["v_min"] = 1.03
        for branch in case["branches"]:
            branch["rating_mva"] = 1.0
    monkeypatch.setattr(builder, "read_case", lambda: case)
    target = tmp_path / "report.html"
    record = builder.build(target)
    assert record["public.voltage_bounds_satisfied"] is (not violates)
    assert record["public.branch_ratings_satisfied"] is (not violates)
    assert record["public.voltage_bounds_satisfied"] is all(
        row["v_min"] <= record[f"public.bus.{i}.v_pu"] <= row["v_max"]
        for i, row in enumerate(case["buses"])
    )
    assert record["public.branch_ratings_satisfied"] is all(
        max(record[f"public.branch.{i}.sending_mva"],
            record[f"public.branch.{i}.receiving_mva"]) <= row["rating_mva"]
        for i, row in enumerate(case["branches"])
    )
    html = read_utf8(target)
    assert ("Voltage magnitudes violate the supplied case limits" in html) is violates
    assert ("These circuit values exceed at least one supplied rating" in html) is violates
    assert ("The plotted bounds are satisfied" in html) is (not violates)


def test_reference_voltage_uses_generator_setpoint(monkeypatch, tmp_path):
    case = builder.read_case()
    case["buses"][0]["v_initial"] = 0.93
    case["generators"][1]["v_set"] = 1.04
    monkeypatch.setattr(builder, "read_case", lambda: case)
    assert builder.public_network(case).buses[0].voltage_pu == 1.04
    target = tmp_path / "report.html"
    builder.build(target)
    parser = Numbers()
    parser.feed(read_utf8(target))
    assert sum(key == "input.generators.1.v_set" for key, _, _ in parser.values) >= 2
    assert not any(key == "input.buses.0.v_initial" for key, _, _ in parser.values)


def test_rating_equality_uses_inclusive_criterion(monkeypatch, tmp_path):
    case = builder.read_case()
    record = builder.calculate()
    for i, branch in enumerate(case["branches"]):
        branch["rating_mva"] = max(record[f"public.branch.{i}.sending_mva"],
                                   record[f"public.branch.{i}.receiving_mva"])
    monkeypatch.setattr(builder, "read_case", lambda: case)
    target = tmp_path / "report.html"
    assert builder.build(target)["public.branch_ratings_satisfied"] is True
    assert "These circuit values do not exceed the supplied ratings" in read_utf8(target)


@pytest.mark.parametrize("bus_count", [3, 4])
def test_nonconverged_report_is_not_written(monkeypatch, tmp_path, bus_count):
    from types import SimpleNamespace

    solve = builder.solve_power_flow

    def fail_selected(network, **kwargs):
        if len(network.buses) == bus_count:
            return SimpleNamespace(converged=False)
        return solve(network, **kwargs)

    monkeypatch.setattr(builder, "solve_power_flow", fail_selected)
    target = tmp_path / "report.html"
    with pytest.raises(ValueError, match="has not converged"):
        builder.build(target)
    assert not target.exists()


def test_inadequate_display_precision_is_rejected():
    with pytest.raises(ValueError, match="needs more displayed digits"):
        builder.format_number(0.004, 2)
    assert builder.format_number(-0.0, 6) == "0.000000"
    assert builder.format_number(-1e-14, 6) == "−1.0e−14"


def test_iteration_count_and_reference_corrections():
    record = builder.calculate()
    assert record["public.iterations"] == 5
    assert record["public.newton_updates"] == 4
    html = read_utf8(REPORT / "report.html")
    assert "mismatch evaluations" in html and "Newton updates" in html
    assert "voltage demand" not in html
    assert "voltage magnitude" in html
    assert "Network Equations" in html
    assert "(4.5.8)–(4.5.10)" in html and "(4.5.11)" not in html
    assert "John Grainger" in html and "William Stevenson" in html and "McGraw-Hill" in html
    assert "conditional on the supplied case limits" in html
    assert "textbook basis for these limits is not established" in html


def test_power_report_is_owned_and_collected_by_ci():
    import yaml

    gate = yaml.safe_load(read_utf8(ROOT / ".claude/quality-gates.yaml"))["gates"]["tests-specialized"]
    assert gate["enabled"] is True
    assert "tests/power/" in gate["test_roots"]
    command = shlex.split(gate["command"])
    assert "tests/power/" in command
    assert not any(arg.startswith("--ignore") and "power" in arg for arg in command)
    path = ROOT / "scripts/ci/detect_touched_domains.py"
    spec = importlib.util.spec_from_file_location("power_ci_detector", path)
    detector = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = detector
    spec.loader.exec_module(detector)
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    selected = detector.touched_domains(["tests/power/analysis/test_module_report.py"], domains)
    assert "specialized" in {domain.name for domain in selected}


def test_displayed_number_consistency():
    """Consistency check: every numerical binding equals a fresh module call."""
    record = builder.calculate()
    parser = Numbers()
    parser.feed(read_utf8(REPORT / "report.html"))
    assert len(parser.values) > 60
    for key, digits, displayed in parser.values:
        assert displayed == builder.format_number(record[key], digits), f"{key}: {REBUILD}"


def test_rebuild(tmp_path):
    destination = tmp_path / "report.html"
    builder.build(destination)
    committed = read_utf8(REPORT / "report.html")
    rebuilt = read_utf8(destination)
    # Keep narrative and displayed precision exact, compare the JSON numerically.
    pattern = r'(<script id="calculation-data" type="application/json">).*?(</script>)'
    assert re.sub(pattern, r'\1\2', committed, flags=re.S) == re.sub(
        pattern, r'\1\2', rebuilt, flags=re.S,
    ), f"Report is stale. Rebuild with: {REBUILD}"
    expected, actual = calculation_data(committed), calculation_data(rebuilt)
    assert expected.keys() == actual.keys()
    for key, value in expected.items():
        if isinstance(value, float) and key != "tol":
            assert actual[key] == pytest.approx(value, rel=0, abs=1e-6), key
        else:
            assert actual[key] == value, key
    for value in actual.values():
        if isinstance(value, float) and value == 0:
            assert math.copysign(1.0, value) == 1.0
    assert b"\r" not in destination.read_bytes()
    changes = json.loads(read_utf8(REPORT / "changes.json"))
    assert changes["revision"] == "2"
    assert "<td>1</td><td>2026-10-21</td>" in rebuilt
    assert "<td>2</td><td>2026-10-22</td>" in rebuilt


def test_crlf_templates_generate_lf_output(monkeypatch, tmp_path):
    for relative in ["docs/api/_assets/brand.css", "docs/api/cfd/turbulent-flat-plate-verification.html"]:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        with target.open("w", encoding="utf-8", newline="\r\n") as stream:
            stream.write(read_utf8(ROOT / relative))
    monkeypatch.setattr(builder, "ROOT", tmp_path)
    destination = tmp_path / "report.html"
    builder.build(destination)
    assert b"\r" not in destination.read_bytes()
    assert read_utf8(destination) == read_utf8(REPORT / "report.html")


def test_report_contract():
    html = read_utf8(REPORT / "report.html")
    assert '<body class="hs-doc">' in html
    assert len(re.findall(r'<section id="s\d+">', html)) == 10
    assert "Draft for maintainer review" in html
    assert "showSaveFilePicker" in html and "power.comments.json" in html
    assert 'id="changes-data"' in html
    assert "data-case" in html and "<title>" in html
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)="https?://', html)
    for ref in re.findall(r'href="#(ref-[^"]+)"', html):
        assert f'id="{ref}"' in html
    ids = re.findall(r'\bid="([^"]+)"', html)
    assert len(ids) == len(set(ids))
    embedded = json.loads(re.search(
        r'<script id="calculation-data" type="application/json">(.*?)</script>', html, re.S,
    ).group(1))
    assert embedded["tol"] == builder.TOL


def test_offline_interactions():
    """Exercise chart, change panel and sidecar save/load without a browser."""
    import shutil
    import subprocess

    node = shutil.which("node")
    if not node:
        pytest.skip("Node is unavailable; offline interaction check requires Node.")
    html = read_utf8(REPORT / "report.html")
    script = re.findall(r"<script>(.*?)</script>", html, re.S)[0]
    changes = re.search(r'<script id="changes-data" type="application/json">(.*?)</script>', html, re.S).group(1)
    harness = r'''
const vm = require('node:vm');
const assert = require('node:assert/strict');
let saved = null;
function start() {
  const nodes = {};
  function element() {
    return {events: {}, children: [], value: '', textContent: '', style: {},
      addEventListener(event, callback) { this.events[event] = callback; },
      replaceChildren() { this.children = []; },
      append(child) { this.children.push(child); }};
  }
  const get = id => nodes[id] ||= element();
  const point = element(); point.dataset = {case: 'Case; bus; demand; limit; pu'};
  const limit = element();
  get('changes-data').textContent = CHANGES;
  get('prior-revision').value = '0';
  const document = {getElementById: get, createElement: element,
    querySelectorAll(selector) { return selector === '[data-case]' ? [point] : [limit]; }};
  const window = {showDirectoryPicker: async () => ({
    getFileHandle: async name => {
      assert.equal(name, 'power.comments.json');
      return {createWritable: async () => ({write: async text => { saved = text; }, close: async () => {}})};
    }})};
  vm.runInNewContext(SCRIPT, {document, window, setTimeout});
  return {get, point, limit};
}
(async () => {
  const ui = start();
  ui.point.events.focus();
  assert.equal(ui.get('chart-detail').textContent, ui.point.dataset.case);
  ui.get('limits').events.click(); assert.equal(ui.limit.style.display, 'none');
  ui.get('limits').events.click(); assert.equal(ui.limit.style.display, '');
  assert.equal(ui.get('change-list').children.length, 3);
  ui.get('prior-revision').value = '2'; ui.get('prior-revision').events.change();
  assert.equal(ui.get('change-list').children[0].textContent, 'No later engineering changes.');
  ui.get('comment-section').value = 's5'; ui.get('comment-text').value = '<example> Review reactive limit.';
  ui.get('add-comment').events.click();
  await ui.get('save-comments').events.click();
  assert.ok(saved.endsWith(String.fromCharCode(10)));
  const parsed = JSON.parse(saved);
  assert.equal(parsed.comments[0].section, 's5');
  assert.equal(parsed.comments[0].text, '<example> Review reactive limit.');
  const other = start();
  await other.get('load-comments').events.change({target: {files: [{text: async () => saved}]}});
  assert.equal(other.get('comment-list').children[0].textContent, 's5: <example> Review reactive limit.');
  // A wrong revision is rejected before importing comments.
  const wrong = start(); parsed.revision = '99';
  await wrong.get('load-comments').events.change({target: {files: [{text: async () => JSON.stringify(parsed)}]}});
  assert.equal(wrong.get('comment-list').children.length, 0);
  assert.equal(wrong.get('comment-status').textContent, 'Invalid sidecar or revision.');
})().catch(error => { console.error(error.message); process.exitCode = 1; });
'''
    command = "const SCRIPT=" + json.dumps(script) + ";const CHANGES=" + json.dumps(changes) + ";" + harness
    result = subprocess.run([node, "-e", command], capture_output=True, text=True, encoding="utf-8", check=False)
    assert result.returncode == 0, result.stderr
