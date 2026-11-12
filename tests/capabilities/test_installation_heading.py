# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Contract tests for the discrete installation heading explorer."""

import copy
import hashlib
import importlib
import json
import os
from pathlib import Path
import re
import subprocess
from html.parser import HTMLParser

import pytest

ROOT = Path(__file__).resolve().parents[2]
MODULE = "scripts.capabilities.build_installation_heading_explorer"


@pytest.fixture
def builder():
    return importlib.import_module(MODULE)


@pytest.fixture
def data(builder):
    return builder.load_data(builder.DATA)


def test_data_covers_every_stage_size_heading(builder, data):
    builder.validate_data(data)
    # The synthetic revision-2 fixture has four stages and three sizes.
    assert data["head_seas_band_deg"] == [-15, 15]
    # Issue #2696 (2026), Deliverable: signed 15-degree grid and tolerance.
    assert data["headings_deg"] == list(range(-180, 181, 15))
    assert len(data["stages"]) == 4
    assert len(data["sizes"]) == 3
    assert data["illustrative"] is True
    assert "invented" in data["source"].lower()
    assert all(len(stage["limits_m"]) == 3 for stage in data["stages"])
    for stage in data["stages"]:
        for values in stage["limits_m"].values():
            assert values[0] == values[-1]


@pytest.mark.parametrize("fault", ["heading", "band", "missing-size", "short", "negative", "boolean", "integer", "precision", "duplicate", "source", "condition", "real", "no-samples", "endpoint-mismatch"])
def test_invalid_data_rejected(builder, data, fault):
    bad = copy.deepcopy(data)
    series = bad["stages"][0]["limits_m"]["small"]
    if fault == "heading":
        bad["headings_deg"][1] = 14
    elif fault == "band":
        bad["head_seas_band_deg"] = [-10, 10]
    elif fault == "missing-size":
        del bad["stages"][0]["limits_m"]["small"]
    elif fault == "short":
        series.pop()
    elif fault in ("negative", "boolean", "integer", "precision"):
        series[0] = {"negative": -1.0, "boolean": True, "integer": 1, "precision": 1.23}[fault]
    elif fault == "endpoint-mismatch":
        series[0] = round(series[-1] + 0.1, 1)
    elif fault == "duplicate":
        bad["stages"].append(bad["stages"][0])
    elif fault == "condition":
        bad["condition_basis"] = ""
    elif fault == "real":
        bad["illustrative"] = False
    elif fault == "no-samples":
        series[:] = [None] * len(series)
    else:
        bad["source"] = ""
    with pytest.raises(ValueError):
        builder.validate_data(bad)


def test_json_lexical_precision_rejected(builder, tmp_path):
    path = tmp_path / "bad.json"
    path.write_text('{"number": 1.00}\n')
    with pytest.raises(ValueError, match="one decimal"):
        builder.load_data(path)


class Resources(HTMLParser):
    def __init__(self):
        super().__init__()
        self.addresses = []

    def handle_starttag(self, tag, attrs):
        self.addresses.extend(value for key, value in attrs if key in ("src", "href"))


@pytest.mark.parametrize("checkout_crlf", [False, True])
def test_two_builds_manifest_revision_and_self_containment(builder, tmp_path, monkeypatch, checkout_crlf):
    read_text, read_bytes = Path.read_text, Path.read_bytes

    def legacy_read_text(path, *args, **kwargs):
        kwargs.setdefault("encoding", "cp1252")
        return read_text(path, *args, **kwargs)

    def checkout_bytes(path):
        raw = read_bytes(path)
        if checkout_crlf and path == ROOT / builder.PAGE_REL:
            return raw.replace(b"\r\n", b"\n").replace(b"\n", b"\r\n")
        return raw

    monkeypatch.setattr(Path, "read_text", legacy_read_text)
    monkeypatch.setattr(Path, "read_bytes", checkout_bytes)
    first, second = tmp_path / "first", tmp_path / "second"
    builder.build(output_dir=first)
    builder.build(output_dir=second)
    for path in first.iterdir():
        assert path.read_bytes() == (second / path.name).read_bytes()
    page = first / builder.PAGE_NAME
    text = page.read_text(encoding="utf-8")
    # Checkout newline conversion must not invalidate generated content equality.
    assert page.read_bytes() == (ROOT / builder.PAGE_REL).read_bytes().replace(b"\r\n", b"\n")
    parser = Resources()
    parser.feed(text)
    assert parser.addresses == []
    assert "@import" not in text and "url(" not in text
    embedded = json.loads(re.search(r'<script id="limits-data" type="application/json">(.*?)</script>', text, re.S)[1])
    assert embedded == builder.load_data(builder.DATA)
    assert embedded["condition_basis"] in text
    assert "fetch(" not in text and "XMLHttpRequest" not in text
    revisions = json.loads(builder.REVISIONS.read_text(encoding="utf-8"))
    assert revisions[-1]["revision"] == embedded["revision"]
    assert text.index('</header>') < text.index('id="revision-history"') < text.index('id="controls"')
    assert f'<td>{embedded["revision"]}</td>' in text
    manifest = json.loads((first / builder.MANIFEST_NAME).read_text(encoding="utf-8"))
    # Revision-2 synthetic fixture: 4 stages × 3 sizes × 14 supplied headings, including both endpoint labels.
    assert manifest["records"] == 168
    for relative, digest in manifest["sha256"].items():
        target = page if relative == builder.PAGE_REL else ROOT / relative
        assert hashlib.sha256(target.read_bytes()).hexdigest() == digest
    assert 'min="-180" max="180" step="15"' in text
    assert 'id="play"' not in text and 'id="pause"' not in text
    assert 'id="theme"' in text
    assert 'station-keeping tolerance ±15°' in text
    assert 'illustrative until heading-resolved results land' in text
    assert 'Status: <strong>illustrative until heading-resolved results land</strong>.' in text
    assert 'Negative headings: port' in text
    assert 'The left half mirrors' not in text
    assert 'ILLUSTRATIVE' in text and 'invented' in text
    assert 'aria-live="polite"' in text


def test_revision_mismatch_rejected(builder, tmp_path):
    revisions = tmp_path / "revisions.json"
    revisions.write_text('[{"revision":"wrong","date":"2026-10-15","change":"Initial issue"}]\n')
    with pytest.raises(ValueError, match="revision"):
        builder.build(output_dir=tmp_path / "out", revisions_path=revisions)


def test_real_data_replacement_is_deferred(builder, data, tmp_path):
    """Unreviewed real limits cannot remove the illustrative caveat in this slice."""
    data["illustrative"] = False
    path = tmp_path / "limits.json"
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match="illustrative"):
        builder.build(data_path=path, output_dir=tmp_path / "out")


def test_script_pure_functions(builder, tmp_path):
    """Use exact lookup and hand calculations written beside the JS assertions.

    Geometry tolerances cover floating-point trig error only, not an
    engineering acceptance limit. Issue #2696 (2026) supplies the band angles.
    """
    builder.build(output_dir=tmp_path)
    text = (tmp_path / builder.PAGE_NAME).read_text(encoding="utf-8")
    script = re.search(r'<script id="chart-script">(.*?)</script>', text, re.S)[1]
    pure = script.split('// DOM wiring')[0]
    checks = r'''
const assert = require('node:assert/strict');
// Hand calculations: exact signed lookup; missing samples never interpolate.
assert.equal(selectedLimit([-15, 0, 15], [1, 4, 2], -15), 1);
assert.equal(selectedLimit([-15, 0, 15], [1, 4, 2], 15), 2);
assert.equal(selectedLimit([-15, 0, 15], [1, 4, 2], 7), null);
assert.equal(selectedLimit([-15, 0, 15], [null, 4, 2], -15), null);
assert.deepEqual(sampleSegments([-30, -15, 0, 15, 30], [1, 2, null, 4, 5]),
  [[[-30, 1], [-15, 2]], [[15, 4], [30, 5]]]);
// OpenStax Precalculus 2e (2021), §8.3: sin(±90°)=±1, cos(±90°)=0.
assert.equal(polarPoint(-90, 2, 1)[0], -2);
assert.deepEqual(polarPoint(0, 2, 1), [0, -2]);
assert.ok(Math.abs(polarPoint(90, 2, 1)[0] - 2) < 1e-10);
assert.equal(outsideBand(15), false);
assert.equal(outsideBand(30), true);
assert.equal(outsideBand(-15), false);
assert.equal(outsideBand(-30), true);
// Hand calculation: 100 × 2 / 4 = 50%; OpenStax Prealgebra 2e (2020), §6.1.
assert.equal(readout(2, 4), 'allowable Hs 2.0 m, 50.0 % of the head-seas limit');
assert.equal(readout(null, 4), 'allowable Hs: no data; head-seas percentage unavailable.');
for (const baseline of [null, 0]) {
  assert.equal(readout(2, baseline), 'allowable Hs 2.0 m, head-seas percentage unavailable.');
}
'''
    subprocess.run(["node", "-e", pure + checks], check=True, env=os.environ.copy())
    assert 'setInterval' not in script and 'clearInterval' not in script
    assert 'nextHeading' not in script
    assert 'visibilitychange' not in script and 'reducedMotion' not in script
    assert 'autoplay' not in script
    assert 'addEventListener("input"' in script
    assert 'addEventListener("change"' in script


@pytest.mark.parametrize("heading, hs, status", [
    (-180, 3.2, "Outside"), (-45, None, "Outside"), (-30, 1.8, "Outside"), (-15, 1.8, "Within"), (0, 3.6, "Within"),
    (15, 3.6, "Within"), (30, 3.4, "Outside"), (180, 3.2, "Outside"),
])
def test_signed_selection_and_missing_data(builder, data, tmp_path, heading, hs, status):
    """Synthetic revision-2 positive values are retained, never mirrored.

    The explicitly supplied port fixture is 1.8 m at -30 and -15 degrees. Its percentage
    is independently 100 × 1.8 / 3.6 = 50.0. At 30 degrees, 100 × 3.4 / 3.6
    rounds to 94.4. These are arithmetic checks, not engineering limits.
    The inclusive ±15-degree band is the premise in issue #2696 (2026).
    """
    stage = next(item for item in data["stages"] if item["name"] == "In air")
    for angle in (-30, -15):
        stage["limits_m"]["small"][data["headings_deg"].index(angle)] = 1.8
    path = tmp_path / "limits.json"
    path.write_text(json.dumps(data))
    builder.build(data_path=path, output_dir=tmp_path)
    text = (tmp_path / builder.PAGE_NAME).read_text(encoding="utf-8")
    script = re.search(r'<script id="chart-script">(.*?)</script>', text, re.S)[1]
    # Only the DOM surface used by the shipped script; no browser is started.
    setup = r'''
const assert = require('node:assert/strict');
class Node {
  constructor(value = '') {
    this.value = value; this.textContent = ''; this.attributes = {};
    this.style = {}; this.className = ''; this.children = [];
    this.classList = {toggle: (name, enabled) => {
      this.className = enabled ? name : '';
    }};
  }
  append(...children) { this.children.push(...children); }
  replaceChildren(...children) { this.children = children; }
  setAttribute(name, value) { this.attributes[name] = String(value); }
  addEventListener() {}
}
const nodes = new Map();
const getNode = id => {
  if (!nodes.has(id)) nodes.set(id, new Node());
  return nodes.get(id);
};
getNode('limits-data').textContent = JSON.stringify(DATA);
getNode('stage').value = String(DATA.stages.findIndex(stage => stage.name === 'In air'));
getNode('size').value = 'small'; getNode('heading').value = String(HEADING);
const document = {
  getElementById: getNode, createElementNS: () => new Node(),
  createElement: () => new Node(), createTextNode: text => text,
  documentElement: {dataset: {}}, addEventListener() {}
};
const window = {matchMedia: () => ({matches: false, addEventListener() {}})};
'''
    expected = ("allowable Hs: no data; head-seas percentage unavailable." if hs is None
                else f"allowable Hs {hs:.1f} m, {100 * hs / 3.6:.1f} % of the head-seas limit")
    checks = f"""
assert.equal(getNode('readout').textContent, {json.dumps(expected)});
assert.equal(getNode('readout').className, {'"outside"' if status == 'Outside' else '""'});
assert.equal(getNode('band-status').textContent,
  '{status} the station-keeping tolerance ±15°. No operational approval is implied.');
assert.equal(getNode('dot').attributes.visibility, {'"hidden"' if hs is None else '"visible"'});
assert.ok(getNode('coverage').textContent.includes('In air · small'));
// The shaded sector starts left of zero and ends right, both above the origin.
const sector = getNode('head-seas-sector').attributes.d;
const endpoints = sector.match(/^M 0,0 L(-[0-9.]+),(-[0-9.]+) A 210,210 0 0 1 ([0-9.]+),(-[0-9.]+) Z$/);
assert.ok(endpoints);
assert.equal(Number(endpoints[1]), -Number(endpoints[3]));
assert.equal(Number(endpoints[2]), Number(endpoints[4]));
assert.ok(getNode('samples').children.some(row => row.children.some(cell => cell.textContent === 'no data')));
// Render each unavailable baseline with a supplied off-bow limit.
getNode('heading').value = '30';
const baselineIndex = DATA.headings_deg.indexOf(0);
const selectedValues = data.stages[Number(getNode('stage').value)].limits_m.small;
const originalBaseline = selectedValues[baselineIndex];
for (const baseline of [null, 0]) {{
  selectedValues[baselineIndex] = baseline;
  render();
  assert.equal(getNode('readout').textContent,
    'allowable Hs 3.4 m, head-seas percentage unavailable.');
}}
selectedValues[baselineIndex] = originalBaseline;
// A different stage must drive the readout, not just its label.
getNode('stage').value = String(DATA.stages.findIndex(stage => stage.name === 'Splash zone'));
getNode('heading').value = '0'; render();
assert.equal(getNode('readout').textContent, 'allowable Hs 2.4 m, 100.0 % of the head-seas limit');
getNode('size').value = 'large'; render();
assert.equal(getNode('readout').textContent, 'allowable Hs 1.6 m, 100.0 % of the head-seas limit');
"""
    subprocess.run(["node", "-e", "const DATA = " + json.dumps(data) + ";\n" +
                    f"const HEADING = {heading};\n" + setup + script + checks],
                   check=True, env=os.environ.copy())
    assert '.readout.outside' in text


def test_missing_and_zero_baselines_and_partial_coverage(builder, data):
    """Issue #2696 permits unavailable baselines; no synthetic fill is inferred."""
    for baseline in (None, 0.0):
        values = data["stages"][0]["limits_m"]["small"]
        values[data["headings_deg"].index(0)] = baseline
        values[0] = values[-1] = None
        builder.validate_data(data)
        assert values[data["headings_deg"].index(-30)] is None


def test_generator_registered(builder):
    from scripts import check_generated_html as checker
    entries = [entry for entry in checker.GENERATORS if entry.script.endswith('build_installation_heading_explorer.py')]
    assert len(entries) == 1
    assert entries[0].outputs == (builder.PAGE_REL,)
    assert checker.validate_registry(ROOT) == []
