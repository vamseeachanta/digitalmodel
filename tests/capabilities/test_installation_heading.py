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
    assert data["head_seas_band_deg"] == [-15, 15]
    assert data["headings_deg"] == list(range(0, 181, 15))
    assert len(data["stages"]) == 4
    assert len(data["sizes"]) == 3
    assert data["illustrative"] is True
    assert "invented" in data["source"].lower()
    assert all(len(stage["limits_m"]) == 3 for stage in data["stages"])


@pytest.mark.parametrize("fault", ["heading", "band", "missing-size", "short", "zero", "negative", "boolean", "integer", "precision", "duplicate", "source"])
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
    elif fault in ("zero", "negative", "boolean", "integer", "precision"):
        series[0] = {"zero": 0.0, "negative": -1.0, "boolean": True, "integer": 1, "precision": 1.23}[fault]
    elif fault == "duplicate":
        bad["stages"].append(bad["stages"][0])
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


def test_two_builds_manifest_revision_and_self_containment(builder, tmp_path):
    first, second = tmp_path / "first", tmp_path / "second"
    builder.build(output_dir=first)
    builder.build(output_dir=second)
    for path in first.iterdir():
        assert path.read_bytes() == (second / path.name).read_bytes()
    page = first / builder.PAGE_NAME
    text = page.read_text()
    parser = Resources()
    parser.feed(text)
    assert parser.addresses == []
    assert "@import" not in text and "url(" not in text
    embedded = json.loads(re.search(r'<script id="limits-data" type="application/json">(.*?)</script>', text, re.S)[1])
    assert embedded == builder.load_data(builder.DATA)
    revisions = json.loads(builder.REVISIONS.read_text())
    assert revisions[-1]["revision"] == embedded["revision"]
    assert text.index('</header>') < text.index('id="revision-history"') < text.index('id="controls"')
    assert f'<td>{embedded["revision"]}</td>' in text
    manifest = json.loads((first / builder.MANIFEST_NAME).read_text())
    for relative, digest in manifest["sha256"].items():
        target = page if relative == builder.PAGE_REL else ROOT / relative
        assert hashlib.sha256(target.read_bytes()).hexdigest() == digest
    assert 'min="0" max="180" step="15"' in text
    assert 'id="play"' not in text and 'id="pause"' not in text
    assert 'id="theme"' in text
    assert 'head-seas sector ±15°' in text
    assert 'station-keeping tolerance' not in text
    assert 'ILLUSTRATIVE' in text and 'invented' in text
    assert 'aria-live="polite"' in text


def test_revision_mismatch_rejected(builder, tmp_path):
    revisions = tmp_path / "revisions.json"
    revisions.write_text('[{"revision":"wrong","date":"2026-10-15","change":"Initial issue"}]\n')
    with pytest.raises(ValueError, match="revision"):
        builder.build(output_dir=tmp_path / "out", revisions_path=revisions)


def test_real_data_swap_needs_only_json(builder, data, tmp_path):
    data["illustrative"] = False
    data["source"] = "Heading-resolved supplied results; demonstration replacement fixture."
    path = tmp_path / "limits.json"
    path.write_text(json.dumps(data))
    builder.build(data_path=path, output_dir=tmp_path / "out")
    text = (tmp_path / "out" / builder.PAGE_NAME).read_text()
    assert 'data-status="results"' in text
    assert '>ILLUSTRATIVE<' not in text
    assert data["source"] in text


def test_script_pure_functions(builder, tmp_path):
    """Execute the actual inline functions without a browser or DOM shim."""
    builder.build(output_dir=tmp_path)
    text = (tmp_path / builder.PAGE_NAME).read_text()
    script = re.search(r'<script id="chart-script">(.*?)</script>', text, re.S)[1]
    pure = script.split('// DOM wiring')[0]
    checks = r'''
const assert = require('node:assert/strict');
assert.equal(selectedLimit([3, 2, 1], 15), 2);
assert.deepEqual(polarPoint(0, 2, 1), [0, -2]);
assert.ok(Math.abs(polarPoint(90, 2, 1)[0] - 2) < 1e-10);
assert.equal(outsideBand(15), false);
assert.equal(outsideBand(30), true);
assert.equal(outsideBand(-15), false);
assert.equal(outsideBand(-30), true);
assert.equal(readout(2, 4), 'allowable Hs 2.0 m, 50.0 % of the head-seas limit');
'''
    subprocess.run(["node", "-e", pure + checks], check=True, env=os.environ.copy())
    assert 'setInterval' not in script and 'clearInterval' not in script
    assert 'nextHeading' not in script
    assert 'visibilitychange' not in script and 'reducedMotion' not in script
    assert 'autoplay' not in script
    assert 'addEventListener("input"' in script
    assert 'addEventListener("change"' in script


def test_head_seas_sector_preserves_supplied_sample_without_warning(builder, data, tmp_path):
    """A heading outside the fixed head-seas sector retains its point limit."""
    builder.build(output_dir=tmp_path)
    text = (tmp_path / builder.PAGE_NAME).read_text()
    script = re.search(r'<script id="chart-script">(.*?)</script>', text, re.S)[1]
    stage = next(item for item in data["stages"] if item["name"] == "In air")
    assert stage["limits_m"]["small"][1:4] == [3.6, 3.4, 3.1]
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
getNode('size').value = 'small'; getNode('heading').value = '30';
const document = {
  getElementById: getNode, createElementNS: () => new Node(),
  createElement: () => new Node(), createTextNode: text => text,
  documentElement: {dataset: {}}, addEventListener() {}
};
const window = {matchMedia: () => ({matches: false, addEventListener() {}})};
'''
    checks = r'''
assert.equal(getNode('readout').textContent,
  'allowable Hs 3.4 m, 94.4 % of the head-seas limit');
assert.equal(getNode('readout').className, '');
assert.equal(getNode('band-status').textContent, 'Outside the head-seas sector ±15°.');
'''
    subprocess.run(["node", "-e", "const DATA = " + json.dumps(data) + ";\n" +
                    setup + script + checks], check=True, env=os.environ.copy())
    assert '.readout.outside' not in text


def test_generator_registered(builder):
    from scripts import check_generated_html as checker
    entries = [entry for entry in checker.GENERATORS if entry.script.endswith('build_installation_heading_explorer.py')]
    assert len(entries) == 1
    assert entries[0].outputs == (builder.PAGE_REL,)
    assert checker.validate_registry(ROOT) == []
