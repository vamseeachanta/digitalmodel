# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Qualitative workflow contract from issue #2695 (2026), deliverable 1.

No numerical engineering example is evaluated. The independent expected graph
is the issue's stated measure -> model -> quantify uncertainty -> decide ->
reinspect -> measure cycle. Missing evidence never establishes a decision.
"""

import copy
import hashlib
from html.parser import HTMLParser
import importlib
import json
from pathlib import Path
import re
from xml.etree import ElementTree

import pytest


@pytest.fixture
def builder():
    return importlib.import_module("scripts.capabilities.build_uncertainty_loop")


@pytest.fixture
def data(builder):
    return json.loads(builder.DATA.read_text(encoding="utf-8"))


class Page(HTMLParser):
    """Read the saved artifact without executing code or opening a browser."""

    def __init__(self, text):
        super().__init__()
        self.elements = []
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        self.elements.append((tag, dict(attrs)))


def test_cycle_evidence_and_unevaluated_decision(builder, data):
    """Expected order and four evidence categories are explicit issue inputs."""
    builder.validate_data(data)
    assert [step["id"] for step in data["steps"]] == [
        "measure", "model", "quantify", "decide", "reinspect"
    ]
    assert [row["id"] for row in data["evidence"]] == [
        "inspection", "survey", "metocean", "operations"
    ]
    assert data["status"] == "synthetic"
    assert data["decision_status"] == "NOT_EVALUATED"
    assert all(row["status"] == "NOT_EVALUATED" for row in data["evidence"])
    assert data["feedback"]["target"] == "model"


@pytest.mark.parametrize("fault", [
    "status", "decision", "missing-step", "duplicate-step", "step-order",
    "missing-evidence", "duplicate-evidence", "blank-unit", "blank-input",
    "blank-output", "blank-missing", "blank-criterion", "evidence-status",
    "feedback-target", "blank-feedback", "blank-rights", "blank-basis",
])
def test_incomplete_or_operational_claims_rejected(builder, data, fault):
    bad = copy.deepcopy(data)
    if fault == "status":
        bad["status"] = "measured"
    elif fault == "decision":
        bad["decision_status"] = "ACCEPTED"
    elif fault == "missing-step":
        bad["steps"].pop()
    elif fault == "duplicate-step":
        bad["steps"].append(bad["steps"][0])
    elif fault == "step-order":
        bad["steps"].reverse()
    elif fault == "missing-evidence":
        bad["evidence"].pop()
    elif fault == "duplicate-evidence":
        bad["evidence"].append(bad["evidence"][0])
    elif fault == "blank-unit":
        bad["evidence"][0]["unit"] = " "
    elif fault.startswith("blank-") and fault.split("-", 1)[1] in {
        "input", "output", "missing", "criterion"
    }:
        bad["steps"][0][fault.split("-", 1)[1]] = " "
    elif fault == "evidence-status":
        bad["evidence"][0]["status"] = "ACCEPTED"
    elif fault == "feedback-target":
        bad["feedback"]["target"] = "decide"
    elif fault == "blank-feedback":
        bad["feedback"]["action"] = " "
    else:
        bad["provenance"][fault.split("-", 1)[1]] = " "
    with pytest.raises(ValueError):
        builder.validate_data(bad)


def test_saved_page_and_manifest_are_reproducible(builder, tmp_path, data):
    first, second = tmp_path / "first", tmp_path / "second"
    builder.build(output_dir=first)
    builder.build(output_dir=second)
    for name in (builder.PAGE_NAME, builder.MANIFEST_NAME):
        assert (first / name).read_bytes() == (second / name).read_bytes()
        assert (first / name).read_bytes() == (builder.OUTPUT / name).read_bytes()
    text = (first / builder.PAGE_NAME).read_text(encoding="utf-8")
    embedded = re.search(
        r'<script id="workflow-data" type="application/json">(.*?)</script>',
        text, re.S,
    )[1]
    assert json.loads(embedded) == data
    manifest = json.loads((first / builder.MANIFEST_NAME).read_text(encoding="utf-8"))
    assert manifest["cycle"] == [
        ["measure", "model"], ["model", "quantify"], ["quantify", "decide"],
        ["decide", "reinspect"], ["reinspect", "measure"],
    ]
    assert manifest["feedback"] == {"source": "process-change", "target": "model"}
    assert manifest["decision_status"] == "NOT_EVALUATED"
    for relative, digest in manifest["sha256"].items():
        assert not Path(relative).is_absolute()
        assert hashlib.sha256((builder.ROOT / relative).read_bytes()).hexdigest() == digest


def test_offline_native_disclosures_and_visible_limitations(builder, tmp_path, data):
    builder.build(output_dir=tmp_path)
    text = (tmp_path / builder.PAGE_NAME).read_text(encoding="utf-8")
    page = Page(text)
    tags = page.elements
    assert [attrs["id"] for tag, attrs in tags if tag == "details"] == [
        "measure", "model", "quantify", "decide", "reinspect", "process-change"
    ]
    assert len([tag for tag, _ in tags if tag == "summary"]) == len(data["steps"]) + 1
    assert all(attrs.get("type") == "application/json"
               for tag, attrs in tags if tag == "script")
    assert not any(key in attrs for _, attrs in tags for key in ("src", "href"))
    assert not any(tag in {"iframe", "object", "embed"} for tag, _ in tags)
    assert "@import" not in text
    assert all(target == "#loop-arrow" for target in re.findall(r"url\((.*?)\)", text))
    assert "fetch(" not in text
    assert 'name="viewport"' in text
    assert "prefers-color-scheme: dark" in text
    assert "@media (max-width:" in text
    assert "summary:focus-visible" in text
    assert "<caption>" in text
    assert 'scope="col"' in text
    assert text.index("</header>") < text.index('id="revision-history"') < text.index('id="evidence"')
    for step in data["steps"]:
        for key in ("input", "output", "missing", "criterion"):
            assert step[key] in text
    for label in (
        "SYNTHETIC", "NOT_EVALUATED", "Return to Measure", "Reassess the historical trend",
        "Remaining life is not probability of failure", "No forecast is calculated",
    ):
        assert label in text


def test_custom_text_is_escaped_once(builder, data, tmp_path):
    """HTML and placeholder-like evidence remain inert text in the saved page."""
    data["steps"][0]["input"] = '</script><img src="invalid"> & @@STEPS@@'
    path = tmp_path / "data.json"
    path.write_text(json.dumps(data), encoding="utf-8")
    builder.build(data_path=path, output_dir=tmp_path / "out")
    text = (tmp_path / "out" / builder.PAGE_NAME).read_text(encoding="utf-8")
    assert '&lt;/script&gt;&lt;img src=&quot;invalid&quot;&gt; &amp; @@STEPS@@' in text
    assert not any(tag == "img" for tag, _ in Page(text).elements)
    embedded = re.search(
        r'<script id="workflow-data" type="application/json">(.*?)</script>',
        text, re.S,
    )[1]
    assert json.loads(embedded) == data


def test_svg_draws_cycle_and_process_change_feedback(builder, tmp_path):
    """A prose list cannot substitute for the requested directed diagram."""
    builder.build(output_dir=tmp_path)
    text = (tmp_path / builder.PAGE_NAME).read_text(encoding="utf-8")
    match = re.search(r"<svg\b.*?</svg>", text, re.S)
    assert match, "The workflow needs a drawn SVG cycle"
    svg = ElementTree.fromstring(match[0])
    ns = {"s": "http://www.w3.org/2000/svg"}
    assert svg.attrib["role"] == "img"
    for reference in svg.attrib["aria-labelledby"].split():
        alternative = svg.find(f".//*[@id='{reference}']", ns)
        assert alternative is not None and "".join(alternative.itertext()).strip()
    nodes = svg.findall("s:g[@data-node]", ns)
    assert [node.attrib["data-node"] for node in nodes] == [
        "measure", "model", "quantify", "decide", "reinspect"
    ]
    assert ["".join(node.itertext()).strip() for node in nodes] == [
        "Measure", "Model", "Quantify uncertainty", "Decide", "Reinspect"
    ]
    edges = svg.findall(".//s:path[@data-source]", ns)
    assert [(edge.attrib["data-source"], edge.attrib["data-target"]) for edge in edges] == [
        ("measure", "model"), ("model", "quantify"), ("quantify", "decide"),
        ("decide", "reinspect"), ("reinspect", "measure"), ("process-change", "model"),
    ]
    assert all(edge.attrib["d"] and edge.attrib["marker-end"] == "url(#loop-arrow)"
               for edge in edges)
    assert edges[-1].attrib["stroke-dasharray"] == "8 6"
    assert svg.find(".//s:marker[@id='loop-arrow']", ns) is not None


def test_freshness_gate_rebuilds_both_uncertainty_outputs(builder, tmp_path, monkeypatch):
    """The repository gate must own the page and digest manifest together."""
    from scripts import check_generated_html as checker

    entries = [entry for entry in checker.GENERATORS
               if entry.script == "scripts/capabilities/build_uncertainty_loop.py"]
    assert len(entries) == 1, "The uncertainty builder needs freshness registration"
    entry = entries[0]
    assert entry.outputs == (
        "docs/domains/asset-integrity/uncertainty-loop.html",
        "docs/domains/asset-integrity/uncertainty-loop.manifest.json",
    )
    assert entry.inputs == (
        "scripts/capabilities/uncertainty_loop/workflow.json",
        "scripts/capabilities/uncertainty_loop/page.html.template",
    )
    monkeypatch.setattr(checker, "GENERATORS", (entry,))
    checker.prepare_shadow(builder.ROOT, tmp_path)
    assert checker.regenerate(tmp_path) == []
    assert checker.compare_outputs(builder.ROOT, tmp_path) == []
    (tmp_path / entry.outputs[0]).write_text("stale", encoding="utf-8")
    (tmp_path / entry.outputs[1]).write_text("{}", encoding="utf-8")
    assert {path for path, _ in checker.compare_outputs(builder.ROOT, tmp_path)} == set(entry.outputs)
