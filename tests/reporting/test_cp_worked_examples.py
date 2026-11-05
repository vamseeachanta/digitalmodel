# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Public worked examples retain their calculated verdicts and state a way forward."""

import copy
import hashlib
import importlib
import json
import re
from html.parser import HTMLParser
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
REPORTS = ROOT / "docs/domains/cathodic_protection/reports"
PAGES = (
    "reference-jacket/cathodic-protection-analysis.html",
    "cases/S02/report.html",
    "cases/S03/report.html",
    "cases/S06/report.html",
)


class VisibleText(HTMLParser):
    def __init__(self):
        super().__init__()
        self.hidden = 0
        self.parts = []

    def handle_starttag(self, tag, attrs):
        if tag in {"script", "style"}:
            self.hidden += 1

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.hidden -= 1

    def handle_data(self, data):
        if not self.hidden:
            self.parts.append(data)


def visible(page):
    parser = VisibleText()
    parser.feed(page)
    return " ".join(" ".join(parser.parts).split())


@pytest.mark.parametrize("path", PAGES)
def test_public_report_identifies_its_worked_example_basis(path):
    text = visible((REPORTS / path).read_text()).lower()
    assert "worked example" in text
    assert ("published geometry" if path.startswith("reference") else "synthetic data") in text
    assert "not an installation design" in text


@pytest.mark.parametrize("path", PAGES)
def test_public_report_has_no_internal_draft_or_client_use_language(path):
    text = visible((REPORTS / path).read_text()).lower()
    for phrase in ("internal review", "internal draft", "draft for review", "client use",
                   "client-use", "client issuance", "not a client issue"):
        assert phrase not in text


@pytest.mark.parametrize("case,count", [("S02", 48), ("S03", 64)])
def test_failed_case_has_a_sourced_design_recommendation(case, count):
    page = (REPORTS / "cases" / case / "report.html").read_text()
    section = re.search(r'<section id="review-8">(.*?)</section>', page, re.S)
    assert section is not None
    text = visible(section.group())
    assert f"{count} anodes" in text
    assert "final current" in text.lower()
    assert "DNV-RP-B401" in text
    assert "not a retrofit quantity" in text
    assert "dnv.com/energy/standards-guidelines/dnv-rp-b401-cathodic-protection-design/" in page


@pytest.fixture
def builder(monkeypatch):
    monkeypatch.syspath_prepend(str(ROOT / "scripts/reporting"))
    return importlib.import_module("build_cp_worked_examples")


@pytest.mark.parametrize("case", ["S02", "S03", "S06"])
def test_case_rebuild_is_deterministic_and_preserves_evidence(builder, tmp_path, case):
    source = REPORTS / "cases" / case
    first, second = tmp_path / "first", tmp_path / "second"
    builder.rebuild_case(source, first, case)
    builder.rebuild_case(source, second, case)
    for path in first.iterdir():
        assert path.read_bytes() == (second / path.name).read_bytes()
    for name in ("input.yml", "results.json", "source-citations.json", "report_citations.json",
                 "release.json", "verification.json"):
        assert (source / name).read_bytes() == (first / name).read_bytes()
    assert (first / "report.html").read_bytes() == (source / "report.html").read_bytes()
    assert json.loads((first / "results.json").read_text())["status"]["result"] == (
        "PASS" if case == "S06" else "FAIL"
    )
    manifest = json.loads((first / "report_manifest.json").read_text())
    assert manifest["document"]["revision"] == "01"
    assert manifest["presentation"]["results_reproduced"] is True


def test_rebuild_rejects_results_that_do_not_reproduce(builder, monkeypatch, tmp_path):
    original = builder.run_cathodic_protection

    def changed(cfg):
        routed = original(cfg)
        routed["results"]["status"]["result"] = "PASS"
        return routed

    monkeypatch.setattr(builder, "run_cathodic_protection", changed)
    with pytest.raises(ValueError, match="recalculated results differ"):
        builder.rebuild_case(REPORTS / "cases/S02", tmp_path / "output", "S02")
    assert not (tmp_path / "output").exists()


def test_public_presentation_preserves_all_numerical_blocks(builder):
    import yaml
    from digitalmodel.reporting.spec import FigureBlock, StatusBlock, TableBlock

    cfg = yaml.safe_load((REPORTS / "cases/S02/input.yml").read_text())
    routed = builder.run_cathodic_protection(cfg)
    source = builder.anode_design_report(routed)
    before = copy.deepcopy(source.model_dump())
    public = builder.public_spec(source, "S02", routed["results"])
    assert source.model_dump() == before
    for old, new in zip(source.sections, public.appendices):
        for original, rendered in zip(old.blocks, new.blocks):
            if isinstance(original, (TableBlock, FigureBlock)):
                assert original.model_dump() == rendered.model_dump()
            if isinstance(original, StatusBlock):
                assert original.status == rendered.status
                assert original.governing_case == rendered.governing_case


def test_reference_refresh_is_idempotent_and_retains_numerical_evidence(builder, tmp_path):
    source = REPORTS / "reference-jacket"
    first, second = tmp_path / "first", tmp_path / "second"
    builder.refresh_reference(source, first)
    builder.refresh_reference(first, second)
    for path in first.iterdir():
        assert path.read_bytes() == (second / path.name).read_bytes() == (source / path.name).read_bytes()
    sidecar = json.loads((first / "cathodic-protection-analysis.citations.json").read_text())
    assert sidecar["report_sha256"] == hashlib.sha256(
        (first / "cathodic-protection-analysis.html").read_bytes()
    ).hexdigest()


def test_rebuild_requires_released_synthetic_inputs(builder, monkeypatch, tmp_path):
    monkeypatch.setattr(builder, "load_fixture", lambda name: {"inputs": {}})
    with pytest.raises(ValueError, match="released synthetic fixture"):
        builder.rebuild_case(REPORTS / "cases/S02", tmp_path / "output", "S02")
