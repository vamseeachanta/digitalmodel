# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reproduction and engineering-basis checks for the reference-jacket analysis."""

import importlib.util
import json
import re
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reporting/build_reference_jacket_analysis.py"


@pytest.fixture
def builder(tmp_path, monkeypatch):
    # Synthetic metadata exercises the real resolver without copying a standard.
    from digitalmodel.cathodic_protection.b401_tables import B401_WIKI_PATH_2021

    page = tmp_path / B401_WIKI_PATH_2021
    page.parent.mkdir(parents=True)
    page.write_text('---\ncode_id: dnv-rp-b401\npublisher: DNV\nrevision: "2021-05"\n---\n')
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    spec = importlib.util.spec_from_file_location("reference_analysis_builder", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_ten_sections_and_seven_design_data_items(builder):
    spec, evidence = builder.build_document()
    assert [s.title for s in spec.sections] == list(builder.SECTION_TITLES)
    design = spec.sections[2]
    titles = [b.markdown.splitlines()[0] for b in design.blocks if b.kind == "text"]
    assert [t[:6] for t in titles] == [f"## 3.{i}" for i in range(1, 8)]
    assert evidence["arrangements"]["stated"]["anode_count"] == 252
    assert evidence["status"] == "DRAFT: input qualification required"


def test_report_reproduces_and_revision_follows_summary(builder, tmp_path):
    first = builder.build(tmp_path / "first")
    second = builder.build(tmp_path / "second")
    assert {p.name: p.read_bytes() for p in first} == {
        p.name: p.read_bytes() for p in second
    }
    page = first[0].read_text()
    assert page.index("Executive Summary</h2>") < page.index("Revision history")
    assert page.index("Revision history") < page.index("Introduction</h2>")
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)=["\']https?://', page)
    assert "assumed, based on previous experience" in page
    assert "3.4 Vessel data" in page and "Not applicable" in page
    assert "criterion" in page and "way forward" in page.lower()


def test_native_comparisons_use_unrounded_values(builder):
    _, evidence = builder.build_document()
    for arrangement in evidence["arrangements"].values():
        for check in arrangement["checks"]:
            assert check["passes"] == (check["provided"] >= check["required"])
            assert check["margin_percent"] == pytest.approx(
                100 * (check["provided"] / check["required"] - 1)
            )
    assert any(s["changes_conclusion"] for s in evidence["sensitivity"])


def test_sidecar_has_schema_fields_and_no_standard_tables(builder, tmp_path):
    paths = builder.build(tmp_path)
    sidecar = json.loads(paths[1].read_text())
    assert sidecar["citations"]
    assert all(c["source_sibling"] == "generic" for c in sidecar["citations"])
    assert all(c["revision"] == "2021-05" for c in sidecar["citations"])
    assert "current_density_A_m2" not in paths[1].read_text()
    assert sidecar["report_sha256"] == builder.digest(paths[0])


def test_citation_resolution_is_fail_closed(builder, tmp_path, monkeypatch):
    missing = tmp_path / "missing-wiki"
    missing.mkdir()
    monkeypatch.setenv("LLM_WIKI_PATH", str(missing))
    with pytest.raises(Exception, match="CitationResolutionError"):
        builder.build(tmp_path / "out")


def test_numbered_captions_follow_evidence(builder, tmp_path):
    page = builder.build(tmp_path)[0].read_text()
    assert page.index("</table>") < page.index("Table 1.1 – Revision history")
    assert "Figure 5.1 – Reference jacket elevation" in page
    assert 'id="reference-3"' in page
    assert 'href="#reference-3"' in page
    assert '<span class="secnum applabel">1</span><h2>Executive Summary</h2>' in page
