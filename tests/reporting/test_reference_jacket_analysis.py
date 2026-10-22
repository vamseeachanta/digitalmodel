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
    assert evidence["status"] == "Issued for information"


def test_report_reproduces_and_revision_follows_summary(builder, tmp_path):
    first = builder.build(tmp_path / "first")
    second = builder.build(tmp_path / "second")
    assert {p.name: p.read_bytes() for p in first} == {
        p.name: p.read_bytes() for p in second
    }
    page = first[0].read_text()
    assert page.index("Executive Summary</h2>") < page.index("Revision history</h2>")
    assert page.index("Revision history</h2>") < page.index("Introduction</h2>")
    history = re.search(r'<section id="revision-history">(.*?)</section>', page, re.S)
    assert history is not None
    assert '<h2>Revision history</h2>' in history.group(1)
    assert "Table " not in history.group(1)
    assert "secnum" not in history.group(1)
    assert page.index('</section>') < page.index('<section id="revision-history">')
    assert "Draft for review" not in page
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)=["\']https?://', page)
    assert "assumed" in page
    assert "previous experience" not in page
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
    assert [s["evaluated_years"] for s in evidence["sensitivity"]] == [20, 25, 30, 50]
    assert all(s["governing_case"] == "mass" for s in evidence["sensitivity"])


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
    assert "Table 1.1 – Revision history" not in page
    assert "Figure 5.1 – Reference jacket elevation" in page
    assert 'id="reference-3"' in page
    assert 'href="#reference-3"' in page
    assert '<span class="secnum applabel">1</span><h2>Executive Summary</h2>' in page


def test_bare_piles_fail_stated_mass_and_resize(builder):
    spec, evidence = builder.build_document()
    stated = evidence["arrangements"]["stated"]
    mass = stated["checks"][0]
    assert not mass["passes"]
    assert mass["required"] == pytest.approx(9139.3, abs=0.1)
    assert mass["margin_percent"] == pytest.approx(-40.4, abs=0.1)
    assert evidence["arrangements"]["rechecked"]["anode_count"] == 424
    assert all(c["passes"] for c in evidence["arrangements"]["rechecked"]["checks"])
    assert "bare" in spec.sections[2].blocks[1].markdown.lower()
    assert "stated trial arrangement" in spec.sections[2].blocks[1].markdown


def test_interactive_figures_use_engine_payloads(builder, tmp_path):
    spec, evidence = builder.build_document()
    figures = {f.figure_id: f for f in spec.figure_blocks() if f.plotly is not None}
    demand = figures["zone-demand"].plotly
    assert len(demand["data"]) == 3
    assert all(".1f" in t["hovertemplate"] for t in demand["data"])
    life = figures["life-count"].plotly
    assert life["data"][0]["y"] == [s["required_count"] for s in evidence["sensitivity"]]
    page = builder.build(tmp_path)[0].read_text()
    assert "Plotly.newPlot" in page
    assert not re.search(r'<script[^>]+src=', page)


def test_client_text_has_declared_basis_without_process_narrative(builder):
    spec, _ = builder.build_document()
    text = " ".join(b.markdown for s in spec.sections for b in s.blocks if b.kind == "text")
    for phrase in ("not reopened", "not independently verified", "LLM_WIKI_PATH",
                   "[I3]", "[I4]", "held 30 m", "remains a draft",
                   "previous experience", "repository alloy density",
                   "repository depth-band boundary", "Native comparisons precede",
                   "held definition", "jacket.yml", "preceding coated-pile",
                   "structural mass discrepancy", "mass-sized quantity"):
        assert phrase not in text
    assert "not a qualified product" in text
    assert "mass governs" in text
    assert "anode size is not optimised" in text
    assert "larger stand-off anode" in text
    assert "Input data file, SHA-256" in text
    assert "including the transition piece" in text


def test_structural_mass_comparison_uses_the_printed_total_basis(builder):
    spec, _ = builder.build_document()
    limitation = spec.sections[6].blocks[0].markdown
    assert limitation.count("Printed structural mass") == 1
    assert ("Printed structural mass is 1,471.0 t; tubular steel plus transition-piece "
            "masses give 1,471.4 t (+0.0 %). The printed figure is read as including "
            "the transition piece. Installed pile mass is not established.") in limitation
    tables = [b for b in spec.appendices[0].blocks if b.kind == "table"]
    comparison = next(t for t in tables if "compared with the paper" in t.title)
    mass = next(r for r in comparison.rows if r[0].startswith("Structural mass,"))
    assert mass[1:5] == ["1,471,000.0", "1,471,364.8", "+0.0 %", "kg"]
    informative = next(t for t in tables if t.title == "Tubular steel mass (informative)")
    assert "Difference" not in informative.columns
    assert informative.rows[0][1] == "1,021,365"


def test_public_revision_preserves_history_and_matches_masthead_and_footer(builder, tmp_path):
    history = builder.revision_history().blocks[0]
    assert history.rows == [
        ["1", "2026-10-22", "Published-geometry worked example label added; calculated results retained."],
        ["0", "2026-10-14", "Issued for information."],
    ]
    page = builder.build(tmp_path)[0].read_text()
    for tag in ("header", "footer"):
        content = re.search(fr"<{tag}.*?</{tag}>", page, re.S).group()
        assert "Revision 01" in content
    assert "to align the basis with" not in page


def test_report_uses_plain_calculation_and_scenario_wording(builder):
    spec, _ = builder.build_document()
    text = " ".join(b.markdown for s in spec.sections for b in s.blocks if b.kind == "text")
    assert "The calculation applies this selection [I2]" in text
    assert "[I2] digitalmodel cathodic protection module, version 2.1.0" in text
    assert ("Illustrative design-life scenarios keep the anode geometry and recompute "
            "the required count") in text
    for phrase in ("existing edition route", "Citation schema:", "native values retained",
                   "Source attribution and the licence remain", "mass-sized arrangement",
                   "retain the reference count", "j is the edition-selected current density",
                   "The existing CP route [I2] supplies the calculation; "
                   "it is not an independent solver benchmark"):
        assert phrase not in text
    assert all(t.source == "[I2]; criterion: provided ≥ required"
               for t in builder.comparison_tables({"arrangements": {"stated": {"checks": []}}}))


def test_trial_and_resized_counts_are_computed_in_load_cases(builder):
    spec, evidence = builder.build_document()
    load_cases = spec.sections[2].blocks[-1].markdown
    stated = evidence["arrangements"]["stated"]["anode_count"]
    resized = evidence["arrangements"]["rechecked"]["anode_count"]
    assert f"stated {stated}-anode trial arrangement" in load_cases
    assert f"re-sized {resized}-anode arrangement" in load_cases
