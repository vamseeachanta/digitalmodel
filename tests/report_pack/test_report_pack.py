"""Tests for the standard report-pack workflow (basename ``report_pack``)."""

import json
import os
import re
from pathlib import Path

import pytest
import yaml

from digitalmodel.reporting import ProvenanceError, render_html
from digitalmodel.report_pack.workflow import (
    MANIFEST_REQUIRED_FIELDS,
    ReportPackConfigError,
    ReportPackManifestError,
    build_report_layer_manifest,
    build_report_spec,
    load_pack,
    router,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLE_DIR = REPO_ROOT / "examples" / "workflows" / "report-pack"
GOLDEN_DIR = Path(__file__).parent / "golden"

TINY_SVG = (
    '<?xml version="1.0" encoding="UTF-8"?>\n'
    '<svg xmlns="http://www.w3.org/2000/svg" width="80" height="20">'
    '<rect width="80" height="20"/></svg>\n'
)

RESULTS_CSV = (
    "sea_state_index,hs,tz,tp,occurrence_fraction,"
    "significant_stress_range,damage_per_year\n"
    "0,3.0,5.0,7.05,0.55,8.42e3,1.18e-3\n"
    "1,8.0,7.0,9.87,0.35,2.31e4,6.94e-3\n"
)


def _base_settings(tmp_path):
    csv_path = tmp_path / "data" / "results.csv"
    csv_path.parent.mkdir(parents=True, exist_ok=True)
    csv_path.write_text(RESULTS_CSV, encoding="utf-8")
    return {
        "document": {
            "number": "B0000-RPT-001-00",
            "revision": "00",
            "title": "Example Fatigue Screening",
            "project": "B0000",
            "client": "Example Client",
            "prepared_by": "Author Placeholder",
            "checked_by": "Checker Placeholder",
            "approved_by": "Approver Placeholder",
        },
        "sections": [
            {"title": "Introduction and Scope", "content": "Scope text."},
            {"title": "Methodology and Assumptions", "content": "Method text."},
            {"title": "Results", "content": "Results narrative."},
            {"title": "Conclusions and Limitations", "content": "Closing text."},
        ],
        "results": {
            "tables": [{"title": "Per-sea-state damage", "csv": "data/results.csv"}]
        },
        "appendices": [
            {"title": "Reference Documents", "content": "Reference list."},
            {"letter": "B", "title": "Calculation Records",
             "files": ["data/results.csv"]},
        ],
        "citations": [
            {
                "code_id": "DNV-RP-C203",
                "publisher": "DNV",
                "revision": "2019 edition",
                "section": "S-N curves",
                "wiki_path": "wikis/marine-engineering/wiki/standards/dnv-rp-c203.md",
                "note": "S-N basis.",
            }
        ],
        "manifest": {
            "issue": "https://github.com/example/repo/issues/1",
            "project": "B0000",
            "artifact_class": "test-report-pack-output",
            "privacy_classification": "test",
            "publishability_decision": "test only",
            "input_source_ids": ["TEST-SRC-1"],
            "raw_output_path": "repo:tmp/raw",
            "final_output_path": "repo:tmp/final",
        },
        "pdf": "off",
        "output_dir": "results",
    }


def _cfg(tmp_path, settings):
    return {
        "basename": "report_pack",
        "report_pack": settings,
        "_config_dir_path": str(tmp_path),
        "_config_file_path": str(tmp_path / "case.yml"),
    }


# ---------------------------------------------------------------------------
# Config validation
# ---------------------------------------------------------------------------


def test_missing_settings_block_rejected():
    with pytest.raises(ReportPackConfigError, match="settings block"):
        router({"basename": "report_pack"})


def test_bad_document_number_rejected(tmp_path):
    settings = _base_settings(tmp_path)
    settings["document"]["number"] = "REPORT-1"
    with pytest.raises(ReportPackConfigError, match="JOB-DOCTYPE-SEQ-REV"):
        router(_cfg(tmp_path, settings))


def test_revision_suffix_mismatch_rejected(tmp_path):
    settings = _base_settings(tmp_path)
    settings["document"]["revision"] = "01"
    with pytest.raises(ReportPackConfigError, match="revision suffix"):
        router(_cfg(tmp_path, settings))


def test_sections_required(tmp_path):
    settings = _base_settings(tmp_path)
    settings["sections"] = []
    with pytest.raises(ReportPackConfigError, match="sections"):
        router(_cfg(tmp_path, settings))


def test_section_without_content_rejected(tmp_path):
    settings = _base_settings(tmp_path)
    settings["sections"] = [{"title": "Introduction"}]
    with pytest.raises(ReportPackConfigError, match="content"):
        router(_cfg(tmp_path, settings))


def test_missing_results_csv_rejected(tmp_path):
    settings = _base_settings(tmp_path)
    settings["results"]["tables"][0]["csv"] = "data/absent.csv"
    with pytest.raises(ReportPackConfigError, match="not found"):
        router(_cfg(tmp_path, settings))


def test_duplicate_appendix_letter_rejected(tmp_path):
    settings = _base_settings(tmp_path)
    settings["appendices"] = [
        {"letter": "A", "title": "One", "content": "x"},
        {"letter": "A", "title": "Two", "content": "y"},
    ]
    with pytest.raises(ReportPackConfigError, match="duplicate appendix letter"):
        router(_cfg(tmp_path, settings))


def test_invalid_citation_rejected(tmp_path):
    settings = _base_settings(tmp_path)
    settings["citations"] = [{"code_id": "X"}]
    with pytest.raises(ReportPackConfigError, match="citations"):
        router(_cfg(tmp_path, settings))


def test_invalid_pdf_mode_rejected(tmp_path):
    settings = _base_settings(tmp_path)
    settings["pdf"] = "maybe"
    with pytest.raises(ReportPackConfigError, match="report_pack.pdf"):
        router(_cfg(tmp_path, settings))


# ---------------------------------------------------------------------------
# Manifest schema validation
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("missing", MANIFEST_REQUIRED_FIELDS)
def test_manifest_required_fields_enforced(tmp_path, missing):
    settings = _base_settings(tmp_path)
    del settings["manifest"][missing]
    with pytest.raises(ReportPackManifestError, match=missing):
        router(_cfg(tmp_path, settings))


def test_manifest_input_source_ids_must_be_strings(tmp_path):
    settings = _base_settings(tmp_path)
    settings["manifest"]["input_source_ids"] = [1, 2]
    with pytest.raises(ReportPackManifestError, match="input_source_ids"):
        router(_cfg(tmp_path, settings))


def test_report_layer_manifest_contents(tmp_path):
    cfg = router(_cfg(tmp_path, _base_settings(tmp_path)))
    manifest_path = tmp_path / "results" / "report-layer-manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    for field in MANIFEST_REQUIRED_FIELDS:
        assert manifest[field], field
    assert manifest["execution_tool"] == "digitalmodel.report_pack.workflow"
    assert manifest["compute_environment"] == "not-recorded"
    assert manifest["citation_evidence_manifest"] == "case_citations.json"
    assert manifest["generated_manifest"] == "case_manifest.json"
    assert "report-layer-manifest.json" in manifest["files"]
    assert manifest["files"] == sorted(manifest["files"])
    assert cfg["report_pack"]["document_number"] == "B0000-RPT-001-00"


def test_build_manifest_helper_orders_and_dedupes():
    manifest = build_report_layer_manifest(
        {
            "issue": "i",
            "project": "p",
            "artifact_class": "a",
            "privacy_classification": "c",
            "publishability_decision": "d",
            "input_source_ids": ["s"],
            "raw_output_path": "r",
            "final_output_path": "f",
        },
        citation_evidence_manifest="c.json",
        generated_manifest="m.json",
        files=["b.md", "a.html", "b.md"],
    )
    assert manifest["files"] == ["a.html", "b.md"]
    assert "parent_issue" not in manifest


# ---------------------------------------------------------------------------
# Section / appendix assembly
# ---------------------------------------------------------------------------


def test_markdown_assembly(tmp_path):
    router(_cfg(tmp_path, _base_settings(tmp_path)))
    md = (tmp_path / "results" / "case_report.md").read_text(encoding="utf-8")
    assert "| Document number | B0000-RPT-001-00 |" in md
    assert "### Revision history" in md
    # Numbered skeleton in declared order.
    assert md.index("## 1. Introduction and Scope") < md.index(
        "## 2. Methodology and Assumptions"
    ) < md.index("## 3. Results") < md.index("## 4. Conclusions and Limitations")
    # Results table injected into the Results section.
    results_at = md.index("## 3. Results")
    conclusions_at = md.index("## 4. Conclusions")
    table_at = md.index("| sea_state_index | hs | tz | tp |")
    assert results_at < table_at < conclusions_at
    # Lettered appendices after body + references.
    assert "## Appendix A — Reference Documents" in md
    assert "## Appendix B — Calculation Records" in md
    assert md.index("## References cited") < md.index("## Appendix A")
    assert "- `data/results.csv`" in md


def test_html_assembly_self_contained(tmp_path):
    router(_cfg(tmp_path, _base_settings(tmp_path)))
    html = (tmp_path / "results" / "case_report.html").read_text(encoding="utf-8")
    assert html.startswith("<!DOCTYPE html>")
    # #2212 part 3: the HTML is the standard report engine's template, so the
    # section is a <section id> with the number drawn by CSS counters (was
    # '<h2 id="section-3">3. Results</h2>') and appendices carry a label span.
    assert '<section id="section-3">' in html
    assert "<h2>Results</h2>" in html
    assert '<section id="appendix-a">' in html
    assert '<span class="secnum applabel">Appendix A</span><h2>Reference Documents</h2>' in html
    assert "<th>sea_state_index</th>" in html
    assert "DNV-RP-C203" in html
    # Self-contained: no external scripts/stylesheets.
    assert "<script" not in html
    assert "http://" not in html and "https://" not in html


# ---------------------------------------------------------------------------
# Delegation to the standard report engine (#2212 part 3)
# ---------------------------------------------------------------------------


def test_html_is_the_engine_render_of_the_pack_spec(tmp_path):
    """The pack's HTML *is* ``render_html(build_report_spec(pack))`` — no
    parallel renderer."""
    router(_cfg(tmp_path, _base_settings(tmp_path)))
    html = (tmp_path / "results" / "case_report.html").read_text(encoding="utf-8")
    spec = build_report_spec(load_pack(_cfg(tmp_path, _base_settings(tmp_path))))
    assert html == render_html(spec)


def test_spec_maps_document_standards_provenance_and_echo(tmp_path):
    settings = _base_settings(tmp_path)
    settings["document"]["date"] = "2026-07-12"
    settings["manifest"]["source_artifacts"] = {"run_config": "repo:case.yml"}
    spec = build_report_spec(load_pack(_cfg(tmp_path, settings)))
    assert spec.document.number == "B0000-RPT-001-00"
    assert spec.document.date == "2026-07-12"
    assert spec.document.revision_history[0].description == "Issued"
    # every distinct cited code becomes a standard/edition chip
    assert [(s.code_id, s.edition) for s in spec.standards] == [
        ("DNV-RP-C203", "2019 edition")
    ]
    assert spec.standards[0].provenance.startswith("wikis/")
    assert spec.citations[0]["code_id"] == "DNV-RP-C203"
    # sections keyed in order; results table lands in the Results section
    assert [s.key for s in spec.sections] == [f"section-{i}" for i in range(1, 5)]
    results = spec.sections[2]
    assert results.title == "Results"
    assert [b.kind for b in results.blocks] == ["text", "table"]
    assert results.blocks[1].columns[0] == "sea_state_index"
    assert results.blocks[1].source == "results.csv"
    # appendices keep their explicit letters as labels
    assert [(a.key, a.label) for a in spec.appendices] == [
        ("appendix-a", "Appendix A"), ("appendix-b", "Appendix B")
    ]
    # provenance: manifest source ids + pointers + every CSV read (digested)
    sources = {(s.kind, s.identifier) for s in spec.provenance.sources}
    assert ("input_source", "TEST-SRC-1") in sources
    assert ("source_artifact", "repo:case.yml") in sources
    assert ("csv", "data/results.csv") in sources
    csv_source = next(s for s in spec.provenance.sources if s.kind == "csv")
    assert csv_source.digest.startswith("sha256:")
    assert spec.manifest["input_source_ids"] == ["TEST-SRC-1"]
    assert spec.input_echo["results"]["tables"] == ["data/results.csv"]
    assert spec.input_echo["pdf"] == "off"


def test_engine_refuses_a_spec_without_provenance(tmp_path):
    spec = build_report_spec(load_pack(_cfg(tmp_path, _base_settings(tmp_path))))
    spec.provenance.sources.clear()
    with pytest.raises(ProvenanceError):
        render_html(spec)


def _settings_with_units_and_svg(tmp_path):
    settings = _base_settings(tmp_path)
    units_csv = tmp_path / "data" / "units.csv"
    units_csv.write_text(
        "frame,x,utilisation\n-,m,-\nMidship,50.0,0.61\n", encoding="utf-8"
    )
    (tmp_path / "data" / "plot.svg").write_text(TINY_SVG, encoding="utf-8")
    settings["results"]["tables"].append(
        {"title": "Frame utilisation", "csv": "data/units.csv", "units_row": True}
    )
    settings["results"]["figures"] = [{"title": "Margin plot", "path": "data/plot.svg"}]
    return settings


def test_units_row_and_svg_figure_render(tmp_path):
    router(_cfg(tmp_path, _settings_with_units_and_svg(tmp_path)))
    out = tmp_path / "results"
    md = (out / "case_report.md").read_text(encoding="utf-8")
    assert "| frame | x | utilisation |\n|---|---|---|\n| - | m | - |\n| Midship |" in md
    assert "![Margin plot](plot.svg)" in md
    html = (out / "case_report.html").read_text(encoding="utf-8")
    assert '<tr class="units"><th>-</th><th>m</th><th>-</th></tr>' in html
    assert "<script" not in html
    # SVG inlined without its XML prolog; no data URI
    assert "<?xml" not in html
    assert '<svg xmlns="http://www.w3.org/2000/svg" width="80" height="20">' in html
    assert "data:image/svg+xml" not in html


def test_inline_units_list_and_conflicts(tmp_path):
    settings = _base_settings(tmp_path)
    settings["results"]["tables"][0]["units"] = ["-", "m", "s", "s", "-", "Pa", "1/yr"]
    spec = build_report_spec(load_pack(_cfg(tmp_path, settings)))
    assert spec.sections[2].blocks[1].units == ["-", "m", "s", "s", "-", "Pa", "1/yr"]
    settings["results"]["tables"][0]["units"] = ["-"]
    with pytest.raises(ReportPackConfigError, match="one entry per column"):
        load_pack(_cfg(tmp_path, settings))
    settings["results"]["tables"][0]["units_row"] = True
    with pytest.raises(ReportPackConfigError, match="not both"):
        load_pack(_cfg(tmp_path, settings))


def test_svg_figure_with_script_rejected(tmp_path):
    settings = _settings_with_units_and_svg(tmp_path)
    (tmp_path / "data" / "plot.svg").write_text(
        '<svg xmlns="http://www.w3.org/2000/svg"><script>alert(1)</script></svg>',
        encoding="utf-8",
    )
    with pytest.raises(ReportPackConfigError, match="must not contain scripts"):
        load_pack(_cfg(tmp_path, settings))


def test_unsupported_figure_format_rejected(tmp_path):
    settings = _base_settings(tmp_path)
    (tmp_path / "data" / "plot.bmp").write_bytes(b"BM")
    settings["results"]["figures"] = [{"title": "x", "path": "data/plot.bmp"}]
    with pytest.raises(ReportPackConfigError, match="unsupported figure format"):
        load_pack(_cfg(tmp_path, settings))


def test_results_section_matches_golden(tmp_path):
    """Golden for one pack section: CSV tables (one with a units row) and an
    inline SVG figure, as the engine renders them. Regenerate with
    ``UPDATE_GOLDENS=1``."""
    spec = build_report_spec(load_pack(_cfg(tmp_path, _settings_with_units_and_svg(tmp_path))))
    html = render_html(spec)
    match = re.search(r'<section id="section-3">.*?</section>', html, re.S)
    assert match, "Results section missing"
    section = match.group(0) + "\n"
    golden = GOLDEN_DIR / "results_section.html"
    if os.environ.get("UPDATE_GOLDENS"):
        golden.parent.mkdir(parents=True, exist_ok=True)
        golden.write_text(section, encoding="utf-8", newline="\n")
    expected = golden.read_text(encoding="utf-8").replace("\r\n", "\n")
    assert section == expected, "results_section.html differs; UPDATE_GOLDENS=1 to accept"


def test_auto_appendix_lettering(tmp_path):
    settings = _base_settings(tmp_path)
    settings["appendices"] = [
        {"title": "First", "content": "x"},
        {"title": "Second", "content": "y"},
        {"letter": "D", "title": "Fourth", "content": "z"},
    ]
    router(_cfg(tmp_path, settings))
    md = (tmp_path / "results" / "case_report.md").read_text(encoding="utf-8")
    assert "## Appendix A — First" in md
    assert "## Appendix B — Second" in md
    assert "## Appendix D — Fourth" in md
    # the HTML keeps the explicit letter too (engine Section.label), not a
    # positional "Appendix C"
    html = (tmp_path / "results" / "case_report.html").read_text(encoding="utf-8")
    assert '<span class="secnum applabel">Appendix D</span><h2>Fourth</h2>' in html
    assert "Appendix C" not in html


def test_citations_sidecar(tmp_path):
    router(_cfg(tmp_path, _base_settings(tmp_path)))
    sidecar = json.loads(
        (tmp_path / "results" / "case_citations.json").read_text(encoding="utf-8")
    )
    assert sidecar["citations"][0]["code_id"] == "DNV-RP-C203"
    assert sidecar["citations"][0]["wiki_path"].startswith("wikis/")


def test_pdf_off_fails_soft_with_message(tmp_path):
    cfg = router(_cfg(tmp_path, _base_settings(tmp_path)))
    assert cfg["report_pack"]["pdf_report"] is None
    assert "disabled" in cfg["report_pack"]["pdf_status"]
    manifest = json.loads(
        (tmp_path / "results" / "case_manifest.json").read_text(encoding="utf-8")
    )
    assert manifest["pdf_report"] is None
    assert not (tmp_path / "results" / "case_report.pdf").exists()


# ---------------------------------------------------------------------------
# Deterministic output
# ---------------------------------------------------------------------------


def test_deterministic_output(tmp_path):
    first = tmp_path / "one"
    second = tmp_path / "two"
    outputs = {}
    for run_dir in (first, second):
        run_dir.mkdir()
        settings = _base_settings(run_dir)
        router(_cfg(run_dir, settings))
        outputs[run_dir] = {
            name: (run_dir / "results" / name).read_bytes()
            for name in (
                "case_report.md",
                "case_report.html",
                "case_citations.json",
                "case_manifest.json",
                "report-layer-manifest.json",
            )
        }
    assert outputs[first] == outputs[second]


# ---------------------------------------------------------------------------
# End-to-end synthetic example (fatg_spectral_fatigue-shaped results)
# ---------------------------------------------------------------------------


def test_example_pack_end_to_end(tmp_path):
    settings = yaml.safe_load((EXAMPLE_DIR / "input.yml").read_text(encoding="utf-8"))
    assert settings["basename"] == "report_pack"
    pack = settings["report_pack"]
    pack["output_dir"] = str(tmp_path / "results")
    pack["pdf"] = "auto"  # exercise the renderer chain; must fail soft if absent
    cfg = router(
        {
            "basename": "report_pack",
            "report_pack": pack,
            "_config_dir_path": str(EXAMPLE_DIR),
            "_config_file_path": str(EXAMPLE_DIR / "input.yml"),
        }
    )
    out = tmp_path / "results"
    md = (out / "input_report.md").read_text(encoding="utf-8")
    # fatg_spectral_fatigue results columns present in the rendered pack.
    assert "| sea_state_index | hs | tz | tp | occurrence_fraction |" in md
    assert "damage_per_year" in md
    assert "## Appendix B — Calculation Records" in md
    html = (out / "input_report.html").read_text(encoding="utf-8")
    assert "<script" not in html
    manifest = json.loads(
        (out / "report-layer-manifest.json").read_text(encoding="utf-8")
    )
    assert manifest["project"] == "B0000"
    assert manifest["input_source_ids"] == ["EXAMPLE-FATG-SCREENING-RESULTS"]
    # PDF is optional: either a real PDF was produced or a clear message recorded.
    status = cfg["report_pack"]["pdf_status"]
    if cfg["report_pack"]["pdf_report"] is None:
        assert "PDF not rendered" in status
    else:
        assert (out / "input_report.pdf").stat().st_size > 0
