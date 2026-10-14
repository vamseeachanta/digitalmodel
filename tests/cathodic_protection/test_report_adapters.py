# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# ABOUTME: Tests for the CP consumers of the standard report engine (#2212 part 2).
# ABOUTME: anode_design across the four routes, the jacket golden HTML, assessment.
"""Tests for :mod:`digitalmodel.cathodic_protection.report_adapters`.

The jacket fixture JSON is the routed cfg returned by
``run_cathodic_protection`` for ``workflow_inputs/jacket.yml`` (sorted keys);
``test_fixture_is_fresh`` fails when the adapter's numbers drift (rewrite the
JSON from a fresh run, ``json.dumps(cfg, sort_keys=True, indent=2)``, when the
drift is intended). Regenerate the golden after an intentional layout change
with::

    UPDATE_GOLDENS=1 pytest tests/cathodic_protection/test_report_adapters.py
"""

from __future__ import annotations

import json
import os
import re
import warnings
from pathlib import Path
from typing import Any

import pytest
import yaml  # type: ignore[import-untyped]

from digitalmodel.cathodic_protection.anode_depletion import DepletionProfile
from digitalmodel.cathodic_protection.cp_reporting import (
    ComplianceCheck,
    ComplianceStatus,
    CPAssessmentReport,
    Recommendation,
    RecommendationPriority,
    RemainingLifeSummary,
)
from digitalmodel.cathodic_protection.cp_survey import CISAnalysisResult, CISSurveyPoint
from digitalmodel.cathodic_protection.engine_adapter import (
    USE_STATUS_CLIENT_EOR,
    USE_STATUS_ENGINEERING_VALIDATION,
    USE_STATUS_LEGACY_UNCITED,
    run_cathodic_protection,
)
from digitalmodel.cathodic_protection.report_adapters import (
    FIG_ANODE_COUNTS,
    FIG_DEMAND_VS_TIME,
    FIG_POTENTIAL_VS_DISTANCE,
    FIG_REMAINING_MASS,
    PLACEHOLDER_DOCUMENT_NUMBER,
    anode_design_report,
    assessment_report,
    build_assessment_spec,
)
from digitalmodel.reporting import (
    ADAPTERS,
    DocumentMeta,
    FigureBlock,
    ReportSpec,
    StatusBlock,
    TableBlock,
    TextBlock,
    build_spec,
    render_html,
)

TESTS_DIR = Path(__file__).resolve().parents[1]
FIXTURE_JSON = TESTS_DIR / "fixtures" / "reporting" / "cp_anode_design_result.json"
INPUT_DIR = TESTS_DIR / "fixtures" / "cathodic_protection" / "workflow_inputs"
GOLDEN = TESTS_DIR / "reporting" / "golden" / "cp_anode_design_jacket.html"
_PLOTLY_SCRIPT_RE = re.compile(r'<script type="text/javascript">.*?</script>\n', re.S)
SECTION_KEYS = [
    "design-basis",
    "current-demand",
    "anode-requirements",
    "adequacy",
    "references",
]
DOCUMENT = {
    "number": "B0000-RPT-042-01",
    "revision": "01",
    "title": "Jacket CP anode design",
    "project": "B0000",
    "client": "Client",
}


def _load_fixture() -> dict[str, Any]:
    with FIXTURE_JSON.open(encoding="utf-8") as stream:
        cfg: dict[str, Any] = json.load(stream)
    return cfg


def _run(name: str) -> dict[str, Any]:
    with (INPUT_DIR / f"{name}.yml").open(encoding="utf-8") as stream:
        cfg: dict[str, Any] = yaml.safe_load(stream)
    if name == "ships":
        cfg["citation_repo_root"] = str(TESTS_DIR / "citations/fixtures")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return run_cathodic_protection(cfg)


def test_published_jacket_reproduction_uses_one_unchanged_assumption_set() -> None:
    """Source totals and untuned anode checkpoints are printed independently."""
    cfg = _run("published_jacket_84m")
    spec = anode_design_report(cfg)
    table = _tables(spec)["Published checkpoints and calculated differences"]
    result = cfg["results"]
    allowances = _tables(spec)["Current density, coating breakdown and demand by zone"].rows[-2]
    assert allowances[0] == "Connected allowances"
    assert allowances[-3:] == [18, 18, 18]
    assert result["current_densities_A_m2"]["upper-bare-seawater"]["climate"] == "published basis"
    ver = result["current_output_verification"]
    expected = [2688.285, 1452.450, 1892.052,
                ver["anode_current_output_initial_A"],
                ver["recommended_anode_count"], ver["anode_current_output_final_A"]]
    targets = [2688.3, 1452.5, 1892.1, 5.01, 569, 3.63]
    for row, computed, target in zip(table.rows, expected, targets):
        assert float(row[1]) == target
        assert float(row[2]) == pytest.approx(computed, abs=0.0001)
        difference = 100 * (computed - target) / target
        expected_difference = "less than 0.1" if difference != 0 and abs(difference) < 0.05 else f"{difference:+.1f}"
        assert row[3] == expected_difference
    text = " ".join(b.markdown for s in spec.sections for b in s.blocks if isinstance(b, TextBlock))
    assert "based on previous project experience" in text
    assert "[1]" in text and "[2]" in text


def test_published_jacket_monitoring_is_depth_true_and_attributed() -> None:
    from xml.etree import ElementTree as ET

    cfg = _run("published_jacket_84m")
    spec = anode_design_report(cfg)
    figure = next(f for f in spec.figure_blocks() if f.svg)
    svg = ET.fromstring(figure.svg)
    assert svg.attrib["data-type"] == "depth_true"
    assert not any("data-member" in n.attrib for n in svg.iter())
    points = [n for n in svg.iter() if "data-potential" in n.attrib]
    scale = 500 / 84.1
    elevations = [-14, -40, -70.5, -84.1] * 2
    potentials = [-0.736, -0.789, -0.637, -0.628, -0.925, -0.950, -0.700, -0.650]
    for point, elevation, potential in zip(points, elevations, potentials):
        assert float(point.attrib["cy"]) == pytest.approx(50 - elevation * scale)
        assert float(point.attrib["cx"]) == pytest.approx(350 + (potential + .8) * 50 / .172)
    assert len(points) == 8
    assert "more negative than -0.800 V" in figure.caption
    table = _tables(spec)["Measured potentials at the stated time of measurement"]
    assert [r[-1] for r in table.rows] == ["FAIL"] * 4 + ["PASS", "PASS", "FAIL", "FAIL"]
    assert all(str(r[2]).startswith("approximately") for r in table.rows[4:])
    text = " ".join(b.markdown for s in spec.sections for b in s.blocks if isinstance(b, TextBlock))
    assert "The paper attributes" in text and "clamped sacrificial anodes" in text
    assert "-70.5 m and -84.1 m" in text
    assert "measured under-protection" in text


def test_published_report_includes_native_rechecks_and_distinct_options() -> None:
    spec = anode_design_report(_run("published_jacket_84m"))
    tables = _tables(spec)
    checked = tables["Installed arrangement: published outputs and independent estimate"]
    assert all(row[5] == "PASS" for row in checked.rows)
    assert "Original and dimensioned equivalent-cylinder arrangement" not in tables
    text = " ".join(b.markdown for s in spec.sections for b in s.blocks if isinstance(b, TextBlock))
    assert "Measured under-protection" in text
    assert "the route's recommended count" not in text
    assert "not re-checked by this calculation" not in text


def test_verification_conclusion_requires_measured_failures() -> None:
    cfg = _run("published_jacket_84m")
    del cfg["inputs"]["structure"]["report_geometry"]
    spec = anode_design_report(cfg)
    text = " ".join(b.markdown for s in spec.sections if s.key == "adequacy"
                    for b in s.blocks if isinstance(b, TextBlock))
    assert "two lowest levels" not in text
    assert "Measured protection is not evaluated" in text
    cfg = _run("published_jacket_84m")
    for reading in cfg["inputs"]["structure"]["report_geometry"]["monitoring"]:
        reading["potential_V"] = -.95
    text = " ".join(b.markdown for s in anode_design_report(cfg).sections if s.key == "adequacy"
                    for b in s.blocks if isinstance(b, TextBlock))
    assert "All latest reported monitoring readings" in text
    assert "two lowest levels" not in text


def _statuses(spec: ReportSpec) -> list[StatusBlock]:
    return [b for s in spec.sections for b in s.blocks if isinstance(b, StatusBlock)]


def test_anode_bank_report_has_standard_sections_and_envelope_figure() -> None:
    cfg = _run("anode_bank")
    spec = anode_design_report(cfg)
    assert [section.key for section in spec.sections] == [
        "design-basis",
        "current-demand",
        "bank-resistance",
        "attenuation",
        "adequacy",
        "references",
    ]
    figures = [b for s in spec.sections for b in s.blocks if isinstance(b, FigureBlock)]
    assert figures and "envelope" in figures[0].title.lower()
    assert cfg["results"]["status"]["use_status"] == USE_STATUS_ENGINEERING_VALIDATION
    assert any(
        "not for client use" in status.detail.lower() for status in _statuses(spec)
    )


def test_anode_bank_report_preserves_unequal_sides_and_names_both_lengths() -> None:
    cfg = _run("anode_bank")
    raw = cfg["inputs"]["banks"][0]["sides"][0]
    second = dict(raw, side_id="flowline-west", length_m=200)
    cfg["inputs"]["banks"][0]["sides"].append(second)
    cfg = run_cathodic_protection(cfg)
    spec = anode_design_report(cfg)
    figures = [b for s in spec.sections for b in s.blocks if isinstance(b, FigureBlock)]
    assert len(figures) == 2
    table = _tables(spec)["Protected length and far-end potential"]
    assert "F103 Eq. (20) length" in table.columns
    assert "Fixed-load extension length" in table.columns
    assert len(table.rows) == 2


def test_anode_bank_report_preserves_edition_specific_citation_paths() -> None:
    spec = anode_design_report(_run("anode_bank"))
    paths = {citation["wiki_path"] for citation in spec.citations}
    assert "wikis/engineering-standards/wiki/standards/dnv-rp-f103-2019.md" in paths
    assert "wikis/engineering-standards/wiki/standards/dnv-rp-b401-2017.md" in paths
    topology = _tables(spec)["Bank sizing and topology"]
    labels = {row[1] for row in topology.rows}
    assert {
        "Installed count",
        "Recommended count",
        "Interaction factor",
        "Cable resistance",
        "Group formula",
        "Count-search outcome",
    } <= labels


def _tables(spec: ReportSpec) -> dict[str, TableBlock]:
    return {
        b.title: b for s in spec.sections for b in s.blocks if isinstance(b, TableBlock)
    }


# --- fixture ---------------------------------------------------------------


def test_fixture_is_fresh() -> None:
    """The JSON fixture equals a fresh run of the jacket input (sorted keys)."""
    fresh = json.loads(json.dumps(_run("jacket"), sort_keys=True))
    assert _load_fixture() == fresh


def test_render_jacket_report_example(tmp_path: Path) -> None:
    """Read back the example HTML without starting a browser."""
    spec = anode_design_report(_run("jacket_report_example"))
    path = tmp_path / "jacket_report_example.html"
    path.write_text(render_html(spec), encoding="utf-8")
    rendered = path.read_text(encoding="utf-8")
    assert 'data-member="leg-1"' in rendered
    assert "Original and increased-count arrangement" in rendered
    assert "#### Recommendation" not in rendered
    assert 'id="cp-limitations"' in rendered


def test_adapters_are_registered_on_import() -> None:
    assert ADAPTERS["cathodic_protection.anode_design"] is anode_design_report
    assert ADAPTERS["cathodic_protection.assessment"] is assessment_report


# --- anode_design: B401 jacket (FAIL, final governs) -------------------------


def test_jacket_spec_layout_and_fail_status() -> None:
    spec = build_spec("cathodic_protection.anode_design", _load_fixture())
    assert [s.key for s in spec.sections] == SECTION_KEYS
    assert [f.figure_id for f in spec.figure_blocks()] == [
        FIG_DEMAND_VS_TIME,
        FIG_ANODE_COUNTS,
    ]
    assert spec.has_plotly()

    status = _statuses(spec)
    assert len(status) == 1
    assert status[0].status == "FAIL"
    assert status[0].governing_case == "final"
    assert "N_final=173" in status[0].detail

    assert [(s.code_id, s.edition, s.provenance) for s in spec.standards] == [
        ("DNV-RP-B401", "May 2021", "verified-2021-tables")
    ]
    sections = {c["section"] for c in spec.citations}
    assert {"Table 8-1", "Table 8-2", "Table 8-4", "Table 8-6"} <= sections
    assert all(
        c["code_id"] == "dnv-rp-b401" and c["revision"] == "2021-05"
        for c in spec.citations
    )
    assert all(c["wiki_path"].startswith("wikis/") for c in spec.citations)

    # placeholder document control, said so in the input echo
    assert spec.document.number == PLACEHOLDER_DOCUMENT_NUMBER
    assert spec.document.revision == "00"
    assert "placeholder" in spec.input_echo["report.document"]
    assert spec.input_echo["inputs"]["design_data"]["design_life"] == 25.0

    # provenance: no file path in the fixture -> results digest only
    kinds = [(src.kind, src.identifier) for src in spec.provenance.sources]
    assert kinds == [("results", "cfg[results]")]
    digest = spec.provenance.sources[0].digest
    assert digest is not None and digest.startswith("sha256:")


def test_jacket_tables_carry_the_route_numbers() -> None:
    spec = anode_design_report(_load_fixture())
    tables = _tables(spec)
    demand = tables["Current density, coating breakdown and demand by zone"]
    assert demand.columns[0] == "Zone"
    by_zone = {row[0]: row for row in demand.rows}
    assert by_zone["submerged"][-3:] == [20.0, 85.0, 208.0]
    assert by_zone["Total"][-1] == 208.0
    assert set(by_zone) == {"submerged", "splash", "atmospheric", "Total"}

    counts = dict(row[:2] for row in tables["Anode mass and count"].rows)
    assert counts["Anodes by mass (N_mass)"] == 55
    assert counts["Anodes by initial current output (N_initial)"] == 13
    assert counts["Anodes by final current output (N_final)"] == 173
    assert counts["Recommended anode count"] == 173

    ri = tables["Anode resistance and current output, 55 anodes, fresh vs depleted"]
    assert [row[0] for row in ri.rows] == [
        "Initial (fresh anode)",
        "Final (depleted anode)",
    ]
    assert ri.rows[0][2] == 0.16148 and ri.rows[1][2] == 0.20677
    assert ri.rows[0][-1] is True and ri.rows[1][-1] is False

    checks = dict(tables["Adequacy checks"].rows)
    assert checks == {
        "final current output": "FAIL",
        "initial current output": "PASS",
        "mass": "PASS",
    }

    used = dict(tables["Where each cited table was used"].rows)
    assert "Table 8-1" in used["submerged: design current density"]
    assert used["atmospheric: design current density"] == "none"


def test_jacket_demand_curve_follows_a_plus_bt() -> None:
    spec = anode_design_report(_load_fixture())
    figure = next(f for f in spec.figure_blocks() if f.figure_id == FIG_DEMAND_VS_TIME)
    assert figure.plotly is not None
    traces = {t["name"]: t for t in figure.plotly["data"]}
    submerged = traces["submerged"]
    assert submerged["x"][0] == 0 and submerged["x"][-1] == 25.0
    # A * i_mean * (a + b t): 5000 * 0.1 * (0.02 + 0.012 * 25) = 160 A at the end
    assert submerged["y"][0] == pytest.approx(5000 * 0.1 * 0.02)
    assert submerged["y"][-1] == pytest.approx(160.0)
    assert all(y == 0 for y in traces["atmospheric"]["y"])
    bars = next(f for f in spec.figure_blocks() if f.figure_id == FIG_ANODE_COUNTS)
    assert bars.plotly is not None
    assert bars.plotly["data"][0]["y"] == [55, 13, 173, 173]


def test_yaml_document_control_is_used_when_present() -> None:
    cfg = _load_fixture()
    cfg["report"] = {"kind": "anode_design", "document": DOCUMENT}
    spec = anode_design_report(cfg)
    assert spec.document == DocumentMeta.model_validate(DOCUMENT)
    assert "report.document" not in spec.input_echo


def test_input_file_named_in_provenance(tmp_path: Path) -> None:
    src = INPUT_DIR / "jacket.yml"
    cfg = _load_fixture()
    cfg["_config_file_path"] = str(src)
    cfg["_config_dir_path"] = str(INPUT_DIR.parent)
    spec = anode_design_report(cfg)
    first = spec.provenance.sources[0]
    assert (first.kind, first.identifier) == ("file", "workflow_inputs/jacket.yml")
    assert first.digest is not None and first.digest.startswith("sha256:")


def test_jacket_html_matches_golden() -> None:
    spec = anode_design_report(_run("jacket_report_example"))
    spec.tool_version = "0.0-test"
    html = render_html(spec)
    assert "<script src=" not in html
    assert '<div class="st fail">' in html
    assert "<b>Governing case:</b> final" in html
    assert 'id="fig-cp-demand-vs-time"' in html
    stripped, n = _PLOTLY_SCRIPT_RE.subn("<!-- plotly.js stripped -->\n", html, count=1)
    assert n == 1
    if os.environ.get("UPDATE_GOLDENS"):
        GOLDEN.write_text(stripped, encoding="utf-8", newline="\n")
    expected = GOLDEN.read_text(encoding="utf-8").replace("\r\n", "\n")
    assert stripped.replace("\r\n", "\n") == expected, (
        "golden differs; UPDATE_GOLDENS=1 to accept"
    )


# --- anode_design: other routes ---------------------------------------------


def test_pipeline_f103_spec_passes_with_spacing_check() -> None:
    spec = anode_design_report(_run("pipeline"))
    assert [s.key for s in spec.sections] == SECTION_KEYS
    assert [f.figure_id for f in spec.figure_blocks()] == [FIG_ANODE_COUNTS]
    status = _statuses(spec)
    assert [(b.status, b.governing_case) for b in status] == [
        ("PASS", "mass"),
        ("PASS", None),
    ]
    assert status[1].label.startswith("Anode spacing")
    # F103 default edition 2019 (test_engine_adapter.test_pipeline_f103_bracelet_design derives
    # these): spacing 1500 / 34 = 44.118 m vs 2 PL = 2 x 1587.424 m. The anode values come from
    # F103 (2019) Table 6-3 itself, so no DNV-RP-B401 table is cited (2010 deferred to B401).
    assert "44.118 m vs 3174.847 m" in status[1].detail
    assert [(s.code_id, s.edition) for s in spec.standards] == [
        ("DNVGL-RP-F103", "September 2019, amended May 2021"),
    ]
    assert {c["code_id"] for c in spec.citations} == {"dnv-rp-f103"}
    tables = _tables(spec)
    counts = dict(row[:2] for row in tables["Anode mass, count and spacing"].rows)
    assert counts["Bracelets by mass (N_mass)"] == 34
    assert counts["Bracelets by final current output (N_final)"] == 6
    assert counts["Bracelets installed (N)"] == 34
    assert counts["Protected length (attenuation)"] == 1587.424
    fj = tables["Coating breakdown factors"].rows[1]
    assert fj[:2] == ["Field joints", "none"]


def test_pipeline_f103_design_basis_shows_field_joint_system_and_infill() -> None:
    # Issue #2256: 2019 FBE field joints (3A) with 4E(2) moulded PU infill.
    with (INPUT_DIR / "pipeline.yml").open(encoding="utf-8") as stream:
        cfg: dict[str, Any] = yaml.safe_load(stream)
    cfg["inputs"]["pipeline"].update(
        field_joint_coating="3A",
        field_joint_infill="4E(2)",
        field_joint_count=100,
        field_joint_length_m=0.4,
    )
    run_cathodic_protection(cfg)
    rows = {
        row[0]: row[1] for row in _tables(anode_design_report(cfg))["Design data"].rows
    }
    assert rows["Field joint coating (system id)"] == "3A+4E(2)"
    assert rows["Field joint infill"] == "4E(2) moulded PU on top"


def test_pipeline_f103_2010_alias_key_is_laid_out_as_f103() -> None:
    with (INPUT_DIR / "pipeline.yml").open(encoding="utf-8") as stream:
        cfg: dict[str, Any] = yaml.safe_load(stream)
    cfg["inputs"]["calculation_type"] = "DNV_RP_F103_2010"
    with pytest.warns(DeprecationWarning):
        run_cathodic_protection(cfg)
    spec = anode_design_report(cfg)
    assert [s.key for s in spec.sections] == SECTION_KEYS
    # Edition 2010 defers the anode values to DNV-RP-B401, so B401 is cited too.
    assert [(s.code_id, s.edition) for s in spec.standards] == [
        ("DNV-RP-F103", "October 2010"),
        ("DNV-RP-B401", "2011"),
    ]
    counts = dict(
        row[:2] for row in _tables(spec)["Anode mass, count and spacing"].rows
    )
    assert counts["Bracelets installed (N)"] == 9


def test_abs_routes_expose_tables_and_status() -> None:
    ships = anode_design_report(_run("ships"))
    assert [s.key for s in ships.sections] == SECTION_KEYS
    assert ships.figure_blocks() == []
    assert not ships.has_plotly()
    assert {citation["code_id"] for citation in ships.citations} == {"abs-gn-ships"}
    assert ships.standards[0].code_id.startswith("ABS GN")
    ships_status = _statuses(ships)
    assert (ships_status[0].status, ships_status[0].governing_case) == (
        "PASS",
        "mean_current_output",
    )
    tables = _tables(ships)
    assert "Anode resistance and current output, fresh vs depleted" in tables
    assert "Layout and spacing" in tables
    assert "Depleted long-flush geometry" in tables
    assert "Accepted model assumptions" in tables
    assert "Design limitations" in tables
    assert "Project bare-current basis (Table 3 comparator only)" in tables
    bare_basis = dict(
        row[:2]
        for row in tables["Project bare-current basis (Table 3 comparator only)"].rows
    )
    assert bare_basis["Dynamic bare density"] == pytest.approx(675.0)
    assert bare_basis["Static bare density"] == pytest.approx(675.0)
    counts = dict(row[:2] for row in tables["Anode mass and count"].rows)
    assert counts["Selected anode count"] == 400
    assert counts["Recommended minimum count"] > 0
    demand = {
        row[0]: row[1:] for row in tables["Current demand by surface and phase"].rows
    }
    assert demand["totals"][0] == pytest.approx(181.52154)
    output_rows = tables["Anode resistance and current output, fresh vs depleted"].rows
    assert output_rows[0][-1] is True
    assert output_rows[1][-1] is True

    fpso = anode_design_report(_run("fpso"))
    assert [s.key for s in fpso.sections] == SECTION_KEYS
    fpso_status = _statuses(fpso)
    assert (fpso_status[0].status, fpso_status[0].governing_case) == ("PASS", "mass")
    checks = dict(_tables(fpso)["Adequacy checks"].rows)
    assert checks == {"current output": "not run", "mass": "PASS"}
    assert (
        dict(row[:2] for row in _tables(fpso)["Anode mass and count"].rows)[
            "Anode count"
        ]
        == 79
    )


@pytest.mark.parametrize(
    ("name", "use_status", "wording"),
    [
        ("jacket", USE_STATUS_CLIENT_EOR, "engineer-of-record check"),
        ("pipeline", USE_STATUS_CLIENT_EOR, "engineer-of-record check"),
        ("ships", USE_STATUS_CLIENT_EOR, "engineer-of-record check"),
        (
            "fpso",
            USE_STATUS_LEGACY_UNCITED,
            "not for client use without an independent check",
        ),
    ],
)
def test_use_status_is_stated_in_adequacy_and_status_detail(
    name: str, use_status: str, wording: str
) -> None:
    """Owner decision 2026-09-27 (epic #2206): every deliverable states its use status."""
    cfg = _run(name)
    assert cfg["results"]["status"]["use_status"] == use_status
    spec = anode_design_report(cfg)
    adequacy = next(s for s in spec.sections if s.key == "adequacy")
    first = adequacy.blocks[0]
    assert isinstance(first, TextBlock)
    assert first.markdown.startswith("Use status:") and wording in first.markdown
    route_status = _statuses(spec)[0]
    assert route_status.detail.endswith(first.markdown)
    assert wording in render_html(spec)


def test_ships_report_states_owner_accepted_interpretations() -> None:
    spec = anode_design_report(_run("ships"))
    adequacy = next(s for s in spec.sections if s.key == "adequacy")
    first = adequacy.blocks[0]
    assert isinstance(first, TextBlock)
    html = render_html(spec)
    assert "engineer-of-record check" in first.markdown
    assumptions = _tables(spec)["Accepted model assumptions"]
    rendered = " | ".join(str(row[0]) for row in assumptions.rows)
    for wording in (
        "owner decision 2026-10-01",
        "arithmetic mean",
        "depleted long-flush resistance",
        "dynamic bare-steel current density",
    ):
        assert wording in rendered
        assert wording in html


def test_missing_use_status_renders_as_not_for_client_use() -> None:
    cfg = _load_fixture()
    del cfg["results"]["status"]["use_status"]
    spec = anode_design_report(cfg)
    assert "Use status: not recorded; not for client use" in _statuses(spec)[0].detail


def test_anode_design_rejects_missing_results_and_unknown_route() -> None:
    with pytest.raises(ValueError, match="cfg\\['results'\\]"):
        anode_design_report({"inputs": {"calculation_type": "DNV_RP_B401_offshore"}})
    with pytest.raises(ValueError, match="no layout for calculation_type 'legacy'"):
        anode_design_report(
            {"inputs": {"calculation_type": "legacy"}, "results": {"x": 1}}
        )


# --- assessment --------------------------------------------------------------


def _assessment(
    overall: ComplianceStatus = ComplianceStatus.NON_COMPLIANT,
) -> CPAssessmentReport:
    return CPAssessmentReport(
        report_title="Pipeline CP assessment",
        report_date="2026-09-01",
        system_id="PL-01",
        asset_description="12in export line",
        compliance_checks=[
            ComplianceCheck(
                check_id="POT-1",
                description="Off potential more negative than -0.85 V",
                standard_reference="ISO 15589-1 Table 1",
                criterion_value=-0.85,
                measured_value=-0.79,
                unit="V",
                status=ComplianceStatus.NON_COMPLIANT,
                notes="KP 1.2",
            ),
            ComplianceCheck(
                check_id="POT-2",
                description="No overprotection",
                standard_reference="ISO 15589-1 5.2",
                criterion_value=-1.2,
                measured_value=-1.05,
                unit="V",
                status=ComplianceStatus.COMPLIANT,
            ),
        ],
        remaining_life=RemainingLifeSummary(
            installation_date=None,
            system_id="PL-01",
            design_life_years=25,
            elapsed_years=10,
            remaining_anode_life_years=8,
            remaining_design_life_years=15,
            life_extension_feasible=False,
            limiting_factor="anode mass",
        ),
        recommendations=[
            Recommendation(
                recommendation_id="R1",
                priority=RecommendationPriority.HIGH,
                description="Retrofit anodes at KP 1.2",
                action_required="Install 2 bracelets",
            )
        ],
        overall_status=overall,
        summary_text="One deficiency at KP 1.2.",
    )


def test_assessment_minimal_report_builds_fail_spec_without_figures() -> None:
    spec = build_assessment_spec(_assessment())
    assert [s.key for s in spec.sections] == [
        "summary",
        "compliance",
        "remaining-life",
        "recommendations",
    ]
    assert spec.figure_blocks() == []
    status = _statuses(spec)[0]
    assert (status.status, status.governing_case) == ("FAIL", "POT-1")
    assert "non compliant: 1" in status.detail
    table = _tables(spec)["Compliance checks"]
    assert table.columns[2] == "Standard reference"
    assert table.rows[0][2] == "ISO 15589-1 Table 1"
    assert _tables(spec)["Recommendations"].rows[0][:2] == ["R1", "high"]
    assert spec.document.number == PLACEHOLDER_DOCUMENT_NUMBER
    assert spec.provenance.sources[0].identifier == "CPAssessmentReport"
    html = render_html(spec)
    assert '<div class="st fail">' in html and "window.PLOTLYENV" not in html


def test_assessment_with_survey_and_depletion_adds_figures() -> None:
    points = [
        CISSurveyPoint(distance_m=0.0, on_potential_V=-0.95, off_potential_V=-0.88),
        CISSurveyPoint(distance_m=50.0, on_potential_V=-0.90, off_potential_V=-0.79),
        CISSurveyPoint(distance_m=100.0, on_potential_V=-1.00, off_potential_V=-0.91),
    ]
    cis = CISAnalysisResult(
        total_points=3,
        protected_points=2,
        underprotected_points=1,
        overprotected_points=0,
        protection_percentage=66.7,
        min_potential_V=-0.91,
        max_potential_V=-0.79,
        mean_potential_V=-0.86,
        deficiency_locations=[50.0],
    )
    profile = DepletionProfile(
        years=[0, 5, 10, 15],
        remaining_mass_kg=[100, 80, 60, 40],
        usable_mass_kg=[80, 60, 40, 20],
        depletion_percentage=[0, 20, 40, 60],
        end_of_life_year=20,
    )
    spec = build_assessment_spec(
        _assessment(ComplianceStatus.COMPLIANT),
        cis_points=points,
        cis_result=cis,
        depletion=profile,
    )
    assert [s.key for s in spec.sections] == [
        "summary",
        "compliance",
        "survey",
        "remaining-life",
        "recommendations",
    ]
    assert [f.figure_id for f in spec.figure_blocks()] == [
        FIG_POTENTIAL_VS_DISTANCE,
        FIG_REMAINING_MASS,
    ]
    potential = spec.figure_blocks()[0]
    assert isinstance(potential, FigureBlock) and potential.plotly is not None
    assert [t["name"] for t in potential.plotly["data"]] == [
        "ON potential",
        "OFF potential",
    ]
    mass = spec.figure_blocks()[1].plotly
    assert mass is not None and mass["data"][1]["y"] == [80, 60, 40, 20]
    assert _statuses(spec)[0].status == "PASS"
    assert (
        dict(row[:2] for row in _tables(spec)["CIS analysis"].rows)[
            "Deficiency locations"
        ]
        == "50"
    )
    assert len(spec.provenance.sources) == 4  # report, CIS result, depletion, points


def test_assessment_adapter_reads_cfg_block_and_document() -> None:
    cfg = {
        "basename": "cathodic_protection",
        "assessment": {
            "report": _assessment().model_dump(mode="json"),
            "cis_points": [{"distance_m": 0.0, "on_potential_V": -0.9}],
        },
        "report": {"kind": "assessment", "document": DOCUMENT},
    }
    spec = build_spec("cathodic_protection.assessment", cfg)
    assert spec.document.number == DOCUMENT["number"]
    assert [f.figure_id for f in spec.figure_blocks()] == [FIG_POTENTIAL_VS_DISTANCE]
    figure = spec.figure_blocks()[0]
    assert figure.plotly is not None
    assert [t["name"] for t in figure.plotly["data"]] == ["ON potential"]
    assert spec.provenance.sources[0].identifier == "cfg[assessment]"
    with pytest.raises(ValueError, match="assessment"):
        assessment_report({"assessment": {}})


@pytest.mark.parametrize("name", ["published_jacket_84m", "jacket_report_example"])
def test_arrangement_report_title_is_analysis(name: str) -> None:
    """An evaluated arrangement is titled as a verification."""
    report = anode_design_report(_run(name))
    assert report.document.title.startswith("Cathodic protection analysis:")


def test_illustrative_report_rechecks_options_once_and_prints_whole_mass() -> None:
    """The example presents checked alternatives without duplicated advice."""
    report = anode_design_report(_run("jacket_report_example"))
    tables = _tables(report)
    assert tables["Original and increased-count arrangement"].rows[-1][1:] == ["FAIL", "PASS"]
    text = " ".join(b.markdown for s in report.sections for b in s.blocks if isinstance(b, TextBlock))
    assert text.count("### Design increase") == 1
    assert text.count("### Monitoring") == 1
    assert text.count("### Retrofit") == 1
    assert "Recommendation for overall status" not in text
    for table in tables.values():
        for row in table.rows:
            if len(row) == 3 and row[-1] == "kg":
                assert "." not in str(row[1])


def test_verification_labels_use_clause_titles_without_legacy_numbers() -> None:
    """Display-enabled reports do not mix obsolete clause-number styles."""
    report = anode_design_report(_run('published_jacket_84m'))
    for section in report.sections:
        assert 'Sec. 7.' not in (section.subtitle or '')
        for block in section.blocks:
            if isinstance(block, TableBlock):
                assert all('Sec. 7.' not in str(row[0]) for row in block.rows)


def test_published_effective_densities_are_not_presented_as_standard_coating_constants() -> None:
    """Published effective demands retain their independent source basis."""
    report = anode_design_report(_run('published_jacket_84m'))
    tables = _tables(report)
    zones = tables['Published zone areas and effective-density treatment']
    assert zones.columns == ['Zone', 'Area']
    assert 'effective' in zones.source
    assert 'a' not in zones.columns and 'b' not in zones.columns
    figure = next(f for f in report.figure_blocks() if f.figure_id == FIG_DEMAND_VS_TIME)
    assert 'Published effective mean densities' in figure.caption
    assert 'connected allowances' in figure.caption
    assert 'breakdown constants' not in figure.caption


def test_published_status_prints_whole_kilograms() -> None:
    """Native status quantities retain their values but display whole masses."""
    report = anode_design_report(_run('published_jacket_84m'))
    details = ' '.join(b.detail for s in report.sections for b in s.blocks if isinstance(b, StatusBlock))
    assert not re.search(r'\d+\.\d+ kg', details)
    assert 'Sec. 7.8' not in details


def test_published_outputs_govern_aggregate_verdict_without_changing_results() -> None:
    """Published outputs with locators govern only the report aggregate checks."""
    from copy import deepcopy

    cfg = _run("published_jacket_84m")
    before = deepcopy(cfg["results"])
    spec = anode_design_report(cfg)
    table = _tables(spec)["Installed arrangement: published outputs and independent estimate"]
    demand = [2688.285, 1892.052, 1452.45 * 25 * 8760 / (2000 * .9)]
    published = [569 * 5.01, 569 * 3.63, 569 * 323.4]
    ver = cfg["results"]["current_output_verification"]
    independent = [ver["total_anode_current_output_initial_A"],
                   ver["total_anode_current_output_final_A"], published[-1]]
    for row, required, capacity, model in zip(table.rows, demand, published, independent):
        assert float(row[1]) == pytest.approx(required, abs=.51 if row[0] == "Mass" else .0001)
        assert float(row[3]) == pytest.approx(capacity, abs=.51 if row[0] == "Mass" else .0001)
        assert row[4] == f"{100 * (capacity / required - 1):+.1f}%"
        assert row[5] == "PASS"
        assert float(row[6]) == pytest.approx(model, abs=.51 if row[0] == "Mass" else .0001)
        assert row[7] == f"{100 * (model / required - 1):+.1f}%"
        assert row[8] == f"{100 * (model / capacity - 1):+.1f}%"
        assert "p. 242, Section 2" in row[-1]
    status = _statuses(spec)[0]
    assert status.status == "PASS"
    assert "Aggregate checks PASS using published outputs" in status.detail
    for text in [status.detail, *[b.markdown for s in spec.sections if s.key == "adequacy"
                                 for b in s.blocks if isinstance(b, TextBlock)]]:
        assert "install 575" not in text
    assert "more positive than -0.800 V" in status.detail
    assert "-70.5" in status.detail and "-84.1" in status.detail
    assert "day 518" in status.detail
    assert cfg["results"] == before


def test_published_output_rule_requires_locators_and_is_independent_of_case_name() -> None:
    """Unlocated outputs do not silently become the aggregate basis."""
    cfg = _run("published_jacket_84m")
    cfg["inputs"]["structure"]["verification_evidence"]["source_key"] = "public-source"
    assert _statuses(anode_design_report(cfg))[0].status == "PASS"
    for target in cfg["inputs"]["structure"]["verification_evidence"]["targets"]:
        target.pop("locator")
    assert _statuses(anode_design_report(cfg))[0].status == "FAIL"


def test_measured_failure_options_recheck_each_density_based_quantity() -> None:
    """Density increments size distinct design and lower-zone retrofit options."""
    import math

    cfg = _run("published_jacket_84m")
    tables = _tables(anode_design_report(cfg))
    totals = [652.9*.01+3726.8*.17+15737*.14+10814.8*.02+18,
              652.9*.02+3726.8*.08+15737*.07+10814.8*.02+18,
              652.9*.04+3726.8*.11+15737*.09+10814.8*.02+18]
    ver = cfg["results"]["current_output_verification"]
    req = cfg["results"]["anode_requirements"]
    mass = totals[1]*25*8760/(2000*.9)
    rows = dict((r[0], r[1:]) for r in tables["Increase at design: demand and recheck"].rows)
    for phase, value in zip(["Initial", "Mean", "Final"], totals):
        assert float(rows[phase+" demand"][0]) == pytest.approx(value, abs=.0001)
    for label, outputs in [("Published outputs", [5.01,3.63]),
                           ("Independent estimate", [ver["anode_current_output_initial_A"],ver["anode_current_output_final_A"]])]:
        n = max(math.ceil(mass/323.4),math.ceil(totals[0]/outputs[0]),math.ceil(totals[2]/outputs[1]))
        assert int(rows[label+" count"][0]) == n
        assert rows[label+" recheck"][0] == "PASS"
        assert float(rows[label+" initial capacity"][0]) >= totals[0]
        assert float(rows[label+" final capacity"][0]) >= totals[2]
        assert float(rows[label+" net mass"][0]) >= mass-.5
    remaining = 25-518/365
    increments = [15737*.02,15737*.01,15737*.01]
    retrofit = dict((r[0],r[1:]) for r in tables["Retrofit: lower-zone addition and recheck"].rows)
    for phase,value in zip(["Initial","Mean","Final"],increments):
        assert float(retrofit[phase+" addition"][0]) == pytest.approx(value,abs=.0001)
    needed_mass = increments[1]*remaining*8760/(2000*.9)
    assert float(retrofit["Remaining life at day 518"][0]) == pytest.approx(remaining,abs=.0001)
    for label,outputs in [("Published outputs",[5.01,3.63]),("Independent estimate",[ver["anode_current_output_initial_A"],ver["anode_current_output_final_A"]])]:
        first = math.ceil(increments[2]/outputs[1])
        n = max(first, math.ceil(needed_mass/323.4),math.ceil(increments[0]/outputs[0]))
        assert int(retrofit[label+" final-only count"][0]) == first
        assert retrofit[label+" final-only mass check"][0] == "FAIL"
        assert retrofit[label+" final-only initial check"][0] == "FAIL"
        assert int(retrofit[label+" all-check count"][0]) == n
        assert retrofit[label+" all-check recheck"][0] == "PASS"
        assert retrofit[label+" governing check"][0] == "initial current"
    text = render_html(anode_design_report(cfg))
    for phrase in ["### Anode geometry alternative", "adds 6 units", "allowance of 6 units"]:
        assert phrase not in text
    for phrase in ["trigger is met", "more positive than -0.800 V", "interval is not evaluated",
                   "wall-thickness", "per-level anode distribution", "remaining life at that reading"]:
        assert phrase in text


def test_option_errors_do_not_stop_verification_report(monkeypatch) -> None:
    """A failed advice recheck preserves the report with one missing-data line."""
    from digitalmodel.cathodic_protection import report_adapters as adapters

    cfg = _run("jacket_report_example")
    def unavailable(*args):
        raise ValueError("a passing equivalent-cylinder option was not established")
    monkeypatch.setattr(adapters, "arrangement_options", unavailable)
    rendered = render_html(anode_design_report(cfg))
    assert "Options are not evaluated: complete demand, output, mass and arrangement data are required." in rendered
    assert "Original and increased-count arrangement" not in rendered
    assert "The rechecked alternatives pass" not in rendered


@pytest.mark.parametrize("name", ["published_jacket_84m", "jacket_report_example"])
def test_verification_summary_is_first_and_binds_result_to_criterion(name) -> None:
    """Every verification presentation opens with a result-led summary."""
    cfg = _run(name)
    spec = anode_design_report(cfg)
    assert spec.sections[0].key == "summary"
    text = spec.sections[0].blocks[0].markdown
    assert text.startswith("The ")
    assert "arrangement is analysed" in text
    if name == "published_jacket_84m":
        assert text.startswith("The published 569-anode arrangement is analysed against the paper's printed outputs")
        assert "Aggregate checks PASS using published outputs." in text
        assert "Measured under-protection" in text
        assert "day 518" in text
        assert "Retrofit assessment" in text
        assert "Appendix A" in text
    else:
        assert "FAIL" in text
        assert "final current" in text
    rendered = render_html(spec)
    assert rendered.index('<section id="summary">') < rendered.index('<section id="design-basis">')


REMOVED_VERIFICATION_TEXT = ['adopted from the existing three-zone example', 'adopted\nfrom the existing three-zone example', 'the existing aluminium alloy density', 'No assumption is tuned separately to a target', 'One declared assumption set is used unchanged', 'the existing aluminium calculation basis', 'digitalmodel', 'verified-2021-tables', 'Edition provenance', 'Wiki path', 'wikis/engineering-standards/', 'This report is a view over the sources below', 'cfg[results]', 'engine-adapter results mapping', 'report.document', 'placeholder', 'Engine digitalmodel.reporting.engine', 'Tool version', 'stand_off', 'buried_mudline', 'Table 8-8 (long_slender_standoff)', 'long_slender_standoff', '[published-jacket-paper', '[b401-', 'published area category', 'published basis', 'a = 1', 'b = 0', 'approved for client use', 'Prepared by', 'Checked by', 'Approved by', 'Issued', 'X0000-CP-000-00', 'unassigned', 'DNV-RP-B401 :', 'from the design data of the case', 'route', 'routed', 'regression case', 'citation not held', 'Input echo', 'Sources declared', 'manifest']

def _visible_text(html: str) -> str:
    from html.parser import HTMLParser

    class Reader(HTMLParser):
        def __init__(self):
            super().__init__()
            self.skip = 0
            self.parts = []
        def handle_starttag(self, tag, attrs):
            if tag in {"script", "style"}:
                self.skip += 1
        def handle_endtag(self, tag):
            if tag in {"script", "style"}:
                self.skip -= 1
        def handle_data(self, data):
            if not self.skip:
                self.parts.append(data)
    reader = Reader()
    reader.feed(html)
    return " ".join(" ".join(reader.parts).split())


@pytest.mark.parametrize("name", ["published_jacket_84m", "jacket_report_example"])
@pytest.mark.parametrize("removed", REMOVED_VERIFICATION_TEXT)
def test_verification_visible_text_excludes_framework_and_draft_placeholders(name, removed) -> None:
    """Each named presentation defect is absent from the visible report."""
    text = _visible_text(render_html(anode_design_report(_run(name))))
    assert " ".join(removed.split()).lower() not in text.lower()


def test_published_numbered_references_locators_precision_and_abstract_boundary() -> None:
    """Published tables retain locators while comparisons use fixed precision."""
    spec = anode_design_report(_run("published_jacket_84m"))
    text = _visible_text(render_html(spec))
    assert "Draft for review" in text
    assert "based on previous project experience" in text
    assert "References" in text and "Dezhu Xu" in text and "10.26855/ea.2023.06.017" in text
    assert "[1], p. 242" in text and "[2]" in text
    assert "not attached to a level or date and are not used" in text
    assert not re.search(r"[-+]?\d+\.\d+[eE][-+]?\d+", text)
    assert not re.search(r"\d+\.\d+ kg", text)
    for row in _tables(spec)["Published checkpoints and calculated differences"].rows:
        assert re.fullmatch(r"(?:[-+]\d+\.\d|less than 0\.1)", str(row[3]))
    for title, table in _tables(spec).items():
        if title == "Design data" or title.startswith(("Published zone", "Current density", "Measured potentials", "Independent estimate", "Installed arrangement", "Increase at design", "Retrofit:", "Published checkpoints")):
            assert "p. 24" in str(table.source) or any("p. 24" in str(r[-1]) for r in table.rows), title
    reference = next(s for s in spec.sections if s.key == "references")
    reftext = " ".join(b.markdown for b in reference.blocks if isinstance(b, TextBlock))
    assert reftext.count("10.26855/ea.2023.06.017") == 1
    assert spec.verification_presentation is True
    assert _load_fixture()["results"]["status"]["result"] == "FAIL"
    assert anode_design_report(_load_fixture()).verification_presentation is False


def test_summary_does_not_claim_unavailable_options_are_rechecked(monkeypatch) -> None:
    """An unavailable advice calculation is also disclosed in the first section."""
    from digitalmodel.cathodic_protection import report_adapters as adapters

    def unavailable(*args):
        raise ValueError("increased-count arrangement did not pass its recheck")
    monkeypatch.setattr(adapters, "arrangement_options", unavailable)
    spec = anode_design_report(_run("jacket_report_example"))
    summary = spec.sections[0].blocks[0].markdown
    assert "Rechecked design alternatives" not in summary
    assert "Design alternatives are not evaluated" in summary


def test_criterion_equality_is_not_described_as_more_positive() -> None:
    """Equality fails a strict more-negative criterion without changing its sign."""
    cfg = _run("published_jacket_84m")
    for r in cfg["inputs"]["structure"]["report_geometry"]["monitoring"]:
        r["potential_V"] = cfg["inputs"]["structure"]["report_geometry"]["criterion_V"]
    detail = _statuses(anode_design_report(cfg))[0].detail
    assert "at or more positive than -0.800 V" in detail


def test_diagram_criterion_has_numbered_standard_reference() -> None:
    """The standard criterion is distinguished from the paper measurements."""
    spec = anode_design_report(_run("published_jacket_84m"))
    figure = next(f for f in spec.figure_blocks() if f.svg)
    assert "Criterion input: based on previous project experience" in figure.caption


def test_all_option_counts_capacities_and_mass_values_match_independent_arithmetic() -> None:
    """Every recheck quantity matches independent current and remaining-life sums."""
    import math

    cfg = _run("published_jacket_84m")
    tables = _tables(anode_design_report(cfg))
    design = [3077.561, 1647.088, 2086.69]
    zone = [15737*(.14-.12),15737*(.07-.06),15737*(.09-.08)]
    radius = math.sqrt(323.4/(math.pi*2.02*2750))
    model = [.25/(.22/(2*math.pi*2.02)*(math.log(4*2.02/r)-1))
             for r in [radius,radius*math.sqrt(1-.9)]]
    for title, demands, hours, retrofit in [
        ("Increase at design: demand and recheck", design,25*8760,False),
        ("Retrofit: lower-zone addition and recheck",zone,(25-518/365)*8760,True),
    ]:
        rows = {r[0]:r[1] for r in tables[title].rows}
        mass = demands[1]*hours/(2000*.9)
        assert float(rows["Required net mass"]) == pytest.approx(mass,abs=.51)
        for label, initial, final in [
            ("Published outputs",5.01,3.63),
            ("Independent estimate",*model),
        ]:
            counts = [math.ceil(mass/323.4),math.ceil(demands[0]/initial),math.ceil(demands[2]/final)]
            for phase,value in zip(["mass","initial","final"],counts):
                assert rows[label+" "+phase+" count"] == value
            n = max(counts)
            for suffix,value in [(" initial capacity",n*initial),(" final capacity",n*final),(" net mass",n*323.4)]:
                assert float(rows[label+suffix]) == pytest.approx(value,abs=.51 if "mass" in suffix else .00051)
            assert rows[label+(" all-check recheck" if retrofit else " recheck")]=="PASS"
            if retrofit:
                first=counts[-1]
                for suffix,value in [(" final-only final capacity",first*final),(" final-only initial capacity",first*initial),(" final-only net mass",first*323.4)]:
                    assert float(rows[label+suffix]) == pytest.approx(value,abs=.51 if "mass" in suffix else .00051)


def test_published_aggregate_pass_without_measured_failure_has_no_options() -> None:
    """A published-output PASS with protected latest readings prints no options."""
    cfg = _run("published_jacket_84m")
    for r in cfg["inputs"]["structure"]["report_geometry"]["monitoring"]:
        r["potential_V"] = -.950
    spec = anode_design_report(cfg)
    assert _statuses(spec)[0].status == "PASS"
    text = " ".join(b.markdown for s in spec.sections if s.key == "adequacy"
                    for b in s.blocks if isinstance(b,TextBlock))
    for phrase in ["### Increase at design","### Retrofit","### Monitoring",
                   "adds 0 units","allowance of 0 units","rechecked alternatives"]:
        assert phrase not in text
    assert "All latest reported monitoring readings" in text


def test_incomplete_routed_option_quantities_still_render() -> None:
    """A real quantity-validation error cannot stop a verification report."""
    cfg = _run("jacket_report_example")
    cfg["results"]["current_output_verification"]["recommended_anode_count"] = 1
    rendered = render_html(anode_design_report(cfg))
    assert "Options are not evaluated: complete demand, output, mass and arrangement data are required." in rendered
    assert "Original and increased-count arrangement" not in rendered


def test_illustrative_assumptions_are_declared_once_as_experience_based() -> None:
    """The illustrative verification states its own assumptions in Design basis."""
    spec = anode_design_report(_run("jacket_report_example"))
    design = next(s for s in spec.sections if s.key=="design-basis")
    text = " ".join(b.markdown for b in design.blocks if isinstance(b,TextBlock))
    assert "Design life 25 years and seawater temperature 10" in text
    assert "net alloy" in text and "equivalent-cylinder" in text
    assert "source: illustrative input" in text
    body = " ".join(b.markdown for s in spec.sections for b in s.blocks if isinstance(b,TextBlock))
    assert body.count("source: illustrative input") == 1


def test_option_model_capacities_use_unrounded_resistance_outputs() -> None:
    """Independent resistance arithmetic is rounded only for printed capacities."""
    import math

    cfg = _run("published_jacket_84m")
    radius = math.sqrt(323.4/(math.pi*2.02*2750))
    outputs = [.25/(.22/(2*math.pi*2.02)*(math.log(4*2.02/r)-1))
               for r in [radius,radius*math.sqrt(1-.9)]]
    tables = _tables(anode_design_report(cfg))
    for title,suffix in [("Increase at design: demand and recheck"," count"),
                         ("Retrofit: lower-zone addition and recheck"," all-check count")]:
        rows = {r[0]:r[1] for r in tables[title].rows}
        count = rows["Independent estimate"+suffix]
        for phase,output in zip(["initial","final"],outputs):
            assert float(rows["Independent estimate "+phase+" capacity"]) == pytest.approx(count*output,abs=.00051)


def test_verification_print_tables_override_scroll_width_and_nowrap() -> None:
    """Wide verification checks are fitted for print rather than horizontally cropped."""
    rendered = render_html(anode_design_report(_run("published_jacket_84m")))
    assert ".doc .tbl table" in rendered
    assert "table-layout:fixed" in rendered
    assert ".doc .tbl-wrap{overflow:visible}" in rendered
    assert ".doc .tbl td" in rendered and "white-space:normal" in rendered


def test_verification_labels_have_no_empty_clause_parentheses() -> None:
    """Removing obsolete section numbers does not leave empty annotation punctuation."""
    for name in ["published_jacket_84m","jacket_report_example"]:
        text = _visible_text(render_html(anode_design_report(_run(name))))
        assert "required ()" not in text


_ANALYSIS_CASES = (
    ("published_jacket_84m", "Cathodic protection analysis: jacket at 84.1 m water depth"),
    ("reference_jacket_60m_members", "Cathodic protection analysis: jacket at 60 m water depth"),
    ("jacket_report_example", "Cathodic protection analysis: illustrative example jacket at 70 m water depth"),
)


def _assert_comparison_only_wording(html: str) -> None:
    """Reject comparison wording in any other visible part of a page."""
    outside = re.sub(
        r'<section id="(?:published-comparison|structural-comparison)">.*?</section>',
        "", html, flags=re.S,
    )
    assert not re.search(r"\bverif\w*", _visible_text(outside), re.I)


@pytest.mark.parametrize("name,title", _ANALYSIS_CASES)
def test_analysis_titles_and_comparison_only_wording(name: str, title: str) -> None:
    """Each analysis declares its purpose without stray comparison wording."""
    spec = anode_design_report(_run(name))
    assert spec.document.title == title
    _assert_comparison_only_wording(render_html(spec))


@pytest.mark.parametrize("name,title", _ANALYSIS_CASES)
def test_comparison_wording_check_rejects_summary_injection(name: str, title: str) -> None:
    """A negative control catches wording injected into the summary."""
    html = render_html(anode_design_report(_run(name)))
    injected = html.replace('<section id="summary">',
                            '<section id="summary"><p>Verification</p>')
    with pytest.raises(AssertionError):
        _assert_comparison_only_wording(injected)


def _assert_no_standard_value_sources(html: str) -> None:
    """Reject numerical table rows attributed to a licensed standard."""
    standard = re.compile(r"DNV[\s-]*RP[\s-]*B\s*401", re.I)
    for table in re.findall(r"<table\b.*?</table>", html, re.S):
        heading = re.search(r"<tr>(.*?)</tr>", table, re.S)
        if heading is None:
            continue
        columns = [_visible_text(c).lower() for c in re.findall(r"<th[^>]*>(.*?)</th>", heading[1], re.S)]
        source_indices = [i for i, c in enumerate(columns) if re.search(r"source|basis|reference|locator", c)]
        for row in re.findall(r"<tr>(.*?)</tr>", table, re.S):
            cells = [_visible_text(c) for c in re.findall(r"<td[^>]*>(.*?)</td>", row, re.S)]
            for i in source_indices:
                if i < len(cells) and standard.search(cells[i]):
                    assert not any(re.search(r"\d", c) for j, c in enumerate(cells) if j != i), cells[0]


@pytest.mark.parametrize("name,title", _ANALYSIS_CASES)
def test_analysis_design_values_have_declared_sources(name: str, title: str) -> None:
    """Design inputs retain numbers while sources distinguish papers and assumptions."""
    spec = anode_design_report(_run(name))
    html = render_html(spec)
    _assert_no_standard_value_sources(html)
    table = _tables(spec)["Design input sources" if name == "reference_jacket_60m_members" else "Design data"]
    for quantity in ("Utilisation factor", "Electrochemical capacity"):
        row = next(r for r in table.rows if r[0] == quantity)
        assert row[-1] == ("illustrative input" if name == "jacket_report_example" else "based on previous project experience")
    for section in spec.sections:
        for block in section.blocks:
            if isinstance(block, FigureBlock) and block.svg:
                for label in re.findall(r"<text\b[^>]*>(.*?)</text>", block.svg, re.S):
                    if "-0.800" in label:
                        assert "DNV" not in label and "[2]" not in label
    if name == "published_jacket_84m":
        text = _visible_text(html)
        assert "supplies capacity" not in text
        assert "potentials -0.800 and -1.050 V" in text
        assert "based on previous project experience" in text


@pytest.mark.parametrize("name,title", _ANALYSIS_CASES)
def test_standard_source_check_rejects_injected_numeric_row(name: str, title: str) -> None:
    """The source-column rule is checked with an injected negative control."""
    html = render_html(anode_design_report(_run(name)))
    html += '<table><tr><th>Value</th><th>Source</th></tr><tr><td>1</td><td>dnv rp b401</td></tr></table>'
    with pytest.raises(AssertionError):
        _assert_no_standard_value_sources(html)


def test_reference_example_inputs_are_labelled_individually() -> None:
    """The recorded example inputs do not imply historical engineering evidence."""
    spec = anode_design_report(_run("reference_jacket_60m_members"))
    rows = {r[0]: r for r in _tables(spec)["Design input sources"].rows}
    for quantity in ("Seawater temperature", "Seawater resistivity", "Coating category", "Design life", "Anode length", "Anode radius"):
        assert rows[quantity][-1] == "illustrative input"


@pytest.mark.parametrize("name,title", _ANALYSIS_CASES)
def test_analysis_percentages_and_citation_residue(name: str, title: str) -> None:
    """All visible differences use one decimal without residual citation keys."""
    html = render_html(anode_design_report(_run(name)))
    text = _visible_text(html)
    assert not re.search(r"[+-]?\d+\.\d{2,}\s*%", text)
    assert "-0.0%" not in text
    assert not re.search(r"dnv-rp-b401\s+20\d\d\s+\[", text, re.I)
    assert not re.search(r"\bEq\.\s*\(", text)
    assert "(B401 )" not in text
    assert "declared input basis resistance" not in text
    assert "with declared input basis breakdown constants" not in text
    if name == "jacket_report_example":
        appendix = next(s for s in anode_design_report(_run(name)).appendices if s.key == "cp-limitations")
        body = " ".join(b.markdown for b in appendix.blocks if isinstance(b, TextBlock))
        assert body.count("percentage difference less than 0.1%") == 3


@pytest.mark.parametrize("name", ["published_jacket_84m", "jacket_report_example"])
def test_analysis_adequacy_subtitle_states_current_output_check(name: str) -> None:
    """The analysis subtitle identifies the check without a spurious basis."""
    spec = anode_design_report(_run(name))
    adequacy = next(s for s in spec.sections if s.key == "adequacy")
    assert adequacy.subtitle == "Current-output check"


def test_illustrative_current_demand_caption_names_declared_coating_constants() -> None:
    """The demand figure describes the declared coating breakdown inputs."""
    spec = anode_design_report(_run("jacket_report_example"))
    figure = next(f for f in spec.figure_blocks() if f.figure_id == FIG_DEMAND_VS_TIME)
    assert "with the declared coating breakdown constants" in figure.caption


def test_illustrative_fresh_anode_radius_is_labelled_as_input() -> None:
    """The fresh radius retains its stated value and illustrative input source."""
    spec = anode_design_report(_run("jacket_report_example"))
    row = next(r for r in _tables(spec)["Design data"].rows
               if r[0] == "Anode equivalent radius (fresh)")
    assert row[1] == 0.05
    assert row[-1] == "illustrative input"
    assert "calculated from declared inputs" not in row


def test_published_fresh_anode_radius_source_regression_pin() -> None:
    """The published radius source remains calculated from declared inputs."""
    cfg = _run("published_jacket_84m")
    spec = anode_design_report(cfg)
    row = next(r for r in _tables(spec)["Design data"].rows
               if r[0] == "Anode equivalent radius (fresh)")
    assert row[1] == cfg["results"]["current_output_verification"]["equivalent_radius_initial_m"]
    assert row[-1] == "calculated from declared inputs"


def test_signed_zero_difference_is_printed_without_negative_zero() -> None:
    """An exact signed zero remains a true zero in report text."""
    from digitalmodel.cathodic_protection.report_elevation import percentage_text

    assert percentage_text(-0.0) == "+0.0%"


def test_illustrative_design_basis_uses_example_input_label() -> None:
    """The example basis uses the same source label as its input table."""
    spec = anode_design_report(_run("jacket_report_example"))
    section = next(s for s in spec.sections if s.key == "design-basis")
    prose = " ".join(b.markdown for b in section.blocks if isinstance(b, TextBlock))
    assert "source: illustrative input" in prose
    assert "based on previous project experience" not in prose


def test_published_summary_and_conclusions_give_computed_retrofit_quantities() -> None:
    """Recommendations reproduce independent sizing from the routed basis."""
    import math
    from digitalmodel.cathodic_protection import _kernels as kernel

    cfg = _run("published_jacket_84m")
    inputs, result = cfg["inputs"], cfg["results"]
    basis = inputs["structure"]["verification_evidence"]["higher_density_basis"]
    targets = {t["quantity"]: t["printed"] for t in inputs["structure"]["verification_evidence"]["targets"]}
    zone = next(z for z in inputs["structure"]["zones"] if z["zone"] == basis["affected_zone"])
    added = [zone["area_m2"] * (basis["zones"][zone["zone"]][p] - zone["effective_current_density_A_m2"][p]) for p in ("initial", "mean", "final")]
    req = result["anode_requirements"]
    elapsed = max(r["elapsed_days"] for r in inputs["structure"]["report_geometry"]["monitoring"])
    hours = (result["design_life_years"] - elapsed / 365) * kernel.HOURS_PER_YEAR
    required_mass = added[1] * hours / (req["electrochemical_capacity_Ah_kg"] * req["utilization_factor"])
    counts = [math.ceil(required_mass / req["individual_mass_kg"]), math.ceil(added[0] / targets["initial_output"]), math.ceil(added[2] / targets["final_output"])]
    count = max(counts)
    demands = [result["connected_current_allowance_A"] + sum(z["area_m2"] * basis["zones"].get(z["zone"], z["effective_current_density_A_m2"])[p] for z in inputs["structure"]["zones"]) for p in ("initial", "mean", "final")]
    design_mass = demands[1] * result["design_life_years"] * kernel.HOURS_PER_YEAR / (req["electrochemical_capacity_Ah_kg"] * req["utilization_factor"])
    design_count = max(math.ceil(design_mass / req["individual_mass_kg"]), math.ceil(demands[0] / targets["initial_output"]), math.ceil(demands[2] / targets["final_output"]))
    spec = anode_design_report(cfg)
    summary = spec.sections[0].blocks[0].markdown
    conclusion = next(b.markdown for s in spec.sections for b in s.blocks if isinstance(b, TextBlock) and b.markdown.startswith("### Conclusions and recommendations"))
    for text in (summary, conclusion):
        assert f"{count} additional anodes" in text
        assert f"{count * req['individual_mass_kg']:,.0f} kg net alloy" in text
        assert "additional-load checks PASS" in text
    assert f"{design_count} anodes" in conclusion
    assert "mass governs" in conclusion
    assert "sensitivity only" in conclusion
    assert "split and intervention date are not evaluated" in conclusion
    assert count * req["individual_mass_kg"] >= required_mass
    assert count * targets["initial_output"] >= added[0]
    assert count * targets["final_output"] >= added[2]


def test_published_retrofit_zone_uses_lower_case_in_prose() -> None:
    """The zone name uses sentence case in prose and retains cell capitals."""
    spec = anode_design_report(_run("published_jacket_84m"))
    summary = spec.sections[0].blocks[0].markdown
    assert "in bare steel in seawater, 30 m to mudline;" in summary
    rows = _tables(spec)["Current density, coating breakdown and demand by zone"].rows
    assert any(r[0] == "Bare steel in seawater, 30 m to mudline" for r in rows)


@pytest.mark.parametrize("name", ["published_jacket_84m", "jacket_report_example"])
def test_utilisation_source_row_does_not_repeat_its_source(name: str) -> None:
    """The source value is stated once without duplication in its source cell."""
    spec = anode_design_report(_run(name))
    row = next(r for r in _tables(spec)["Design data"].rows
               if r[0] == "Utilisation factor source")
    assert row[1] == ("based on previous project experience"
                      if name == "published_jacket_84m" else "illustrative input")
    assert row[-1] == ""


def test_reference_summary_recommends_the_minimum_aggregate_arrangement() -> None:
    """The passing stated arrangement is named consistently in its summary."""
    cfg = _run("reference_jacket_60m_members")
    summary = anode_design_report(cfg).sections[0].blocks[0].markdown
    count = cfg["results"]["current_output_verification"]["anode_count"]
    assert f"The minimum aggregate arrangement is recommended at {count} anodes" in summary
    assert "The rechecked aggregate quantity" not in summary


def test_each_analysis_summary_states_the_recommended_quantity() -> None:
    """The reference and illustrative summaries state their routed counts."""
    for name in ("reference_jacket_60m_members", "jacket_report_example"):
        cfg = _run(name)
        summary = anode_design_report(cfg).sections[0].blocks[0].markdown
        count = cfg["results"]["current_output_verification"]["recommended_anode_count"]
        assert f"{count} anodes" in summary or f"{count}-anode" in summary
        assert "recommended" in summary


def test_spatial_and_reference_comparison_limitations_are_explicit() -> None:
    """Local findings and unlike geometric quantities retain their limitations."""
    published = render_html(anode_design_report(_run("published_jacket_84m")))
    assert "Aggregate checks cannot detect this local under-protection." in _visible_text(published)
    cfg = _run("reference_jacket_60m_members")
    spec = anode_design_report(cfg)
    summary = spec.sections[0].blocks[0].markdown
    angle = next(c for c in cfg["structural_model"]["comparisons"] if c["quantity"] == "Leg 1 inclination in 3D")
    assert f"{angle['difference_percent']:+.1f}%" in summary
    assert "remain unresolved" in summary
    assert "minimum aggregate arrangement" in summary
    assert "no selected anode product" in summary
    assert "anode size requires confirmation" in summary
    table = _tables(spec)["Structural dimensions and mass compared with the paper"]
    projection = [r for r in table.rows if "y-projection" in r[0] or "y projection" in r[0]]
    assert projection
    assert all(r[3] == "not comparable" for r in projection)
    figure = next(f for f in spec.figure_blocks() if f.figure_id == "fig-reference-jacket-elevation")
    assert "both measures are listed" in figure.caption


def test_reference_summary_recommends_rechecked_count_after_stated_failure() -> None:
    """A failing stated count is never labelled as the recommended quantity."""
    cfg = yaml.safe_load((INPUT_DIR / "reference_jacket_60m_members.yml").read_text())
    needed = _run("reference_jacket_60m_members")["results"]["current_output_verification"]["recommended_anode_count"]
    cfg["inputs"]["anode"]["count"] = needed - 1
    cfg = run_cathodic_protection(cfg)
    checked = cfg["inputs"]["anode"]["count"]
    summary = anode_design_report(cfg).sections[0].blocks[0].markdown
    assert f"recommended at {checked} anodes" not in summary
    needed = cfg["results"]["current_output_verification"]["recommended_anode_count"]
    assert f"recommended at {needed} anodes" in summary


def test_reference_unavailable_recheck_does_not_claim_a_minimum(monkeypatch: pytest.MonkeyPatch) -> None:
    """Unavailable sizing is not described as a qualified minimum arrangement."""
    from digitalmodel.cathodic_protection import report_reference

    cfg = yaml.safe_load((INPUT_DIR / "reference_jacket_60m_members.yml").read_text())
    cfg["inputs"]["anode"]["count"] = _run("reference_jacket_60m_members")["results"]["current_output_verification"]["recommended_anode_count"] - 1
    cfg = run_cathodic_protection(cfg)

    def unavailable(config: Any) -> Any:
        raise ValueError("a passing aggregate arrangement is not established")

    monkeypatch.setattr(report_reference, "reference_arrangements", unavailable)
    summary = anode_design_report(cfg).sections[0].blocks[0].markdown
    assert "minimum aggregate arrangement" not in summary
    assert "A passing aggregate quantity is not evaluated" in summary
