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
SECTION_KEYS = ["design-basis", "current-demand", "anode-requirements", "adequacy", "references"]
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
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return run_cathodic_protection(cfg)


def _statuses(spec: ReportSpec) -> list[StatusBlock]:
    return [b for s in spec.sections for b in s.blocks if isinstance(b, StatusBlock)]


def _tables(spec: ReportSpec) -> dict[str, TableBlock]:
    return {b.title: b for s in spec.sections for b in s.blocks if isinstance(b, TableBlock)}


# --- fixture ---------------------------------------------------------------


def test_fixture_is_fresh() -> None:
    """The JSON fixture equals a fresh run of the jacket input (sorted keys)."""
    fresh = json.loads(json.dumps(_run("jacket"), sort_keys=True))
    assert _load_fixture() == fresh


def test_adapters_are_registered_on_import() -> None:
    assert ADAPTERS["cathodic_protection.anode_design"] is anode_design_report
    assert ADAPTERS["cathodic_protection.assessment"] is assessment_report


# --- anode_design: B401 jacket (FAIL, final governs) -------------------------


def test_jacket_spec_layout_and_fail_status() -> None:
    spec = build_spec("cathodic_protection.anode_design", _load_fixture())
    assert [s.key for s in spec.sections] == SECTION_KEYS
    assert [f.figure_id for f in spec.figure_blocks()] == [FIG_DEMAND_VS_TIME, FIG_ANODE_COUNTS]
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
    assert all(c["code_id"] == "dnv-rp-b401" and c["revision"] == "2021-05" for c in spec.citations)
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
    assert [row[0] for row in ri.rows] == ["Initial (fresh anode)", "Final (depleted anode)"]
    assert ri.rows[0][2] == 0.16148 and ri.rows[1][2] == 0.20677
    assert ri.rows[0][-1] is True and ri.rows[1][-1] is False

    checks = dict(tables["Adequacy checks"].rows)
    assert checks == {"final current output": "FAIL", "initial current output": "PASS", "mass": "PASS"}

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
    spec = anode_design_report(_load_fixture())
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
    assert stripped.replace("\r\n", "\n") == expected, "golden differs; UPDATE_GOLDENS=1 to accept"


# --- anode_design: other routes ---------------------------------------------


def test_pipeline_f103_spec_passes_with_spacing_check() -> None:
    spec = anode_design_report(_run("pipeline"))
    assert [s.key for s in spec.sections] == SECTION_KEYS
    assert [f.figure_id for f in spec.figure_blocks()] == [FIG_ANODE_COUNTS]
    status = _statuses(spec)
    assert [(b.status, b.governing_case) for b in status] == [("PASS", "mass"), ("PASS", None)]
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
    rows = {row[0]: row[1] for row in _tables(anode_design_report(cfg))["Design data"].rows}
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
    counts = dict(row[:2] for row in _tables(spec)["Anode mass, count and spacing"].rows)
    assert counts["Bracelets installed (N)"] == 9


def test_abs_routes_expose_tables_and_status() -> None:
    ships = anode_design_report(_run("ships"))
    assert [s.key for s in ships.sections] == SECTION_KEYS
    assert ships.figure_blocks() == []
    assert not ships.has_plotly()
    assert ships.citations == []
    assert ships.standards[0].code_id.startswith("ABS GN")
    ships_status = _statuses(ships)
    assert (ships_status[0].status, ships_status[0].governing_case) == ("FAIL", "final")
    tables = _tables(ships)
    assert "Anode resistance and current output, fresh vs depleted" in tables
    demand = {row[0]: row[1:] for row in tables["Current demand by surface and phase"].rows}
    assert demand["totals"][0] == pytest.approx(181.52154)

    fpso = anode_design_report(_run("fpso"))
    assert [s.key for s in fpso.sections] == SECTION_KEYS
    fpso_status = _statuses(fpso)
    assert (fpso_status[0].status, fpso_status[0].governing_case) == ("PASS", "mass")
    checks = dict(_tables(fpso)["Adequacy checks"].rows)
    assert checks == {"current output": "not run", "mass": "PASS"}
    assert dict(row[:2] for row in _tables(fpso)["Anode mass and count"].rows)["Anode count"] == 79


@pytest.mark.parametrize(
    ("name", "use_status", "wording"),
    [
        ("jacket", USE_STATUS_CLIENT_EOR, "engineer-of-record check"),
        ("pipeline", USE_STATUS_CLIENT_EOR, "engineer-of-record check"),
        ("ships", USE_STATUS_LEGACY_UNCITED, "not for client use without an independent check"),
        ("fpso", USE_STATUS_LEGACY_UNCITED, "not for client use without an independent check"),
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


def test_missing_use_status_renders_as_not_for_client_use() -> None:
    cfg = _load_fixture()
    del cfg["results"]["status"]["use_status"]
    spec = anode_design_report(cfg)
    assert "Use status: not recorded; not for client use" in _statuses(spec)[0].detail


def test_anode_design_rejects_missing_results_and_unknown_route() -> None:
    with pytest.raises(ValueError, match="cfg\\['results'\\]"):
        anode_design_report({"inputs": {"calculation_type": "DNV_RP_B401_offshore"}})
    with pytest.raises(ValueError, match="no layout for calculation_type 'legacy'"):
        anode_design_report({"inputs": {"calculation_type": "legacy"}, "results": {"x": 1}})


# --- assessment --------------------------------------------------------------


def _assessment(overall: ComplianceStatus = ComplianceStatus.NON_COMPLIANT) -> CPAssessmentReport:
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
    assert [s.key for s in spec.sections] == ["summary", "compliance", "remaining-life", "recommendations"]
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
        total_points=3, protected_points=2, underprotected_points=1, overprotected_points=0,
        protection_percentage=66.7, min_potential_V=-0.91, max_potential_V=-0.79,
        mean_potential_V=-0.86, deficiency_locations=[50.0],
    )
    profile = DepletionProfile(
        years=[0, 5, 10, 15], remaining_mass_kg=[100, 80, 60, 40],
        usable_mass_kg=[80, 60, 40, 20], depletion_percentage=[0, 20, 40, 60],
        end_of_life_year=20,
    )
    spec = build_assessment_spec(
        _assessment(ComplianceStatus.COMPLIANT), cis_points=points, cis_result=cis, depletion=profile,
    )
    assert [s.key for s in spec.sections] == [
        "summary", "compliance", "survey", "remaining-life", "recommendations",
    ]
    assert [f.figure_id for f in spec.figure_blocks()] == [FIG_POTENTIAL_VS_DISTANCE, FIG_REMAINING_MASS]
    potential = spec.figure_blocks()[0]
    assert isinstance(potential, FigureBlock) and potential.plotly is not None
    assert [t["name"] for t in potential.plotly["data"]] == ["ON potential", "OFF potential"]
    mass = spec.figure_blocks()[1].plotly
    assert mass is not None and mass["data"][1]["y"] == [80, 60, 40, 20]
    assert _statuses(spec)[0].status == "PASS"
    assert dict(row[:2] for row in _tables(spec)["CIS analysis"].rows)["Deficiency locations"] == "50"
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
