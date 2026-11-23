"""Tests for the Concept Data Sheet renderer (issue #1027).

Builds a real :class:`VariantScreening` via ``screen_concept`` (mirroring
``tests/floating_wind/test_screening.py``), generates a data sheet on the shared
reporting backbone (#1018), and asserts the key fields are present and that it
renders to a standalone HTML document without error.
"""

import pytest

from digitalmodel.floating_wind.floaters import IEA_15MW_RNA
from digitalmodel.floating_wind.report import (
    CONCEPT_DATA_SHEET_BACKBONE,
    ConceptProvenance,
    concept_data_sheet,
    render_concept_data_sheet_html,
    write_concept_data_sheet,
)
from digitalmodel.floating_wind.screening import (
    LoadCase,
    ScreeningCriteria,
    screen_concept,
)
from digitalmodel.reporting import ReportBackbone
from digitalmodel.orcaflex.batch_parametric import ParameterSweep


SEMI_BASE = dict(
    n_columns=3,
    column_diameter=12.5,
    column_radius=51.75,
    draft=20.0,
    pontoon_height=7.0,
    pontoon_volume=10000.0,
    steel_mass_t=4000.0,
)


@pytest.fixture
def load_cases() -> list[LoadCase]:
    return [
        LoadCase(name="operational", hs_m=2.5, tp_s=8.0, steady_load_kN=2400.0),
        LoadCase(name="extreme", hs_m=10.0, tp_s=14.0, steady_load_kN=3000.0),
    ]


@pytest.fixture
def variant(load_cases):
    res = screen_concept(
        "semi",
        SEMI_BASE,
        sweeps=[ParameterSweep(name="column_diameter", values=[12.5])],
        topside=IEA_15MW_RNA,
        load_cases=load_cases,
        criteria=ScreeningCriteria(
            mooring_stiffness_kN_per_m=400.0, max_offset_m=30.0
        ),
    )
    return res.variants[0]


def test_builds_on_the_reporting_backbone():
    # The renderer must consume the shared backbone, not a parallel one.
    assert isinstance(CONCEPT_DATA_SHEET_BACKBONE, ReportBackbone)
    keys = [s.key for s in CONCEPT_DATA_SHEET_BACKBONE.sections]
    assert keys == [
        "header",
        "parameters",
        "derived",
        "verdict",
        "tradespace",
        "provenance",
    ]


def test_sheet_contains_key_fields(variant, load_cases):
    sections = concept_data_sheet(
        variant,
        load_cases=[lc.name for lc in load_cases],
    )
    assert isinstance(sections, list)
    assert all(isinstance(s, str) for s in sections)
    blob = "".join(sections)

    # Archetype + identity.
    assert "SEMI" in blob
    assert variant.case_id in blob
    # Verdict + governing check.
    assert variant.governing_check in blob
    # Derived properties: GM + natural periods.
    assert "GM (metacentric height)" in blob
    assert "Heave natural period" in blob
    assert "Pitch natural period" in blob
    # Screening verdict table covers the checks across load cases.
    for chk in variant.checks:
        assert chk.name in blob
    assert "operational" in blob and "extreme" in blob
    # Provenance: solver tier + version.
    assert "Provenance" in blob
    assert "closed-form" in blob
    assert "digitalmodel version" in blob


def test_tradespace_section_present_when_supplied(variant):
    sections = concept_data_sheet(
        variant,
        tradespace_rank=1,
        pareto_front_size=5,
        correlations={"param_column_diameter": 0.82, "GM_m": -0.41},
    )
    blob = "".join(sections)
    assert "Trade-space Position" in blob
    assert "Pareto" in blob
    assert "param_column_diameter" in blob


def test_tradespace_section_omitted_when_absent(variant):
    # No trade-space info -> the section builder returns "" and the backbone
    # drops it from the ordered output.
    sections = concept_data_sheet(variant)
    blob = "".join(sections)
    assert "Trade-space Position" not in blob


def test_compact_mode_skips_tradespace(variant):
    # COMPACT_SKIP: even with data, compact mode drops the trade-space section.
    full = "".join(concept_data_sheet(variant, tradespace_rank=1))
    compact = "".join(concept_data_sheet(variant, tradespace_rank=1, mode="compact"))
    assert "Trade-space Position" in full
    assert "Trade-space Position" not in compact


def test_high_fidelity_provenance_stamp(variant):
    prov = ConceptProvenance(
        solver_tier="OrcaWave diffraction + OrcaFlex motion",
        high_fidelity=True,
        tool_versions={"OrcaWave": "11.4", "OrcaFlex": "11.4"},
    )
    blob = "".join(concept_data_sheet(variant, provenance=prov))
    assert "OrcaWave diffraction + OrcaFlex motion" in blob
    assert "OrcaWave version" in blob
    assert "11.4" in blob


def test_renders_standalone_html_document(variant, load_cases):
    doc = render_concept_data_sheet_html(
        variant, load_cases=[lc.name for lc in load_cases]
    )
    assert doc.startswith("<!DOCTYPE html>")
    assert "</html>" in doc
    assert "<title>" in doc
    assert "<style>" in doc  # backbone renderer injected the CSS
    assert "Concept Data Sheet" in doc
    assert variant.case_id in doc


def test_writes_html_file(variant, tmp_path):
    out = tmp_path / "sheet.html"
    written = write_concept_data_sheet(variant, out, tradespace_rank=1)
    assert written == out
    assert out.exists()
    text = out.read_text(encoding="utf-8")
    assert text.startswith("<!DOCTYPE html>")
    assert "Trade-space Position" in text
