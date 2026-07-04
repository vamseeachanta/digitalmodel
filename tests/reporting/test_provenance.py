"""Tests for the reporting provenance / SSOT-view module (#1019)."""

from __future__ import annotations

import pytest

from digitalmodel.common.assumption_ledger import (
    AssumptionLedger,
    AssumptionSource,
    Confidence,
)
from digitalmodel.reporting import (
    DataSource,
    Provenance,
    ProvenanceError,
    ReportSection,
    SectionMode,
    assemble_report,
    assumption_ledger_block,
    provenance_block,
)


def _ledger() -> AssumptionLedger:
    led = AssumptionLedger()
    led.record(
        "environment.water_depth",
        100.0,
        AssumptionSource.ASSUMED_DEFAULT,
        "Default screening depth",
        Confidence.LOW,
        reference="default",
        impact=4,
    )
    return led


# --- mandatory provenance ---------------------------------------------------


def test_require_raises_without_sources() -> None:
    with pytest.raises(ProvenanceError):
        Provenance().require()


def test_assemble_report_fails_closed_without_provenance() -> None:
    with pytest.raises(ProvenanceError):
        assemble_report("Untraceable", provenance=Provenance())


def test_assemble_report_with_provenance_renders_document() -> None:
    prov = Provenance().add("spec", "spec.yml", digest="abc123")
    html = assemble_report("Test Report", provenance=prov, ledger=_ledger())
    assert "<!DOCTYPE html>" in html
    assert "Test Report" in html
    # provenance section present and references the source
    assert 'id="provenance"' in html
    assert "spec.yml" in html and "abc123" in html
    # ledger rendered as the assumptions block
    assert 'id="assumptions"' in html
    assert "environment.water_depth" in html


def test_assemble_report_writes_file(tmp_path) -> None:
    prov = Provenance().add("solver_queue", "completed/run_42")
    out = tmp_path / "r.html"
    result = assemble_report("R", provenance=prov, output_path=out)
    assert result == out and out.exists()
    assert "completed/run_42" in out.read_text()


def test_body_sections_render_before_provenance() -> None:
    prov = Provenance().add("file", "data.csv")

    def _body(data, **_):
        return '<div class="section" id="body">BODYMARK</div>'

    html = assemble_report(
        "R",
        provenance=prov,
        sections=[ReportSection("body", "Body", SectionMode.ALWAYS, _body)],
        data={},
    )
    assert html.index("BODYMARK") < html.index('id="provenance"')


# --- block builders ---------------------------------------------------------


def test_provenance_block_lists_each_source() -> None:
    prov = Provenance().add("spec", "a.yml").add("analysis_store", "store://x")
    block = provenance_block(prov)
    assert "a.yml" in block and "store://x" in block
    assert "view" in block.lower()  # view-not-copy framing


def test_assumption_ledger_block_empty_states_all_supplied() -> None:
    block = assumption_ledger_block(AssumptionLedger())
    assert "every value was user-supplied" in block


def test_assumption_ledger_block_renders_rows() -> None:
    block = assumption_ledger_block(_ledger())
    assert "environment.water_depth" in block
    assert "assumed_default" in block
    assert "Default screening depth" in block


def test_html_is_escaped_in_blocks() -> None:
    prov = Provenance().add("file", "<script>x</script>")
    block = provenance_block(prov)
    assert "<script>x</script>" not in block
    assert "&lt;script&gt;" in block


def test_datasource_optional_fields() -> None:
    ds = DataSource(kind="spec", identifier="s.yml")
    assert ds.digest is None and ds.retrieved_at is None
