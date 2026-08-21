# ABOUTME: Tests for the standard AceEngineer engineering calculation report.
# ABOUTME: Guards the section order, provenance colouring, and print-safety fix.
"""Tests for :mod:`digitalmodel.reporting.calc_report`.

The house format's value is that every report looks and reads the same, so the
things worth pinning are the section order, the provenance encoding, and the
completeness gate that stops a half-written report shipping.
"""

import pytest

from digitalmodel.reporting import (
    CALC_REPORT_SKELETON,
    CalcReport,
    Confidence,
    DataRow,
    DesignDataGroup,
    Equation,
    KPI,
    MethodBlock,
    Objective,
    Reference,
    ResultBlock,
    ValidationItem,
    VariableDef,
    WayForwardStage,
)

EXPECTED_SECTIONS = [
    "Objective",
    "Design data",
    "Analysis methodology",
    "Results",
    "Validation status",
    "Way forward",
    "References & provenance",
]


def minimal_report(**overrides) -> CalcReport:
    """A complete report with one entry per section."""
    base = dict(
        report_id="AE-TEST-001",
        title="Test calculation",
        discipline="Test",
        lede="A lede.",
        objective=Objective(purpose="Do the thing.", scope=["Scope: narrow"]),
        design_data=[DesignDataGroup(
            caption="Inputs",
            rows=[DataRow(label="Length", value="100", unit="ft")])],
        methodology=[MethodBlock(
            heading="Governing relations",
            equations=[Equation(
                markup="<var>y</var> = <var>x</var>",
                variables=[VariableDef(symbol="x", description="Input", unit="-")])])],
        results=[ResultBlock(
            heading="Primary result",
            confidence=Confidence.VALIDATED,
            caption="Checked against reference.")],
        validation=[ValidationItem(
            claim="Primary result", basis="Reference case",
            confidence=Confidence.VALIDATED)],
        way_forward=[WayForwardStage(heading="Stage A", prose="Next.")],
        references=[Reference(text="A standard.")],
    )
    base.update(overrides)
    return CalcReport(**base)


def test_house_section_order_is_fixed():
    """Section order is the standard; reports must not reorder it."""
    assert [s.label for s in CALC_REPORT_SKELETON.sections] == EXPECTED_SECTIONS


def test_complete_report_passes_the_completeness_gate():
    status = minimal_report().completeness()
    assert status.complete, status.summary()
    assert status.required_filled == status.required_total


def test_missing_section_fails_the_completeness_gate():
    """A report without results is not shippable, and says which slot is empty."""
    status = minimal_report(results=[]).completeness()
    assert not status.complete
    assert "results" in status.missing_blocks


def test_assumptions_are_optional_but_design_data_is_not():
    keys = {b.key: b.required
            for s in CALC_REPORT_SKELETON.sections for b in s.blocks}
    assert keys["design_data"] is True
    assert keys["assumptions"] is False


@pytest.mark.parametrize(
    "confidence,css,label",
    [
        (Confidence.VALIDATED, "cfd", "Validated"),
        (Confidence.ANALYTICAL, "ro", "Analytical"),
        (Confidence.PENDING, "proj", "Pending"),
    ],
)
def test_confidence_maps_to_house_provenance_colours(confidence, css, label):
    """Provenance is encoded as colour — the honesty guardrail."""
    assert confidence.css_class == css
    assert confidence.label == label


def test_result_block_requires_an_explicit_confidence():
    """A result with unstated standing is the failure the colour system prevents."""
    with pytest.raises(Exception):
        ResultBlock(heading="No confidence given")


def test_rendered_report_contains_every_section_and_is_self_contained():
    html = minimal_report().render_html()
    for heading in EXPECTED_SECTIONS:
        assert heading.replace("&", "&amp;") in html or heading in html
    # Self-contained: no external fetches, since CSP blocks CDNs and the file
    # must render from an email attachment.
    assert "http://" not in html
    assert "cdn." not in html


def test_equations_are_numbered_continuously_across_method_blocks():
    """Equation numbers must not restart per subsection."""
    report = minimal_report(methodology=[
        MethodBlock(heading="First", equations=[
            Equation(markup="<var>a</var>"), Equation(markup="<var>b</var>")]),
        MethodBlock(heading="Second", equations=[Equation(markup="<var>c</var>")]),
    ])
    html = report.render_html()
    for number in ("(1)", "(2)", "(3)"):
        assert number in html


def test_preliminary_flag_is_surfaced_in_the_report():
    """A preliminary report must say so where a reader cannot miss it."""
    html = minimal_report(preliminary=True).render_html()
    assert "Preliminary" in html
    assert "Results &mdash; preliminary" in html


def test_print_stylesheet_collapses_the_data_grid():
    """Regression: the 1240px screen grid truncated table values when printed.

    A client-facing report silently losing the right-hand column of its inputs
    is worse than an ugly one, so the print override ships with the standard
    rather than being remembered per report.
    """
    html = minimal_report().render_html()
    assert "@media print" in html
    assert ".datagrid { display: block !important; }" in html


def test_a_reference_with_a_url_renders_as_a_link():
    """A reference a reader cannot follow is a claim of provenance, not
    provenance. Where a source has a public URL, it should be reachable."""
    from digitalmodel.reporting.calc_report import Reference
    r = Reference(text="ITTC 7.5-03-01-01", url="https://example.org/ittc")
    report = minimal_report(references=[r])
    html_out = report.render_html()
    assert 'href="https://example.org/ittc"' in html_out
    assert "ITTC 7.5-03-01-01" in html_out
    assert 'rel="noopener noreferrer"' in html_out


def test_a_reference_without_a_url_is_still_valid():
    """Purchased standards and client-supplied models have no public URL.
    The field is where-applicable, never required."""
    from digitalmodel.reporting.calc_report import Reference
    report = minimal_report(references=[Reference(text="Client hull model")])
    html_out = report.render_html()
    assert "Client hull model" in html_out
    assert "<a href" not in html_out.split('class="refs"')[1].split("</ol>")[0]


def test_a_non_http_reference_url_is_not_linkified():
    """A javascript: or data: URL in a citation field is an injection vector
    and no legitimate reference needs one."""
    from digitalmodel.reporting.calc_report import Reference
    report = minimal_report(
        references=[Reference(text="bad", url="javascript:alert(1)")])
    html_out = report.render_html()
    assert "javascript:" not in html_out


def test_revision_history_renders_when_present():
    """The header carries the CURRENT revision; a reviewer holding an earlier
    copy needs the trail behind it."""
    from digitalmodel.reporting.calc_report import RevisionEntry
    report = minimal_report(revision_history=[
        RevisionEntry(revision="A", date="2026-08-21",
                      description="First issue", by="AE"),
        RevisionEntry(revision="B", date="2026-08-22",
                      description="Conditions updated"),
    ])
    html_out = report.render_html()
    assert "Revision history" in html_out
    assert "First issue" in html_out and "Conditions updated" in html_out


def test_revision_history_is_optional():
    """A first issue has no history and must not render an empty table."""
    assert "Revision history" not in minimal_report().render_html()
