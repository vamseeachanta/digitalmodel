# ABOUTME: Tests for the standard AceEngineer engineering calculation report.
# ABOUTME: Guards the section order, provenance colouring, and print-safety fix.
"""Tests for :mod:`digitalmodel.reporting.calc_report`.

The house format's value is that every report looks and reads the same, so the
things worth pinning are the section order, the provenance encoding, and the
completeness gate that stops a half-written report shipping.
"""

import re

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


def test_design_data_tables_stack_rather_than_sitting_abreast():
    """A multi-column datagrid truncated values, it did not merely tighten them.

    The track promised a 260px minimum, `.kv table{min-width:0}` defeated the
    440px floor this sheet sets for every other table, and `.kv{overflow:hidden}`
    turned the contradiction into values cut mid-word with no visual cue. Two
    client-facing reports lost data to it before it was found.
    """
    html_out = minimal_report().render_html()
    grid = re.search(r"\.datagrid\{[^}]*\}", html_out)
    assert grid, "the datagrid rule must exist"
    assert "grid-template-columns:1fr" in grid.group(0), grid.group(0)
    assert "auto-fit" not in grid.group(0), (
        "a multi-column track reintroduces the truncation")


def test_a_long_design_data_value_can_wrap_instead_of_being_clipped():
    """The label is sized to its content and the value takes the remainder, so
    a long value wraps rather than being clipped by the container."""
    html_out = minimal_report().render_html()
    assert re.search(r"\.kv td:last-child\{[^}]*white-space:normal", html_out), (
        "the value cell must be allowed to wrap")
    assert re.search(r"\.kv td:first-child\{[^}]*white-space:nowrap", html_out), (
        "the label cell must not wrap; that is what keeps a row on one line")


def test_print_uses_auto_table_layout_not_fixed():
    """`fixed` split every row 50/50, spending half the page measure on a short
    label while the value wrapped beside it."""
    html_out = minimal_report().render_html()
    assert "table-layout: auto" in html_out
    assert "table-layout: fixed" not in html_out


# ---------------------------------------------------------------------------
# The organisation marks
#
# A report carries the issuing organisation in three places that have to agree
# -- masthead, footer byline, foot-bar. They are driven from one field so they
# cannot drift apart, and that field defaults to the house mark so that adding
# it moved no report that already existed.
# ---------------------------------------------------------------------------

#: The three marks exactly as they rendered *before* `organisation` and
#: `organisation_logo` existed, captured from the previous revision of the
#: module. They are byte-for-byte, not fuzzy matches: the point of the default
#: is that every report already in a client's hands still renders the same.
HOUSE_MASTHEAD = '<div class="brand">Ace<b>Engineer</b> &middot; Test</div>'
HOUSE_BYLINE = "<p>Prepared by AceEngineer Test.</p>"
HOUSE_FOOT_BAR = '<div class="foot-bar"><span>AceEngineer &middot; Test</span>'

#: SHA-256 of the whole rendered document for the same report, captured at the
#: same point. The three assertions above name what changed if this moves; this
#: one catches a change anywhere else in the page. A deliberate house-format
#: change is expected to move it -- regenerate it then, having first read the
#: diff and confirmed the marks above are still intact.
HOUSE_RENDER_SHA256 = (
    "5f8da8c0b4bb55e634a8250ed51b74727c1922dfb28ef49a494cb17ac0d9c558")


def test_default_organisation_renders_the_house_marks_unchanged():
    """A report that sets neither field renders byte-identically to before."""
    import hashlib

    html_out = minimal_report().render_html()
    assert HOUSE_MASTHEAD in html_out, "the masthead wordmark moved"
    assert HOUSE_BYLINE in html_out, "the footer byline moved"
    assert HOUSE_FOOT_BAR in html_out, "the foot-bar mark moved"
    assert "brand-logo" not in html_out, (
        "the house masthead is a wordmark; no logo element belongs in it")
    assert hashlib.sha256(html_out.encode()).hexdigest() == HOUSE_RENDER_SHA256, (
        "the rendered house report is no longer byte-identical; see "
        "HOUSE_RENDER_SHA256")


def test_organisation_carries_to_all_three_marks():
    """Setting the organisation moves every mark, not just the masthead.

    A masthead that says one organisation over a footer that says another is
    the mixed-branding defect this field exists to prevent.
    """
    html_out = minimal_report(organisation="Nordwind Marine Ltd").render_html()
    assert '<div class="brand">Nordwind Marine Ltd &middot; Test</div>' in html_out
    assert "<p>Prepared by Nordwind Marine Ltd Test.</p>" in html_out
    assert ('<div class="foot-bar"><span>Nordwind Marine Ltd &middot; Test</span>'
            in html_out)
    assert "AceEngineer" not in html_out, (
        "no house mark may survive in a report issued by someone else")


def test_only_the_house_name_gets_the_two_tone_wordmark():
    """Another organisation's name is set plain, not split at a guessed seam."""
    html_out = minimal_report(organisation="Ace Marine").render_html()
    assert "<b>" not in re.search(
        r'<div class="brand">.*?</div>', html_out, re.S).group(0)


def test_an_organisation_logo_renders_in_the_masthead():
    pixel = ("data:image/gif;base64,"
             "R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7")
    html_out = minimal_report(
        organisation="Nordwind Marine Ltd", organisation_logo=pixel).render_html()
    brand = re.search(r'<div class="brand">.*?</div>', html_out, re.S).group(0)
    assert f'src="{pixel}"' in brand
    assert 'alt="Nordwind Marine Ltd"' in brand, (
        "the logo carries the organisation as its accessible name")
    assert brand.index("<img") < brand.index("Nordwind"), (
        "the logo precedes the name on the first line")


def test_a_logo_that_is_not_self_contained_is_refused():
    """A remote logo leaves a broken image on a client reading offline, and an
    arbitrary URI in an ``src`` is the injection surface the reference links
    are already guarded against."""
    report = minimal_report(organisation_logo="https://example.invalid/logo.png")
    with pytest.raises(ValueError, match="data:image/"):
        report.render_html()


# ---------------------------------------------------------------------------
# The two revision tiers
#
# A client-facing report carries two different kinds of "revision" and they
# must never be confused for one another:
#
#   * the MAIN revision -- letters A, B, C -- which moves only when the report
#     is issued to the client. It is set by hand, never derived, never
#     auto-incremented. It is the record of what the client has been given.
#   * the INTERNAL revisions -- A.1, A.2, ... nested under the main letter that
#     is currently pending -- which record what staff changed between issues.
#
# The main table stays first and stays the client-facing record; the internal
# log sits beneath it, labelled so a client reading the PDF cannot mistake one
# for the other. `incorporates` on a main row names the internal range that
# issue folded in, which is the link between the two tiers.
# ---------------------------------------------------------------------------


def _revised_report(**overrides):
    """A report carrying the pre-existing single-tier revision history."""
    from digitalmodel.reporting.calc_report import RevisionEntry
    base = dict(revision_history=[
        RevisionEntry(revision="A", date="2026-08-21",
                      description="First issue", by="AE"),
    ])
    base.update(overrides)
    return minimal_report(**base)


#: The revision-history section exactly as it rendered *before* the internal
#: tier existed. Byte-for-byte, for the same reason the organisation marks are:
#: a report that does not opt into the second tier must be unmoved by its
#: arrival. If this string has to change, the change is not backward
#: compatible and the two-tier feature has leaked into the single-tier path.
SINGLE_TIER_REVHIST = (
    '<table class="revhist"><thead><tr><th>Rev</th><th>Date</th>'
    '<th>Description</th><th>By</th></tr></thead>'
    '<tbody><tr><td>A</td><td>2026-08-21</td>'
    '<td>First issue</td><td>AE</td></tr></tbody></table>'
)

#: SHA-256 of the whole rendered document for that same single-tier report.
#: HOUSE_RENDER_SHA256 above covers a report with no revision history at all;
#: this one covers the report that has one and declines the second tier, which
#: is the case the internal-revision work could most easily have moved.
#: Captured by rendering this exact report against the module as it stood
#: *before* `InternalRevision` and `incorporates` were added, not by copying
#: the value the changed code happened to produce.
SINGLE_TIER_RENDER_SHA256 = (
    "8bc5345b0ea3e25af0ae3d6e52d100aabf7aa73ccf9f98fa389eba72f8edbabf")


def test_a_single_tier_report_is_unmoved_by_the_internal_tier():
    """Setting neither `internal_revisions` nor `incorporates` renders exactly
    what rendered before either field existed."""
    import hashlib

    html_out = _revised_report().render_html()
    assert SINGLE_TIER_REVHIST in html_out, (
        "the single-tier revision table moved; the two-tier feature has "
        "leaked into the path that does not use it")
    assert "Internal revisions" not in html_out, (
        "a report with no internal revisions must not render the subsection")
    assert "Incorporates" not in html_out, (
        "the Incorporates column must not appear when no row names a range")
    assert hashlib.sha256(html_out.encode()).hexdigest() == SINGLE_TIER_RENDER_SHA256


def test_internal_revisions_render_beneath_the_main_table():
    """The main table stays first; the internal log sits under it, labelled."""
    from digitalmodel.reporting.calc_report import InternalRevision

    html_out = _revised_report(internal_revisions=[
        InternalRevision(revision="A.2", date="2026-08-22",
                         description="Waterline lengths corrected", by="Naval architecture"),
        InternalRevision(revision="A.1", date="2026-08-21",
                         description="First assembly", by="Naval architecture"),
    ]).render_html()

    assert "Issued revisions" in html_out and "Internal revisions" in html_out
    # Order on the page is the whole point: the client-facing record first.
    assert html_out.index("Issued revisions") < html_out.index("Internal revisions")
    # And the internal rows are below the main row, not interleaved with it.
    assert html_out.index("First issue") < html_out.index("Waterline lengths corrected")
    assert "A.1" in html_out and "A.2" in html_out


def test_the_internal_table_reuses_the_house_revision_styling():
    """A second visual language for the same kind of content is a defect."""
    from digitalmodel.reporting.calc_report import InternalRevision

    html_out = _revised_report(internal_revisions=[
        InternalRevision(revision="A.1", date="2026-08-21",
                         description="First assembly", by="Naval architecture"),
    ]).render_html()
    assert html_out.count('<table class="revhist">') == 2, (
        "both tiers render as the house revision table")


def test_the_caller_order_of_internal_revisions_is_preserved():
    """The module renders what it is given; it does not re-sort.

    Descending order is the house convention, but sorting "A.10" against
    "A.9" as strings would put them in the wrong order, and the caller
    already knows the order it means.
    """
    from digitalmodel.reporting.calc_report import InternalRevision

    html_out = _revised_report(internal_revisions=[
        InternalRevision(revision="A.10", date="2026-08-22", description="Tenth"),
        InternalRevision(revision="A.9", date="2026-08-21", description="Ninth"),
    ]).render_html()
    assert html_out.index("Tenth") < html_out.index("Ninth")


def test_a_main_row_can_name_the_internal_range_it_incorporates():
    """The link between the tiers: which internal work an issue folded in."""
    from digitalmodel.reporting.calc_report import InternalRevision, RevisionEntry

    html_out = minimal_report(
        revision_history=[RevisionEntry(
            revision="A", date="TBA", description="First issue",
            incorporates="A.1–A.6", by="Naval architecture")],
        internal_revisions=[InternalRevision(
            revision="A.1", date="2026-08-20", description="First assembly")],
    ).render_html()
    assert "<th>Incorporates</th>" in html_out
    assert "A.1–A.6" in html_out
    # The column sits in the main table, between the description and the by.
    assert re.search(
        r"<th>Description</th><th>Incorporates</th><th>By</th>", html_out)


def test_the_main_revision_is_never_derived_from_the_internal_ones():
    """The main letter moves only on issue to the client, by hand.

    Ten internal revisions under a pending Rev A leave the header at Rev A.
    """
    from digitalmodel.reporting.calc_report import InternalRevision

    report = _revised_report(internal_revisions=[
        InternalRevision(revision=f"A.{i}", date="2026-08-21",
                         description=f"Change {i}")
        for i in range(1, 11)])
    assert report.revision == "A"
    assert "Rev A" in report.render_html()
    assert "Rev B" not in report.render_html()


def test_internal_revisions_alone_still_render_the_section():
    """A report whose first issue has not happened yet still has a change log."""
    from digitalmodel.reporting.calc_report import InternalRevision

    html_out = minimal_report(internal_revisions=[
        InternalRevision(revision="A.1", date="2026-08-20",
                         description="First assembly")]).render_html()
    assert "Revision history" in html_out and "Internal revisions" in html_out


def test_internal_revisions_default_to_empty():
    """No existing report changes shape by upgrading."""
    assert minimal_report().internal_revisions == []
