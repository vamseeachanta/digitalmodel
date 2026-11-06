"""HTML layout audit examples; no engineering values or solver execution.

Source: reporting/calc_report.py, CalcReport._section/_toc and MethodBlock.render;
reporting/assets/calc_report_format.html, numbered L1/L2 sections (repository
revision at introduction). Fixture labels are independent ordinal calculations:
the first section's first and second subsections are 1.1 and 1.2; the next
section's first subsection is 2.1. These are document positions, not limits.

Plan-lite: before merge, run this module to confirm assessment isolation across
valid/invalid/valid documents and the appended third section's 3.1 ordinal.
Separate negative controls will check contents order and subsection labels.
The audit implementation will remain unchanged; CSS/browser visibility will
remain NOT_EVALUATED.
"""

import importlib.util
from pathlib import Path

import pytest

from digitalmodel.reporting.layout_audit import audit_calc_report_html


DOCUMENT = """
<nav class="toc"><a href="#first">First</a><a href="#next">Next</a></nav>
<main class="main">
  <section id="first">
    <div class="l2" id="method-a"><div class="l2head">
      <span class="n">1.1</span><h3>First method</h3></div></div>
    <div class="l2" id="method-b"><div class="l2head">
      <span class="n">1.2</span><h3>Second method</h3></div></div>
  </section>
  <section id="next">
    <div class="l2" id="result"><div class="l2head">
      <span class="n">2.1</span><h3>Result</h3></div></div>
  </section>
</main>
"""


def test_explicit_labels_follow_document_order_and_reset_per_section():
    checks = audit_calc_report_html(DOCUMENT)
    assert checks["anchors"].status == "PASS"
    assert checks["contents_order"].status == "PASS"
    assert checks["subsection_labels"].status == "PASS"
    assert checks["visible_numbering"].status == "NOT_EVALUATED"


def test_layout_assessment_is_independent_between_documents():
    """Hand count: each unrelated section has its first subsection, 1.1/2.1.

    Both labels below are wrong; repeated IDs and reversed contents are
    separate structural failures. No browser visibility is inferred.
    """
    first_checks = audit_calc_report_html(DOCUMENT)
    assert first_checks["anchors"].status == "PASS"
    assert first_checks["contents_order"].status == "PASS"
    assert first_checks["subsection_labels"].status == "PASS"
    assert first_checks["visible_numbering"].status == "NOT_EVALUATED"
    original_checks = first_checks.copy()

    unrelated_document = """
    <nav class="toc"><a href="#closing">Closing</a><a href="#opening">Opening</a></nav>
    <main class="main">
      <section id="opening">
        <div class="l2" id="repeated"><div class="l2head">
          <span class="n">1.2</span><h3>Opening method</h3></div></div>
      </section>
      <section id="closing">
        <div class="l2" id="repeated"><div class="l2head">
          <span class="n">2.2</span><h3>Closing method</h3></div></div>
      </section>
    </main>
    """
    unrelated_checks = audit_calc_report_html(unrelated_document)
    assert unrelated_checks["anchors"].status == "FAIL"
    assert unrelated_checks["contents_order"].status == "FAIL"
    assert unrelated_checks["subsection_labels"].status == "FAIL"
    assert unrelated_checks["visible_numbering"].status == "NOT_EVALUATED"
    assert first_checks == original_checks

    repeated_checks = audit_calc_report_html(DOCUMENT)
    assert repeated_checks == original_checks
    assert repeated_checks["visible_numbering"].status == "NOT_EVALUATED"


def test_appended_section_uses_document_ordinal():
    """Hand count: after DOCUMENT's two sections, the new first label is 3.1.

    A 2.2 label would instead continue the second section's subsection count.
    Contents positions are first, next, appended, independently of labels.
    """
    document = DOCUMENT.replace(
        "</nav>", '<a href="#appended">Appended</a></nav>'
    ).replace(
        "</main>",
        """
        <section id="appended">
          <div class="l2" id="appended-method"><div class="l2head">
            <span class="n">3.1</span><h3>Appended method</h3></div></div>
        </section>
        </main>
        """,
    )
    checks = audit_calc_report_html(document)
    assert checks["anchors"].status == "PASS"
    assert checks["contents_order"].status == "PASS"
    assert checks["subsection_labels"].status == "PASS"
    assert checks["visible_numbering"].status == "NOT_EVALUATED"

    wrong_contents = document.replace(
        '<a href="#next">Next</a><a href="#appended">Appended</a>',
        '<a href="#appended">Appended</a><a href="#next">Next</a>',
    )
    contents_checks = audit_calc_report_html(wrong_contents)
    assert contents_checks["anchors"].status == "PASS"
    assert contents_checks["contents_order"].status == "FAIL"
    assert contents_checks["subsection_labels"].status == "PASS"
    assert contents_checks["visible_numbering"].status == "NOT_EVALUATED"

    wrong_label = document.replace(">3.1<", ">2.2<")
    label_checks = audit_calc_report_html(wrong_label)
    assert label_checks["anchors"].status == "PASS"
    assert label_checks["contents_order"].status == "PASS"
    assert label_checks["subsection_labels"].status == "FAIL"
    assert "3.1" in " ".join(label_checks["subsection_labels"].details)
    assert label_checks["visible_numbering"].status == "NOT_EVALUATED"


@pytest.mark.parametrize("replacement", ['id="method-a"', 'id=""'])
def test_duplicate_or_empty_anchor_is_reported(replacement):
    checks = audit_calc_report_html(DOCUMENT.replace('id="method-b"', replacement))
    assert checks["anchors"].status == "FAIL"
    assert checks["anchors"].details


@pytest.mark.parametrize(
    "links",
    [
        '<a href="#next">Next</a><a href="#first">First</a>',
        '<a href="#missing">Missing</a>',
        '<a href="#method-a">Not a section</a>',
        '<a href="#first">First</a><a href="#first">Repeated</a>',
        '<a href="other.html#first">External</a>',
    ],
)
def test_contents_must_resolve_to_unique_sections_in_document_order(links):
    start, end = DOCUMENT.index("<nav"), DOCUMENT.index("</nav>")
    document = DOCUMENT[:start] + f'<nav class="toc">{links}' + DOCUMENT[end:]
    assert audit_calc_report_html(document)["contents_order"].status == "FAIL"


@pytest.mark.parametrize("label", ["1.1", "2.2", "incorrect"])
def test_wrong_subsection_label_is_reported(label):
    checks = audit_calc_report_html(DOCUMENT.replace(">2.1<", f">{label}<"))
    assert checks["subsection_labels"].status == "FAIL"
    assert "2.1" in " ".join(checks["subsection_labels"].details)


def test_missing_number_element_is_a_structural_failure():
    checks = audit_calc_report_html(DOCUMENT.replace('<span class="n">2.1</span>', ""))
    assert checks["subsection_labels"].status == "FAIL"


@pytest.mark.parametrize(
    "css",
    [
        '.l2head .n::after {content: counter(sec) "." counter(sub)}',
        ".l2head .n::after {content: none}",
        ".l2head .n {display: none}",
    ],
)
def test_empty_labels_never_pass_on_css_text_alone(css):
    document = f"<style>{css}</style>" + DOCUMENT.replace(">2.1<", "><")
    checks = audit_calc_report_html(document)
    assert checks["subsection_labels"].status == "NOT_EVALUATED"
    assert checks["visible_numbering"].status == "NOT_EVALUATED"


@pytest.mark.parametrize("document", ["", "<main></main>", "<p>Not a report</p>"])
def test_absent_layout_is_not_a_passing_assessment(document):
    checks = audit_calc_report_html(document)
    assert all(check.status == "NOT_EVALUATED" for check in checks.values())


def test_explicit_mismatch_takes_precedence_over_unresolved_css_labels():
    document = DOCUMENT.replace(">1.1<", "><").replace(">2.1<", ">wrong<")
    assert audit_calc_report_html(document)["subsection_labels"].status == "FAIL"


def test_duplicate_section_id_also_invalidates_contents_resolution():
    document = DOCUMENT.replace('id="next"', 'id="first"')
    checks = audit_calc_report_html(document)
    assert checks["anchors"].status == "FAIL"
    assert checks["contents_order"].status == "FAIL"


def test_contents_target_cannot_collide_with_nonsection_anchor():
    document = '<div id="first"></div>' + DOCUMENT
    checks = audit_calc_report_html(document)
    assert checks["contents_order"].status == "FAIL"


def test_percent_encoded_fragment_resolves_to_section_id():
    document = DOCUMENT.replace('href="#first"', 'href="#%66irst"')
    assert audit_calc_report_html(document)["contents_order"].status == "PASS"


def test_number_like_equation_spans_do_not_count_as_subsections():
    document = DOCUMENT.replace(
        "<h3>Result</h3>", '<h3>Result</h3><span class="n">x</span>'
    )
    assert audit_calc_report_html(document)["subsection_labels"].status == "PASS"


def test_current_concrete_consumer_without_running_pdf_entry_point():
    """Inventory: sole concrete MethodBlock builder; no stylesheet override.

    Source: docs/collide_pe/dynacard-undulations/build_report.py, report.
    The expected structure follows unique IDs and ascending section positions;
    empty number spans require CSS rendering and cannot establish visibility.
    No engineering result from this consumer is asserted or qualified here.
    """
    root = Path(__file__).resolve().parents[2]
    path = root / "docs/collide_pe/dynacard-undulations/build_report.py"
    spec = importlib.util.spec_from_file_location("layout_consumer", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    checks = audit_calc_report_html(module.report.render_html())
    assert checks["anchors"].status == "PASS"
    assert checks["contents_order"].status == "PASS"
    assert checks["subsection_labels"].status == "NOT_EVALUATED"
    assert checks["visible_numbering"].status == "NOT_EVALUATED"
