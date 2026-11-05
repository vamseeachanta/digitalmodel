# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Namespace and label placement controls for static report graphics."""

from html.parser import HTMLParser

import pytest

from digitalmodel.reporting import layout_audit


def graphic(label):
    return f'<svg id="curve" viewBox="0 0 100 40"><text x="10" y="20">{label}</text></svg>'


@pytest.mark.parametrize("tag, expected", [("span", "FAIL"), ("tspan", "PASS")])
def test_shared_layout_check_distinguishes_html_and_svg_labels(tag, expected):
    document = graphic(f"<{tag}>12.5</{tag}>")

    class TextOnly(HTMLParser):
        text = ""

        def handle_data(self, data):
            self.text += data

    parser = TextOnly()
    parser.feed(document)
    assert parser.text == "12.5"  # Both variants pass the old text-only oracle.
    assert layout_audit.audit_calc_report_html(document)["svg_text"].status == expected


@pytest.mark.parametrize("namespace", ["", ' xmlns="http://www.w3.org/2000/svg"'])
def test_svg_labels_accept_implicit_and_explicit_namespaces(namespace):
    document = graphic('<tspan class="value">12.5</tspan>').replace(
        '<svg ', f'<svg{namespace} ', 1
    )
    check = layout_audit.audit_svg_text(document, expected_labels={"curve": ["12.5"]})
    assert check.status == "PASS"
    assert check.details == ()


@pytest.mark.parametrize(
    "document",
    [
        graphic("<span>12.5</span>"),
        graphic('<foreignObject><span>12.5</span></foreignObject>'),
        graphic('<foreignObject><tspan>12.5</tspan></foreignObject>'),
    ],
)
def test_html_namespace_descendants_of_svg_text_fail(document):
    check = layout_audit.audit_svg_text(document)
    assert check.status == "FAIL"
    assert check.details


@pytest.mark.parametrize(
    "document",
    [
        graphic("13.0") + "<p>12.5</p>",
        graphic("") + '<svg><text>12.5</text></svg>',
        '<svg id="curve"><tspan>12.5</tspan></svg>',
        '<svg id="curve"><foreignObject><p>12.5</p></foreignObject></svg>',
        graphic("12.5") + graphic("12.5"),
        graphic("12.5") + '<div id="curve">12.5</div>',
        graphic("<!--12.5-->"),
        '<div id="curve">12.5</div>',
        "",
    ],
)
def test_numeric_label_must_belong_to_text_in_the_named_graphic(document):
    check = layout_audit.audit_svg_text(document, expected_labels={"curve": ["12.5"]})
    assert check.status == "FAIL"
    assert check.details


def test_numeric_labels_check_values_and_multiplicity_without_changing_markup():
    document = graphic('<tspan>12.5</tspan> &amp; <tspan>7.0</tspan>')
    assert layout_audit.audit_svg_text(
        document, expected_labels={"curve": ["12.5 & 7.0"]}
    ).status == "PASS"
    assert layout_audit.audit_svg_text(
        document, expected_labels={"curve": ["12.5 & 7.0", "12.5 & 7.0"]}
    ).status == "FAIL"
    assert layout_audit.audit_svg_text(
        document, expected_labels={"curve": ["12.5 & 8.0"]}
    ).status == "FAIL"


def test_legitimate_html_in_foreign_object_outside_text_is_allowed():
    document = '<svg><foreignObject><p>Caption</p></foreignObject></svg>' + graphic("12.5")
    assert layout_audit.audit_svg_text(document).status == "PASS"


@pytest.mark.parametrize("document", ["", "<p>12.5</p>", "<svg><path/></svg>"])
def test_absent_svg_text_has_no_passing_label_assessment(document):
    assert layout_audit.audit_svg_text(document).status == "NOT_EVALUATED"


def test_script_and_escaped_examples_do_not_create_graphics():
    document = '<script>const example = "<svg><text><span>12.5</span></text></svg>";</script>'
    document += "<pre>&lt;svg&gt;&lt;text&gt;12.5&lt;/text&gt;&lt;/svg&gt;</pre>"
    assert layout_audit.audit_svg_text(document).status == "NOT_EVALUATED"


def test_unrelated_math_recovery_does_not_fail_svg_assessment():
    document = '<math><span>Equation prose</span></math>' + graphic("12.5")
    assert layout_audit.audit_svg_text(document).status == "PASS"


@pytest.mark.parametrize("tag, expected", [("span", "FAIL"), ("tspan", "PASS")])
def test_calc_report_generated_markup_uses_shared_namespace_qa(tag, expected):
    from digitalmodel.reporting import CalcReport, MethodBlock, Objective

    svg = graphic(f'<{tag} class="value">12.5</{tag}>')
    report = CalcReport(
        report_id="TEST-RPT-001",
        title="Synthetic label example",
        discipline="Test",
        lede="Synthetic inputs for structural checks.",
        objective=Objective(purpose="Check SVG text structure."),
        methodology=[MethodBlock(heading="Graphic", figure_svg=svg)],
    )
    before = report.model_dump()
    document = report.render_html()
    assert svg in document
    assert layout_audit.audit_calc_report_html(document)["svg_text"].status == expected
    assert layout_audit.audit_svg_text(
        document, expected_labels={"curve": ["12.5"]}
    ).status == expected
    assert report.model_dump() == before
    assert document == report.render_html()
