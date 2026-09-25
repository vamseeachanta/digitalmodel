# ABOUTME: Golden-HTML and determinism tests for the standard report engine.
# ABOUTME: Plain golden byte-equal; Plotly golden equal after stripping plotly.js.
"""Tests for :mod:`digitalmodel.reporting.engine` and ``figures``.

Regenerate the goldens after an intentional template/CSS change with::

    UPDATE_GOLDENS=1 pytest tests/reporting/test_engine_golden.py
"""

from __future__ import annotations

import json
import os
import re
from pathlib import Path

import pytest

from digitalmodel.citations.schema import Citation
from digitalmodel.reporting import (
    DocumentMeta,
    FigureBlock,
    Provenance,
    ProvenanceError,
    ReportSpec,
    Section,
    StandardLabel,
    StatusBlock,
    TableBlock,
    TextBlock,
    figure_from_columns,
    markdown_to_html,
    plotly_div,
    render_html,
    write_report,
)

GOLDEN_DIR = Path(__file__).parent / "golden"
WIKI = "wikis/marine-engineering/wiki/standards/dnv-rp-b401.md"
_PLOTLY_SCRIPT_RE = re.compile(
    r'<script type="text/javascript">.*?</script>\n', re.S
)


def fixture_spec(*, plotly: bool = False) -> ReportSpec:
    """A small but complete spec exercising every block type."""
    blocks = [
        TextBlock(
            markdown=(
                "Sacrificial anode design per **DNV-RP-B401**.\n\n"
                "- coating breakdown from Table 10-1\n"
                "- seawater resistivity `0.3 ohm.m`\n\n"
                "## Cases\n\n"
                "1. Initial\n2. Mean\n3. Final"
            )
        ),
        TableBlock(
            title="Current demand by zone",
            columns=["Zone", "Area", "Initial", "Final"],
            units=["-", "m2", "A", "A"],
            rows=[["Splash", 120.5, 3.2e-4, 0.041], ["Submerged", 980, 12.0, 15.25]],
            source="cp.demand()",
        ),
        StatusBlock(
            label="Anode mass adequate",
            status="FAIL",
            governing_case="Final, zone Submerged",
            detail="400 kg supplied < 520 kg required",
        ),
        StatusBlock(label="Current output adequate", status="PASS"),
    ]
    if plotly:
        blocks.append(
            FigureBlock(
                title="Current demand vs time",
                caption="Demand per zone over the design life.",
                figure_id="fig-demand",
                plotly=figure_from_columns(
                    "line", [0, 10, 20], {"Splash": [1.0, 1.5, 2.0], "Submerged": [12, 13, 15]},
                    title="Current demand", x_label="years", y_label="A",
                ),
            )
        )
    else:
        blocks.append(
            FigureBlock(
                title="Anode sketch",
                caption="Schematic only.",
                figure_id="fig-sketch",
                svg='<svg xmlns="http://www.w3.org/2000/svg" width="80" height="20">'
                    '<rect width="80" height="20"/></svg>',
            )
        )
    return ReportSpec(
        document=DocumentMeta(
            number="B0000-RPT-001-00",
            revision="00",
            title="CP anode design <example>",
            project="B0000",
            client="Client & Co",
            date="2026-09-25",
            prepared_by="AP",
            checked_by="CP",
            approved_by="XP",
        ),
        standards=[StandardLabel(code_id="DNV-RP-B401", edition="2021", provenance=WIKI)],
        citations=[
            Citation(code_id="DNV-RP-B401", publisher="DNV", revision="2021",
                     section="Table 10-1", wiki_path=WIKI, note="coating breakdown"),
        ],
        sections=[Section(key="design", title="Anode design", subtitle="Basis", blocks=blocks)],
        provenance=Provenance().add("file", "inputs.yml", digest="sha256:abc",
                                    description="routed config"),
        input_echo={"environment": {"depth_m": 100.0, "temp_c": 12}, "zones": ["Splash", "Submerged"]},
        appendices=[Section(key="records", title="Calculation records",
                            blocks=[TextBlock(markdown="See `results.csv`.")])],
        tool_version="0.0-test",
    )


def _normalise(text: str) -> str:
    return text.replace("\r\n", "\n")


def _check_golden(name: str, html: str) -> None:
    path = GOLDEN_DIR / name
    if os.environ.get("UPDATE_GOLDENS"):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(html, encoding="utf-8", newline="\n")
    expected = _normalise(path.read_text(encoding="utf-8"))
    assert _normalise(html) == expected, f"{name} differs; UPDATE_GOLDENS=1 to accept"


# --- goldens ---------------------------------------------------------------


def test_plain_report_matches_golden():
    html = render_html(fixture_spec())
    assert "<script" not in html
    _check_golden("standard_report_plain.html", html)


def test_plotly_report_matches_golden_without_inline_bundle():
    html = render_html(fixture_spec(plotly=True))
    assert html.count("<script type=\"text/javascript\">") == 2  # bundle + figure
    stripped, n = _PLOTLY_SCRIPT_RE.subn("<!-- plotly.js stripped -->\n", html, count=1)
    assert n == 1
    assert "Plotly.newPlot" in stripped
    _check_golden("standard_report_plotly.html", stripped)


# --- invariants ------------------------------------------------------------


@pytest.mark.parametrize("plotly", [False, True])
def test_html_is_offline_and_printable(plotly):
    html = render_html(fixture_spec(plotly=plotly))
    assert "<script src=" not in html
    assert "https://" not in html.split("<body")[1]  # no CDN/link leaks in body
    assert "@media print" in html
    assert ("window.PLOTLYENV" in html) is plotly


def test_standard_chips_in_header_and_footer():
    html = render_html(fixture_spec())
    header = html.split('id="header-standards"')[1].split("</div>")[0]
    footer = html.split('id="footer-standards"')[1].split("</div>")[0]
    for part in (header, footer):
        assert 'class="chip std"' in part
        assert "DNV-RP-B401" in part
        assert '<span class="ed">2021</span>' in part
        assert WIKI in part


def test_document_control_and_input_echo_present():
    html = render_html(fixture_spec())
    assert "<title>B0000-RPT-001-00 Rev 00 - CP anode design &lt;example&gt;</title>" in html
    assert "Client &amp; Co" in html
    assert "<td>Date</td><td>2026-09-25</td>" in html
    assert 'id="input-echo"' in html
    assert "<td>environment.depth_m</td><td>100</td>" in html
    assert "<td>zones</td><td>[&#34;Splash&#34;, &#34;Submerged&#34;]</td>" in html
    assert "<td>Table 10-1</td>" in html  # references cited
    assert "<td>sha256:abc</td>" in html  # data sources
    assert "Appendix A" in html


def test_status_blocks_render_pass_and_fail():
    html = render_html(fixture_spec())
    assert '<div class="st fail">' in html
    assert "<b>Governing case:</b> Final, zone Submerged" in html
    assert '<div class="st ok">' in html


def test_two_renders_are_byte_identical():
    assert render_html(fixture_spec(plotly=True)) == render_html(fixture_spec(plotly=True))


def test_missing_provenance_fails_closed():
    spec = fixture_spec()
    spec.provenance.sources.clear()
    with pytest.raises(ProvenanceError):
        render_html(spec)


def test_write_report_artifacts_and_manifest(tmp_path):
    artifacts = write_report(fixture_spec(), tmp_path / "out", "cp_design", pdf="off")
    assert artifacts.html_path.name == "cp_design.html"
    assert artifacts.pdf_path is None
    assert artifacts.pdf_status.rendered is False
    citations = json.loads(artifacts.citations_json_path.read_text(encoding="utf-8"))
    assert citations["citations"][0]["code_id"] == "DNV-RP-B401"
    raw = artifacts.manifest_path.read_text(encoding="utf-8")
    manifest = json.loads(raw)
    assert list(manifest) == sorted(manifest)
    assert manifest["artifacts"] == {
        "citations": "cp_design_citations.json",
        "html": "cp_design.html",
        "manifest": "cp_design_manifest.json",
        "pdf": None,
    }
    assert manifest["standards"] == [{"code_id": "DNV-RP-B401", "edition": "2021",
                                      "provenance": WIKI}]
    assert manifest["citations_count"] == 1
    assert manifest["pdf"]["message"] == "pdf rendering disabled (pdf: off)"
    assert manifest["tool_version"] == "0.0-test"
    assert "timestamp" not in raw and "generated_at" not in raw
    # Re-running produces byte-identical artifacts.
    again = write_report(fixture_spec(), tmp_path / "out2", "cp_design", pdf="off")
    for a, b in ((artifacts.html_path, again.html_path),
                 (artifacts.citations_json_path, again.citations_json_path),
                 (artifacts.manifest_path, again.manifest_path)):
        assert a.read_bytes() == b.read_bytes()


def test_image_figure_embedded_as_data_uri(tmp_path):
    png = tmp_path / "fig.png"
    png.write_bytes(b"\x89PNG\r\n\x1a\n" + b"\x00" * 8)
    spec = fixture_spec()
    spec.sections[0].blocks.append(
        FigureBlock(title="Raster", figure_id="fig-raster", image_path=str(png)))
    html = render_html(spec)
    assert 'src="data:image/png;base64,' in html
    spec.sections[0].blocks[-1] = FigureBlock(title="Missing", figure_id="fig-missing",
                                              image_path=str(tmp_path / "nope.png"))
    with pytest.raises(FileNotFoundError):
        render_html(spec)


# --- figures ---------------------------------------------------------------


def test_plotly_div_is_stable_and_uses_the_figure_id():
    fig = {"data": [{"type": "scatter", "y": [2, 1], "x": [0, 1]}], "layout": {"title": {"text": "T"}}}
    first = plotly_div(fig, "fig-a")
    reordered = {"layout": {"title": {"text": "T"}}, "data": [{"x": [0, 1], "y": [2, 1], "type": "scatter"}]}
    assert first == plotly_div(reordered, "fig-a")
    assert 'id="fig-a"' in first
    assert "<script src=" not in first
    assert '"x":[0,1],"y":[2,1]' in first  # sorted keys inside each trace


def test_plotly_div_rejects_invalid_figures():
    with pytest.raises(ValueError):
        plotly_div({"data": [{"type": "scatter", "bogus": 1}]}, "fig-bad")


def test_figure_from_columns_shapes():
    fig = figure_from_columns("bar", ["a", "b"], {"s": [1, 2]}, title="t", x_label="x", y_label="y")
    assert fig["data"][0]["type"] == "bar"
    assert fig["layout"]["barmode"] == "group"
    line = figure_from_columns("line", [0, 1], {"s": [1, 2]})
    assert line["data"][0]["mode"] == "lines+markers"
    with pytest.raises(ValueError, match="kind must be"):
        figure_from_columns("pie", [0], {"s": [1]})  # type: ignore[arg-type]
    with pytest.raises(ValueError, match="has 1 values for 2 x values"):
        figure_from_columns("line", [0, 1], {"s": [1]})
    with pytest.raises(ValueError, match="at least one"):
        figure_from_columns("line", [0, 1], {})


# --- markdown subset -------------------------------------------------------


def test_markdown_subset_escapes_and_structures():
    html = markdown_to_html("# H\n\npara <b>x</b> **bold**\n\n- one\n- two\n\n```\ncode <x>\n```")
    assert html.split("\n") == [
        "<h3>H</h3>",
        "<p>para &lt;b&gt;x&lt;/b&gt; <strong>bold</strong></p>",
        "<ul><li>one</li><li>two</li></ul>",
        "<pre><code>code &lt;x&gt;</code></pre>",
    ]
