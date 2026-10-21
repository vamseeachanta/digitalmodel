"""Public citation metadata reaches reference rows without unsafe HTML."""

from dataclasses import asdict

from digitalmodel.citations import public_references
from digitalmodel.citations.schema import Citation
from digitalmodel.reporting import (
    DocumentMeta,
    Provenance,
    ReportSpec,
    Section,
    TextBlock,
    render_html,
)


def report(citations):
    return ReportSpec(
        document=DocumentMeta(
            number="B0000-RPT-001-00", revision="00", title="Reference analysis",
            project="Generic example", client="Not applicable",
        ),
        sections=[Section(key="summary", title="Summary", blocks=[TextBlock(markdown="Reference metadata only.")])],
        provenance=Provenance().add("calculation", "synthetic-test"),
        citations=citations,
    )


def references(html):
    return html.split('<section id="references-cited">', 1)[1].split("</section>", 1)[0]


def test_public_references_survive_mapping_and_json_round_trip():
    original = [public_references.get_public_reference(key) for key in (
        "NASA-DRAG-EQUATION", "ORCINA-LINKS-DATA", "PASSING-SHIP-METHOD",
    )]
    spec = report([asdict(citation) for citation in original])
    restored = ReportSpec.model_validate_json(spec.model_dump_json())
    assert restored.citations == [asdict(citation) for citation in original]
    html = render_html(restored)
    rows = references(html)
    assert rows.count("2026-10-20") == 3
    for citation in original:
        assert citation.code_id in rows
        assert citation.publisher in rows
        assert (citation.source_url or citation.repository_path) in rows
    assert 'href="https://www1.grc.nasa.gov/' in rows
    assert "Retrieved / accessed" in rows
    assert '<span class="k">Citations</span> 3' in html
    assert "No external references cited." not in html


def test_public_reference_content_is_autoescaped():
    citation = Citation(
        code_id="PUBLIC-REFERENCE", publisher="Example publisher",
        revision="Public web edition", section="Method <script>alert(1)</script>",
        source_url="https://example.org/?x=1&y=2", retrieved_on="2026-10-20",
        note='<img src=x onerror="alert(1)">',
    )
    rows = references(render_html(report([citation])))
    assert '<script>' not in rows and '<img ' not in rows
    assert "&lt;script&gt;" in rows and "&lt;img " in rows
    assert 'href="https://example.org/?x=1&amp;y=2"' in rows


def test_empty_reference_negative_control_keeps_uncited_status():
    html = render_html(report([]))
    assert "No external references cited." in references(html)
    assert '<span class="k">Citations</span> 0' in html


def wiki_reference():
    return Citation(code_id="WIKI-REFERENCE", publisher="Example publisher", revision="Edition", section="Section", wiki_path="wikis/method.md", note="Wiki note")


def test_wiki_only_reference_table_keeps_original_markup():
    rows = references(render_html(report([wiki_reference()])))
    assert '<thead><tr><th>#</th><th>Code</th><th>Revision</th><th>Section</th><th>Wiki path</th><th>Note</th></tr></thead>' in rows
    assert '<tr><td>1</td><td>WIKI-REFERENCE</td><td>Edition</td><td>Section</td><td class="path">wikis/method.md</td><td>Wiki note</td></tr>' in rows
    assert "Retrieved / accessed" not in rows


def test_mixed_reference_rows_have_consistent_columns():
    rows = references(render_html(report([wiki_reference(), public_references.get_public_reference("NASA-DRAG-EQUATION"), public_references.get_public_reference("PASSING-SHIP-METHOD")])))
    data_rows = rows.split("<tbody>", 1)[1].split("</tbody>", 1)[0].split("<tr>")[1:]
    assert len(data_rows) == 3
    assert all(row.count("<td") == 8 for row in data_rows)
    assert '<td class="path">wikis/method.md</td><td></td>' in data_rows[0]
    assert "2026-10-20" in data_rows[1] and "2026-10-20" in data_rows[2]
