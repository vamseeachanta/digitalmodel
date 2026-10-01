"""Conservation and link-integrity tests for report presentation transforms."""

import pytest

from digitalmodel.asset_integrity.assessment.crack_fe_report_presentation import polish


def test_links_preserve_numeric_evidence_svg_and_existing_anchors():
    page = ('<html><head></head><body><section id="s3"><h2>Design Data</h2>'
            '<div id="fig-3-1"><svg><text>Figure 3.1</text></svg>'
            '<p class="caption">Figure 3.1 – Model</p></div>'
            '<p>Figure 3.1 and Section 3. <span data-src="result:/x">3.141</span>'
            '<a href="#fig-3-1">Figure 3.1</a></p></section></body></html>')
    result = polish(page)
    assert '<svg><text>Figure 3.1</text></svg>' in result
    assert '<span data-src="result:/x">3.141</span>' in result
    assert '<p class="caption">Figure 3.1 – Model</p>' in result
    assert result.count('<a href="#fig-3-1">Figure 3.1</a>') == 2
    assert '<a href="#s3">Section 3</a>' in result
    assert '<a href="#fig-3-1"><a' not in result


def test_only_body_emphasis_changes_and_entities_survive():
    page = ('<head></head><h2><b>Heading</b></h2><th><strong>Header</strong></th>'
            '<p><b>Finding</b> &lt; 1 &amp; <strong>uncertain</strong></p>')
    result = polish(page)
    assert '<h2><b>Heading</b></h2>' in result
    assert '<th><strong>Header</strong></th>' in result
    assert '<span>Finding</span> &lt; 1 &amp; <span>uncertain</span>' in result
    assert 'font-weight:400' in result


def test_operational_locators_are_retained_in_internal_ledger():
    page = ('<head></head><p>J governs (owner card G14); p0b_crotch_a2p35.</p>'
            '<p data-src="result:/state">p0b_crotch_a2p35</p>'
            '<!-- REPORT_PRESENTATION_INTERNAL_REFERENCES -->')
    result = polish(page, narrative_aliases={'p0b_crotch_a2p35': 'crotch flaw, depth 2.350 mm'})
    assert 'J governs (<a href="#presentation-i1">[I4]</a>)' in result
    assert 'crotch flaw, depth 2.350 mm' in result
    assert '<p data-src="result:/state" data-raw="p0b_crotch_a2p35">crotch flaw, depth 2.350 mm <a href="#presentation-i2">[I5]</a></p>' in result
    assert 'Owner decision record: owner card G14.' in result
    assert 'p0b_crotch_a2p35' in result


def test_missing_ledger_target_and_bad_explicit_reference_fail_closed():
    with pytest.raises(ValueError, match='internal-reference marker'):
        polish('<p>owner card G14</p>')
    with pytest.raises(ValueError, match='missing reference target'):
        polish('<p>Section 3.1</p>', reference_targets={'Section 3.1': 'missing'})


def test_longest_reference_and_protected_code():
    page = ('<head></head><section id="s3"><div id="mesh"></div></section>'
            '<p>Section 3.1, Section 3.10.</p><code>owner card G14</code>')
    result = polish(page, reference_targets={'Section 3.1': 'mesh'})
    assert '<a href="#mesh">Section 3.1</a>' in result
    assert 'Section 3.10' in result
    assert '<code>owner card G14</code>' in result


def test_bibliography_protected_from_repeated_ledger_translation():
    page = ('<head></head><section id="s9"><div id="internal-references">'
            '<p>owner card G14; raw_case.</p></div></section>')
    result = polish(page, narrative_aliases={'raw_case': 'Physical model'})
    assert '<p>owner card G14; raw_case.</p>' in result
    assert 'presentation-i' not in result


def test_owner_variants_retain_exact_locators_in_ledger():
    page = ('<p>owner decision V03; owner cards B11, J05; owner S02.</p>'
            '<!-- REPORT_PRESENTATION_INTERNAL_REFERENCES -->')
    result = polish(page)
    narrative = result.split('</p>', 1)[0]
    assert 'owner ' not in narrative
    for original in ('owner decision V03', 'owner cards B11, J05', 'owner S02'):
        assert 'Owner decision record: ' + original + '.' in result
    assert narrative.count('<a href="#presentation-i') == 3


def test_source_lists_translate_explicit_aliases_with_original_and_citations():
    page = ('<p data-src="result:/missing">residual_stress_basis, psf_basis</p>'
            '<span data-src="result:/number">1.250</span>'
            '<!-- REPORT_PRESENTATION_INTERNAL_REFERENCES -->')
    result = polish(page, narrative_aliases={
        'residual_stress_basis': 'residual-stress evidence',
        'psf_basis': 'partial-factor evidence', '1.250': 'must not replace'})
    assert 'data-raw="residual_stress_basis, psf_basis"' in result
    assert 'residual-stress evidence, partial-factor evidence ' in result
    assert result.split('</p>')[0].count('<a href="#presentation-i') == 2
    assert '<span data-src="result:/number">1.250</span>' in result
