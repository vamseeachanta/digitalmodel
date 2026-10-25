"""Renderer regressions for the coverage-claim grammar in issue #2349.

Synthetic counts come from test_installation_report_layout.summary(), as specified
in issue #2349's worked example: 61 verified + 0 failed + 95 missing = 156 planned.
No physical coefficient or engineering acceptance criterion is used.
"""

import copy
from html import escape, unescape
from pathlib import Path
import re

import pytest

from digitalmodel.workflows import vessel_capability_report as report
from tests.workflows.test_installation_report_layout import design_row, summary


SEPARATORS = pytest.mark.parametrize(
    'separator', ['  ', '\t', '\n'], ids=['spaces', 'tab', 'newline']
)
MATCHING = (
    '61 of 156 baseline cases verified; '
    '0 numerical failures and 95 missing cells'
)


@SEPARATORS
@pytest.mark.parametrize(
    ('claim', 'expected_error'),
    [
        ('60 of 156 baseline cases verified', '60 VERIFIED stated'),
        ('61 of 160 baseline cases verified', '160 planned cases stated'),
        ('1 numerical failure', '1 FAILED stated'),
        ('1 missing cell', '1 MISSING stated'),
    ],
    ids=['verified', 'planned', 'failed', 'missing'],
)
def test_stale_whitespace_claims_rejected_for_every_count(claim, expected_error, separator):
    """61 + 0 + 95 = 156; 60 verified, 160 planned or either singular 1 is stale."""
    status = claim.replace(' ', separator)
    with pytest.raises(ValueError, match=re.escape(expected_error)):
        report.render_html(summary(), config={'design_data': [design_row(status=status)]})


@SEPARATORS
@pytest.mark.parametrize(
    ('claim', 'expected_error'),
    [
        ('60 of 156 baseline cases verified', '60 VERIFIED stated'),
        ('61 of 160 baseline cases verified', '160 planned cases stated'),
        ('1 numerical failure', '1 FAILED stated'),
        ('1 missing cell', '1 MISSING stated'),
    ],
    ids=['verified', 'planned', 'failed', 'missing'],
)
def test_later_variant_claim_cannot_hide_behind_matching_claim(claim, expected_error, separator):
    """The first claim matches 61 + 0 + 95 = 156; each later stale count must fail."""
    status = MATCHING + '; ' + claim.upper().replace(' ', separator)
    with pytest.raises(ValueError, match=re.escape(expected_error)):
        report.render_html(summary(), config={'design_data': [design_row(status=status)]})


@SEPARATORS
@pytest.mark.parametrize('casing', [str.upper, str.title], ids=['upper', 'mixed'])
def test_repeated_matching_singular_whitespace_claims_preserve_text(separator, casing):
    """61 verified + 1 failed + 1 missing = 63 planned; both occurrences agree."""
    # Synthetic input and hand sum: issue #2349, matching-singular worked example.
    data = summary()
    data['counts'] = {'VERIFIED': 61, 'FAILED': 1, 'MISSING': 1}
    before = copy.deepcopy(data)
    claim = '61 of 63 baseline cases verified; 1 numerical failure and 1 missing cell'
    status = claim + '; ' + casing(claim).replace(' ', separator)
    html = report.render_html(data, config={'design_data': [design_row(status=status)]})
    assert escape(status) in html
    assert data == before


def test_coverage_documentation_worked_example():
    """61 + 1 + 1 = 63: valid input renders; changing only 63 to 64 raises."""
    path = Path('docs/api/installation/coverage-status-claims.html')
    document = (Path(__file__).resolve().parents[2] / path).read_text(encoding='utf-8')
    examples = re.findall(r'<pre><code>(.*?)</code></pre>', document, re.DOTALL)
    assert len(examples) == 1
    exec(compile(unescape(examples[0]), str(path), 'exec'), {})
