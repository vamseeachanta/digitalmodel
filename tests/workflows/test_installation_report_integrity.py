"""Both issued formats must enforce screening semantics and resolvable appendices."""
from io import BytesIO
import hashlib
import json
import re

import pytest
from pypdf import PdfReader

from digitalmodel.workflows.installation_full_report_pdf import render_full_pdf
from digitalmodel.workflows import vessel_capability_report as html_report
from tests.workflows.test_vessel_capability_screening import payload, summary
from tests.workflows.test_mudmat_sensitivity_reporting import sensitivity


def pdf_text(data, screen, **kwargs):
    raw = json.dumps(data).encode()
    screen['provenance']['summary'] = {'sha256': hashlib.sha256(raw).hexdigest()}
    stream = BytesIO()
    render_full_pdf(data, screen, stream, summary_bytes=raw, **kwargs)
    return '\n'.join(page.extract_text() for page in PdfReader(stream).pages)


@pytest.mark.parametrize('edition', ['html', 'pdf'])
@pytest.mark.parametrize('defect', ['training', 'horizon', 'alert'])
def test_both_formats_reject_invalid_forecast_before_output(edition, defect):
    data, screen = summary(), payload()
    frame = screen['demo']['scenarios'][0]['frames'][0]
    channel = frame['channels'][0]
    if defect == 'training':
        channel['training_end_s'] = 480
    elif defect == 'horizon':
        frame['forecast_horizon_s'] = 240
    else:
        channel['exceedance'] = dict(status='calibrated', window_probability=.9,
            limit=channel['assumed_limit'], alert=False, alert_probability=.5)
    raw = json.dumps(data).encode()
    screen['provenance']['summary'] = {'sha256': hashlib.sha256(raw).hexdigest()}
    stream = BytesIO()
    with pytest.raises(ValueError):
        if edition == 'html':
            html_report.render_html(data, config={}, screening=screen)
        else:
            render_full_pdf(data, screen, stream, summary_bytes=raw)
    assert stream.getvalue() == b''


def test_pdf_emits_per_criterion_verdicts_in_appendix_order():
    data, screen, evidence = summary(), payload(), sensitivity()
    text = pdf_text(data, screen, sensitivity=evidence,
                    sensitivity_bytes=json.dumps(evidence).encode())
    headings = re.findall(r'^Appendix ([A-D])\.', text, re.MULTILINE)
    assert headings == ['A', 'B', 'C', 'D']
    assert 'Table C-1.' in text
    assert 'Acceptable against Master-link proxy' in ' '.join(text.split())
    assert 'Not acceptable against Master-link proxy' in ' '.join(text.split())
    assert 'Not evaluated' in text


def test_html_contents_resolve_all_emitted_appendices_and_labels_are_unique():
    text = html_report.render_html(summary(), config={}, screening=payload(),
                                   sensitivity=sensitivity())
    for letter in 'abcd':
        assert f'href="#appendix-{letter}"' in text
        assert f'id="appendix-{letter}"' in text
    labels = re.findall(r'Table ([A-Z]-\d+)\.', text)
    assert len(labels) == len(set(labels))


def test_valid_wave_preview_remains_explicitly_conditional():
    data, screen = summary(), payload()
    screen['demo']['default_mode'] = 'wave_preview'
    for channel in screen['demo']['scenarios'][0]['frames'][0]['channels']:
        channel['wave_preview'] = channel['forecast'].copy()
    text = pdf_text(data, screen)
    assert 'The simulated future wave trace is supplied input.' in ' '.join(text.split())


def test_unsupported_forecast_mode_is_rejected_before_output():
    data, screen = summary(), payload()
    screen['demo']['default_mode'] = 'unsupported'
    with pytest.raises(ValueError, match='mode'):
        pdf_text(data, screen)


def test_substitution_note_does_not_reference_an_absent_appendix():
    data = summary()
    data['cases'][0]['solved_time_step_s'] = .0125
    text = html_report.render_html(data, config={}, screening=payload())
    assert 'Appendix D' not in text
    assert 'solved at 0.0125 s' in text
