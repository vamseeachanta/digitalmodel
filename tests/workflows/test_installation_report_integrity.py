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
    flat = ' '.join(text.split())
    assert 'Within provisional Master-link proxy' in flat
    assert 'Exceeds provisional Master-link proxy' in flat
    assert 'Not evaluated' in text


@pytest.mark.parametrize('edition', ['html', 'pdf'])
def test_provisional_criteria_never_rendered_as_acceptance(edition):
    """Acceptance is NOT EVALUATED; verdicts against provisional criteria must not read as acceptance."""
    data, screen = summary(), payload()
    if edition == 'html':
        text = html_report.render_html(data, config={}, screening=screen)
    else:
        text = ' '.join(pdf_text(data, screen).split())
    assert 'Acceptable against' not in text
    assert 'acceptable against' not in text.lower()


def test_html_contents_resolve_all_emitted_appendices_and_labels_are_unique():
    text = html_report.render_html(summary(), config={}, screening=payload(),
                                   sensitivity=sensitivity())
    for letter in 'abcd':
        assert f'href="#appendix-{letter}"' in text
        assert f'id="appendix-{letter}"' in text
    labels = re.findall(r'Table ([A-Z]-\d+)\.', text)
    assert len(labels) == len(set(labels))


def _wave_preview_input():
    data, screen = summary(), payload()
    screen['demo']['default_mode'] = 'wave_preview'
    for channel in screen['demo']['scenarios'][0]['frames'][0]['channels']:
        channel['wave_preview'] = channel['forecast'].copy()
    raw = json.dumps(data).encode()
    screen['provenance']['summary'] = {'sha256': hashlib.sha256(raw).hexdigest()}
    return data, screen, raw


def _render(edition, data, screen, raw, stream):
    if edition == 'html':
        return html_report.render_html(data, config={}, screening=screen)
    return render_full_pdf(data, screen, stream, summary_bytes=raw)


def test_pdf_rejects_wave_preview_before_output():
    """Preview timing is not bound by bind_screening, so the PDF must not issue it."""
    data, screen, raw = _wave_preview_input()
    stream = BytesIO()
    with pytest.raises(ValueError, match='not supported until preview timing checks exist'):
        render_full_pdf(data, screen, stream, summary_bytes=raw)
    assert stream.getvalue() == b''


def test_both_editions_reject_the_same_wave_preview_input_identically():
    errors = {}
    for edition in ('html', 'pdf'):
        data, screen, raw = _wave_preview_input()
        stream = BytesIO()
        with pytest.raises(ValueError) as caught:
            _render(edition, data, screen, raw, stream)
        assert stream.getvalue() == b''
        errors[edition] = (type(caught.value), str(caught.value))
    assert errors['html'] == errors['pdf']
    assert 'not supported until preview timing checks exist' in errors['pdf'][1]


def _history_only_with_preview(location):
    """A history_only payload that still carries preview data must not publish an unchecked benchmark."""
    data, screen = summary(), payload()
    assert screen['demo']['default_mode'] == 'history_only'
    channel = screen['demo']['scenarios'][0]['frames'][0]['channels'][0]
    if location == 'channel_preview':
        channel['wave_preview'] = dict(times=[361, 480], values=[175.0, 150.0])
    if location == 'channel_metrics':
        channel['wave_preview_metrics'] = {'oracle_wave_fir': {'rmse': 5.5}}
    if location == 'channel_both':
        channel['wave_preview'] = dict(times=[361, 480], values=[175.0, 150.0])
        channel['wave_preview_metrics'] = {'oracle_wave_fir': {'rmse': 5.5}, 'autoregression': {'rmse': 28.022},
                                           'persistence': {'rmse': 55.7}, 'history_mean': {'rmse': 28.106}}
    if location == 'frame':
        screen['demo']['scenarios'][0]['frames'][0]['wave_preview'] = dict(times=[361], values=[1.0])
    if location == 'demo':
        screen['demo']['wave_preview_metrics'] = None
    if location == 'top_level':
        screen['wave_preview'] = []
    raw = json.dumps(data).encode()
    screen['provenance']['summary'] = {'sha256': hashlib.sha256(raw).hexdigest()}
    return data, screen, raw


@pytest.mark.parametrize('location', ['channel_preview', 'channel_metrics', 'channel_both', 'frame', 'demo', 'top_level'])
def test_history_only_payload_carrying_preview_is_rejected_by_both_editions(location):
    errors = {}
    for edition in ('html', 'pdf'):
        data, screen, raw = _history_only_with_preview(location)
        stream = BytesIO()
        with pytest.raises(ValueError, match='not supported until preview timing checks exist') as caught:
            _render(edition, data, screen, raw, stream)
        assert stream.getvalue() == b''
        errors[edition] = (type(caught.value), str(caught.value))
    assert errors['html'] == errors['pdf']


def test_pdf_narrative_makes_no_preview_claim():
    """The PDF admits only history-only payloads, so its cover and Appendix B must not describe a wave preview."""
    flat = ' '.join(pdf_text(summary(), payload()).split())
    for phrase in ('wave-preview demonstration', 'irregular-wave preview', 'same payload-derived'):
        assert phrase not in flat, phrase


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
