"""Synthetic diagnostic envelopes; retained native corpus is separately governed."""
import pytest

from digitalmodel.ansys.cylinder_results_diagnostics import classify_diagnostics

TOTALS = (b' NUMBER OF WARNING MESSAGES ENCOUNTERED= 0\r\n'
          b' NUMBER OF ERROR   MESSAGES ENCOUNTERED= 0\r\n')


def test_notes_and_ordinary_table_tokens_do_not_become_warnings():
    raw = (b' WARNING TOLERANCE (1) = 20\r\n'
           b' *** NOTE *** ELAPSED TIME = 1.0 TIME= 12:00:00\r\n'
           b' Synthetic informational body.\r\n\r\n'+TOTALS)
    result = classify_diagnostics(raw, b'', b'', b'', nerr_nmerr=200)
    assert result['status'] == 'COMPLETE'
    assert result['events'][0]['byte_offset'] == 29
    assert result['events'][0]['severity'] == 'NOTE'


@pytest.mark.parametrize('severity', [b'WARNING',b'ERROR',b'FATAL ERROR',b'MYSTERY'])
def test_any_blocking_or_unknown_severity_is_incomplete(severity):
    raw=b' *** '+severity+b' *** TIME= 12:00:00\n synthetic body\n\n'+TOTALS
    assert classify_diagnostics(raw,b'',b'',b'',nerr_nmerr=200)['status']=='INCOMPLETE'


@pytest.mark.parametrize('fault', ['missing','stream','decode','malformed','total','suppressed','nerr','truncated','duplicate'])
def test_diagnostic_evidence_gap_is_not_numeric_failure(fault):
    raw=TOTALS;err=b'';stdout=b'';nerr=200
    if fault=='missing':err=None
    if fault=='stream':stdout=b'launcher message'
    if fault=='decode':raw=b'\xff'+raw
    if fault=='malformed':raw=b' *** WARNING **\n'+raw
    if fault=='total':raw=raw.replace(b'= 0',b'= 1')
    if fault=='suppressed':raw=b'additional messages suppressed\n'+raw
    if fault=='nerr':nerr=-1
    if fault=='truncated':raw=raw[:-1]
    if fault=='duplicate':raw+=TOTALS
    assert classify_diagnostics(raw,err,stdout,b'',nerr_nmerr=nerr)['status']=='INCOMPLETE'


def test_ordinary_native_style_banners_are_not_diagnostic_envelopes():
    raw=(b' ***** MAPDL SOLUTION ROUTINE *****\n'
         b' *** SELECTION OF ELEMENT TECHNOLOGIES FOR APPLICABLE ELEMENTS ***\n'
         b' *** ELEMENT MATRIX FORMULATION TIMES\n'+TOTALS)
    result=classify_diagnostics(raw,b'',b'',b'',nerr_nmerr=200)
    assert result['status']=='COMPLETE'
    assert result['events']==[]
