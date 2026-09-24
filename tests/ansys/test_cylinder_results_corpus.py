"""Read-only opt-in corpus replay; native files are never copied into fixtures."""
import hashlib
import json
import os
from pathlib import Path

import pytest

from digitalmodel.ansys.cylinder_results_diagnostics import classify_diagnostics


def test_retained_diagnostic_identity_and_offset_replay():
    configured=os.environ.get('ANSYS_DIAGNOSTIC_CORPUS_ROOT')
    if not configured:
        pytest.skip('Private observed corpus root not supplied; synthetic tests remain separate')
    root=Path(configured).resolve()
    record_path=Path(__file__).resolve().parents[2]/'docs/plans/evidence/2026-09-14-issue-2121-diagnostic-characterization.json'
    summary=json.loads(record_path.read_bytes())
    full_record=os.environ.get('ANSYS_DIAGNOSTIC_CHARACTERIZATION')
    if not full_record:
        pytest.skip('Private complete diagnostic occurrence record not supplied')
    full_bytes=Path(full_record).read_bytes()
    assert hashlib.sha256(full_bytes).hexdigest()==summary['full_occurrence_record']['sha256']
    characterization=json.loads(full_bytes)
    count=0
    for entry in characterization['files']:
        path=(root/entry['relative_path']).resolve()
        assert path.is_relative_to(root)
        raw=path.read_bytes()
        assert hashlib.sha256(raw).hexdigest()==entry['sha256']
        result=classify_diagnostics(raw,b'',b'',b'',nerr_nmerr=200)
        assert result['status']=='INCOMPLETE'  # Every retained file contains warnings.
        observed={(e['byte_offset'],e['severity']) for e in result['events'] if e['file']=='output'}
        assert observed == {(d['header_byte_start'],d['severity']) for d in entry['records']}
        assert not any('malformed diagnostic candidate' in e for e in result['errors'])
        for diagnostic in entry['records']:
            assert (diagnostic['header_byte_start'],diagnostic['severity']) in observed
            body=raw[diagnostic['body_byte_start']:diagnostic['body_byte_end_exclusive']]
            assert hashlib.sha256(body).hexdigest()==diagnostic['body_sha256']
            header=raw[diagnostic['header_byte_start']:diagnostic['header_byte_end_exclusive']]
            assert header.rstrip(b'\r\n').decode()==diagnostic['exact_heading_line']
            count+=1
    assert count==217
