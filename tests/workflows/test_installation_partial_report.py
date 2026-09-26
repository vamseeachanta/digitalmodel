import json
from threading import Barrier, get_ident

import pytest

from digitalmodel.workflows import installation_partial_report as report


def test_case_detail_hashes_are_escaped(tmp_path):
    row = dict(index=0, hs_m=1, tp_s=8, status='VERIFIED', run_dir=str(tmp_path),
               channels={}, simulation_sha256='<img src=x onerror=alert(1)>',
               trace_sha256='<script>bad</script>')
    html = report._case_details([row], tmp_path)
    assert '<img' not in html and '<script>' not in html
    assert '&lt;img' in html and '&lt;script&gt;' in html


@pytest.mark.parametrize('workers', [0, -1, 1.5, True, '3'])
def test_invalid_workers_rejected(workers):
    with pytest.raises(ValueError, match='workers'):
        report.collect_cases({'cases': []}, {'cases': []}, workers=workers)


def test_parallel_cases_preserve_serial_order_and_use_three_threads(monkeypatch):
    cases = [dict(hs_m=i + 1, tp_s=8, seed=1) for i in range(3)]
    snapshot = {'cases': [dict(index=i, status='COMPLETED', **c) for i, c in enumerate(cases)]}
    monkeypatch.setattr(report, '_case', lambda i, *args: dict(index=i, status='FAILED'))
    serial = report.collect_cases(snapshot, {'cases': cases})
    barrier, identities = Barrier(3), set()
    def inspect(i, *args):
        identities.add(get_ident())
        barrier.wait(timeout=5)
        return dict(index=i, status='FAILED')
    monkeypatch.setattr(report, '_case', inspect)
    assert report.collect_cases(snapshot, {'cases': cases}, workers=3) == serial
    assert len(identities) == 3


def test_parallel_worker_exception_aborts_report(monkeypatch, tmp_path):
    from hashlib import sha256
    matrix = tmp_path / 'matrix.json'
    matrix.write_text(json.dumps({'cases': [dict(hs_m=1, tp_s=8, seed=1)]}))
    campaign = tmp_path / 'campaign.json'
    campaign.write_text(json.dumps({'matrix_sha256': sha256(matrix.read_bytes()).hexdigest(),
        'cases': [dict(index=0, status='COMPLETED', hs_m=1, tp_s=8, seed=1)]}))
    def broken(*args):
        raise ValueError('invalid evidence')
    monkeypatch.setattr(report, '_case', broken)
    output = tmp_path / 'report.html'
    with pytest.raises(ValueError, match='invalid evidence'):
        report.generate_report(campaign, matrix, output, workers=3)
    assert not output.exists() and not output.with_suffix('.json').exists()


def test_snapshot_does_not_promote_running_case(monkeypatch, tmp_path):
    matrix = {'cases': [{'hs_m': .25, 'tp_s': 4, 'seed': 1}]}
    snapshot = {'cases': [{'index': 0, 'status': 'RUNNING', 'hs_m': .25, 'tp_s': 4, 'seed': 1}]}
    monkeypatch.setattr(report, '_case', lambda *args: pytest.fail('Must not inspect running output'))
    rows = report.collect_cases(snapshot, matrix)
    assert rows[0]['status'] == 'RUNNING'
    assert 'channels' not in rows[0]


def test_component_envelope_keeps_case_and_end():
    rows = [dict(index=0, hs_m=.25, tp_s=4, channels={
        'Sling#5_end_A': dict(object='Sling#5', position='End A', variable='Effective tension',
                            units='kN', minimum=-2, maximum=8)}),
            dict(index=1, hs_m=.5, tp_s=5, channels={
        'Sling#5_end_A': dict(object='Sling#5', position='End A', variable='Effective tension',
                            units='kN', minimum=-1, maximum=12)})]
    result = report.component_envelopes(rows)[0]
    assert result['minimum']['index'] == 0
    assert result['maximum']['index'] == 1
    assert result['position'] == 'End A'
    assert result['units'] == 'kN'


def test_refuses_overwrite(tmp_path):
    output = tmp_path / 'existing.html'
    output.write_text('preserved')
    with pytest.raises(FileExistsError):
        report.generate_report(tmp_path / 'missing.json', tmp_path / 'matrix.json', output)
    assert output.read_text() == 'preserved'


def test_html_escapes_external_labels_and_no_acceptance():
    summary = dict(created_utc='now', counts={'VERIFIED': 0}, cases=[], envelopes=[],
                   matrix_sha256='a', campaign_sha256='b', engineering_acceptance='NOT EVALUATED')
    html = report.render_html(summary, {'<bad>': 'x&y'})
    assert '&lt;bad&gt;' in html
    assert 'No operating window has been established' in html
    assert 'No data available' not in html


def test_completed_without_traces_is_incomplete(monkeypatch):
    matrix = {'cases': [{'hs_m': .25, 'tp_s': 4, 'seed': 1}]}
    snapshot = {'cases': [{'index': 0, 'status': 'COMPLETED', **matrix['cases'][0]}]}
    monkeypatch.setattr(report, '_case', lambda *args: {'status': 'NOT EVALUATED'})
    assert report.collect_cases(snapshot, matrix)[0]['status'] == 'INCOMPLETE'


def test_coordinate_mismatch_rejected():
    matrix = {'cases': [{'hs_m': .25, 'tp_s': 4, 'seed': 1}]}
    snapshot = {'cases': [{'index': 0, 'status': 'COMPLETED', 'hs_m': 2, 'tp_s': 4, 'seed': 1}]}
    with pytest.raises(ValueError, match='coordinates'):
        report.collect_cases(snapshot, matrix)


def test_governing_arc_is_retained_per_extreme():
    def row(index, position, minimum, maximum):
        return dict(index=index, hs_m=index + 1, tp_s=8, channels={'bend': dict(
            object='Jumper', variable='Bend moment', units='kN.m', position=position,
            minimum=minimum, maximum=maximum)})
    result = report.component_envelopes([row(0, {'arc_m': 3}, -5, 10),
                                          row(1, {'arc_m': 8}, -1, 20)])[0]
    assert result['minimum']['position'] == {'arc_m': 3}
    assert result['maximum']['position'] == {'arc_m': 8}


def test_mixed_units_fail_closed():
    rows = [dict(index=i, hs_m=1, tp_s=8, channels={'stress': dict(object='Jumper',
        variable='Stress', units=units, minimum=0, maximum=1)}) for i, units in enumerate(('kPa', 'MPa'))]
    with pytest.raises(ValueError, match='units'):
        report.component_envelopes(rows)


def test_active_link_scheme_rejected():
    with pytest.raises(ValueError, match='relative'):
        report.render_html(dict(counts={}, cases=[]), {'click': 'javascript:alert(1)'})


def test_metadata_mutation_during_verification_rejected(monkeypatch, tmp_path):
    directory = tmp_path / 'installation_traces'
    directory.mkdir()
    metadata = directory / 'metadata.json'
    metadata.write_text('{"trace_sha256": "x"}')
    def changed(*args):
        metadata.write_text('{"trace_sha256": "x", "altered": true}')
        return dict(status='NOT EVALUATED', trace_sha256='x')
    monkeypatch.setattr(report, '_case', changed)
    case = dict(hs_m=1, tp_s=8, seed=1)
    snapshot = {'cases': [dict(index=0, status='COMPLETED', run_dir=str(tmp_path), **case)]}
    with pytest.raises(ValueError, match='changed'):
        report.collect_cases(snapshot, {'cases': [case]})


def test_compact_metadata_removes_event_arrays():
    channels = {'a': {'events': {'low_tension': {'events': [1, 2], 'total_duration_s': 3}}}}
    assert report._compact_channels(channels)['a']['events']['low_tension'] == {'total_duration_s': 3}
