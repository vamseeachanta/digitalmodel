from hashlib import sha256
import json

import pytest

from digitalmodel.workflows.installation_full_report import generate_report, dashboard_parts, _findings


def inputs(tmp_path):
    row = dict(index=0, hs_m=1, tp_s=8, seed=1, status='VERIFIED', channels={},
               run_dir=str(tmp_path), settings={}, simulation_sha256='sim', trace_sha256='trace')
    summary = dict(created_utc='source time', counts={'VERIFIED': 1}, cases=[row],
                   envelopes=[], campaign_sha256='a', matrix_sha256='b')
    channel = dict(id='wave_elevation', label='Wave', units='m', history=dict(times=[0, 1], values=[0, 1]),
                   forecast=dict(times=[2, 121], values=[0, 1]), assumed_limit=None)
    payload = dict(title='Example', created_utc='payload time', criteria=[], limitations=[],
                   cases=[dict(index=0, hs_m=1, tp_s=8, status='WITHIN_ASSUMPTIONS', checks=[], metrics={})],
                   demo=dict(case_index=0, hs_m=1, tp_s=8, source_label='Simulated',
                             frames=[dict(now_s=1, channels=[channel])]), boundaries=[])
    source = tmp_path / 'source.json'
    source.write_text(json.dumps(summary), encoding='utf-8')
    payload['provenance'] = {'summary': {'sha256': sha256(source.read_bytes()).hexdigest()}}
    data = tmp_path / 'payload.json'
    data.write_text(json.dumps(payload), encoding='utf-8')
    return source, data


def test_integrates_full_engineering_sections_and_one_dashboard(tmp_path):
    source, data = inputs(tmp_path)
    output = tmp_path / 'full-r7.html'
    generate_report(source, data, output, {'title': 'Engineering review', 'revision': 'r7'})
    text = output.read_text(encoding='utf-8')
    for label in ('Document control', 'Contents', 'Introduction', 'Summary and conclusions',
                  'Design data', 'Analysis methodology', 'Validation status', 'References',
                  'Appendix A', 'Appendix B', '5.4', '5.5'):
        assert label in text
    assert text.count('id="envelope"') == 1 and text.count('id="payload"') == 1
    assert 'Table A-001.' in text
    assert 'Figure 5-${' in text
    assert '<iframe' not in text and 'Pamphlet' not in text
    assert 'forecastperformance notcalculated' not in text
    assert output.with_suffix('.json').exists()


def test_source_digest_mismatch_rejected_without_output(tmp_path):
    source, data = inputs(tmp_path)
    source.write_text('{}')
    with pytest.raises(ValueError, match='digest'):
        generate_report(source, data, tmp_path / 'full.html', {})
    assert not (tmp_path / 'full.html').exists()


def test_case_coordinate_mismatch_rejected(tmp_path):
    source, data = inputs(tmp_path)
    payload = json.loads(data.read_text())
    payload['cases'][0]['hs_m'] = 2
    data.write_text(json.dumps(payload))
    with pytest.raises(ValueError, match='coordinates'):
        generate_report(source, data, tmp_path / 'full.html', {})


def test_dashboard_extraction_fails_closed_on_unrecognized_markup():
    with pytest.raises(ValueError):
        dashboard_parts('<html>unexpected</html>')


def test_config_path_is_pinned_and_output_is_not_overwritten(tmp_path):
    source, data = inputs(tmp_path)
    config = tmp_path / 'config.json'
    config.write_text(json.dumps({'revision': 'R7', 'prepared_by': 'Automated report generation'}))
    output = tmp_path / 'report.html'
    record = generate_report(source, data, output, config)
    assert record['evidence']['Report configuration']['sha256'] == sha256(config.read_bytes()).hexdigest()
    with pytest.raises(FileExistsError):
        generate_report(source, data, output, config)


def test_unsafe_reference_link_is_rejected(tmp_path):
    source, data = inputs(tmp_path)
    with pytest.raises(ValueError, match='reference'):
        generate_report(source, data, tmp_path / 'report.html',
                        {'references': [{'label': 'bad', 'path': 'javascript:alert(1)'}]})


def test_failed_hoist_ratio_cannot_be_described_as_above_limit():
    payload = {'cases': [{'index': 0, 'hs_m': 1, 'tp_s': 8, 'status': 'EXCEEDS_ASSUMPTIONS', 'checks': [
        {'status': 'FAIL', 'minimum_static_ratio': .05, 'required_ratio': .1, 'governing_channel': 'hoist'}]}]}
    findings = ' '.join(_findings(payload))
    assert 'is below' in findings
    assert 'meets or exceeds' not in findings


@pytest.mark.parametrize('locator', ['/etc/record', '\\rooted', 'C:\\record'])
def test_root_absolute_references_rejected(tmp_path, locator):
    source, data = inputs(tmp_path)
    with pytest.raises(ValueError, match='reference'):
        generate_report(source, data, tmp_path / 'report.html',
                        {'references': [{'label': 'record', 'path': locator}]})


def test_relative_case_mapping_changes_links_not_source_evidence(tmp_path):
    source, data = inputs(tmp_path)
    original = source.read_bytes()
    output = tmp_path / 'reports/full.html'
    mapping = {'dataset_root': '..', 'case_run_dirs': {'0': 'runs/relocated'}}
    record = generate_report(source, data, output, mapping)
    assert '../runs/relocated/extracted/report.html' in output.read_text(encoding='utf-8')
    assert record['demand_summary']['cases'][0]['run_dir'] == str(tmp_path)
    assert source.read_bytes() == original


@pytest.mark.parametrize('mapping', [{}, {'1': 'runs/x'}, {'0': '../outside'},
                                    {'0': '/absolute'}, {'0': 'C:\\absolute'}])
def test_incomplete_or_unsafe_case_mapping_rejected(tmp_path, mapping):
    source, data = inputs(tmp_path)
    with pytest.raises(ValueError, match='mapping|relative'):
        generate_report(source, data, tmp_path / 'report.html',
                        {'dataset_root': '.', 'case_run_dirs': mapping})


def test_unevaluated_checks_not_headlined():
    case = dict(index=0, hs_m=1, tp_s=8, status='NOT_EVALUATED', checks=[
        dict(status='NOT_EVALUATED', utilization=9, limit_kN=1, demand_kN=9, governing_channel='unqualified')])
    assert _findings({'cases': [case]}) == []
