"""Shared engineering cover preserves jumper defaults and exposes mudmat placeholders."""
import copy
from hashlib import sha256
import json
import re

import pytest

from digitalmodel.workflows import installation_full_report as jumper
from digitalmodel.workflows import vessel_capability_report as mudmat
from digitalmodel.workflows import vessel_capability_snapshot as snapshot
from digitalmodel.workflows import vessel_capability_layout as layout
from pathlib import Path


def summary():
    return dict(created_utc='2026-01-01T00:00:00Z',counts={'VERIFIED':61,'MISSING':95},cases=[],
        envelopes=[],critical_periods=[],design_basis={'mass_t':5.},matrix_sha256='a'*64,campaign_sha256='b'*64)


def test_cover_jumper_default_semantics_retained():
    text=jumper._cover({},'timestamp')
    assert 'Jumper installation engineering report' in text
    assert 'Recorded demand, assumed-criteria envelopes and simulated monitoring' in text
    assert 'R7' in text and text.count('href="#section-')==8
    assert '<td>Reviewed by</td><td>Not assigned</td>' in text


def test_mudmat_hierarchy_placeholders_and_summary_immutable():
    data=summary();before=copy.deepcopy(data)
    html=mudmat.render_html(data,config={'title':'Structure report','decisions':['<pending>'],'supplements':['awaiting evidence']})
    assert data==before
    for index in range(1,9):assert f'id="section-{index}"' in html and f'href="#section-{index}"' in html
    for name in ('Document control','5.4 Provisional installation envelope','5.5 Two-minute forecasting',
                 'Validation status','References and revision history','Appendix A','Appendix B'):
        assert name in html
    assert '&lt;pending&gt;' in html and '<pending>' not in html
    assert 'Not established' in html and 'forecast validation remains pending' in html
    ids=re.findall(r'\bid="([^"]+)"',html);assert len(ids)==len(set(ids))
    assert 'VERIFIED: 61' in html and 'MISSING: 95' in html
    assert 'pre{white-space:pre-wrap;overflow-wrap:anywhere}' in html


def test_zero_duration_does_not_identify_governing_tp():
    rows=[dict(hs_m=1,verified_periods=[4,5],complete_tp_row=True,
        peak_tension={'tp_s':5,'peak_tension_kN':3},low_duration={'tp_s':4,'maximum_low_tension_duration_s':0})]
    html=mudmat._critical_table(rows)
    assert 'No governing Tp' in html and 'no positive-duration nonpositive-tension events' in html
    assert '4 / 0.000' not in html


def pinned(tmp_path):
    source=tmp_path/'source.json';source.write_text(json.dumps(summary()))
    config=tmp_path/'config.json';config.write_text(json.dumps({'revision':'R1','supplements':['Pending']}))
    return dict(source=source,source_sha256=sha256(source.read_bytes()).hexdigest(),config=config,
        config_sha256=sha256(config.read_bytes()).hexdigest(),output=tmp_path/'new.html')


def test_snapshot_rerender_compact_pinned_manifest(tmp_path):
    args=pinned(tmp_path);before=args['source'].read_bytes()
    result=snapshot.render_snapshot(**args)
    assert args['source'].read_bytes()==before
    assert 'cases' not in result and 'demand_summary' not in result
    assert result['source']['sha256']==args['source_sha256']
    assert result['source_snapshot_utc']==summary()['created_utc']
    assert result['html_sha256']==sha256(args['output'].read_bytes()).hexdigest()
    assert result==json.loads(args['output'].with_suffix('.json').read_bytes())
    with pytest.raises(FileExistsError):snapshot.render_snapshot(**args)


@pytest.mark.parametrize('key',['source_sha256','config_sha256'])
def test_snapshot_bad_pin_before_output(tmp_path,key):
    args=pinned(tmp_path);args[key]='0'*64
    with pytest.raises(ValueError):snapshot.render_snapshot(**args)
    assert not args['output'].exists()


def test_design_data_and_findings_are_plain_context_not_metric_changes():
    data=summary();original=copy.deepcopy(data)
    html=mudmat.render_html(data,config={'design_data':[design_row(value='<1>')],
        'summary_findings':['Governing sampled tension <review>']})
    assert 'Governing sampled tension &lt;review&gt;' in html
    assert '&lt;1&gt;' in html and 'Not recorded' in html
    assert '<svg' in html and 'Figure 4-1.' in html
    assert data==original


def test_snapshot_source_change_during_render_prevents_output(tmp_path,monkeypatch):
    args=pinned(tmp_path)
    def tamper(*a,**k):args['source'].write_text('{}');return '<html></html>'
    monkeypatch.setattr(snapshot,'render_html',tamper)
    with pytest.raises(ValueError):snapshot.render_snapshot(**args)
    assert not args['output'].exists()


def design_row(**kwargs):
    return dict(dict(parameter='Mass',value=5.,unit='t',source='Source',status='Recorded',effect_if_changed='Not recorded'),**kwargs)


@pytest.mark.parametrize('rows',[[{'parameter':'Mass'}],['bad'],'bad',
    [design_row(basis_key='missing')],[design_row(value=6.,basis_key='mass_t')]])
def test_design_data_validation_rejects_unbound_values(rows):
    with pytest.raises(ValueError):mudmat.render_html(summary(),config={'design_data':rows})


def test_audit_claims_and_unlinked_design_context_explicit():
    html=mudmat.render_html(summary(),config={'design_data':[design_row()]})
    assert 'source event audit is not established' in html
    assert 'authored configuration context' in html
    assert 'over the 600 s record' not in html and 'recorded analysis interval' in html
    assert '6.1 Subsequent review context' not in html
    data=summary();data['event_audits']=[{'status':'VERIFIED'}]
    assert 'source audit records' in mudmat.render_html(data)


def mixed_cases():
    return [dict(index=0,status='VERIFIED',hs_m=1.,tp_s=8.,run_dir='run',simulation_sha256='a',trace_sha256='b',
        channels={'load':dict(position='End A',units='kN',minimum=0.,maximum=1.)}),dict(index=1,status='FAILED')]


def test_appendix_includes_verified_and_excluded_counts_and_caption():
    data=summary();data['cases']=mixed_cases()
    html=layout._appendices(data,Path('.'),{})
    assert '1 verified case' in html and '1 excluded case' in html
    assert '</table><p class="caption">Table A-001.' in html
    assert 'Only VERIFIED cases' in html


def test_appendix_rejects_changed_table_contract(monkeypatch):
    data=summary();data['cases']=mixed_cases()
    monkeypatch.setattr(layout,'_case_details',lambda *a:'<table></table><table></table>')
    with pytest.raises(ValueError):layout._appendices(data,Path('.'),{})


def test_midwrite_source_change_leaves_only_partial_outputs(tmp_path,monkeypatch):
    args=pinned(tmp_path);original=Path.read_text
    def mutate(path,*a,**k):
        value=original(path,*a,**k)
        if str(path).endswith('new.html.partial'):args['source'].write_text('{}')
        return value
    monkeypatch.setattr(Path,'read_text',mutate)
    with pytest.raises(ValueError):snapshot.render_snapshot(**args)
    assert not args['output'].exists() and not args['output'].with_suffix('.json').exists()
    assert Path(str(args['output'])+'.partial').exists()


def test_nonfinite_raw_basis_rejected():
    data=summary();data['design_basis']['mass_t']=float('nan')
    with pytest.raises(ValueError):mudmat.render_html(data)
