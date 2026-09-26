from copy import deepcopy
import json
import shutil
import subprocess

import pytest

from digitalmodel.workflows.installation_envelope_dashboard import JAVASCRIPT, render_dashboard


def payload():
    channel = dict(id='wave', label='Wave', units='m',
                   history=dict(times=[0, 1], values=[0, 1]),
                   forecast=dict(times=[2, 121], values=[0, 1]),
                   truth=dict(times=[2, 121], values=[1, 0]), assumed_limit=None,
                   fit_status='fitted')
    return dict(title='Test', created_utc='now', criteria=[], limitations=[],
                cases=[dict(index=0, hs_m=1, tp_s=8, status='NOT_EVALUATED',
                            reason='No criteria', metrics={})],
                demo=dict(case_index=0, hs_m=1, tp_s=8, source_label='Simulated case',
                          frames=[dict(now_s=1, channels=[channel])]))


def test_offline_dashboard_labels_history_forecast_and_demo():
    html = render_dashboard(payload())
    for label in ('SIMULATED DEMO', 'Not connected offshore', '120 s prediction',
                  'NOW', 'Show withheld simulated truth', 'Assumed criteria'):
        assert label in html
    assert '<script src=' not in html and 'https://cdn' not in html
    assert 'type="range"' in html and '<svg' in html


def test_script_termination_and_title_are_escaped():
    data = payload()
    data['title'] = '</script><img src=x onerror=alert(1)>'
    html = render_dashboard(data)
    assert '</script><img' not in html
    assert '\\u003c/script\\u003e' in html


@pytest.mark.parametrize('change', ['history_future', 'forecast_past', 'short_horizon', 'nan'])
def test_invalid_temporal_or_nonfinite_data_rejected(change):
    data = deepcopy(payload())
    channel = data['demo']['frames'][0]['channels'][0]
    if change == 'history_future':
        channel['history']['times'][-1] = 2
    elif change == 'forecast_past':
        channel['forecast']['times'][0] = 1
    elif change == 'short_horizon':
        channel['forecast']['times'][-1] = 100
    else:
        channel['forecast']['values'][0] = float('nan')
    with pytest.raises(ValueError):
        render_dashboard(data)


def test_duplicate_or_mislocated_demo_case_rejected():
    data = payload()
    data['demo']['hs_m'] = 2
    with pytest.raises(ValueError, match='demo'):
        render_dashboard(data)


def test_snapshot_must_match_exact_scenario_and_frame():
    data = payload()
    data['snapshot'] = dict(hs_m=1, tp_s=8, now_s=1)
    assert render_dashboard(data)
    data['snapshot']['now_s'] = 2
    with pytest.raises(ValueError, match='snapshot'):
        render_dashboard(data)


def test_wave_preview_mode_requires_aligned_preview_and_explicit_labels():
    data = payload()
    data['demo']['default_mode'] = 'wave_preview'
    with pytest.raises(ValueError, match='preview'):
        render_dashboard(data)
    channel = data['demo']['frames'][0]['channels'][0]
    channel['wave_preview'] = deepcopy(channel['forecast'])
    html = render_dashboard(data)
    assert 'SIMULATED WAVE PREVIEW INPUT' in html
    assert 'offshore wave prediction not validated' in html
    assert 'id="forecast-mode"' in html
    channel['wave_preview']['times'][0] = 3
    with pytest.raises(ValueError, match='preview'):
        render_dashboard(data)


def test_node_controls_keep_demo_case_separate_from_selected_cell():
    node = shutil.which('node')
    if not node:
        pytest.skip('Node unavailable')
    data = payload()
    first = data['demo']
    second = deepcopy(first)
    second.update(case_index=1, hs_m=2)
    data['demo'] = {'scenarios': [first, second]}
    data['cases'].append(dict(index=1, hs_m=2, tp_s=8, status='EXCEEDS_ASSUMPTIONS',
                              reason='Assumed limit exceeded', metrics={}))
    script = r'''
class Element {
 constructor(){this.children=[];this.attrs={};this.events={};this.checked=false;this.value=0;this.textContent='';}
 setAttribute(k,v){this.attrs[k]=v;}
 append(...items){this.children.push(...items);}
 replaceChildren(...items){this.children=items;}
 addEventListener(k,fn){this.events[k]=fn;}
}
const elements={};
global.document={getElementById:id=>elements[id]||(elements[id]=new Element()),
 createElement:()=>new Element(),createElementNS:()=>new Element()};
document.getElementById('payload').textContent=INPUT;
'''.replace('INPUT', json.dumps(json.dumps(data)))
    checks = r'''
if(byId('truth-toggle').checked)throw Error('Truth must default hidden');
selectCell(payload.cases[1]);
if(scenarioIndex!==0)throw Error('Cell selection changed prescribed demo case');
byId('scenario').events.change({target:{value:'1'}});
if(scenarioIndex!==1||frameIndex!==0)throw Error('Scenario change failed');
if(!byId('demo-label').textContent.includes('Hs 2 m'))throw Error('Wrong marker label');
byId('truth-toggle').checked=true;byId('truth-toggle').events.change();
if(byId('charts').children.length!==1)throw Error('Chart update failed');
const note=metricsNote({units:'kN',metrics:{autoregression:{rmse:3},persistence:{rmse:1},history_mean:{rmse:2}}});
if(!note.textContent.includes('No demonstrated advantage'))throw Error('Misleading benefit claim');
forecastMode='wave_preview';
if(!metricsNote({id:'wave_elevation'}).textContent.includes('not a wave prediction'))throw Error('Mislabelled wave input');
const utilization=utilizationText({channels:[{id:'load',label:'Sling',units:'kN',assumed_limit:3,wave_preview:{values:[4,6]}}]});
if(!utilization.includes('2.000')||!utilization.includes('does not reclassify'))throw Error('Utilization scope incorrect');
console.log('controls verified');
'''
    result = subprocess.run([node, '-'], input=script + JAVASCRIPT + checks,
                            text=True, capture_output=True, check=False)
    assert result.returncode == 0, result.stderr
    assert 'controls verified' in result.stdout
