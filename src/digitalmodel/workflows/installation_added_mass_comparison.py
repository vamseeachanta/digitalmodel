"""Compare fifteen verified one-at-a-time added-mass cases; no acceptance selection."""
import argparse
from datetime import datetime, timezone
from hashlib import sha256
from html import escape
import json
import math
from pathlib import Path

import numpy as np

from digitalmodel.workflows.installation_response_metrics import tension_event_metrics
from digitalmodel.workflows.installation_added_mass_evidence import pinned_json, read_cases, reverify


def validate_coverage(bracket, identities):
    rows = bracket.get('cases', [])
    if len(rows) != 15 or len(identities) != 15 or len(set(identities)) != 15:
        raise ValueError('Fifteen unique composite case IDs required')
    if set(identities) != {r['id'] for r in rows}:
        raise ValueError('Completed cases do not match bracket IDs')
    pairs = {(r['source_case_index'], r['factor']) for r in rows}
    indices = {r['source_case_index'] for r in rows}
    if len(indices) != 5 or pairs != {(i, f) for i in indices for f in (.5, 1., 1.5)}:
        raise ValueError('Complete three-factor by five-case matrix required')
    for row in rows:
        if type(row['source_case_index']) is not int or type(row['factor']) not in (int, float):
            raise ValueError('Numeric factor and integer source index required')
        for value in (row['factor'], row['added_mass_t'], *[row['settings'][k] for k in ('hs_m', 'tp_s', 'seed')]):
            if isinstance(value, bool) or not math.isfinite(value) or value < 0:
                raise ValueError('Finite nonnegative case data required')
    for index in indices:
        group = [r for r in rows if r['source_case_index'] == index]
        if any(r['settings'] != group[0]['settings'] for r in group):
            raise ValueError('Matched wave/numerical settings differ')


def absolute_delta(value, reference):
    return None if value is None or reference is None else value-reference


def channel_identity(channel):
    return json.dumps([channel['object'], channel['variable'], channel.get('position'), channel['units']], sort_keys=True)


def verify_matched_arrays(left, right):
    for key in ('time', 'wave_elevation'):
        if not np.array_equal(left[key], right[key]):
            raise ValueError('Matched time/wave arrays differ')


def peak_summary(events, duration_threshold_s=.1):
    def maximum(items):
        usable = [e for e in items if e.get('retension_peak') is not None]
        return max(usable, key=lambda e:e['retension_peak']).copy() if usable else None
    filtered = [e for e in events if e['duration_s'] >= duration_threshold_s]
    uncensored = [e for e in filtered if not any(e.get(k, False) for k in
                  ('left_censored', 'right_censored', 'retension_window_censored'))]
    return dict(all_observed=maximum(events), duration_ge_0_1_s=maximum(filtered),
        duration_ge_0_1_s_uncensored=maximum(uncensored), duration_threshold_s=duration_threshold_s,
        left_censored_count=sum(e.get('left_censored', False) for e in events),
        right_censored_count=sum(e.get('right_censored', False) for e in events),
        window_censored_count=sum(e.get('retension_window_censored', False) for e in events),
        filtered_excluded_count=len(events)-len(uncensored),
        below_duration_threshold_count=len(events)-len(filtered),
        duration_qualified_censored_count=len(filtered)-len(uncensored),
        duration_resolution_limited_count=sum(abs(e['duration_s']-duration_threshold_s) <= duration_threshold_s for e in events))


def calculate_channels(arrays, metadata):
    output = []; times = arrays['time']; interval = float(times[1]-times[0])
    if not np.isclose(interval, .1, rtol=0, atol=1e-10):
        raise ValueError('This declared duration ranking requires 0.1 s logging')
    for key, channel in metadata['channels'].items():
        variable = channel['variable']
        if variable not in ('Effective tension', 'unstretched_length_minus_span_m'):
            continue
        values = arrays[key]
        if not np.isfinite(values).all():
            raise ValueError('Nonfinite response array')
        row = dict(identity=channel_identity(channel), channel=key, object=channel['object'],
                   variable=variable, position=channel.get('position'), units=channel['units'])
        if variable == 'Effective tension':
            if channel['units'] != 'kN':
                raise ValueError('Effective tension must be in kN')
            diagnostics = tension_event_metrics(times, values, units='kN')
            events = diagnostics['low_tension']
            for event in events['events']:
                mask=(times>=event['start_s']) & (times<=event['end_s'])
                event['classification']='nonpositive_with_negative_samples' if np.any(values[mask]<0) else 'exact_zero_plateau'
                event['duration_resolution_limited']=abs(event['duration_s']-.1)<=.1
                event['peak_floored']=event['retension_peak']==0 and event['retension_peak_time_s'] is None
            peaks = peak_summary(events['events'], .1)
            peak = peaks['all_observed']; filtered = peaks['duration_ge_0_1_s_uncensored']
            row.update(metrics=dict(maximum_tension_kN=float(values.max()), minimum_signed_tension_kN=float(values.min()),
                nonpositive_total_s=events['total_duration_s'], nonpositive_longest_s=events['maximum_duration_s'],
                event_count=events['event_count'], postexit_peak_kN=peak['retension_peak'] if peak else None,
                strict_negative_event_count=diagnostics['compression']['event_count'],
                duration_filtered_uncensored_peak_kN=filtered['retension_peak'] if filtered else None), event_summary=peaks)
        else:
            if channel['units'] != 'm':
                raise ValueError('Chord deficit must be in m')
            row['metrics'] = dict(maximum_chord_deficit_m=float(values.max()), minimum_chord_deficit_m=float(values.min()))
        output.append(row)
    if not output or len({r['identity'] for r in output}) != len(output):
        raise ValueError('Missing or duplicate semantic response channels')
    return output


def _matched(verified):
    for item in verified:
        group = [x for x in verified if x['case']['source_case_index'] == item['case']['source_case_index']]
        reference = next(x for x in group if x['case']['factor'] == 1.)
        verify_matched_arrays(item['arrays'], reference['arrays'])
        if item['context'] != reference['context']:
            raise ValueError('Matched model/solver/profile/extractor/logging context differs')
        left = {channel_identity(c) for c in item['metadata']['channels'].values()}
        right = {channel_identity(c) for c in reference['metadata']['channels'].values()}
        if left != right:
            raise ValueError('Matched semantic channel coverage differs')


def _comparisons(cases):
    rows = []
    for case in cases:
        reference = next(r for r in cases if r['source_case_index'] == case['source_case_index'] and r['factor'] == 1.)
        lookup = {r['identity']:r for r in reference['channels']}
        for channel in case['channels']:
            baseline = lookup[channel['identity']]['metrics']
            rows.append(dict(case_id=case['id'], source_case_index=case['source_case_index'], factor=case['factor'],
                identity=channel['identity'], object=channel['object'], position=channel['position'], units=channel['units'],
                values=channel['metrics'], absolute_delta_from_factor_1={k:absolute_delta(v,baseline[k]) for k,v in channel['metrics'].items()}))
    return rows


def _rankings(cases):
    result = []
    keys = ('maximum_tension_kN', 'minimum_signed_tension_kN', 'nonpositive_total_s', 'nonpositive_longest_s',
            'maximum_chord_deficit_m', 'postexit_peak_kN', 'duration_filtered_uncensored_peak_kN')
    for factor in (.5, 1., 1.5):
        for key in keys:
            candidates = [dict(case_id=c['id'], source_case_index=c['source_case_index'],
                hs_m=c['settings']['hs_m'], tp_s=c['settings']['tp_s'], object=r['object'], position=r['position'],
                channel=r['channel'], value=r['metrics'][key]) for c in cases if c['factor']==factor
                for r in c['channels'] if r['metrics'].get(key) is not None]
            select = min if key.startswith('minimum_') else max
            winner = select(candidates,key=lambda r:r['value']) if candidates else None
            if winner and 'peak_kN' in key:
                case=next(c for c in cases if c['id']==winner['case_id'])
                channel=next(r for r in case['channels'] if r['channel']==winner['channel'])
                event_key='all_observed' if key=='postexit_peak_kN' else 'duration_ge_0_1_s_uncensored'
                winner['event']=channel['event_summary'][event_key]
            if winner and key.startswith('nonpositive') and winner['value']==0:
                winner = dict(value=0, governing_case=None, interpretation='No nonpositive-tension duration; no governing case')
            result.append(dict(factor=factor, metric=key, governing=winner,
                treatment='Duration-filtered, uncensored' if key.startswith('duration_filtered') else 'As-observed, unfiltered'))
    return result


def _findings(result):
    paragraphs=[]
    for row in result['rankings']:
        value=row['governing']
        if row['metric']=='maximum_tension_kN' and value:
            paragraphs.append(f"Factor {row['factor']:g}: the largest retained tensile demand is {value['value']:.6g} kN at {value['object']} {value['position']}, {value['case_id']} (Hs {value['hs_m']:g} m, Tp {value['tp_s']:g} s). This is a demand comparison; no capacity acceptance is established.")
        if row['metric']=='duration_filtered_uncensored_peak_kN' and value:
            event=value['event']
            paragraphs.append(f"Factor {row['factor']:g}: the filtered 2 s post-exit peak is {value['value']:.6g} kN at {value['object']} {value['position']}, {value['case_id']}. The initiating event lasts {event['duration_s']:.9g} s and is classified {event['classification']}; duration resolution limited: {event['duration_resolution_limited']}. This is not evidence of a physical snap load.")
    return '<p>'+'</p><p>'.join(escape(text) for text in paragraphs)+'</p>'


def _detail_rows(comparisons):
    keys=('maximum_tension_kN','minimum_signed_tension_kN','nonpositive_total_s','nonpositive_longest_s',
          'maximum_chord_deficit_m','postexit_peak_kN','duration_filtered_uncensored_peak_kN')
    rows=[]
    for row in comparisons:
        for key in keys:
            if key in row['values']:
                rows.append([row['case_id'],row['factor'],row['object'],row['position'],key,
                             row['values'][key],row['absolute_delta_from_factor_1'][key]])
    return rows


def _summary_rows(rankings):
    rows=[]
    for row in rankings:
        point=row['governing']
        if point is None:
            rows.append([row['factor'],row['metric'],row['treatment'],'n/a',
                         'No qualifying event','n/a','n/a','n/a','n/a','No post-exit peak'])
            continue
        if point.get('interpretation'):
            rows.append([row['factor'],row['metric'],row['treatment'],point['value'],
                         'No governing case','n/a','n/a','n/a','n/a',point['interpretation']])
            continue
        event=point.get('event')
        note=''
        if event:
            censored=any(event[k] for k in ('left_censored','right_censored','retension_window_censored'))
            note=f"{event['classification']}; duration {event['duration_s']:.9g} s; resolution limited: {event['duration_resolution_limited']}; censored: {censored}; peak floored: {event['peak_floored']}"
        rows.append([row['factor'],row['metric'],row['treatment'],point['value'],point['case_id'],
                     point['hs_m'],point['tp_s'],point['object'],point['position'],note])
    return rows


def render_html(result):
    def table(headers, rows, caption):
        content = '<table><tr>'+''.join('<th>'+escape(h)+'</th>' for h in headers)+'</tr>'
        content += ''.join('<tr>'+''.join('<td>'+escape(f'{v:.6g}' if isinstance(v,float) else str(v) if v is not None else 'Not available')+'</td>' for v in row)+'</tr>' for row in rows)
        return content+'</table><p>'+escape(caption)+'</p>'
    summary=_summary_rows(result['rankings'])
    body='<h1>Vertical added-mass diagnostic comparison</h1><p>Technical review; engineering acceptance NOT EVALUATED.</p>'
    body+='<h2>1 Introduction</h2><p>Three Z added-mass factors are compared at five matched sea states. Factor 1 is the precise bracket reference, not the historical rounded-coefficient campaign.</p>'
    body+='<h2>2 Summary and conclusions</h2>'+_findings(result)
    body+='<h2>3 Design data and methodology</h2><p>Original XY added mass, drag and other model properties are retained. Verified stored end-to-end chord deficits include sag and elastic effects; they are not physical slack lengths. Post-exit peaks use a 2 s window. Filtered ranking requires event duration ≥0.1 s (one actual logging interval) and excludes left/right-censored events and truncated post-exit windows. Events within one logging interval of this threshold are resolution limited. Only absolute same-channel deltas are calculated.</p>'
    masses=sorted({(c['factor'],c['added_mass_t']) for c in result['cases']})
    body+=table(['Factor (1)','Vertical added mass (t)'],masses,'Table 1. Prescribed one-at-a-time added-mass bracket.')
    windows=sorted({(c['sample_count'],*c['extraction_interval_s'],c['logging_interval_s']) for c in result['cases']})
    body+=table(['Samples (count)','Start (s)','End (s)','Logging interval (s)'],windows,'Table 2. Retained trace extraction window; build-up is excluded.')
    body+='<h2>4 Validation and limitations</h2><ul>'+''.join('<li>'+escape(s)+'</li>' for s in result['limitations'])+'</ul>'
    body+='<h3>Solver warnings</h3><ul>'+''.join('<li>'+escape(r['id']+': '+w)+'</li>' for r in result['cases'] for w in r['warnings'])+'</ul>'
    body+='<h2>5 Results</h2>'+table(['Factor (1)','Metric','Treatment','Value (kN, s or m as identified)','Case ID','Hs (m)','Tp (s)','Component','Location','Event qualification'],summary,'Table 3. Governing observed response per factor. Post-exit maxima are not evidence of resolved snap loading. Only the explicitly duration-filtered rows apply the duration and censoring filter.')
    body+='<h2>Appendix A Detailed component results</h2>'+table(['Composite case','Factor (1)','Component','Location','Metric','Value (kN, s or m as identified)','Absolute delta (same units)'],_detail_rows(result['comparisons']),'Table A-1. Same-channel comparisons. Not available indicates absent post-exit evidence, not zero.')
    body+='<p>Event censoring counts, resolution flags, extraction windows and source digests are retained in <a href="comparison.json">comparison.json</a>.</p>'
    return '<!doctype html><html lang="en"><meta charset="utf-8"><title>Added-mass comparison</title><style>body{font:14px Arial;margin:32px;color:#213547}table{border-collapse:collapse;width:100%;font-size:12px}th,td{border:1px solid #bbc;padding:5px;vertical-align:top}th{background:#eef2f6}</style>'+body+'</html>'


def generate_comparison(sequence_path, sequence_sha256, bracket_path, bracket_sha256, output):
    output=Path(output).resolve()
    if output.exists():
        raise FileExistsError('New comparison output required')
    sequence=pinned_json(sequence_path,sequence_sha256); bracket=pinned_json(bracket_path,bracket_sha256)
    if not sequence.get('finished_utc') or (Path(sequence_path).parent/'sequence.lock').exists():
        raise ValueError('Finished unlocked sequence required')
    identities=[r['id'] for batch in sequence.get('batch_results',[]) for r in batch['result']['cases']]
    validate_coverage(bracket,identities)
    evidence=[];verified=read_cases(sequence,bracket,bracket_sha256,evidence)
    validate_coverage(bracket,[r['case']['id'] for r in verified]);_matched(verified)
    cases=[dict(item['case'],channels=calculate_channels(item['arrays'],item['metadata']),audit=item['audit'],
        extraction_interval_s=[float(item['arrays']['time'][0]),float(item['arrays']['time'][-1])],
        sample_count=len(item['arrays']['time']),
        logging_interval_s=item['context']['logging_interval_s'],warnings=item['warnings'],
        warning_count=len(item['warnings'])) for item in verified]
    result=dict(created_utc=datetime.now(timezone.utc).isoformat(),engineering_acceptance='NOT EVALUATED',
        status='verified_diagnostic_comparison',cases=cases,comparisons=_comparisons(cases),rankings=_rankings(cases),
        input_sha256=dict(sequence=sequence_sha256,bracket=bracket_sha256),sources=evidence,
        limitations=bracket['limitations']+[
            'Post-exit peaks are observed maxima over a fixed 2 s window, not amplification factors and not evidence of resolved snap loading.',
            'Endpoint event counts are separate channel records and do not count distinct physical events.',
            'One random seed per sea state; no across-seed expectation or validated operating envelope is established.',
            'Only the intended post-build-up extraction window is compared; stationarity is not established.',
            'Negative signed tension is a native response diagnostic, not physical sling compression capacity.',
            'Chord arrays are retained derived diagnostics; raw endpoint XYZ arrays are unavailable for independent reconstruction.',
            'Solver warnings, including any RAO extrapolation or compression segmentation warning, remain unresolved response-basis limitations.'])
    reverify(evidence);pinned_json(sequence_path,sequence_sha256);pinned_json(bracket_path,bracket_sha256)
    raw=json.dumps(result,indent=2,allow_nan=False);html=render_html(result)
    output.mkdir(parents=True,exist_ok=False);marker=output/'INCOMPLETE';marker.write_text('Output verification pending')
    for path,text in [(output/'comparison.json',raw),(output/'comparison.html',html)]:
        with path.open('x',encoding='utf-8',newline='\n') as stream:stream.write(text)
        if path.read_bytes()!=text.encode('utf-8'):raise ValueError('Comparison artifact readback mismatch')
    reverify(evidence);pinned_json(sequence_path,sequence_sha256);pinned_json(bracket_path,bracket_sha256);marker.unlink()
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    for name in ('sequence','bracket'):
        parser.add_argument('--'+name,required=True,type=Path)
        parser.add_argument('--'+name+'-sha256',required=True)
    parser.add_argument('--output',required=True,type=Path);args=parser.parse_args()
    result=generate_comparison(args.sequence,args.sequence_sha256,args.bracket,args.bracket_sha256,args.output)
    print(json.dumps({'status':result['status'],'cases':len(result['cases'])}))


if __name__=='__main__':main()
