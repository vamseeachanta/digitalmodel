"""Collect completed numerical checks; never launch or release dependent stages."""
import argparse
from datetime import datetime, timezone
from hashlib import sha256
from html import escape
import json
from pathlib import Path
import re

from digitalmodel.workflows.installation_numerical_compare import _read_pinned
from digitalmodel.workflows.installation_numerical_compare_plan import (
    _documents, _preparation_row, _anchor_row, _bind_run, _selected, compare_planned_pair,
)
from digitalmodel.workflows.installation_seastates import _json


def _resolve(base, value):
    return (base / value).resolve()


def _config(path, digest, consumed):
    path=Path(path).resolve()
    config=json.loads(_read_pinned(path,digest,consumed))
    if config.get('schema_version')!=1 or not config.get('cases'):
        raise ValueError('Nonempty stage schema version 1 required')
    for key in ('plan_path','preparation_manifest'):
        config[key]=str(_resolve(path.parent,config[key]))
    seen=set(); runs=set()
    for case in config['cases']:
        allowed={'case_id','parent','candidate','parent_receipt_sha256','parent_metadata_sha256'}
        if set(case)!=allowed:raise ValueError('Exact supported case keys required')
        name=case['case_id']
        if not isinstance(name,str) or not re.fullmatch(r'[A-Za-z0-9_-]+',name) or name in seen:
            raise ValueError('Unique safe case IDs required')
        seen.add(name)
        for key in ('parent','candidate'):case[key]=str(_resolve(path.parent,case[key]))
        if case['candidate']==case['parent'] or case['candidate'] in runs:
            raise ValueError('Fresh distinct candidate run directories required')
        runs.add(case['candidate'])
    parents=[Path(c['parent']) for c in config['cases']]
    candidates=[Path(c['candidate']) for c in config['cases']]
    for candidate in candidates:
        others=parents+[p for p in candidates if p!=candidate]
        if any(candidate.is_relative_to(p) or p.is_relative_to(candidate) for p in others):
            raise ValueError('Candidate run roots must be disjoint from all parent/candidate roots')
    return config


def _pool(path,digest,consumed):
    raw=json.loads(_read_pinned(Path(path).resolve(),digest,consumed))
    if not isinstance(raw,dict) or not isinstance(raw.get('results'),list):
        raise ValueError('Completed pool summary/results required')
    results=raw['results']
    counts=dict(total_files=len(results),successful=sum(r.get('status')=='success' for r in results),
        failed=sum(r.get('status') in ('failed','exception') for r in results))
    if any(type(raw.get(k)) is not int or raw[k]!=v for k,v in counts.items()):
        raise ValueError('Pool summary counts mismatch')
    rows={}
    for result in results:
        name=result['file_path']
        if name in rows:raise ValueError('Duplicate pool result')
        rows[name]=result
    return rows


def _candidate(config,case,result,consumed):
    row=result.get('row',{})
    if result.get('status')!='success' or row.get('status')!='completed':
        raise ValueError('Every selected pool row must be successful and completed')
    if row.get('id')!=case['case_id'] or Path(row['run_dir']).resolve()!=Path(case['candidate']):
        raise ValueError('Pool case/run path mismatch')
    pair={k:config[k] for k in ('plan_path','plan_sha256','preparation_manifest','preparation_sha256')}
    pair.update(case)
    plan,prepared_path,planned,actual=_documents(pair,consumed)
    child,_,_=_selected(plan,planned,case['case_id'])
    parent_id=child['compare_to']
    parent=(_preparation_row(actual[parent_id],prepared_path.parent,consumed) if parent_id in actual else
        _anchor_row(parent_id,plan['source_anchors'][parent_id],consumed))
    _bind_run(case['parent'],case['parent_receipt_sha256'],parent,consumed)
    prepared=actual[case['case_id']]
    expected=_preparation_row(prepared,prepared_path.parent,consumed)
    if (row.get('model_sha256'),row.get('request_sha256'))!=expected:
        raise ValueError('Pool model/request pins differ from native preparation')
    model=_resolve(prepared_path.parent,prepared['model_path'])
    if Path(row['model']).resolve()!=model or Path(row['request']).resolve()!=model.parent/'request.yml':
        raise ValueError('Pool model/request paths differ from native preparation')
    root=Path(case['candidate']); run_path=root/'run.json'
    run_digest=sha256(run_path.read_bytes()).hexdigest()
    receipt=json.loads(_read_pinned(run_path,run_digest,consumed))
    if receipt.get('status')!='completed' or (receipt.get('model_sha256'),receipt.get('request_sha256'))!=expected:
        raise ValueError('Candidate receipt is not a completed prepared run')
    metadata=json.loads(_read_pinned(root/'installation_traces/metadata.json',row['trace_metadata_sha256'],consumed))
    if metadata.get('trace_sha256')!=row.get('trace_sha256'):
        raise ValueError('Pool trace digest mismatch')
    pair.update(candidate_receipt_sha256=run_digest,candidate_metadata_sha256=row['trace_metadata_sha256'])
    return pair


def _preflight(config,pool,consumed,output):
    pairs=[]
    if set(pool)!={c['case_id'] for c in config['cases']}:
        raise ValueError('Pool/stage case coverage must match exactly')
    for case in config['cases']:
        if case['case_id'] not in pool:raise ValueError('Missing selected pool result')
        for name in ('parent','candidate'):
            root=Path(case[name])
            if output.is_relative_to(root) or root.is_relative_to(output):
                raise ValueError('Output must be isolated from run evidence')
        pair=_candidate(config,case,pool[case['case_id']],consumed)
        for suffix,key in [('run.json','parent_receipt_sha256'),('installation_traces/metadata.json','parent_metadata_sha256')]:
            _read_pinned(Path(case['parent'])/suffix,case[key],consumed)
        pairs.append(pair)
    for path in consumed:
        if path.is_relative_to(output):raise ValueError('Output overlaps pinned evidence')
    return pairs


def _html(summary):
    rows=''.join('<tr><td><a href="'+escape(row['comparison_file'],quote=True)+'">'+escape(row['case_id'])+'</a></td>'+
        ''.join(f'<td>{escape(str(row[k]))}</td>' for k in
        ('kind','status','comparison_sha256'))+'</tr>' for row in summary['cases'])
    return '<!doctype html><html><head><meta charset="utf-8"><title>Numerical check collection</title>'+(
        '<style>body{font:16px Arial;max-width:1200px;margin:3em auto;padding:1em}table{border-collapse:collapse}'
        'td,th{padding:.6em;border:1px solid #aaa;text-align:left}td:last-child{font:11px monospace}</style></head><body>'
        '<h1>Numerical check collection</h1><h2>1 Introduction</h2><p>Completed numerical checks are compared against pinned parent runs.</p>'
        '<h2>2 Summary and conclusions</h2><p>Stage status: '+escape(summary['status'])+'. Review is required. '
        'Dependent stage release remains disabled; engineering acceptance is NOT EVALUATED.</p>'
        '<h2>3 Method and evidence</h2><p>Created UTC: '+escape(summary['created_utc'])+'. Case count: '+str(len(summary['cases']))+'</p>'
        '<dl>'+''.join('<dt>'+escape(key)+'</dt><dd><code>'+escape(summary[key])+'</code></dd>' for key in
            ('config_sha256','pool_sha256','generator_sha256'))+'</dl><p>Successful pool rows are bound to the pinned native preparation before the existing '
        'planned-pair comparator verifies models, requests, metadata and traces. No solver is launched.</p>'
        '<h2>4 Results</h2><table><tr><th>Case</th><th>Control</th><th>Status</th><th>Comparison SHA-256</th></tr>'
        +rows+'</table><p>Table 4-1. Collected diagnostic comparisons.</p>'
        '<h2>5 Limitations and next review</h2><p>A BLOCKED logging result prevents any implied progression. '
        'A verified diagnostic does not establish convergence, physical qualification or an operating envelope. '
        'Per-case JSON retains channel metrics, warning differences and provenance for technical review.</p></body></html>')


def collect_stage(config_path,config_sha256,pool_result,pool_sha256,output):
    output=Path(output).resolve()
    if output.exists():raise FileExistsError('Exclusive new collection output required')
    consumed={};config=_config(config_path,config_sha256,consumed)
    pool=_pool(pool_result,pool_sha256,consumed);pairs=_preflight(config,pool,consumed,output)
    output.mkdir(parents=True);(output/'INCOMPLETE').write_text('Collection verification pending',encoding='utf-8')
    rows=[]
    for pair in pairs:
        result=compare_planned_pair(pair)
        if result.get('status') not in ('VERIFIED_DIAGNOSTIC','BLOCKED'):
            raise ValueError('Unknown comparison disposition')
        result.update(requires_review=True,dependent_stage_release=False)
        path=output/(pair['case_id']+'.json');_json(path,result)
        rows.append(dict(case_id=pair['case_id'],kind=result['kind'],status=result['status'],
            comparison_file=path.name,comparison_sha256=sha256(path.read_bytes()).hexdigest()))
    summary=dict(schema_version=1,status='BLOCKED' if any(r['status']=='BLOCKED' for r in rows) else 'REVIEW_REQUIRED',
        requires_review=True,dependent_stage_release=False,engineering_acceptance='NOT EVALUATED',cases=rows,
        created_utc=datetime.now(timezone.utc).isoformat(),config_sha256=config_sha256,pool_sha256=pool_sha256,
        generator_sha256=sha256(Path(__file__).read_bytes()).hexdigest(),
        sources=[dict(path=str(p),sha256=h) for p,h in consumed.items()])
    for path,digest in consumed.items():_read_pinned(path,digest,{})
    html=_html(summary);(output/'summary.html').write_text(html,encoding='utf-8')
    if (output/'summary.html').read_text(encoding='utf-8')!=html:raise ValueError('HTML readback mismatch')
    summary['html_sha256']=sha256((output/'summary.html').read_bytes()).hexdigest()
    _json(output/'summary.json',summary);(output/'INCOMPLETE').unlink()
    return summary


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    for key in ('config','config-sha256','pool-result','pool-result-sha256','output'):parser.add_argument('--'+key,required=True)
    args=parser.parse_args()
    result=collect_stage(args.config,args.config_sha256,args.pool_result,args.pool_result_sha256,args.output)
    print(json.dumps(dict(status=result['status'],requires_review=True,dependent_stage_release=False)))
    return 2 if result['status']=='BLOCKED' else 0


if __name__=='__main__':raise SystemExit(main())
