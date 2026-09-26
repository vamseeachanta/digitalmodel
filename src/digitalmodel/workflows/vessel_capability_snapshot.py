"""Rerender a pinned structure-demand snapshot without recalculating evidence."""
import argparse
from datetime import datetime, timezone
from hashlib import sha256
import json
import os
from pathlib import Path

from digitalmodel.workflows.installation_added_mass_bracket import read_pinned
from digitalmodel.workflows.vessel_capability_report import render_html


def _reference(path,digest,base):
    return dict(path=os.path.relpath(path,base).replace('\\','/'),sha256=digest)


def render_snapshot(source,source_sha256,config,config_sha256,output,screening=None,screening_sha256=None):
    output=Path(output).resolve();sidecar=output.with_suffix('.json')
    partials=[Path(str(p)+'.partial') for p in (output,sidecar)]
    if any(p.exists() for p in (output,sidecar,*partials)) or output==sidecar:
        raise FileExistsError('Exclusive new HTML and provenance destinations required')
    source,raw=read_pinned(source,source_sha256);config_path,config_raw=read_pinned(config,config_sha256)
    summary=json.loads(raw);settings=json.loads(config_raw)
    screen=None
    if screening is not None:
        if not screening_sha256:raise ValueError('Screening payload requires a pinned SHA-256 digest')
        screening_path,screening_raw=read_pinned(screening,screening_sha256);screen=json.loads(screening_raw)
        if screen.get('provenance',{}).get('summary',{}).get('sha256')!=source_sha256:
            raise ValueError('Screening payload is not bound to the pinned source snapshot')
    created=datetime.now(timezone.utc).isoformat();settings=dict(settings,rendered_utc=created)
    html=render_html(summary,output.parent,settings,screen)
    dependencies=['vessel_capability_snapshot','vessel_capability_report','vessel_capability_layout',
        'installation_report_layout','installation_partial_report','installation_added_mass_bracket']
    record=dict(schema_version=1,created_utc=created,source_snapshot_utc=summary['created_utc'],
        source=_reference(source,source_sha256,output.parent),config=_reference(config_path,config_sha256,output.parent),
        html_sha256=sha256(html.encode('utf-8')).hexdigest(),engineering_acceptance='NOT EVALUATED',
        reproduction=dict(module='digitalmodel.workflows.vessel_capability_snapshot',
            parameters=['--source','--source-sha256','--config','--config-sha256','--output','--screening','--screening-sha256']),
        dependency_sha256={name:sha256(Path(__file__).with_name(name+'.py').read_bytes()).hexdigest() for name in dependencies})
    if screen is not None:
        record['screening']=_reference(screening_path,screening_sha256,output.parent)
    pins=[(source,source_sha256),(config_path,config_sha256)]+([(screening_path,screening_sha256)] if screen is not None else [])
    for pin in pins:read_pinned(*pin)
    output.parent.mkdir(parents=True,exist_ok=True)
    for path,text in zip(partials,[html,json.dumps(record,indent=2,allow_nan=False)]):
        with path.open('x',encoding='utf-8',newline='\n') as stream:stream.write(text)
        if path.read_text(encoding='utf-8')!=text:raise ValueError('Presentation readback mismatch')
    for pin in pins:read_pinned(*pin)
    for partial,destination in zip(partials,(output,sidecar)):partial.rename(destination)
    return record


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    for key in ('source','source-sha256','config','config-sha256','output'):parser.add_argument('--'+key,required=True)
    parser.add_argument('--screening');parser.add_argument('--screening-sha256')
    args=parser.parse_args()
    result=render_snapshot(args.source,args.source_sha256,args.config,args.config_sha256,args.output,
        args.screening,args.screening_sha256)
    print(json.dumps(result,indent=2))


if __name__=='__main__':main()
