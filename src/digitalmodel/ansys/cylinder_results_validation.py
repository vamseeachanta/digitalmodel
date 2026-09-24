"""Compose raw canary evidence checks; attributed bytes remain caller responsibility."""
import re
from decimal import Decimal

from digitalmodel.ansys.cylinder_results import EvidenceError, parse_station_values, validate_recovery, decimal_context
from digitalmodel.ansys.cylinder_results_audit import parse_configuration, verify_load_audit, split_load_audit
from digitalmodel.ansys.cylinder_results_cdb import parse_model_cdb, verify_model
from digitalmodel.ansys.cylinder_results_diagnostics import classify_diagnostics
from digitalmodel.ansys.cylinder_results_listings import (
    extract_block, parse_nodal_listing, parse_contribution_listing,
)
from digitalmodel.ansys.cylinder_results_state import (
    parse_state_values, parse_support_reactions, validate_precision_witness,
)

REQUIRED = ('model.cdb','station_values.txt','state_values.txt','support_reactions.txt',
            'precision_witness.txt','native.out','jobname.err','stdout','stderr')


def _recovery(artifacts, case, values):
    raw, stations = artifacts['native.out'], case['stations']
    listing = parse_nodal_listing(extract_block(raw,'STRESS'), stations, 'stress')
    listing.update(parse_nodal_listing(extract_block(raw,'DISP'), stations, 'displacement'))
    contributions = parse_contribution_listing(extract_block(raw,'PRESOL'), stations,
                        element_ids={e['element_id'] for e in case['elements']})
    validate_recovery(values, stations, listing, contributions)


def _reaction_listing(raw, expected):
    """Strict synthetic-tested RF table profile; unsupported native headers refuse."""
    from digitalmodel.ansys.cylinder_results import parse_e24
    if raw.lstrip().startswith(b'PRINT F'):
        from digitalmodel.ansys.cylinder_results_native_reactions import verify_reactions
        return verify_reactions(raw,expected)
    lines = [line.rstrip(b'\r') for line in raw.split(b'\n') if line.strip()]
    if not lines or lines[0].split() != [b'NODE',b'FX',b'FY']:
        raise EvidenceError('Unsupported or missing PRRSOL force table header')
    found = {}
    for line in lines[1:]:
        if len(line)!=56 or not re.fullmatch(rb' *[1-9]\d*',line[:8]):
            raise EvidenceError('Unsupported or truncated PRRSOL node row')
        node = int(line[:8])
        if node in found or node not in expected:
            raise EvidenceError('Duplicate or unexpected PRRSOL support node')
        parse_e24(line[8:32],'N')
        value, quantum = parse_e24(line[32:56],'N')
        limit = max(Decimal('1e-12')*max(abs(value),abs(expected[node])),Decimal('1e-8'))
        if quantum>limit/100 or abs(value-expected[node])>limit:
            raise EvidenceError('PRRSOL/support export disagreement or low precision')
        found[node] = value
    if found.keys()!=expected.keys():
        raise EvidenceError('Incomplete PRRSOL support set')


def _parse_all(case, artifacts):
    token, stations = case['case_token'], case['stations']
    verify_model(parse_model_cdb(artifacts['model.cdb']),case)
    values = parse_station_values(artifacts['station_values.txt'],token,stations)
    parse_state_values(artifacts['state_values.txt'],token,len(case['nodes']),len(case['elements']))
    validate_precision_witness(artifacts['precision_witness.txt'],token)
    native = artifacts['native.out']
    if (b'CURRENT ANSYS CONFIGURATION' in native or b'No surface loads to list.' in native
            or re.search(rb'(?m)^ *PRINT ', native)):
        from digitalmodel.ansys.cylinder_results_native_status import verify_case_titles
        verify_case_titles(artifacts['native.out'],token)
    config = parse_configuration(artifacts['native.out'])
    diagnostic = classify_diagnostics(artifacts['native.out'],artifacts['jobname.err'],
                    artifacts['stdout'],artifacts['stderr'],nerr_nmerr=config['nerr_nmerr'])
    if diagnostic['status']!='COMPLETE':
        raise EvidenceError('; '.join(diagnostic['errors']))
    verify_load_audit(split_load_audit(artifacts['native.out'])['loads'],case)
    _recovery(artifacts,case,values)
    nodes = [n for n in case['nodes'] if n['node_id'] in case['bottom_node_ids']]
    reactions = parse_support_reactions(artifacts['support_reactions.txt'],token,nodes)
    _reaction_listing(extract_block(artifacts['native.out'],'SUPPORT'),reactions['by_node'])
    return values,reactions['sum_rfy_n']


@decimal_context
def validate_native_evidence(case, artifacts, approved_reference_hash, observed_reference_hash):
    """COMPLETE is evidence parsing only; numerical and independent gates remain."""
    errors = []
    if (not isinstance(approved_reference_hash,str)
            or not re.fullmatch(r'[0-9a-f]{64}',approved_reference_hash)
            or observed_reference_hash!=approved_reference_hash):
        errors.append('Approved reference hash is absent or differs')
    for name in REQUIRED:
        if not isinstance(artifacts.get(name),bytes):
            errors.append(f'Missing raw native artifact: {name}')
    values, force = {}, None
    if not errors:
        try:
            values,force = _parse_all(case,artifacts)
        except (EvidenceError,KeyError,ValueError,TypeError,UnicodeError) as exc:
            errors.append(str(exc))
    return {'status':'INCOMPLETE' if errors else 'COMPLETE','errors':errors,
            'values':values,'rfy_sum':force,'engineering_qualified':False,
            'native_grammar_compatibility':'Explicit synthetic and retained v261 layouts only; unsupported layouts refuse'}
