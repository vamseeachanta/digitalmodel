"""Describe frozen cylinder inputs; never calculate or launch native results."""
from pathlib import Path, PurePosixPath

from digitalmodel.ansys.analysis_records import read_json, verify_reference

RELATIVE = Path('examples/ansys/cylinder-benchmark')
QUANTITIES = ('sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz', 'sigma_vm', 'u_r', 'u_z')


def _reference(repo, name, expected, resolver):
    relative = PurePosixPath(name)
    if relative.is_absolute() or '..' in relative.parts or '\\' in name:
        raise ValueError('invalid benchmark reference')
    path = repo/RELATIVE/relative
    if path.is_symlink() or not path.resolve().is_relative_to((repo/RELATIVE).resolve()):
        raise ValueError('redirected benchmark reference')
    identity = 'repository/' + (RELATIVE/relative).as_posix()
    ref = {'id': identity, 'sha256': expected, 'role': 'input_basis', 'required': True}
    resolver[identity] = path
    verify_reference(ref, resolver)
    return ref


def _responses(metadata, evidence_ids):
    rows = []
    for station in metadata['stations']:
        for quantity in QUANTITIES:
            rows.append({'name': station['id'] + '.' + quantity,
                'definition': 'Frozen cylinder canary ' + quantity + '; retained basis and native evidence criteria govern',
                'location': 'X=' + station['x_mm'] + ' mm; Y=' + station['y_mm'] + ' mm; axisymmetric',
                'unit': 'mm' if quantity.startswith('u_') else 'MPa', 'value': None,
                'calculation_status': 'not_evaluated', 'limitations': ['native-not-attempted'],
                'evidence_ids': evidence_ids, 'inherited_findings': []})
    rows.append({'name': 'support.RFY', 'definition': 'Sum of constrained-node axial reaction forces',
        'location': 'All constrained bottom Y=0 nodes including midside nodes', 'unit': 'N',
        'value': None, 'calculation_status': 'not_evaluated', 'limitations': ['native-not-attempted'],
        'evidence_ids': evidence_ids, 'inherited_findings': []})
    return rows


def _parameters(metadata):
    units = {'axial_length_mm': 'mm', 'delta_temperature_C': 'degC',
        'external_pressure_mpa': 'MPa', 'inner_radius_mm': 'mm',
        'poisson_ratio': '1', 'wall_thickness_mm': 'mm', 'youngs_modulus_mpa': 'MPa'}
    values = {key: metadata['basis'][key] for key in units}
    values.update(pressure_mpa=metadata['pressure_mpa'],
        radial_divisions=str(metadata['radial_divisions']), axial_divisions=str(metadata['axial_divisions']))
    units.update(pressure_mpa='MPa', radial_divisions='1', axial_divisions='1')
    return values, units


def _case(metadata, refs, source_revision, observed_at):
    values, units = _parameters(metadata)
    identities = [ref['id'] for ref in refs]
    return {'case_id': metadata['case_id'], 'component_id': 'open-ended-cylinder',
        'model_revision': metadata['deck_sha256'], 'parameters': values,
        'author': 'unverified', 'author_status': 'unverified', 'superseded_by': [],
        'capture_role': 'pending_native', 'source_kind': 'unverified', 'execution_status': 'unknown',
        'attempt_consumed': False, 'native_attempt_count': 0,
        'retention_rights': 'approved', 'use_rights': 'unresolved', 'generated_at': observed_at,
        'input_descriptor': {'load_basis': 'Internal pressure; outer pressure zero; no end-cap load; delta temperature zero',
            'source_revision': source_revision, 'matrix_preparer': 'Codex',
            'solver': {'model': 'PLANE183 axisymmetric Q8', 'requested_product': 'ansys',
                'requested_release': '2026 R1', 'observed_native_version': None,
                'cores': 1, 'parallel': 'smp'},
            'frame': 'X radial; Y axial; Z hoop; tension positive; bottom UY=0; top free',
            'parameter_units': units, 'dependencies': identities,
            'basis': metadata['basis'], 'benchmark_manifest_reference': refs[0],
            'retention_basis': 'Repository-generated inputs; standing non-destructive lifecycle authority; no native result retained'},
        'evidence': refs, 'responses': _responses(metadata, identities)}


def pending_inputs(repo_root, *, source_revision, observed_at, manifest_sha256, reference_sha256):
    """Return pending cases and local evidence resolver for an unchanged bundle.

    The manifest is an input identity, not current execution authorization. This
    reader does not validate a native runtime or invoke the frozen checker.
    """
    repo = Path(repo_root)
    bundle = repo/RELATIVE
    resolver = {}
    base_refs = [_reference(repo, 'manifest.json', manifest_sha256, resolver)]
    manifest = read_json(bundle/'manifest.json')
    if manifest.get('schema') != 'cylinder-b1-1':
        raise ValueError('unsupported prepared manifest')
    artifacts = {row['path']: row['sha256'] for row in manifest['artifacts']}
    if len(artifacts) != len(manifest['artifacts']):
        raise ValueError('duplicate prepared artifact')
    if artifacts[manifest['reference']] != reference_sha256:
        raise ValueError('frozen reference identity differs')
    for name in ('prepared/basis-criteria.json', manifest['reference']):
        base_refs.append(_reference(repo, name, artifacts[name], resolver))
    cases = []
    for entry in manifest['cases']:
        refs = list(base_refs)
        for name in (entry['deck'], entry['metadata']):
            refs.append(_reference(repo, name, artifacts[name], resolver))
        metadata = read_json(bundle/entry['metadata'])
        if metadata['case_id'] != entry['case_id'] or metadata['deck_sha256'] != artifacts[entry['deck']]:
            raise ValueError('prepared case identity differs')
        cases.append(_case(metadata, refs, source_revision, observed_at))
    if [case['case_id'] for case in cases] != manifest['case_order']:
        raise ValueError('prepared case order differs')
    return cases, resolver
