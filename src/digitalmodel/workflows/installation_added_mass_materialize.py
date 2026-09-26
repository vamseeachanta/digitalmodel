"""Native materialization/readback of a bracket case; no dynamics are run."""
import argparse
import copy
from hashlib import sha256
import json
import math
import os
from pathlib import Path

import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader
from digitalmodel.workflows.installation_added_mass_bracket import read_pinned, named_body, expected_change, code_hashes
from digitalmodel.workflows.installation_seastates import (
    _snapshot, _structure, _verify, _configure, _wave_reference, _request, _validate_timeout, _json)
from digitalmodel.workflows.installation_trace_extract import _validate_profile


def verify_structure(before, after, body_name, coefficient_z):
    normalized = copy.deepcopy(after)
    old, actual = named_body(before, body_name), named_body(normalized, body_name)
    ca = actual['AddedMassCoefficient']
    if len(ca) != 3 or not math.isclose(ca[2], coefficient_z, rel_tol=1e-12, abs_tol=1e-12):
        raise ValueError('Native Z added-mass override did not apply')
    ca[2] = old['AddedMassCoefficient'][2]
    if _structure(normalized) != _structure(before):
        raise ValueError('Nonselected native structural property changed')


def validate_extraction(extraction, settings, timeout_seconds):
    _validate_timeout(timeout_seconds)
    if extraction.get('period') != [0., settings['duration_s']]:
        raise ValueError('Extraction must cover complete intended interval')
    _validate_profile(extraction.get('supplemental_profile'))
    if not extraction.get('time_histories'):
        raise ValueError('Explicit time-history extraction required')


def _inside(root, name):
    target = (root / name).resolve()
    if not target.is_relative_to(root):
        raise ValueError('Bracket path escapes study')
    return target


def _selected(study, case_id, manifest_sha256):
    _, raw = read_pinned(study/'manifest.json', manifest_sha256)
    manifest = json.loads(raw)
    if manifest.get('schema_version') != 1 or manifest.get('status') != 'prepared_not_native_verified':
        raise ValueError('Unsupported bracket manifest schema/status')
    if (study/'.incomplete').exists():
        raise ValueError('Bracket preparation is incomplete')
    matches = [r for r in manifest['cases'] if r['id'] == case_id]
    if len(matches) != 1:
        raise ValueError('One unique bracket case required')
    row = matches[0]
    master, _ = read_pinned(_inside(study, manifest['master_file']), manifest['master_sha256'])
    change, change_raw = read_pinned(_inside(study, row['change_file']), row['change_sha256'])
    expected = expected_change(row['settings'], manifest['wave_name'], manifest['body_name'],
                               manifest['source_ca'], row['coefficient_z'])
    if yaml.load(change_raw.decode('utf-8-sig'), Loader=OrcaFlexLoader) != expected:
        raise ValueError('Change differs from declared wave/Z-only payload')
    if (change.parent/expected['BaseFile']).resolve() != master:
        raise ValueError('BaseFile does not resolve to pinned master')
    mass = row['factor'] * manifest['baseline_added_mass_t']
    if not math.isclose(row['coefficient_z'] * manifest['reference_mass_t'], mass, rel_tol=2e-12):
        raise ValueError('Declared factor/coefficient inconsistent')
    if not math.isclose(row['added_mass_t'], mass, rel_tol=1e-12):
        raise ValueError('Declared added_mass_t differs from factor/baseline')
    return manifest, row, master, change, raw


def _objects(model, extraction):
    channels, geometry = _validate_profile(extraction['supplemental_profile'])
    names = {row['object'] for row in channels} | set(geometry)
    names |= {row['object'] for row in extraction['time_histories']}
    names |= {row['object'] for row in extraction.get('range_graphs', [])}
    existing = {obj.name for obj in model.objects}
    if names - existing:
        raise ValueError('Extraction profile references absent native objects')


def _native_models(api, master, change, manifest, row, extraction):
    baseline = api.Model(threadCount=1)
    baseline.LoadData(str(master))
    before = _snapshot(api, baseline)
    reference = _wave_reference(baseline.environment)
    model = api.Model(threadCount=1)
    model.LoadData(str(change))
    _verify(model, row['settings'], reference)
    body = model[manifest['body_name']]
    actual = [getattr(body, 'AddedMassCoefficient'+axis) for axis in 'XYZ']
    expected = manifest['source_ca'][:2] + [row['coefficient_z']]
    if any(not math.isclose(a, b, rel_tol=1e-12, abs_tol=1e-12) for a, b in zip(actual, expected)):
        raise ValueError('Native variation added-mass override did not apply')
    # A BaseFile model saves a partial variation, not an expanded snapshot.
    # Build the standalone model from the pinned master and verified change.
    model = baseline
    model[manifest['body_name']].AddedMassCoefficientZ = row['coefficient_z']
    _configure(model, copy.deepcopy(row['settings']))
    _verify(model, row['settings'], reference)
    verify_structure(before, _snapshot(api, model), manifest['body_name'], row['coefficient_z'])
    _objects(model, extraction)
    return model, before, reference


def _write_request(output, extraction, settings, solver_version, timeout_seconds):
    _request(output, extraction, settings, solver_version, timeout_seconds)
    request = yaml.safe_load((output/'request.yml').read_bytes())
    removed = [text for text in request['limitations'] if text.startswith('Stationary extraction')]
    if len(removed) != 1:
        raise ValueError('Unexpected inherited stationarity limitation')
    request['limitations'] = [text for text in request['limitations'] if text not in removed]
    request['limitations'].append(f"Intended extraction window 0–{settings['duration_s']:g} s excludes {settings['buildup_s']:g} s build-up; stationarity is unverified.")
    (output/'request.yml').write_text(yaml.safe_dump(request, sort_keys=False), encoding='utf-8')
    if yaml.safe_load((output/'request.yml').read_bytes()) != request or request['extraction'] != extraction:
        raise ValueError('Request/extraction readback mismatch')


def verify_solver(api, solver_version, identity):
    if (not isinstance(identity, dict) or identity.get('requested') != solver_version
            or identity.get('resolved_version') != solver_version or api.DLLVersion() != solver_version):
        raise ValueError('Native solver identity/version mismatch')
    library = Path(identity.get('resolved_lib_path', '')).resolve(strict=True)
    if not library.is_file():
        raise ValueError('Native solver library missing')
    return dict(identity, library_sha256=sha256(library.read_bytes()).hexdigest())


def materialize_bracket_case(api, study, case_id, output, *, manifest_sha256, extraction_request,
                             extraction_request_sha256, solver_version, timeout_seconds, solver_identity):
    study, output = Path(study).resolve(), Path(output).resolve()
    if output.exists():
        raise FileExistsError('New native materialization output required')
    if not isinstance(solver_version, str) or not solver_version.strip():
        raise ValueError('Explicit solver version required')
    identity = verify_solver(api, solver_version, solver_identity)
    manifest, row, master, change, manifest_raw = _selected(study, case_id, manifest_sha256)
    request_path, request_raw = read_pinned(extraction_request, extraction_request_sha256)
    extraction = yaml.safe_load(request_raw)['extraction']
    validate_extraction(extraction, row['settings'], timeout_seconds)
    if manifest['body_name'] not in {c['object'] for c in extraction['supplemental_profile']['channels']}:
        raise ValueError('Supplemental extraction must observe bracket body')
    model, before, reference = _native_models(api, master, change, manifest, row, extraction)
    output.mkdir(parents=True, exist_ok=False)
    (output/'.incomplete').write_text('Native preparation has not completed.\n')
    model.SaveData(str(output/'model.yml'))
    model.LoadData(str(output/'model.yml'))
    _verify(model, row['settings'], reference)
    verify_structure(before, _snapshot(api, model), manifest['body_name'], row['coefficient_z'])
    _objects(model, extraction)
    _write_request(output, extraction, row['settings'], solver_version, timeout_seconds)
    read_pinned(master, manifest['master_sha256'])
    read_pinned(change, row['change_sha256'])
    read_pinned(request_path, extraction_request_sha256)
    if (study/'manifest.json').read_bytes() != manifest_raw:
        raise ValueError('Manifest changed during materialization')
    receipt = dict(schema_version=1, status='native_verified_not_run', engineering_acceptance='NOT EVALUATED',
        solver=identity, code_sha256=code_hashes(),
        case_id=case_id, settings=row['settings'], factor=row['factor'], added_mass_t=row['added_mass_t'],
        coefficient_z=row['coefficient_z'], wave_reference=reference,
        model_sha256=sha256((output/'model.yml').read_bytes()).hexdigest(),
        request_sha256=sha256((output/'request.yml').read_bytes()).hexdigest(),
        dependencies=dict(master_sha256=manifest['master_sha256'], change_sha256=row['change_sha256'],
            manifest_sha256=sha256(manifest_raw).hexdigest(), extraction_request_sha256=extraction_request_sha256),
        extraction_source=os.path.relpath(request_path, output).replace('\\', '/'),
        generator_sha256=sha256(Path(__file__).read_bytes()).hexdigest(), request='request.yml')
    _json(output/'generation.json', receipt)
    (output/'.incomplete').unlink()
    return receipt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('study', type=Path)
    parser.add_argument('case_id')
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--manifest-sha256', required=True)
    parser.add_argument('--extraction-request', type=Path, required=True)
    parser.add_argument('--extraction-request-sha256', required=True)
    parser.add_argument('--solver-version', required=True)
    parser.add_argument('--timeout-seconds', type=float, required=True)
    args = vars(parser.parse_args())
    from digitalmodel.workflows.orcaflex_reproduce import _load_api
    api, identity = _load_api({'solver_version': args['solver_version']})
    if identity['resolved_version'] != args['solver_version']:
        raise ValueError('Native solver version mismatch')
    print(json.dumps({'status': materialize_bracket_case(api, solver_identity=identity, **args)['status']}))


if __name__ == '__main__':
    main()
