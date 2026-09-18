"""Verify retained native evidence before summarizing the assumed vessel example."""
import csv
import hashlib
import json
import math
from pathlib import Path

import numpy as np
from .example_vessel_presol import read_presol
from .example_vessel_fea_results import linearize_solid, screen_tensors


def _read(path):
    return json.loads(Path(path).read_text(encoding='utf-8'))


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def verify_run(path):
    path = Path(path)
    execution = _read(path/'execution.json')
    run = execution.get('result', {})
    if (run.get('return_code') != 0 or run.get('timed_out', True)
            or run.get('owned_processes_remaining') != 0
            or not run.get('containment_verified') or not run.get('evidence_complete')
            or not execution.get('reservation_released')):
        raise ValueError('Native execution, evidence or process settlement not verified')
    manifest = _read(path/'result-manifest.json')
    for name, record in manifest['files'].items():
        if Path(name).name != name:
            raise ValueError('Manifest file must be a local basename')
        source = path/name
        if source.stat().st_size != record['bytes'] or digest(source) != record['sha256']:
            raise ValueError('Raw evidence size/digest mismatch')
    required = {'execution.json', 'model.json', 'vessel.inp', 'vessel.out',
                'displacements.csv', 'reactions.csv', 'input-manifest.json'}
    if not required.issubset(manifest['files']):
        raise ValueError('Required engineering evidence absent from manifest')
    inputs = _read(path/'input-manifest.json')['files']
    if not {'model.json', 'vessel.inp'}.issubset(inputs):
        raise ValueError('Input provenance incomplete')
    for name in ('model.json', 'vessel.inp'):
        if inputs[name] != manifest['files'][name]['sha256']:
            raise ValueError('Input/output provenance digest mismatch')
    if '*** ERROR ***' in (path/'vessel.out').read_text(encoding='utf-8', errors='replace'):
        raise ValueError('Native listing contains an error')
    return execution, manifest


def _kinematics(path, model):
    nodes = {n[0]: np.array(n[1:]) for n in model['nodes']}
    with (path/'displacements.csv').open() as stream:
        rows = list(csv.DictReader(stream))
    ids = [int(float(r['node_id'])) for r in rows]
    if len(ids) != len(set(ids)) or set(ids) != set(nodes):
        raise ValueError('Displacement node coverage mismatch')
    inner = min(math.hypot(n[1], n[2]) for n in nodes.values())
    bore = []
    for row, identity in zip(rows, ids):
        node = nodes[identity]
        radius = math.hypot(node[1], node[2])
        u = np.array([float(row[k]) for k in ('ux', 'uy', 'uz')])
        if not np.isfinite(u).all():
            raise ValueError('Nonfinite displacement')
        if abs(radius-inner) < 1e-6:
            bore.append(float(np.dot(u[1:], node[1:])/radius))
    with (path/'reactions.csv').open() as stream:
        reactions = list(csv.DictReader(stream))
    reaction_ids = [float(r['node_id']) for r in reactions]
    if len(reaction_ids) != len(set(reaction_ids)) or set(reaction_ids) != {n[0] for n in model['reference_nodes']}:
        raise ValueError('Reaction node coverage mismatch')
    force, moment = np.zeros(3), np.zeros(3)
    for row in reactions:
        f = np.array([float(row[k]) for k in ('fx', 'fy', 'fz')])
        if not np.isfinite(f).all():
            raise ValueError('Nonfinite reaction')
        force += f
        moment += np.cross(nodes[int(float(row['node_id']))], f)
    thrust = model['basis']['pressure_mpa']*math.pi*inner**2
    error = max(float(np.linalg.norm(force)/thrust), float(np.linalg.norm(moment)/(thrust*inner)))
    if error > 1e-4:
        raise ValueError('Force/moment equilibrium residual exceeds 0.01 percent')
    return dict(bore_radial_mm=[min(bore), max(bore)],
                reaction_force_n=force.tolist(), reaction_moment_nmm=moment.tolist(),
                equilibrium_error_ratio=error, equilibrium_tolerance=1e-4, equilibrium_passed=True)


def _benchmark(model, tensors, kinematics, solid):
    radius = min(math.hypot(n[2], n[3]) for n in model['nodes'])
    thickness = (max(math.hypot(n[2], n[3]) for n in model['nodes'])-radius if solid
                 else model['elements'][0]['thickness_mm'])
    p = model['basis']['pressure_mpa']
    material = model['basis']['material']
    e, nu = material['elastic_modulus_mpa'], material['poisson_ratio']
    a = p*radius**2/((radius+thickness)**2-radius**2)
    displacement = radius/e*((1-2*nu)*a+(1+nu)*a*(radius+thickness)**2/radius**2)
    hoop_key, axial_key = ('sy', 'sx') if solid else ('sx', 'sy')
    errors = dict(hoop=max(abs(r[hoop_key]/(p*radius/thickness)-1) for r in tensors['mid']),
                  axial=max(abs(r[axial_key]/a-1) for r in tensors['mid']),
                  bore_displacement=max(abs(u/displacement-1) for u in kinematics['bore_radial_mm']))
    return dict(reference_hoop_mpa=p*radius/thickness, reference_axial_mpa=a,
                reference_bore_displacement_mm=displacement, relative_errors=errors,
                tolerance=.05, passed=max(errors.values()) <= .05,
                reference='classical closed-end cylinder equilibrium and Lame elasticity')


def summarize_run(path, wiki_root):
    path = Path(path)
    execution, manifest = verify_run(path)
    model = _read(path/'model.json')
    solid = 'radial_layers' in model['basis']
    filename = 'stress_nodes.txt' if solid else 'stress_mid_nodes.txt'
    if filename not in manifest['files']:
        raise ValueError('Unaveraged stress listing absent from verified manifest')
    points = read_presol(path/filename, model, kind='solid' if solid else 'shell')
    tensors = linearize_solid(model, points) if solid else points
    basis = model['basis']
    result = screen_tensors(tensors['mid'], tensors['top'], tensors['bottom'],
        pressure=basis['pressure_mpa'], stress=basis['material']['screening_stress_mpa'], wiki_root=wiki_root,
        raw_points=points if solid else [row for surface in points.values() for row in surface])
    result.update(case_id=path.name, case=basis['case'], formulation='solid' if solid else 'shell',
        local_pitch_mm=basis['local_pitch_mm'], far_pitch_mm=basis['far_pitch_mm'],
        radial_layers=basis.get('radial_layers'), nodes=len(model['nodes']), elements=len(model['elements']),
        raw_manifest_sha256=digest(path/'result-manifest.json'), input_deck_sha256=digest(path/'vessel.inp'),
        duration_seconds=execution['result']['duration_seconds'], kinematics=_kinematics(path, model))
    if basis['case'] == 'intact':
        result['benchmark'] = _benchmark(model, tensors, result['kinematics'], solid)
    return result
