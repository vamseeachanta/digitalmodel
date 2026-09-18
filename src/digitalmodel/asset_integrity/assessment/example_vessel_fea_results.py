"""Elastic shell-result diagnostics under explicit example assumptions."""
from __future__ import annotations

import csv
import math
from dataclasses import asdict
from pathlib import Path

import numpy as np
from digitalmodel.citations.schema import Citation, validate_citation

WIKI_PATH = 'wikis/asset-management/wiki/standards/api-579-1/2007-example-verification.md'
COMPONENTS = ('sx', 'sy', 'sz', 'sxy', 'syz', 'sxz')


def invariants(row):
    x, y, z, xy, yz, xz = (float(row[k]) for k in COMPONENTS)
    if not all(math.isfinite(v) for v in (x, y, z, xy, yz, xz)):
        raise ValueError('Stress components must be finite')
    vm = math.sqrt(((x-y)**2 + (y-z)**2 + (z-x)**2)/2 + 3*(xy*xy+yz*yz+xz*xz))
    principal = np.linalg.eigvalsh([[x, xy, xz], [xy, y, yz], [xz, yz, z]])
    return dict(von_mises_mpa=vm, trace_mpa=x+y+z,
                minimum_in_plane_principal_mpa=(x+y)/2 - math.sqrt(((x-y)/2)**2+xy*xy),
                minimum_principal_mpa=float(principal[0]))


def read_stresses(path, expected_ids):
    with Path(path).open(encoding='utf-8', newline='') as stream:
        rows = list(csv.DictReader(stream))
    found = {}
    for row in rows:
        raw_id = float(row['element_id'])
        if not math.isfinite(raw_id) or raw_id != int(raw_id):
            raise ValueError('Invalid element identity')
        identity = int(raw_id)
        if identity in found:
            raise ValueError('Duplicate element identity')
        found[identity] = dict(element_id=identity, **{k: float(row[k]) for k in COMPONENTS})
        invariants(found[identity])
    if set(found) != set(expected_ids):
        raise ValueError('Incomplete or unexpected element coverage')
    return [found[i] for i in expected_ids]


def _citation(wiki_root=None):
    citation = Citation(code_id='api-579-1', publisher='API', revision='2007',
        section='Annex B1 §§B1.2.2/B1.3.2; Eqs. B1.1–B1.5; Annex B2 §B2.4.1.2', wiki_path=WIKI_PATH,
        source_sibling='generic', note='120 MPa allowable input is assumed, not a material-table value')
    validate_citation(citation, repo_root=wiki_root)
    return asdict(citation)


def linearize_segments(segments):
    """Annex B2 flat-path integral; x axial, y hoop, z radial (SCL direction)."""
    if not segments:
        raise ValueError('Through-wall segments required')
    start, end = segments[0][0], segments[-1][1]
    thickness = end-start
    if not math.isfinite(thickness) or thickness <= 0:
        raise ValueError('Positive finite section thickness required')
    middle = (start+end)/2
    force = {k: 0. for k in COMPONENTS}
    moment = dict(force)
    previous = start
    for lo, hi, low, high in segments:
        if lo != previous or not hi > lo:
            raise ValueError('Through-wall segments must be ordered and contiguous')
        invariants(low)
        invariants(high)
        for k in COMPONENTS:
            force[k] += (hi-lo)*(low[k]+high[k])/2
            moment[k] += (hi-lo)*(low[k]*(2*lo+hi-3*middle)+high[k]*(lo+2*hi-3*middle))/6
        previous = hi
    mid = {k: force[k]/thickness for k in COMPONENTS}
    for key in ('sz', 'sxz', 'syz'):
        moment[key] = 0.  # B2.4.1.2 excludes bending components involving the SCL direction.
    top = {k: mid[k]+6*moment[k]/thickness**2 for k in COMPONENTS}
    bottom = {k: mid[k]-6*moment[k]/thickness**2 for k in COMPONENTS}
    return mid, top, bottom


def _cylindrical(row, theta):
    x, y, z, xy, yz, xz = (row[k] for k in COMPONENTS)
    c, s = math.cos(theta), math.sin(theta)
    return dict(sx=x, sy=s*s*y+c*c*z-2*s*c*yz, sz=c*c*y+s*s*z+2*s*c*yz,
                sxy=-s*xy+c*xz, sxz=c*xy+s*xz, syz=s*c*(z-y)+(c*c-s*s)*yz)


def linearize_solid(model, points):
    """Four unaveraged radial corner paths per element column, no in-plane averaging."""
    nodes = {n[0]: n[1:] for n in model['nodes']}
    values = {(r['element_id'], r['node_id']): r for r in points}
    columns = {}
    for element in model['elements']:
        columns.setdefault(element['column_id'], []).append(element)
    output = dict(mid=[], top=[], bottom=[])
    for identity, elements in columns.items():
        elements.sort(key=lambda e: e['radial_layer'])
        if [e['radial_layer'] for e in elements] != list(range(model['basis']['radial_layers'])):
            raise ValueError('Incomplete radial layer coverage')
        for corner in range(4):
            segments = []
            for e in elements:
                ids = e['nodes'][corner], e['nodes'][corner+4]
                radii = [math.hypot(nodes[n][1], nodes[n][2]) for n in ids]
                theta = math.atan2(nodes[ids[0]][2], nodes[ids[0]][1])
                rows = [_cylindrical(values[e['element_id'], n], theta) for n in ids]
                segments.append((*radii, *rows))
            for key, row in zip(('mid', 'top', 'bottom'), linearize_segments(segments)):
                output[key].append(dict(row, column_id=identity, corner=corner))
    return output


def screen_tensors(mid, top, bottom, *, pressure, stress, wiki_root=None, raw_points=None):
    citation = _citation(wiki_root)
    if not mid or len(mid) != len(top) or len(mid) != len(bottom):
        raise ValueError('Three equally sized nonempty surface tensor sets required')
    if not all(math.isfinite(v) and v > 0 for v in (pressure, stress)):
        raise ValueError('Finite positive pressure and assumed allowable required')
    m = [invariants(row) for row in mid]
    faces = [invariants(row) for row in [*top, *bottom]]
    raw = [invariants(row) for row in raw_points] if raw_points is not None else []
    if raw_points is not None and not raw:
        raise ValueError('Supplied raw tensor set must be nonempty')
    demands = dict(membrane=max(r['von_mises_mpa'] for r in m),
                   membrane_plus_bending=max(r['von_mises_mpa'] for r in faces),
                   local_failure=max(0., max(r['trace_mpa'] for r in faces + raw)))
    limits = dict(membrane=stress, membrane_plus_bending=1.5*stress, local_failure=4*stress)
    utilization = {k: demands[k]/limits[k] for k in demands}
    governing = max(utilization, key=utilization.get)
    maximum = utilization[governing]
    return dict(demands_mpa=demands, limits_mpa=limits, utilization=utilization,
                governing_criterion=governing, acceptance=maximum <= 1,
                pressure_limit_mpa=pressure/maximum if maximum else None,
                pressure_limit_scope='proportional-load estimate; direct lower-pressure verification required',
                pressure_mpa=pressure, min_membrane_principal_mpa=min(r['minimum_principal_mpa'] for r in m),
                min_in_plane_membrane_principal_mpa=min(r['minimum_in_plane_principal_mpa'] for r in m),
                citations=[citation], raw_trace_bound_checked=bool(raw), code_qualified_actual_asset=False,
                status='CONDITIONAL ELASTIC CHECKS PASS' if maximum <= 1 else 'ELASTIC CHECK NON-PASS',
                scope='all membrane assigned general-primary bound; all surface stress assigned primary',
                qualification='mesh, boundary, equilibrium, shell applicability and other modes require separate verification')
