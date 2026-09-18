"""Conditional API 579:2007 example; assumed inputs never qualify an actual asset."""
from __future__ import annotations

import math
import hashlib
import json
from dataclasses import asdict
from pathlib import Path

from digitalmodel.citations.schema import Citation, validate_citation
from .example_vessel_data import critical_profiles, sample

WIKI_PATH = 'wikis/asset-management/wiki/standards/api-579-1/2007-example-verification.md'
FOLIAS = (1.001, -.014195, .29090, -.096420, .020890, -.0030540,
          2.9570e-4, -1.8462e-5, 7.1553e-7, -1.5631e-8, 1.4656e-10)
# Table 5.4: TSF, lambda cutoff, C1 ... C6 (source verification in sidecar).
CIRC = (
    (.7, .21, .99221, -.11959, -.057333, .016948, -.0017976, .000069114),
    (.75, .48, .96801, -.23780, -.32678, .20684, -.046537, .0039436),
    (.8, .67, .94413, -.31256, -.69968, .65020, -.22102, .028799),
    (.9, .98, .89962, -.38860, -1.6485, 2.3445, -1.2534, .25331),
    (1., 1.23, .85947, -.40012, -2.7979, 5.0729, -3.5217, .91877),
    (1.2, 1.66, .78654, -.25322, -5.7982, 13.858, -13.118, 4.6436),
    (1.4, 2.03, .72335, .011528, -9.3536, 26.031, -29.372, 12.387),
    (1.8, 2.66, .60737, .93796, -19.239, 64.267, -91.307, 48.962),
    (2.3, 3.35, .49304, 2.1692, -32.459, 122.45, -202.43, 127.27),
)


def _positive(*values):
    if not all(math.isfinite(v) and v > 0 for v in values):
        raise ValueError('Expected finite positive engineering inputs')


def citation_sidecar(wiki_root):
    citation = Citation(code_id='api-579-1', publisher='API', revision='2007',
                        section='Part 5 §§5.2.5,5.4.2,5.4.3; Tables 5.2/5.4; Part 2 Table 2.3; Annex A §A.3.4 Eq.A.10',
                        wiki_path=WIKI_PATH, source_sibling='generic',
                        note='Original PDF verified remotely; example assumptions, not asset acceptance')
    validate_citation(citation, repo_root=Path(wiki_root))
    return [asdict(citation)]


def folias(lam):
    if not math.isfinite(lam) or lam < 0:
        raise ValueError('Flaw parameter must be finite and nonnegative')
    lam = min(lam, 20.)
    value = 0.
    for coefficient in reversed(FOLIAS):
        value = value * lam + coefficient
    return value


def _rsf(ratio, mt):
    return min(1., ratio / (1 - (1 - ratio) / mt))


def _curve(lam, row):
    if lam <= row[1]:
        return .2
    return sum(coefficient / lam ** power for power, coefficient in enumerate(row[2:]))


def circumferential_required(lam, tsf):
    if not math.isfinite(lam) or not math.isfinite(tsf) or not 0 <= lam <= 9:
        raise ValueError('Circumferential parameter outside Table 5.4 domain')
    if not .7 <= tsf <= 2.3:
        raise ValueError('TSF outside Table 5.4 domain')
    for row in CIRC:
        if tsf == row[0]:
            return _curve(lam, row)
    for low, high in zip(CIRC, CIRC[1:]):
        if low[0] < tsf < high[0]:
            fraction = (tsf - low[0]) / (high[0] - low[0])
            return _curve(lam, low) * (1 - fraction) + _curve(lam, high) * fraction
    raise ValueError('Unresolved TSF bracket')


def profile_rsf(points, tc, diameter):
    """Enumerate every contiguous sample interval with trapezoidal metal-loss area."""
    _positive(tc, diameter)
    if len(points) < 2:
        raise ValueError('At least two profile samples required')
    cumulative = [0.]
    for x, t in points:
        _positive(t)
        if not math.isfinite(x) or t > tc + 1e-8:
            raise ValueError('Invalid coordinate or thickness above reference wall')
    for (xa, ta), (xb, tb) in zip(points, points[1:]):
        if xb <= xa:
            raise ValueError('Profile coordinates must strictly increase')
        cumulative.append(cumulative[-1] + (xb - xa) * (tc - (ta + tb) / 2))
    best = dict(rsf=1., interval_mm=[points[0][0], points[-1][0]], loss_area_mm2=0.)
    for i, first in enumerate(points[:-1]):
        for j in range(i + 1, len(points)):
            length = points[j][0] - first[0]
            loss = max(0., cumulative[j] - cumulative[i])
            ratio = 1 - loss / (length * tc)
            value = _rsf(ratio, folias(1.285 * length / math.sqrt(diameter * tc)))
            if value < best['rsf']:
                best = dict(rsf=value, interval_mm=[first[0], points[j][0]], loss_area_mm2=loss)
    return best


def _validate_grid(grid, tc, basis):
    rows = grid['rows']
    if not rows:
        raise ValueError('Empty thickness grid')
    coordinates = set()
    for row in rows:
        x, s, t = row['local_x_mm'], row['local_s_mm'], row['assessed_mm']
        _positive(t)
        for key in ('uncertainty_mm', 'future_loss_mm'):
            if row[key] != basis[key]:
                raise ValueError('Grid allowances differ from assessment basis')
        expected = row['current_mm'] - basis['uncertainty_mm'] - basis['future_loss_mm']
        if not math.isfinite(expected) or abs(t - expected) > 1e-8:
            raise ValueError('Grid assessed thickness lineage mismatch')
        if not all(math.isfinite(v) for v in (x, s)) or t > tc + 1e-8:
            raise ValueError('Invalid grid value')
        if (x, s) in coordinates:
            raise ValueError('Duplicate grid coordinate')
        coordinates.add((x, s))
    if len(coordinates) != len({x for x, _ in coordinates}) * len({s for _, s in coordinates}):
        raise ValueError('Thickness grid must be complete rectangular sampling')


def _stage(rsf, rt, lam_c, diameter, tc, mawp, pressure, limits):
    circ_applicable = lam_c <= 9 and diameter / tc >= 20 and .7 <= rsf <= 1.
    tsf = 1 / rsf  # Eq.5.18 with both weld joint efficiencies explicitly assumed 1.
    required = circumferential_required(lam_c, tsf) if circ_applicable else None
    circ_pass = required is not None and rt >= required
    reduced = mawp * min(1., rsf / .9)
    passed = limits and circ_pass and pressure <= reduced
    reasons = []
    if not circ_applicable:
        reasons.append('circumferential applicability limits not satisfied; escalate assessment')
    elif not circ_pass:
        reasons.append('circumferential remaining thickness below required curve')
    if not limits:
        reasons.append('limiting flaw criteria not satisfied; escalate assessment')
    if pressure > reduced:
        reasons.append('target pressure exceeds longitudinal reduced pressure diagnostic')
    return dict(rsf=rsf, rsfa=.9, mawp_reduced_mpa=reduced,
                non_pass_reasons=reasons,
                pressure_rating_qualified=False,
                pressure_rating_note='Diagnostic only; all applicability and circumferential checks required',
                target_pressure_mpa=pressure, pressure_margin_mpa=reduced - pressure,
                circumferential_lambda=lam_c, tsf=tsf, required_circumferential_rt=required,
                circumferential_applicable=circ_applicable, circumferential_pass=circ_pass,
                limiting_criteria_pass=limits,
                status='CONDITIONAL EXAMPLE PASS' if passed else 'EXAMPLE NON-PASS',
                acceptance=passed)


def _validate_basis(basis):
    for key in ('uncertainty_mm', 'future_loss_mm'):
        if not math.isfinite(basis[key]) or basis[key] < 0:
            raise ValueError('Allowances must be finite and nonnegative')
    _positive(basis['nominal_mm'], basis['shell_tangent_length_mm'], basis['inside_radius_mm'])
    stress = basis['material']['screening_stress_mpa']
    if basis['target_pressure_mpa_g'] > .385 * stress:
        raise ValueError('Pressure exceeds Annex A.3.4(a) scope')
    if basis['nominal_mm'] > .5 * basis['inside_radius_mm']:
        raise ValueError('Thick-wall model outside this example scope')


def _geometry_lineage(area, grid, basis):
    rows = grid['rows']
    for coordinate, extent in (('local_x_mm', area.axial_extent_mm),
                               ('local_s_mm', area.circumferential_extent_mm)):
        values = [r[coordinate] for r in rows]
        if min(values) > -extent / 2 or max(values) < extent / 2:
            raise ValueError('Grid does not cover full flaw footprint')
    if any(r['area_id'] != area.area_id for r in rows):
        raise ValueError('Grid area identity mismatch')
    for row in rows:
        outside = (abs(row['local_x_mm']) >= area.axial_extent_mm / 2
                   or abs(row['local_s_mm']) >= area.circumferential_extent_mm / 2)
        if outside and abs(row['current_mm'] - basis['nominal_mm']) > 1e-8:
            raise ValueError('Unreported metal loss at or outside declared footprint')
    if abs(min(r['current_mm'] for r in rows) - area.minimum_mm) > 1e-8:
        raise ValueError('Grid minimum differs from example area basis')
    payload = {'area': asdict(area), 'rows': rows, 'basis': basis}
    raw = json.dumps(payload, sort_keys=True, separators=(',', ':'), allow_nan=False).encode()
    return hashlib.sha256(raw).hexdigest()


def assess(area, grid, basis, wiki_root):
    """Assess only assumed internal-pressure shell cases, at supplied sample resolution."""
    citations = citation_sidecar(wiki_root)
    _validate_basis(basis)
    tc = basis['nominal_mm'] - basis['uncertainty_mm'] - basis['future_loss_mm']
    diameter = 2 * basis['inside_radius_mm']
    pressure = basis['target_pressure_mpa_g']
    stress = basis['material']['screening_stress_mpa']
    _positive(tc, diameter, pressure, stress, area.axial_extent_mm, area.circumferential_extent_mm)
    _validate_grid(grid, tc, basis)
    digest = _geometry_lineage(area, grid, basis)
    profiles = critical_profiles(grid['rows'])
    minimum = min(t for _, t in profiles['axial'])
    rt = minimum / tc
    lam = 1.285 * area.axial_extent_mm / math.sqrt(diameter * tc)
    lam_c = 1.285 * area.circumferential_extent_mm / math.sqrt(diameter * tc)
    distance = min(area.centre_x_mm - area.axial_extent_mm / 2,
                   basis['shell_tangent_length_mm'] - area.centre_x_mm - area.axial_extent_mm / 2)
    limits = rt >= .2 and minimum >= 2.5 and distance >= 1.8 * math.sqrt(diameter * tc)
    mawp = stress * tc / (diameter / 2 + .6 * tc)
    level2 = profile_rsf(profiles['axial'], tc, diameter)
    args = (rt, lam_c, diameter, tc, mawp, pressure, limits)
    return dict(area_id=area.area_id, example_data=True, code_qualified_actual_asset=False,
                input_sha256=digest,
                sound_mawp_mpa=mawp, reference_thickness_mm=tc, minimum_assessed_mm=minimum,
                remaining_thickness_ratio=rt, discontinuity_distance_mm=distance,
                level1=_stage(_rsf(rt, folias(lam)), *args),
                level2={**_stage(level2['rsf'], *args), 'governing_interval': level2},
                assumptions=['external smooth LTA; no cracks or grooves; Type A cylinder',
                             'noncyclic service; sufficient toughness; outside creep range',
                             'internal pressure only; other loads negligible',
                             'both weld efficiencies 1; no interacting flaws or nearby nozzles',
                             'generic assumed stress; no code-qualified material',
                             'diameter unchanged by external metal loss; uncertainty deducted once'],
                citations=citations)


def convergence(area, basis, wiki_root, pitches=(6.25, 3.125)):
    """Compare two spatial quadratures; does not represent FEA convergence."""
    if len(pitches) != 2 or not pitches[0] > pitches[1] > 0:
        raise ValueError('Two decreasing positive sampling pitches required')
    results = [assess(area, sample(area, pitch), basis, wiki_root) for pitch in pitches]
    coarse, fine = (r['level2'] for r in results)
    change = abs(coarse['rsf'] - fine['rsf']) / fine['rsf']
    stable = all(results[0][k]['acceptance'] == results[1][k]['acceptance']
                 for k in ('level1', 'level2'))
    return dict(pitches_mm=list(pitches), rsf=[r['level2']['rsf'] for r in results],
                relative_rsf_change=change, status_stable=stable, criterion_fraction=.01,
                criterion_met=change <= .01 and stable,
                scope='CTP interval quadrature only; example-selected 1% convergence criterion',
                input_sha256=[r['input_sha256'] for r in results])


def head_check(basis, wiki_root):
    """Undamaged 2:1 elliptical heads, nominal 20 mm, assumed full-efficiency joints."""
    citations = citation_sidecar(wiki_root)
    citations[0]['section'] = 'Annex A §A.3.6, 2:1 elliptical heads; physical PDF p.538 / printed A-10'
    _validate_basis(basis)
    pressure, stress = basis['target_pressure_mpa_g'], basis['material']['screening_stress_mpa']
    diameter = 2*basis['inside_radius_mm']
    thickness = 20. - basis['uncertainty_mm'] - basis['future_loss_mm']
    _positive(pressure, stress, thickness)
    factor = (2+2**2)/6  # Annex A.3.6, Eq. A.33; assumed 2:1 ellipsoidal geometry.
    required = pressure*diameter*factor/(2*stress-.2*pressure)
    mawp = 2*stress*thickness/(factor*diameter+.2*thickness)
    applicable = diameter*(.44*2+.02)/thickness <= 500
    return dict(assessed_head_thickness_mm=thickness, required_thickness_mm=required,
                mawp_mpa=mawp, acceptance=applicable and pressure <= mawp,
                applicability=applicable, head_nominal_mm=20., inside_height_mm=diameter/4,
                assumed_joint_efficiency=1., code_qualified_actual_asset=False,
                scope='nominal pressure check; head junction loads excluded from local shell FE model',
                citations=citations)
