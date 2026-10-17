"""Solver-free minimum-tension basis and explicitly approximate hand equilibrium.

ABS Guidance Notes on Drilling Riser Analysis (2017), Section 3, Subsection 3,
item iii), Eqs (3.1)-(3.2). API RP 16Q, Design, Selection, Operation and
Maintenance of Marine Drilling Riser Systems, second edition (2017): clause
text shall be confirmed against a held edition before report use.
"""
from __future__ import annotations

import math
from .assembly import tensioner_system_factor
from .stackup import minimum_slip_ring_tension

G = 9.81  # model preparation assumption, m/s²
MEMBERS = ('Riser', 'Choke', 'Kill')  # no other attached auxiliary line in this specification
LINKS = ('Tens_AFT', 'Tens_FWD', 'Tens_STBD', 'Tens_PORT')
BASIS = ('ABS Guidance Notes on Drilling Riser Analysis (2017), Section 3, '
         'Subsection 3, item iii), Eqs (3.1)-(3.2); API RP 16Q, Design, Selection, '
         'Operation and Maintenance of Marine Drilling Riser Systems, second '
         'edition (2017); clause text to be confirmed against a held edition before report use')


def sections(line: dict) -> list[tuple]:
    """Read both supported specification section layouts."""
    p = line['properties']
    if 'LineType, Length, TargetSegmentLength' in p:
        return [(r[0], float(r[1])) for r in p['LineType, Length, TargetSegmentLength']]
    return [(r['LineType'], float(r['Length'])) for r in p['Sections']]


def member_basis(spec: dict, name: str) -> dict:
    """Flooded steel Ws, fluid head once, with actual above-water steel weight."""
    line = next(l for l in spec['generic']['lines'] if l['name'] == name)
    types = {t['name']: t for t in spec['generic']['line_types']}
    rho_w = spec['environment']['water']['density']
    ws = area = ea = length = 0.0
    for ref, span in sections(line):
        t = types[ref]
        bore = math.pi / 4 * t['inner_diameter']**2
        steel = math.pi / 4 * (t['outer_diameter']**2 - t['inner_diameter']**2)
        # Lower flex joint at -1000 m; the upper 20 m is in air.
        wet = min(span, max(0, 1000 - length))
        ws += G * steel * (t['properties']['MaterialDensity'] * span - rho_w * wet)
        area += bore * span
        ea += float(t['properties']['E']) * steel * span
        length += span
    ai = area / length
    head = G * ai * (line['properties']['ContentsDensity'] * length - rho_w * 1000)
    return dict(ws_kn=ws, internal_area_m2=ai, head_kn=head, length_m=length,
                ea_kn=ea / length)


def link_geometry(spec: dict, ring_z: float, factor: float) -> tuple:
    """Interpolate spring tensions and resolve fleet angle from link endpoints."""
    vertical, lengths, cosines = 0.0, [], []
    for link in spec['generic']['links']:
        if link['name'] not in LINKS:
            continue
        props = link['properties']
        endpoints = next(v for k, v in props.items() if k.startswith('Connection, '))
        a, b = endpoints
        dz = float(a[3]) - ring_z - float(b[3])
        length = math.sqrt((a[1]-b[1])**2 + (a[2]-b[2])**2 + dz**2)
        table = props['SpringLength, SpringTension']
        if not table[0][0] <= length <= table[-1][0]:
            raise ValueError('Tensioner outside spring table')
        for (x0, y0), (x1, y1) in zip(table, table[1:]):
            if x0 <= length <= x1:
                force = y0 + (y1-y0)*(length-x0)/(x1-x0)
                break
        cosine = dz / length
        vertical += factor * force * cosine
        lengths.append(length)
        cosines.append(cosine)
    if len(lengths) != 4:
        raise ValueError('Expected four tensioner links')
    return vertical, lengths, sum(cosines) / 4


def tension_basis(spec: dict) -> dict:
    """Derive acceptance inputs; half slip weight is a HAND assumption only."""
    parts = [member_basis(spec, name) for name in MEMBERS]
    ws = sum(p['ws_kn'] for p in parts)
    ai = sum(p['internal_area_m2'] for p in parts)
    rho_w = spec['environment']['water']['density']
    tsr = minimum_slip_ring_tension(ws, 0, ai, 2.2*G, 1020, rho_w*G, 1000)
    slip = next(l for l in spec['generic']['lines'] if l['name'] == 'Riser Slip Joint')
    st = next(t for t in spec['generic']['line_types'] if t['name'] == 'Slip Joint')
    ring = next(b for b in spec['generic']['buoys_6d'] if b['name'] == 'Tensioner Ring')
    slip_mass = st['mass_per_length'] + math.pi/4*st['inner_diameter']**2*slip['properties']['ContentsDensity']
    carried = G*(ring['mass'] + slip_mass * sum(s for _, s in sections(slip)) / 2)
    weight = ws + sum(p['head_kn'] for p in parts)
    stiffness = sum(p['ea_kn']/p['length_m'] for p in parts)
    target = tensioner_system_factor(4, 1, 1) * (tsr + carried)
    # Straight extensible string at zero offset/current: mean axial load is
    # top effective tension minus half effective weight. Pressure strain omitted.
    ring_z = ring['initial_position'][2] + (target-carried-weight/2)/stiffness
    base_vertical, _, cosine = link_geometry(spec, ring_z, 1)
    return dict(ws_kn=ws, internal_area_m2=ai, tsrmin_kn=tsr, carried_weight_kn=carried,
                effective_weight_kn=weight, stiffness_kn_m=stiffness,
                required_vertical_kn=target, spring_table_factor=math.ceil(target/base_vertical*1000)/1000,
                fleet_cosine=cosine, system_factor=tensioner_system_factor(4, 1, cosine),
                mechanical_hydraulic_efficiency=1, point_z_m=-1000, n_units=4, n_fail=1,
                fwt=1.05, buoyancy_kn=0, mud_density_te_m3=2.2,
                member_basis=parts, source=BASIS, carried_weight_basis='hand estimate: ring plus half dry slip-joint and contents weight; native uses End B global reaction')


def checks(vertical_kn: float, carried_kn: float, tsrmin_kn: float, ws_kn: float,
           bottom_kn: float, member_min_kn: float) -> dict:
    """Binding checks; positive margin meets the stated criterion, zero included."""
    if not all(math.isfinite(v) for v in (vertical_kn, carried_kn, tsrmin_kn, ws_kn, bottom_kn, member_min_kn)):
        raise ValueError('Nonfinite tension evidence')
    a = vertical_kn - carried_kn - tsrmin_kn
    b = 0.75 * vertical_kn - carried_kn - tsrmin_kn
    bottom = bottom_kn - 0.05 * ws_kn
    return dict(check_a_margin_kn=a, check_b_margin_kn=b, bottom_margin_kn=bottom,
                check_a_pass=a >= 0 and bottom >= 0 and member_min_kn >= 0,
                check_b_pass=b >= 0, member_min_kn=member_min_kn)


def hand_estimate(spec: dict, basis: dict, offset: float, speed: float, *, factor: float | None = None) -> dict:
    """Approximate extensible straight-string/constant-tension drag equilibrium.

    No native acceptance: no pressure strain, contact, bending or nonlinear shape.
    Current sag uses q²L³/(24T²); native reactions and profiles remain required.
    """
    factor = basis['spring_table_factor'] if factor is None else factor
    length = 1020.0
    q = 0.5 * spec['environment']['water']['density'] * 1.2 * (0.65+2*0.1651) * speed**2
    def state(vertical):
        top = vertical - basis['carried_weight_kn']
        mean = max(1, top-basis['effective_weight_kn']/2)
        z = 20 + mean/basis['stiffness_kn_m'] - offset**2/(2*length) - q*q*length**3/(24*mean**2)
        force, lengths, _ = link_geometry(spec, z, factor)
        return force-vertical, lengths, z
    low, high = basis['effective_weight_kn']+basis['carried_weight_kn'], 18000.0
    for _ in range(80):
        mid = (low+high)/2
        if state(mid)[0] > 0:
            low = mid
        else:
            high = mid
    vertical = (low+high)/2
    residual, lengths, z = state(vertical)
    if abs(residual) > 1e-5:
        raise ValueError('Hand equilibrium not bracketed')
    bottom = vertical-basis['carried_weight_kn']-basis['effective_weight_kn']
    return dict(vertical_kn=vertical, link_lengths_m=lengths, ring_z_m=z,
                **checks(vertical, basis['carried_weight_kn'], basis['tsrmin_kn'], basis['ws_kn'], bottom, min(p['ea_kn']/p['length_m']/basis['stiffness_kn_m']
                    * (vertical-basis['carried_weight_kn']-basis['effective_weight_kn']/2)
                    - (p['ws_kn']+p['head_kn'])/2 for p in basis['member_basis'])),
                qualification='hand estimate only; native checks pending')
