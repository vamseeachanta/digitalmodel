"""Solver-free minimum-tension basis and explicitly approximate hand equilibrium.

ABS Guidance Notes on Drilling Riser Analysis (2017), Section 3, Subsection 3,
item iii), Eqs (3.1)-(3.2). API RP 16Q, Design, Selection, Operation and
Maintenance of Marine Drilling Riser Systems, second edition (2017): clause
text shall be confirmed against a held edition before report use.
"""
from __future__ import annotations

import math
from .assembly import minimum_top_tension_16q, tensioner_system_factor
from .stackup import minimum_slip_ring_tension

G = 9.81  # model preparation assumption, m/s²
MEMBERS = ('Riser', 'Choke', 'Kill')  # no other attached auxiliary line in this specification
LINKS = ('Tens_AFT', 'Tens_FWD', 'Tens_STBD', 'Tens_PORT')
BASIS = ('ABS Guidance Notes on Drilling Riser Analysis (2017), Section 3, '
         'Subsection 3, item iii), Eqs (3.1)-(3.2); API RP 16Q, Design, Selection, '
         'Operation and Maintenance of Marine Drilling Riser Systems, second '
         'edition (2017); clause text to be confirmed against a held edition before report use')


def surviving_share(*, basis: dict) -> float:
    """Derive (N-n)/N from the recorded arrangement, with neutral efficiency."""
    for key in ('n_units', 'n_fail'):
        if isinstance(basis[key], bool) or not isinstance(basis[key], int):
            raise ValueError('Tensioner counts must be integers')
    return 1 / tensioner_system_factor(basis['n_units'], basis['n_fail'], 1)


def weight_reserve(*, basis: dict) -> float:
    """Derive the bottom-tension share fwt-1 from the same recorded basis."""
    fwt = basis['fwt']
    if isinstance(fwt, bool) or not math.isfinite(fwt) or fwt < 1:
        raise ValueError('fwt must be finite and at least one')
    return fwt - 1


def sections(line: dict) -> list[tuple]:
    """Read both supported specification section layouts."""
    p = line['properties']
    if 'LineType, Length, TargetSegmentLength' in p:
        return [(r[0], float(r[1])) for r in p['LineType, Length, TargetSegmentLength']]
    return [(r['LineType'], float(r['Length'])) for r in p['Sections']]


def column_geometry(spec: dict) -> dict:
    """ABS 2017 §3/3 iii, Eqs (3.1)-(3.2): Hm and Hw are vertical heads.

    Narrow hand model: straight members, horizontal mean sea level z=0,
    attachment elevations relative to the ring and lower flex joint.
    Unsupported topology must be qualified by another calculation.
    """
    buoys = {b['name']: b for b in spec['generic']['buoys_6d']}
    # z_global=z_reference+z_local requires unrotated attachment frames.
    if any(any(float(angle) != 0 for angle in buoys[name]['properties'].get('InitialAttitude', [0, 0, 0]))
           for name in ('Tensioner Ring', 'Flex Joint')):
        raise ValueError('Hand geometry requires unrotated attachment frames')
    top = float(buoys['Tensioner Ring']['initial_position'][2])
    bottom = float(buoys['Flex Joint']['initial_position'][2])
    if not all(math.isfinite(v) for v in (top, bottom)) or not bottom < 0 <= top:
        raise ValueError('Hand basis requires ring at/above sea level and flex joint below it')
    return dict(ring_elevation_m=top, point_z_m=bottom, hm_m=top-bottom,
                hw_m=-bottom, datum='mean sea level z=0 m; positive z upward')


def member_basis(spec: dict, name: str) -> dict:
    """Flooded steel Ws, fluid head once, with actual above-water steel weight."""
    line = next(member for member in spec['generic']['lines'] if member['name'] == name)
    types = {t['name']: t for t in spec['generic']['line_types']}
    rho_w = spec['environment']['water']['density']
    geometry = column_geometry(spec)
    ends = next(v for k, v in line['properties'].items() if k.startswith('Connection, '))
    if [e[0] for e in ends] != ['Tensioner Ring', 'Flex Joint']:
        raise ValueError('Hand basis requires ring-to-flex-joint members')
    top = geometry['ring_elevation_m'] + float(ends[0][3])
    bottom = geometry['point_z_m'] + float(ends[1][3])
    if not bottom < 0 <= top:
        raise ValueError('Member attachments must span mean sea level')
    total_length = math.fsum(span for _, span in sections(line))
    if total_length <= 0 or any(span <= 0 for _, span in sections(line)):
        raise ValueError('Member section lengths must be positive')
    # Straight reference chord, parameterised by unstretched section arclength.
    dz_ds = (top-bottom)/total_length
    weights, areas, rigidities, spans, drag, wet_spans = [], [], [], [], [], []
    for ref, span in sections(line):
        t = types[ref]
        bore = math.pi / 4 * t['inner_diameter']**2
        steel = math.pi / 4 * (t['outer_diameter']**2 - t['inner_diameter']**2)
        # z(s)=z_A-(z_A-z_B)*s/L; wet arclength intersects z<=0.
        # Archimedes: flooded steel Ws=g*As*(rho_s*L-rho_w*Lwet).
        z_top = top - dz_ds*math.fsum(spans)
        wet = min(span, max(0, span-z_top/dz_ds))
        wet_spans.append(wet)
        # Morison drag per unit length: q=0.5*rho_w*Cd*D*U^2.
        drag.append(float(t['properties']['Cdn']) * float(t['outer_diameter']) * wet)
        weights.append(G * steel * (t['properties']['MaterialDensity'] * span - rho_w * wet))
        areas.append(bore * span)
        rigidities.append(float(t['properties']['E']) * steel * span)
        spans.append(span)
    length = math.fsum(spans)
    ai = math.fsum(areas) / length
    if len({types[ref]['inner_diameter'] for ref, _ in sections(line)}) != 1:
        raise ValueError('Hand pressure-head basis requires constant member bore')
    density = float(line['properties']['ContentsDensity'])
    if not math.isfinite(density) or density < 0:
        raise ValueError('ContentsDensity must be finite and nonnegative')
    head = G * ai * (density * (top-bottom) - rho_w * (-bottom))
    return dict(ws_kn=math.fsum(weights), internal_area_m2=ai, head_kn=head, length_m=length,
                ea_kn=math.fsum(rigidities) / length, mud_density_te_m3=density,
                hm_m=top-bottom, hw_m=-bottom, wet_section_lengths_m=wet_spans, cd_d_wet_integral_m2=math.fsum(drag))


def link_geometry(spec: dict, ring_z: float, factor: float) -> tuple:
    """Interpolate spring tensions and resolve fleet angle from link endpoints."""
    vertical, lengths, cosines = [], [], []
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
        vertical.append(factor * force * cosine)
        lengths.append(length)
        cosines.append(cosine)
    if len(lengths) != 4:
        raise ValueError('Expected four tensioner links')
    return math.fsum(vertical), lengths, math.fsum(cosines) / len(cosines)


def tension_basis(spec: dict) -> dict:
    """Derive acceptance inputs; half slip weight is a HAND assumption only."""
    parts = [member_basis(spec, name) for name in MEMBERS]
    geometry = column_geometry(spec)
    densities = {p['mud_density_te_m3'] for p in parts}
    if len(densities) != 1:
        raise ValueError('Member ContentsDensity values must agree')
    mud_density = densities.pop()
    arrangement = spec['tensioner_arrangement']
    share = surviving_share(basis=arrangement)
    weight_reserve(basis=arrangement)
    ws = math.fsum(p['ws_kn'] for p in parts)
    ai = math.fsum(p['internal_area_m2'] for p in parts)
    # Equivalent heads preserve sum(Ai_i * rho * g * H_i) at offset attachments.
    geometry['hm_m'] = math.fsum(p['internal_area_m2']*p['hm_m'] for p in parts)/ai
    geometry['hw_m'] = math.fsum(p['internal_area_m2']*p['hw_m'] for p in parts)/ai
    rho_w = spec['environment']['water']['density']
    tsr = minimum_slip_ring_tension(ws, 0, ai, mud_density*G, geometry['hm_m'], rho_w*G, geometry['hw_m'], f_wt=arrangement['fwt'])
    slip = next(member for member in spec['generic']['lines'] if member['name'] == 'Riser Slip Joint')
    st = next(t for t in spec['generic']['line_types'] if t['name'] == 'Slip Joint')
    ring = next(b for b in spec['generic']['buoys_6d'] if b['name'] == 'Tensioner Ring')
    slip_mass = st['mass_per_length'] + math.pi/4*st['inner_diameter']**2*slip['properties']['ContentsDensity']
    carried = G*(ring['mass'] + slip_mass * math.fsum(s for _, s in sections(slip)) / 2)
    weight = math.fsum([ws, *(p['head_kn'] for p in parts)])
    stiffness = math.fsum(p['ea_kn']/p['length_m'] for p in parts)
    target = (tsr + carried) / share
    # Straight extensible string at zero offset/current: mean axial load is
    # top effective tension minus half effective weight. Pressure strain omitted.
    ring_z = ring['initial_position'][2] + (target-carried-weight/2)/stiffness
    _, _, cosine = link_geometry(spec, ring_z, 1)
    fleet_adjusted = minimum_top_tension_16q(1, n_units=arrangement['n_units'],
        n_fail=arrangement['n_fail'], efficiency=1, fleet_angle_factor=1/cosine)
    return dict(ws_kn=ws, internal_area_m2=ai, tsrmin_kn=tsr, carried_weight_kn=carried,
                effective_weight_kn=weight, stiffness_kn_m=stiffness,
                required_vertical_kn=target, spring_table_factor=arrangement['spring_table_factor'],
                fleet_cosine=cosine, system_factor=1/share, fleet_adjusted_system_factor=fleet_adjusted,
                mechanical_hydraulic_efficiency=1, **geometry,
                n_units=arrangement['n_units'], n_fail=arrangement['n_fail'],
                fwt=arrangement['fwt'], buoyancy_kn=0, mud_density_te_m3=mud_density,
                member_basis=parts, source=BASIS, carried_weight_basis='hand estimate: ring plus half dry slip-joint and contents weight; native uses End B global reaction')


def checks(vertical_kn: float, carried_kn: float, tsrmin_kn: float, ws_kn: float,
           bottom_kn: float, member_min_kn: float, *, basis: dict) -> dict:
    """Binding checks; positive margin meets the stated criterion, zero included."""
    if not all(math.isfinite(v) for v in (vertical_kn, carried_kn, tsrmin_kn, ws_kn, bottom_kn, member_min_kn)):
        raise ValueError('Nonfinite tension evidence')
    a = vertical_kn - carried_kn - tsrmin_kn
    b = surviving_share(basis=basis) * vertical_kn - carried_kn - tsrmin_kn
    bottom = bottom_kn - weight_reserve(basis=basis) * ws_kn
    return dict(check_a_margin_kn=a, check_b_margin_kn=b, bottom_margin_kn=bottom,
                check_a_pass=a >= 0 and bottom >= 0 and member_min_kn >= 0,
                check_b_pass=b >= 0, member_min_kn=member_min_kn)


def hand_estimate(spec: dict, basis: dict, offset: float, speed: float, *, factor: float | None = None) -> dict:
    """Approximate extensible straight-string/constant-tension drag equilibrium.

    No native acceptance: no pressure strain, contact, bending or nonlinear shape.
    Current sag uses q²L³/(24T²); native reactions and profiles remain required.
    """
    factor = basis['spring_table_factor'] if factor is None else factor
    geometry = column_geometry(spec)
    lengths = {p['length_m'] for p in basis['member_basis']}
    if len(lengths) != 1:
        raise ValueError('Hand sag basis requires equal member lengths')
    length = lengths.pop()
    # Equivalent uniform q conserves the integrated wet-member Morison drag.
    # The sag equation q²L³/(24T²) remains an explicitly approximate hand rule.
    parts = [member_basis(spec, name) for name in MEMBERS]
    q = (0.5 * spec['environment']['water']['density'] * speed**2
         * math.fsum(p['cd_d_wet_integral_m2'] for p in parts) / length)
    def state(vertical):
        top = vertical - basis['carried_weight_kn']
        mean = max(1, top-basis['effective_weight_kn']/2)
        z = geometry['ring_elevation_m'] + mean/basis['stiffness_kn_m'] - offset**2/(2*length) - q*q*length**3/(24*mean**2)
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
                    - (p['ws_kn']+p['head_kn'])/2 for p in basis['member_basis']), basis=basis),
                qualification='hand estimate only; native checks pending')
