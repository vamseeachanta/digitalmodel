"""Native-channel adapter; interface objects are injected, never imported here.

Sign basis: OrcaFlex help, Line results: End loads (accessed 2026-10-17):
End B global force is applied by parent to line, so positive slip-joint GZ
is a downward reaction on the ring. Links: Results defines global endpoints.
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Lineresults,Endloads.htm
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Links,Results.htm
"""
import math
from .generic_tension import G, LINKS, MEMBERS, surviving_share

FIELDS = ('support_intact_min_kn', 'support_loss_min_kn', 'bottom_sum_min_kn',
          'member_min_kn', 'table_clearance_min_m')


def summarize_samples(rows: list[dict], *, basis: dict) -> dict:
    """Use simultaneous force samples; missing evidence never qualifies."""
    keys = ('vertical_kn', 'carried_kn', 'bottom_kn', 'member_min_kn', 'table_clearance_m')
    if not rows or any(any(k not in r or not math.isfinite(r[k]) for k in keys) for r in rows):
        raise ValueError('Missing or nonfinite tension evidence')
    return dict(zip(FIELDS, (
        min(r['vertical_kn']-r['carried_kn'] for r in rows),
        min(surviving_share(basis=basis)*r['vertical_kn']-r['carried_kn'] for r in rows),
        min(r['bottom_kn'] for r in rows), min(r['member_min_kn'] for r in rows),
        min(r['table_clearance_m'] for r in rows))))


def extract(model, period, end_b, *, static: bool = False, basis: dict) -> dict:
    """Extract static or main-stage evidence without starting a solver."""
    def series(name, variable, end=None):
        obj = model[name]
        args = () if end is None else (end,)
        values = ([float(obj.StaticResult(variable, *args))] if static else
                  [float(v) for v in obj.TimeHistory(variable, period, *args)])
        if not values or not all(math.isfinite(v) for v in values):
            raise ValueError('Empty or nonfinite tension channel')
        return values
    vertical, clearances = [], []
    for name in LINKS:
        force = series(name, 'Tension')
        length = series(name, 'Length')
        az, bz = series(name, 'End A Z'), series(name, 'End B Z')
        if len({len(v) for v in (force, length, az, bz)}) != 1 or min(length) <= 0:
            raise ValueError('Mismatched link channels')
        table_length = [float(v) for v in model[name].SpringLength]
        vertical.append([f*(a-b)/l for f, a, b, l in zip(force, az, bz, length)])
        clearances.append([min(l-min(table_length), max(table_length)-l) for l in length])
    carried = [G*float(model['Tensioner Ring'].Mass)+v
               for v in series('Riser Slip Joint', 'End GZ force', end_b)]
    bottom = [series(name, 'Effective tension', end_b) for name in MEMBERS]
    minima = []
    for name in MEMBERS:
        graph = (model[name].RangeGraph('Effective tension') if static else
                 model[name].RangeGraph('Effective tension', period))
        profile = [float(v) for v in (graph.Mean if static else graph.Min)]
        if not profile or not all(math.isfinite(v) for v in profile):
            raise ValueError('Empty or nonfinite member profile')
        minima.append(min(profile))
    arrays = vertical + clearances + bottom + [carried]
    if len({len(v) for v in arrays}) != 1:
        raise ValueError('Mismatched simultaneous tension channels')
    return summarize_samples([dict(vertical_kn=math.fsum(v[i] for v in vertical), carried_kn=carried[i],
        bottom_kn=math.fsum(v[i] for v in bottom), member_min_kn=min(minima),
        table_clearance_m=min(v[i] for v in clearances)) for i in range(len(carried))], basis=basis)
