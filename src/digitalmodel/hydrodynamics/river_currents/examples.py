# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build two synthetic worked examples from one cited, revisioned basis file.

Run ``python -m digitalmodel.hydrodynamics.river_currents.examples --basis
examples/hydrodynamics/river_currents/basis.json --output <output-directory>``.
The builder writes a self-contained analysis page, full-precision JSON results
and a SHA-256 manifest. Displayed results use one decimal place; decisions use
unrounded values. Sources and assumptions are carried by the basis file.
"""

import argparse
import hashlib
import html
import json
from pathlib import Path

from digitalmodel.hydrodynamics.models import VesselProperties

from .bank_effects import (
    bend_factor, bend_superelevation, blockage_factor, corrected_profile, lateral_factor,
)
from .extremes import fit_gumbel, fit_weibull, seasonal_summary
from .profiles import power_law_profile, river_velocity_profile
from .vessel_forces import current_load, shallow_drag_factor, towing_assessment


def compute(data: dict) -> dict:
    """Evaluate the flow-characterisation and river-towing examples in SI units."""
    b, e, t = data['bank'], data['extremes'], data['towing']
    w = fit_weibull(e['weibull_series_m_s'])
    g = fit_gumbel(e['annual_maxima_m_s'])
    bank = {
        'centre_factor': lateral_factor(b['width_m'] / 2, b['width_m'], b['bank_retardation']),
        'quarter_width_factor': lateral_factor(b['width_m'] / 4, b['width_m'], b['bank_retardation']),
        'confinement_factor': blockage_factor(b['width_m'], b['depth_m'], b['beam_m'], b['draft_m']),
        'outer_bank_bias_factor': bend_factor(b['width_m'], b['width_m'], b['outer_bias']),
        'superelevation_m': bend_superelevation(b['surface_speed_m_s'], b['width_m'],
                                               b['bend_radius_m'], b['gravity_m_s2']),
        'weibull_shape': w.shape, 'weibull_scale_m_s': w.scale,
        'weibull_10_year_m_s': w.return_level(e['return_period_years'], e['independent_samples_per_year']),
        'gumbel_location_m_s': g.location, 'gumbel_scale_m_s': g.scale,
        'gumbel_10_year_m_s': g.return_level(e['return_period_years']),
        'seasons': seasonal_summary(e['seasonal_speeds_m_s'], e['season_labels']),
    }
    if e['return_period_years'] != 10:
        raise ValueError("worked example uses a ten-year return period")
    mean = t['discharge_m3_s'] / (t['width_m'] * t['depth_m'])
    surface = mean * (t['exponent_m'] + 1) / t['exponent_m']
    factor = lateral_factor(t['bank_position_m'], t['width_m'], t['bank_retardation'])
    factor *= blockage_factor(t['width_m'], t['depth_m'], t['beam_m'], t['draft_m'])
    profile = corrected_profile(lambda z: power_law_profile(
        z, t['depth_m'], surface, t['exponent_m']), factor)
    vessel = VesselProperties('generic', t['length_m'], t['beam_m'], t['draft_m'], t['displacement_t'])
    loads = current_load(vessel, profile, t['depth_m'], water_density=t['density_kg_m3'],
                         n_slices=t['integration_slices'])
    uniform = current_load(vessel, lambda z: surface * factor, t['depth_m'],
                           water_density=t['density_kg_m3'])
    assessment = towing_assessment(loads, t['available_pull_N'], efficiency=t['efficiency'],
                                    reserve_factor=t['reserve_factor'], yaw_capacity_Nm=t['yaw_capacity_Nm'])
    return {'bank_and_extremes': bank, 'towing': {
        'section_mean_m_s': mean, 'surface_speed_m_s': surface * factor,
        'shallow_drag_factor': shallow_drag_factor(t['depth_m'], t['draft_m']),
        'loads': loads, 'uniform_surface_loads': uniform, 'assessment': assessment,
        'surge_reduction_percent': 100 * (1 - loads['Fx_surge_N'] / uniform['Fx_surge_N']),
    }}


def profile_svg(depth: float, surface_speed: float) -> str:
    """Draw the shipped power-law profile as inline SVG with SI axis labels."""
    heights, speeds = river_velocity_profile(31, depth, surface_velocity=surface_speed)
    points = ' '.join(f'{60 + 360 * u / surface_speed:.1f},{300 - 250 * z / depth:.1f}'
                      for z, u in zip(heights, speeds))
    return (
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 500 350" role="img" '
        'aria-label="Synthetic river vertical power-law velocity profile">'
        '<title>River velocity versus height above bed</title>'
        '<path d="M60 50 V300 H420" fill="none" stroke="black"/>'
        f'<polyline points="{points}" fill="none" stroke="blue" stroke-width="2"/>'
        '<text x="130" y="340">Velocity (m/s)</text>'
        '<text x="70" y="30">height above bed (m)</text>'
        '<text x="30" y="300">0</text>'
        f'<text x="20" y="55">{depth:.1f}</text>'
        f'<text x="410" y="320">{surface_speed:.1f}</text></svg>'
    )


def _rows(values: list[tuple[str, float, str]]) -> str:
    return ''.join(f'<tr><td>{html.escape(label)}</td><td>{value:.1f}</td>'
                   f'<td>{html.escape(unit)}</td></tr>' for label, value, unit in values)


def render(data: dict, results: dict) -> str:
    """Render summary, recorded revision history, calculations and known limits."""
    revision = data['revision']
    records = data['revisions']
    if not records or records[-1]['revision'] != revision:
        raise ValueError("declared revision must match the latest revision record")
    history = ''.join('<tr>' + ''.join(f'<td>{html.escape(str(row[key]))}</td>'
                                     for key in ('revision', 'date', 'change')) + '</tr>'
                      for row in records)
    b, t, inp = results['bank_and_extremes'], results['towing'], data['towing']
    bank_rows = _rows([(key.replace('_', ' '), b[key], unit) for key, unit in (
        ('centre_factor', 'dimensionless'), ('quarter_width_factor', 'dimensionless'),
        ('confinement_factor', 'dimensionless'), ('outer_bank_bias_factor', 'dimensionless'),
        ('superelevation_m', 'm'), ('weibull_10_year_m_s', 'm/s'), ('gumbel_10_year_m_s', 'm/s'))])
    tow_rows = _rows([
        ('Section mean', t['section_mean_m_s'], 'm/s'),
        ('Corrected surface speed', t['surface_speed_m_s'], 'm/s'),
        ('Slot drag multiplier', t['shallow_drag_factor'], 'dimensionless'),
        ('Integrated surge', t['loads']['Fx_surge_N'] / 1000, 'kN'),
        ('Uniform surface surge', t['uniform_surface_loads']['Fx_surge_N'] / 1000, 'kN'),
        ('Integrated sway', t['loads']['Fy_sway_N'] / 1000, 'kN'),
        ('Integrated yaw', t['loads']['Mz_yaw_Nm'] / 1000, 'kN m'),
        ('Surge reduction', t['surge_reduction_percent'], '%'),
        ('Required nominal pull', t['assessment']['required_bollard_pull_N'] / 1000, 'kN'),
        ('Available nominal pull', inp['available_pull_N'] / 1000, 'kN'),
    ])
    assumptions = ''.join(f'<li>{html.escape(a)}</li>' for a in data['assumptions'])
    sources = ''.join(f'<li><a href="{html.escape(s["url"], quote=True)}">'
                      f'{html.escape(s["title"])}</a>; opened {s["retrieved_utc"][:10]}.</li>'
                      for s in data['sources'])
    seasons = ''.join(f'<tr><td>{html.escape(s)}</td><td>{r["count"]}</td>'
                      f'<td>{r["mean"]:.1f}</td><td>{r["maximum"]:.1f}</td></tr>'
                      for s, r in b['seasons'].items())
    verdict = 'GO' if t['assessment']['go'] else 'NO-GO'
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>River current analysis</title>
<style>body{{max-width:900px;margin:2em auto;font-family:sans-serif;line-height:1.5}}
table{{border-collapse:collapse}}th,td{{border:1px solid #aaa;padding:0.4em}}svg{{max-width:500px;width:100%}}</style>
</head><body><h1>River current analysis</h1><p>Revision {html.escape(revision)}</p>
<h2>Summary</h2><p>{html.escape(data['basis'])} The towing case gives {verdict}:
required nominal pull {t['assessment']['required_bollard_pull_N']/1000:.1f} kN against
{inp['available_pull_N']/1000:.1f} kN available. This compares steady force and yaw capacities only.</p>
<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>{history}</table>
<h2>Bank effects and extremes</h2><p>The synthetic channel is {data['bank']['width_m']:.1f} m wide and
{data['bank']['depth_m']:.1f} m deep, with a {data['bank']['beam_m']:.1f} m beam and
{data['bank']['draft_m']:.1f} m draft obstruction. The lateral quadratic has unit mean;
the confinement factor is A/(A-A_v). The assumed outer-bank bias is independent of
the constant-speed superelevation u²/g * ln((R+W/2)/(R-W/2)).</p>
<table><tr><th>Quantity</th><th>Value</th><th>Unit</th></tr>{bank_rows}</table>
<p>Ten synthetic quantiles reproduce a Weibull with shape {b['weibull_shape']:.1f},
scale {b['weibull_scale_m_s']:.1f} m/s and an annual-maxima Gumbel with location
{b['gumbel_location_m_s']:.1f} m/s, scale {b['gumbel_scale_m_s']:.1f} m/s.
{html.escape(data['extremes']['location_basis'])} These are distribution checks, not measured extremes.</p>
<table><tr><th>Season</th><th>Count</th><th>Mean (m/s)</th><th>Maximum (m/s)</th></tr>{seasons}</table>
<h2>River towing</h2><p>A {inp['width_m']:.1f} m wide, {inp['depth_m']:.1f} m deep section with
{inp['discharge_m3_s']:.1f} m³/s discharge loads a generic rectangular barge:
length {inp['length_m']:.1f} m, beam {inp['beam_m']:.1f} m, draft {inp['draft_m']:.1f} m.
Section mean is Q/(WH); surface speed is mean*(m+1)/m, then multiplied by the
mid-channel and section blockage factors. The current is from the bow; vessel speed is zero.</p>
<p>Signed quadratic velocity is integrated over height H-T to H by midpoint slices.
The slot multiplier is [H/(H-T)]². Existing approximate load coefficients are reused.
Nominal pull demand is hypot(Fx,Fy)*reserve/efficiency, with reserve {inp['reserve_factor']:.1f}
and efficiency {inp['efficiency']:.1f}. Yaw demand is compared separately with the stated
{inp['yaw_capacity_Nm']:.1f} N m capacity. More nominal pull or reduced current is needed
for the force criterion; actual towing requires a calibrated resistance and thrust assessment.</p>
<table><tr><th>Quantity</th><th>Value</th><th>Unit</th></tr>{tow_rows}</table>
{profile_svg(inp['depth_m'], t['surface_speed_m_s'])}
<h2>Assumptions and limits</h2><ul>{assumptions}</ul>
<p>Tidal reversal, unsteady river hydraulics and calibrated operational acceptance remain outside this analysis (#2000, #2002).</p>
<h2>Sources</h2><ul>{sources}</ul><p>The basis file also identifies the reused repository modules.</p>
</body></html>
'''


def build(basis_path: Path, output_dir: Path) -> dict:
    """Write deterministic deliverables and record digests of their committed bytes."""
    raw = basis_path.read_bytes()
    data = json.loads(raw)
    results = compute(data)
    page = render(data, results)
    outputs = {'river-current-analysis.html': page.encode(),
               'river-current-analysis.results.json': (json.dumps(results, indent=2, sort_keys=True) + '\n').encode()}
    manifest = {'revision': data['revision'], 'basis_sha256': hashlib.sha256(raw).hexdigest(),
                'outputs': {name: hashlib.sha256(content).hexdigest() for name, content in outputs.items()}}
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, content in outputs.items():
        (output_dir / name).write_bytes(content)
    (output_dir / 'river-current-analysis.manifest.json').write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + '\n', encoding='utf-8')
    return manifest


def main() -> None:
    """Read explicit basis and output locations from the command line."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--basis', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    build(args.basis, args.output)


if __name__ == '__main__':
    main()
