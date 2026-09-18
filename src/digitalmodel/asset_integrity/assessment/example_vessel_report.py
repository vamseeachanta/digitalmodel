"""Example geometry report; all normative assessment results remain pending."""
from html import escape
import math


def _preview(grid):
    """Aggregate sample cells for display only, retaining each bin's minimum."""
    xs, ss = grid["x_mm"], grid["s_mm"]
    nx, ns = len(xs), len(ss)
    bx, bs = math.ceil(nx / 40), math.ceil(ns / 40)
    buckets = {}
    for i, row in enumerate(grid["rows"]):
        key = (i // ns // bx, i % ns // bs)
        buckets[key] = min(row["assessed_mm"], buckets.get(key, math.inf))
    def edge(axis, index):
        if index == 0:
            return axis[0]
        if index >= len(axis):
            return axis[-1]
        return (axis[index - 1] + axis[index]) / 2
    cells = [(edge(xs, ix * bx), edge(xs, (ix + 1) * bx),
              edge(ss, iy * bs), edge(ss, (iy + 1) * bs), value)
             for (ix, iy), value in sorted(buckets.items())]
    return dict(bounds=(xs[0], xs[-1], ss[0], ss[-1]), cells=cells)


def _colour(thickness, nominal):
    fraction = max(0, min(1, thickness / nominal))
    return f"rgb({220 - 185 * fraction:.0f},{55 + 55 * fraction:.0f},{35 + 175 * fraction:.0f})"


def _legend(area_id, nominal):
    return (f'<defs><linearGradient id="colorbar-{area_id}" x1="0" y1="1" x2="0" y2="0">'
            f'<stop offset="0" stop-color="{_colour(0, nominal)}"/>'
            f'<stop offset="1" stop-color="{_colour(nominal, nominal)}"/></linearGradient></defs>'
            f'<rect x="665" y="65" width="20" height="200" fill="url(#colorbar-{area_id})"/>'
            f'<text x="695" y="70">{nominal:.3f} mm</text>'
            f'<text x="695" y="169">{nominal / 2:.3f} mm</text>'
            '<text x="695" y="269">0.000 mm</text><text x="665" y="294">Assessed wall</text>')


def _heatmap(area_id, grid, nominal):
    preview = _preview(grid)
    x0, x1, s0, s1 = preview["bounds"]
    scale = min(530 / (x1 - x0), 320 / (s1 - s0))
    width, height = (x1 - x0) * scale, (s1 - s0) * scale
    cells = []
    for left, right, bottom, top, wall in preview["cells"]:
        cells.append(f'<rect x="{65 + (left-x0)*scale:.3f}" '
                     f'y="{65 + (s1-top)*scale:.3f}" '
                     f'width="{(right-left)*scale:.3f}" height="{(top-bottom)*scale:.3f}" '
                     f'fill="{_colour(wall, nominal)}"><title>'
                     f'Bin x=[{left:.3f}, {right:.3f}], s=[{bottom:.3f}, {top:.3f}] mm; '
                     f'minimum assessed wall={wall:.3f} mm</title></rect>')
    return (f'<svg viewBox="0 0 810 450" role="img" aria-label="Area {area_id} example thickness map" '
            f'data-map-width="{width:.6f}" data-map-height="{height:.6f}">'
            + _legend(area_id, nominal) + ''.join(cells)
            + '<text x="65" y="20">Equal axial/arc scale; minimum-bin preview (≤40 × 40 bins)</text>'
            + f'<text x="65" y="45">Arc s: {s0:.1f} to {s1:.1f} mm ↑</text>'
            + f'<text x="65" y="{85+height:.2f}">Axial x: {x0:.1f} to {x1:.1f} mm →</text></svg>')


def _sampling_table(study):
    rows = ''.join(f'<tr><td>{r["pitch_mm"]:.2f}</td><td>{r["points"]}</td>'
                   f'<td>{r["axial_loss_area_mm2"]:.3f}</td>'
                   f'<td>{r["circumferential_loss_area_mm2"]:.3f}</td>'
                   f'<td>{r["developed_loss_mm3"]:.3f}</td></tr>' for r in study["resolutions"])
    return ('<table><tr><th>Target pitch (mm)</th><th>Points</th><th>Axial loss area (mm²)</th>'
            '<th>Arc loss area (mm²)</th><th>Developed-surface loss integral (mm³)</th></tr>'
            + rows + '</table>')


def _area_section(area, grid, study, basis):
    area_id = area["area_id"]
    fine = study["resolutions"][-1]
    minimum = area["minimum_mm"] - basis["uncertainty_mm"] - basis["future_loss_mm"]
    return (f'<section><h2>Area {area_id}</h2><p>Example current minimum: {area["minimum_mm"]:.3f} mm; '
            f'assessed minimum: {minimum:.3f} mm. '
            f'Location: x={area["centre_x_mm"]} mm, θ={area["theta_deg"]}°.</p>'
            + _heatmap(area_id, grid, basis["nominal_mm"])
            + '<p class="caption">Display bins retain the minimum sampled wall within each bin; '
            'they are a preview, not a reconstructed geometry or an acceptance map.</p>'
            f'<p>Full data downloads use the finest retained {grid["target_pitch_mm"]:g} mm target pitch; '
            f'actual pitches: {grid["actual_pitch_x_mm"]:.6f} mm axial, '
            f'{grid["actual_pitch_s_mm"]:.6f} mm circumferential. '
            f'<a href="area-{area_id.lower()}-grid.csv">Full wall grid CSV</a> · '
            f'<a href="area-{area_id.lower()}-profiles.json">Both critical profiles</a></p>'
            + _sampling_table(study)
            + f'<p class="caption">Sampling of the defined geometry: {escape(study["status"])}. '
            f'Finest developed-loss error against its closed-form integral: '
            f'{fine["developed_loss_error_fraction"]:.3%}.</p>'
            '<p><strong>Conclusion:</strong> example geometry is generated; the sampling criterion concerns '
            'integrated geometric quantities only. Material/code calibration and every code/FE/repair '
            'acceptance result remain <strong>NOT EVALUATED</strong>.</p></section>')


def _basis_text(basis):
    return (f'<p>Basis: {2*basis["inside_radius_mm"]:g} mm inside diameter; '
            f'{basis["shell_tangent_length_mm"]:g} mm tangent-to-tangent shell; '
            f'{basis["nominal_mm"]:.3f} mm nominal wall; '
            f'{basis["target_pressure_mpa_g"]:.2f} MPa gauge target, '
            f'{basis["reduced_pressure_trial_mpa_g"]:.2f} MPa reduced-pressure trial, '
            f'{basis["assessment_temperature_c"]:g} °C assessment temperature. '
            'Example material properties are assumed; code qualification and supplemental-load assessments are pending.</p>'
            f'<p>Assessed wall = example current wall − {basis["uncertainty_mm"]:.3f} mm uncertainty '
            f'− {basis["future_loss_mm"]:.3f} mm future loss. Both critical profiles use this assessed field. '
            f'The horizon of {basis["future_horizon_years"]:g} years and '
            f'{basis["future_corrosion_rate_mm_per_year"]:.3f} mm/year loss rate are assumptions, '
            'not inspection-derived life estimates.</p>')


def _material_text(basis):
    m = basis["material"]
    rows = (("Elastic modulus (MPa)", m["elastic_modulus_mpa"]),
            ("Poisson ratio (dimensionless)", m["poisson_ratio"]),
            ("Yield stress (MPa)", m["yield_mpa"]),
            ("Tensile strength, information only (MPa)", m["tensile_mpa"]),
            ("Assumed screening stress (MPa)", m["screening_stress_mpa"]),
            ("Density (kg/m³)", m["density_kg_m3"]))
    table = ''.join(f'<tr><td>{label}</td><td>{value:.3f}</td></tr>' for label, value in rows)
    return ('<h2>Assumed example material</h2><p>Generic homogeneous isotropic carbon steel; '
            f'properties assigned at {m["property_temperature_c"]:g} °C. '
            'No SA-516 Grade 70 qualification is implied.</p><table>'
            '<tr><th>Property (unit)</th><th>Assumed value</th></tr>' + table + '</table>'
            '<p class="caption">User-authorized example assumptions; not material-table extracts.</p>'
            '<p>The screening stress is independently assigned, not a code-qualified allowable. '
            'The von Mises elastic-perfectly-plastic model uses the assumed yield stress without hardening; '
            'tensile strength supplies neither a hardening point nor a failure criterion. '
            'Weld/HAZ differentiation, local-failure, fracture and fatigue data remain unestablished.</p>'
            f'<p>Constitutive-model scope: {escape(m["constitutive_use"])}.</p>'
            f'<p>Expansion coefficient: {m["expansion_per_k"]:.6f} K⁻¹ over '
            f'{m["expansion_range_c"][0]:g}–{m["expansion_range_c"][1]:g} °C, '
            f'with {m["stress_free_temperature_c"]:g} °C stress-free reference. '
            'Thermal restraints are not assigned. Strength/modulus interpolation is not authorized by this range.</p>')


def _method_text(basis):
    return ('<h2>Method and decisions</h2><p>Each smooth compact depression is controlled analytically. '
            'Area B adds a narrow central depression; C/D use rounded plateau shoulders. '
            'Grid pitches are snapped separately per axis to include centre and footprint edges. '
            'Minima are imposed by construction and do not qualify sampling.</p>'
            f'<p>The developed-surface loss integral uses arc coordinates at the bore radius '
            f'{basis["inside_radius_mm"]:g} mm. It integrates thickness loss over a developed area; '
            'it is not physical removed volume and does not include the cylindrical radial factor.</p>'
            '<p>Sampling criterion: at most 1% change in integrated axial/arc profile loss and developed '
            'loss between the finest two pitches, plus at most 1% developed-loss error against the '
            'closed-form integral of the same defined geometry. This checks numerical integration, '
            'not independent geometry validity, code acceptance or real inspection spacing. '
            'FE mesh convergence: NOT EVALUATED.</p>'
            '<p>Next: verify edition-specific API 579 procedures using the declared example properties, calibrate example '
            'outcomes, then execute qualified Level 3 and repair models. A solver error will not count as '
            'engineering failure. Repair design acceptance will remain separate from fabrication/NDE/testing.</p>'
            '<p><a href="assumptions.json">Assumptions and geometry</a> · '
            '<a href="sampling.json">Sampling study</a> · <a href="manifest.json">SHA-256 manifest</a></p>')


def render_report(basis, grids, studies):
    sections = ''.join(_area_section(a, grids[a["area_id"]], studies[a["area_id"]], basis)
                       for a in basis["areas"])
    routes = (("A", "Level 1"), ("B", "Level 2 after Level 1 non-pass"),
              ("C", "Level 3 after Levels 1–2 non-pass"),
              ("D", "Repair after Levels 1–3 non-pass"))
    matrix = ''.join(f'<tr><td>{a}</td><td>{r}</td><td>NOT EVALUATED</td></tr>' for a, r in routes)
    return ('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" '
            'content="width=device-width,initial-scale=1"><title>Four-area vessel — engineering progress</title>'
            '<style>body{max-width:1000px;margin:35px auto;padding:20px;font:16px/1.5 system-ui;'
            'color:#193040}h1,h2{color:#153e57}table{border-collapse:collapse;width:100%;font-size:14px}'
            'th,td{padding:8px;border:1px solid #abc;text-align:left}th{background:#eef3f6}'
            'svg{width:100%;height:auto}.notice{background:#fff2d9;padding:18px}'
            '.caption{font-size:13px;color:#456}section{margin-top:35px}a{color:#006394}</style></head><body>'
            '<h1>Four-area pressure vessel<br>Engineering progress report</h1>'
            '<p class="notice"><strong>EXAMPLE DATA — assumed and generated, not field measurements.</strong> '
            'All four areas are assumed far apart and non-interacting. Intended assessment routes have '
            'not been established by calculation. This is a geometry/data deliverable.</p>'
            + _basis_text(basis) + _material_text(basis)
            + '<table><tr><th>Area</th><th>Target route</th><th>Code result</th></tr>' + matrix + '</table>'
            '<p class="caption">Target scenario matrix. No result is inferred from the selected shapes.</p>'
            + sections + _method_text(basis) + '</body></html>\n')
