# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Deterministic self-contained lookup graphs read from the pilot database."""

from html import escape

from .rao_database import RAODatabase


def comparison_graph(entries):
    """Overlay head-sea heave on common axes for the filtered hull family."""
    xmin = min(float(e.rao_data.frequencies.min()) for e in entries)
    xmax = max(float(e.rao_data.frequencies.max()) for e in entries)
    traces = [(e, e.rao_data.amplitudes[:, list(e.rao_data.directions).index(0), 2]) for e in entries]
    ymax = max(max(float(values.max()), 1e-12) for _, values in traces)
    colors = ("#0066aa", "#cc5500", "#208020", "#8844aa", "#bb3344", "#665500")
    svg = [f'<h3>Head-sea comparison</h3><svg viewBox="0 0 900 550" role="img" aria-label="Head-sea heave comparison"><rect x="60" y="30" width="760" height="280" fill="#f5f5f5" stroke="#aaa"/><text x="5" y="40">{ymax:.1f}</text><text x="10" y="310">0</text><text x="65" y="335">{xmin:.2f}</text><text x="780" y="335">{xmax:.2f}</text><text x="330" y="335">Frequency (rad/s)</text><text x="75" y="50">Heave (m/m), 0 deg</text>']
    for index, (entry, values) in enumerate(traces):
        attrs = " ".join(f'data-{key}="{value:g}"' for key, value in entry.hull_params.items())
        points = " ".join(f"{60+760*(float(w)-xmin)/max(xmax-xmin,1e-12):.1f},{310-280*float(a)/ymax:.1f}" for w,a in zip(entry.rao_data.frequencies,values))
        color = colors[index % len(colors)]
        dash = ' stroke-dasharray="6 3"' if entry.metadata["form"] == "analytic_ship" else ""
        svg.append(f'<g class="comparison-line" data-form="{escape(entry.metadata["form"])}" {attrs}><polyline points="{points}" stroke="{color}"{dash}><title>{escape(entry.variation_id)}</title></polyline><text x="{60+(index%4)*200}" y="{370+(index//4)*35}" fill="{color}">{escape(entry.variation_id)}</text></g>')
    return "".join(svg) + '</svg><p>Figure 1. Head-sea heave across variants on common axes; dashed curves denote the analytic ship form.</p>'


def build_lookup(database_path, revisions, results):
    """Build a report from Parquet readback and its append-only revision data."""
    if not revisions or revisions[-1]["revision"] != results["revision"]:
        raise ValueError("Newest revision must equal the declared report revision")
    db = RAODatabase()
    db.load_from_disk(database_path)
    entries = list(db.query({}))
    rows = "".join(f'<tr><td>{r["revision"]}</td><td>{escape(r["date"])}</td><td>{escape(r["change"])}</td></tr>' for r in revisions)
    document = [f'<!doctype html><html lang="en"><meta charset="utf-8"><title>Parametric hull motion analysis</title><style>body{{font:16px sans-serif;max-width:1200px;margin:2em auto;padding:1em}}table{{border-collapse:collapse}}td,th{{padding:.4em;border:1px solid #aaa}}svg{{width:100%;height:auto}}polyline{{fill:none;stroke-width:2}}section{{margin:1em 0}}</style><h1>Parametric hull motion analysis</h1><p>Revision {results["revision"]}</p><h2>Summary</h2><p>{escape(results["summary"])}</p><h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>{rows}</table><p>Table 1. Report issues.</p><h2>Lookup</h2><p>Each case is filtered by its form and length, beam and draught (m). Curves are linear connections between solved points; no off-grid interpolation is qualified.</p>']
    document.append('<label>Form <select id="form"><option value="">All</option><option>box</option><option>analytic_ship</option></select></label>')
    for key in ("length_m", "beam_m", "draft_m"):
        values = sorted({e.hull_params[key] for e in entries})
        options = "".join(f'<option>{v:g}</option>' for v in values)
        document.append(f'<label> {key} <select id="{key}"><option value="">All</option>{options}</select></label>')
    colors = ("#0066aa", "#cc5500", "#208020", "#8844aa", "#bb3344", "#665500")
    document.append(comparison_graph(entries))
    figure_number = 1
    for entry in entries:
        attrs = " ".join(f'data-{key}="{value:g}"' for key, value in entry.hull_params.items())
        params = ", ".join(f"{key}={value:g} m" for key, value in entry.hull_params.items())
        document.append(f'<section class="case" data-form="{escape(entry.metadata["form"])}" {attrs}><h3>{escape(entry.variation_id)}</h3><p>{params}</p>')
        for dof, name in enumerate(("Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw")):
            unit = "m/m" if dof < 3 else "deg/m"
            x = entry.rao_data.frequencies
            traces = entry.rao_data.amplitudes[:, :, dof]
            ymax = max(float(traces.max()), 1e-12)
            xmin, xmax = float(x.min()), float(x.max())
            span = max(xmax-xmin, 1e-12)
            document.append(f'<svg viewBox="0 0 620 245" role="img" aria-label="{name} amplitude"><rect x="65" y="15" width="520" height="175" fill="#f5f5f5" stroke="#aaa"/><text x="5" y="30">{ymax:.1f}</text><text x="15" y="190">0</text><text x="65" y="213">{xmin:.2f}</text><text x="550" y="213">{xmax:.2f}</text><text x="220" y="240">Frequency (rad/s)</text><text x="80" y="35">{name} ({unit})</text>')
            for heading_index, heading in enumerate(entry.rao_data.directions):
                points = " ".join(f"{65+520*(float(w)-xmin)/span:.1f},{190-175*float(a)/ymax:.1f}" for w, a in zip(x, traces[:, heading_index]))
                color = colors[heading_index % len(colors)]
                document.append(f'<polyline points="{points}" stroke="{color}"><title>{heading:g} deg</title></polyline><text x="{350+heading_index*90}" y="35" fill="{color}">{heading:g} deg</text>')
            figure_number += 1
            document.append(f'</svg><p>Figure {figure_number}. {name} amplitude for {escape(entry.variation_id)}; headings denote wave propagation.</p>')
        document.append('</section>')
    document.append('<h2>Criteria and numerical record</h2>')
    if "basis" in results:
        basis, criteria = results["basis"], results["criteria"]
        document.append(f'<p>Solver: Capytaine {escape(results["solver_version"])}. Reference: {escape(basis["reference_file"])} at revision {escape(basis["source_revision"])}; the no-lid run is compared. Geometry and loading source digests are retained with the database manifest.</p><p>Pointwise benchmark bound: {criteria["heave_absolute_tolerance_m_per_m"]:g} m/m plus {100*criteria["heave_relative_tolerance"]:.1f}% of the reference amplitude. Finest-pair convergence bound: {criteria["convergence_absolute_tolerance_m_per_m"]:g} m/m. These are beforehand pilot screening inputs.</p>')
    if "benchmark" in results:
        benchmark = results["benchmark"]
        document.append(f'<p>Matched box heave uses {100*benchmark["max_fraction_of_bound"]:.1f}% of the beforehand pointwise bound. Criterion satisfied: {str(benchmark["passed"]).lower()}. The comparison is reproduction of an open-source calculation, not independent experimental validation.</p>')
    if "convergence" in results:
        convergence = results["convergence"]
        document.append(f'<p>Panel counts: {escape(str(convergence["panel_counts"]))}. Finest-pair heave change uses {100*convergence["max_absolute_change_m_per_m"]/convergence["criterion_m_per_m"]:.1f}% of the stated absolute bound. Criterion satisfied: {str(convergence["passed"]).lower()}. {escape(convergence["scope"])}.</p>')
    document.append('<h2>Basis and limits</h2><p>Linear, inviscid, zero-speed potential flow; rigid bodies; no viscous or mooring damping. The box uses the merged reference loading and scaled gyradii; the analytic ship uses uniform-density mesh inertia. These are pilot design assumptions, not measured loading conditions. All forms are fore-aft symmetric. Coordinates use x along the hull, y across the beam and z upward, with waterline z=0. Headings are wave propagation angles measured counterclockwise from +x; 0 deg is the matched longitudinal case. No lid is used to retain the reference condition. Solver warnings identify possible irregular frequencies at upper sampled points; these remain unqualified for design use. Range coverage is limited to the listed discrete dimensions and frequencies. Roll resonance and independent experimental qualification remain outside this pilot.</p>')
    document.append('<script>function filterCases(){document.querySelectorAll(".case,.comparison-line").forEach(e=>{let hide=["form","length_m","beam_m","draft_m"].some(k=>{let v=document.getElementById(k).value;return v && e.getAttribute("data-"+k)!==v;});e.style.display=hide?"none":"";});}document.querySelectorAll("select").forEach(e=>e.addEventListener("change",filterCases));</script></html>\n')
    return "".join(document)
