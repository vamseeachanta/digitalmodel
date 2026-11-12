"""Markdown and inline SVG presentation for the generic worked static cases."""

import math


def number(value):
    """Keep absent evidence distinct from a numerical zero."""
    return "not established" if value is None else format(value, ".6g")


def offset(value):
    """Describe the screen's radial or horizontal offset convention."""
    if isinstance(value, dict):
        return f"cross (x={number(value['x_m'])}, y={number(value['y_m'])}) m"
    return f"{number(value)} m"


def table(headers, rows, caption):
    """Put captions below their tables, including the terminating blank line."""
    lines = ["| " + " | ".join(headers) + " |", "| " + " | ".join(["---"] * len(headers)) + " |"]
    lines += ["| " + " | ".join(str(cell) for cell in row) + " |" for row in rows]
    return "\n".join(lines) + f"\n\n{caption}\n\n"


def inputs_section(cases):
    """Separate common cited properties from case-specific assumed dimensions."""
    models = "[models.py](../../../../src/digitalmodel/subsea/catenary_riser/models.py)"
    rows = [
        ["Young's modulus", "Pa", "207e9", "cited", models + "::STEEL_API_5L_X65; MPa to Pa"],
        ["Steel density", "kg/m³", "7850", "cited", models + "::STEEL_API_5L_X65"],
        ["Specified minimum yield strength", "Pa", "448e6", "cited", models + "::STEEL_API_5L_X65; nominal API 5L X65, MPa to Pa"],
        ["Content density, 8/10 nominal cases", "kg/m³", "850 oil", "assumed", models + "::PRODUCTION_OIL nominal value"],
        ["Content density, 12 nominal cases", "kg/m³", "0 empty gas-lift proxy", "assumed", "worked-config-1; no operating gas density established"],
        ["Seawater density", "kg/m³", "1025", "assumed", models + "::SEAWATER nominal value"],
        ["Gravity", "m/s²", "9.80665", "assumed", "worked-config-1 constant acceleration"],
        ["Internal design pressure at hang-off", "Pa", "20.0e6", "assumed", "worked-config-1; one pressure pair applied throughout"],
        ["External pressure at surface hang-off", "Pa", "0", "assumed", "worked-config-1; no depth-dependent hydrostatics"],
        ["Grounded bare tail", "m", "0.5 × water depth", "assumed", "worked-config-1; frictionless mobile touchdown"],
        ["Near / zero / far / cross offsets", "m", "−0.05z / 0 / +0.05z / (0, +0.05z)", "assumed", "z = water depth; horizontal offsets only"],
        ["SCR angle axis, from vertical", "deg", "8, 10, 12, 14, 16, 18, 20", "assumed", "worked-config-1 grid"],
        ["SLWR angle axis, from vertical", "deg", "6, 8, 10, 12", "assumed", "worked-config-1 grid"],
        ["SLWR buoyancy factor axis", "1", "1.5, 2.0, 2.5", "assumed", "buoyed weight = bare weight × (1 − factor)"],
        ["SLWR sag fraction axis", "1", "0.1, 0.2, 0.3", "assumed", "sag elevation / water depth"],
        ["SLWR hog fraction axis", "1", "0.25, 0.35, 0.5", "assumed", "hog elevation / water depth"],
        ["Maximum static utilisation", "1", "0.67", "assumed", "an assumed screening ratio of 0.67 (two thirds rounded to two decimals) on von Mises stress; a project shall replace it with the governing code design factor"],
        ["Minimum bend radius", "m", "OD/(2 × 0.002)", "assumed", "0.2% outer-fibre elastic bending strain; not a code value"],
        ["Maximum top tension", "N", "0.6 × yield strength × steel area", "assumed", "worked-config-1 tension screen"],
        ["Minimum hog clearance", "m", "50.0", "assumed", "worked-config-1; SCR has no hog bend"],
        ["Numerical shape closure", "m", "1e-6", "cited", "[static.py](../../../../src/digitalmodel/riser_configuration_screen/static.py)::CLOSURE_M"],
        ["Pipe coating / added equipment", "1", "none included", "assumed", "bare-pipe displacement and uniform contents"],
        ["Metadata", "1", "provisional=true; criteria_revision=worked-config-1", "assumed", "generic assumed values for a public worked configuration; no project data"],
        ["Routing", "1", "basename=riser_configuration_screen", "cited", "existing python -m digitalmodel router; per-case results folder in YAML"],
    ]
    text = table(["Input", "Unit", "Value", "Status", "Source / basis"], rows,
                 "Table 2. Common inputs and criteria; cited nominal properties do not establish a material certificate.")
    rows = []
    for case in cases:
        settings, pipe = case["inputs"], case["inputs"]["pipe"]
        rows.append([case["case_id"], number(settings["water_depth_m"]), number(pipe["outer_diameter_m"]),
                     f"{pipe['wall_thickness_m']*1000:.3f}", number(settings["submerged_weight_n_m"]),
                     "depth/size assumed; weight derived", f"[{case['case_id']}.yml](../../../../examples/workflows/riser-worked-configurations/{case['case_id']}.yml)"])
    return text + table(["Case / riser type", "Depth (m)", "OD (m)", "Wall (mm)", "Bare submerged weight (N/m)", "Status", "Source"], rows,
                        "Table 3. Generic nominal dimensions chosen for demonstration (assumed), not a design. w = g × (ρsteel Asteel + ρcontent Abore − ρseawater Adisplaced); A = π D²/4, steel area = displaced area − bore area.")


def result_tables(case):
    """Report metrics, criterion envelopes and zero/governing comparators."""
    if not case["offset_results"]:
        return case["missing_evidence"] + ".\n\n"
    zero = case["offset_results"][case["zero_offset_index"]]
    governing = case["offset_results"][case["governing_offset_index"]]
    metrics = [("top_tension_kn", "Top tension", "kN"), ("horizontal_tension_kn", "Horizontal tension", "kN"),
               ("top_angle_deg", "Top angle from vertical", "deg"), ("suspended_length_m", "Suspended length", "m"),
               ("horizontal_span_m", "Suspended horizontal span", "m"), ("minimum_bend_radius_m", "Minimum bend radius", "m"),
               ("hog_clearance_m", "Hog clearance", "m"), ("static_von_mises_utilisation", "Static von Mises utilisation", "1")]
    rows = [[label, unit, number(zero[key]), number(governing[key])] for key, label, unit in metrics]
    if case["inputs"]["riser_type"] == "SCR":
        rows[6][2:] = ["not applicable: no hog", "not applicable: no hog"]
    text = table(["Quantity", "Unit", "Zero offset", "Governing offset"], rows,
                 f"Table {case['case_id']}-1. Rank-1 static quantities; governing offset {offset(governing['offset_m'])}. Horizontal span excludes the grounded tail.")
    rows = []
    for name, check in case["constraint_envelope"].items():
        criterion = check["criterion"]
        display = lambda value: "not applicable" if check["status"] == "not_applicable" else number(value)
        rows.append([name, f"{criterion['relation']} {number(criterion['limit'])} {criterion['unit']}",
                     display(zero["criteria"][name]["computed"]), display(governing["criteria"][name]["computed"]),
                     display(check["computed"]), "not applicable" if check["margin"] is None else offset(check["offset_m"]),
                     display(check["margin"]), check["status"], criterion["basis"]])
    text += table(["Criterion", "Limit", "Zero computed", "Governing computed", "Envelope computed", "Controlling offset", "Margin", "Status", "Basis"], rows,
                  f"Table {case['case_id']}-2. Each constraint is evaluated over all offsets. Margin is limit minus computed for upper limits, computed minus limit for lower limits; units follow the limit. SCR hog values are not applicable, rather than missing evidence.")
    return text + comparator_table(case, zero, governing)


def comparator_table(case, zero, governing):
    """Show geometry at zero/governing offsets and stress at every offset."""
    comparisons = [(label, name, comp) for label, row in [("zero", zero), ("governing", governing)]
                   for name, comp in row["comparators"].items() if name != "static_von_mises_utilisation"]
    comparisons += [(offset(row["offset_m"]), "static_von_mises_utilisation",
                     row["comparators"]["static_von_mises_utilisation"]) for row in case["offset_results"]]
    rows = [[label, name, number(comp["comparator_value"]), number(comp["screen_value"]),
             number(comp["difference_percent"]), comp.get("governing_critical_section", "not applicable"),
             number(comp["relative_tolerance"]), comp["status"], comp.get("missing_evidence") or "none"]
            for label, name, comp in comparisons]
    return table(["Offset", "Quantity (N, m or 1)", "Comparator", "Screen", "Difference (%)",
                  "Governing critical section", "Relative tolerance (1)", "Status", "Missing evidence"], rows,
                 f"Table {case['case_id']}-3. Comparator criterion: absolute relative difference ≤ 1e-6 before serialization. "
                 "Signed difference = 100 × (screen − comparator)/comparator. Stress is compared at all four offsets; "
                 "a difference outside tolerance is not established and requires the named evidence.")


def stress_scope(data):
    """State stress independence, empirical tolerance and unresolved comparisons."""
    evidence = data["stress_comparator_evidence"]
    text = ("The stress comparator uses the same thin-wall formulation as the screen, with an independent implementation "
            "and independent section selection. Both bending signs are evaluated at hang-off and touchdown, and at sag "
            "and hog for SLWR. Hoop stress is (pi − pe) OD/(2t), pressure end force is π(pi ID² − pe OD²)/4, "
            "axial stress is (T + Fpressure)/Asteel ± E OD κ/2, and utilisation is "
            "√(axial² + hoop² − axial × hoop)/yield. Hang-off uses Ttop and κ = w H/Ttop²; touchdown/sag use "
            "H and κ = w/H; hog uses H and κ = abs(w(1 − factor))/H. The maximum over those sections is retained "
            "(first named section on an exact tie). H and Ttop are exposed screen inputs to the comparator, so independent "
            "global equilibrium is not established by the stress comparison. Agreement establishes the named-section "
            "comparison within tolerance; continuous interior extrema are not established. It is not a code check and "
            "does not provide thick-wall or FE verification.\n\n")
    text += (f"The stress criterion is 1e-6 relative before output rounding, based on numerical-noise agreement across "
             f"{evidence['base_offset_rows']} base offset rows and {evidence['sensitivity_runs']} sensitivity runs "
             f"({evidence['sensitivity_offset_rows']} offset rows, including the base rows). The observed maximum absolute "
             f"difference is {number(evidence['maximum_absolute_difference_percent'])} %. No sampling allowance is applied. "
             f"Observed governing critical sections: {', '.join(evidence['governing_critical_sections'])}. Hang-off is an "
             "explicitly sampled endpoint. A sampled screen value lower than the comparator at an unsampled critical "
             "section understates that section's utilisation; the sampled screen can miss an exact critical section or "
             "govern at an interior sample. Agreement here does not exclude that limitation for other configurations.\n\n")
    missing = evidence["not_established_rows"]
    text += "Stress comparator not established rows: " + ("none at the stated tolerance.\n\n" if not missing else "\n\n")
    for row in missing:
        text += (f"- {row['case_id']}, {row['wall_step']}, {offset(row['offset_m'])}: not established; "
                 f"missing evidence: {row['comparison']['missing_evidence']}.\n")
    return text + ("\n" if missing else "")


def sensitivity_section(cases):
    """Show signed percentage changes and feasibility of each reselection."""
    text = ("## Size sensitivity\n\nGeneric nominal wall steps (assumed), not a vendor catalogue. "
            "Each wall step reruns the unchanged grid with recomputed submerged weight and 0.6 Sy Asteel tension limit. "
            "Each row uses the rank-1 candidate; a violated row remains infeasible. The reported metrics are the maximum "
            "top tension and utilisation and minimum bend radius over all four offsets. Percentages use the six-significant-figure "
            "metric values serialized in JSON, relative to the base wall. Candidate reselection can change geometry as well as loading.\n\n")
    for case in cases:
        text += f"### {case['case_id']}\n\n"
        rows = []
        for step in case["sensitivity"]:
            metrics, change = step["metrics"], step["change_percent"]
            rows.append([step["wall_step"], f"{step['wall_thickness_m']*1000:.3f}", number(step["submerged_weight_n_m"]),
                         number(step["max_top_tension_n"] / 1000),
                         number(metrics.get("top_tension_kn")), number(change.get("top_tension_kn")),
                         number(metrics.get("static_von_mises_utilisation")), number(change.get("static_von_mises_utilisation")),
                         number(metrics.get("minimum_bend_radius_m")), number(change.get("minimum_bend_radius_m")),
                         "feasible" if step["feasible"] else "not feasible"])
        text += table(["Wall step", "Wall (mm)", "Weight (N/m)", "Tension limit (kN)", "Top tension maximum over offsets (kN)", "ΔT (%)", "Utilisation maximum over offsets (1)", "ΔU (%)", "Radius minimum over offsets (m)", "ΔR (%)", "Disposition"], rows,
                      f"Table {case['case_id']}-4. Wall sensitivity against the base; full selected grid coordinates and individual constraint margins are retained in JSON.")
    return text


def reproduce_section(cases):
    """Give executable repository-relative commands without machine identity."""
    return """## What is not established

- Dynamic response is not established; wave/current time histories, vessel motion transfer functions and a qualified dynamic model are absent.
- Fatigue damage and life are not computed here; stress-cycle histories, S-N curves, stress concentration factors and a fatigue design basis are absent.
- VIV response is not established; current profiles, modal response and a qualified VIV model are absent.
- A code check to a named edition is not established; the governing code edition, load combinations, design factors, collapse/burst and combined-loading checks are absent. The nominal X65 property citation does not establish code compliance.
- A vendor line-pipe catalogue match is not established; vendor dimensions, tolerances, corrosion allowance, mill certificates and procurement specifications are absent.
- Installation, seabed/soil interaction, elastic extension and equipment qualification are not established; installation cases, soil data, axial-strain equilibrium and hardware capacities are absent.

## Reproduce appendix

Run from the repository root in the existing environment. No package synchronization or network access is required.

```bash
PYTHONPATH=src uv run --no-sync python scripts/riser_capability/build_scr_slwr_worked_configurations.py
PYTHONPATH=src uv run --no-sync python scripts/riser_capability/build_scr_slwr_worked_configurations.py --check
PYTHONPATH=src uv run --no-sync python -m pytest tests/riser_capability tests/riser_configuration_screen -q
uv run --no-sync python scripts/enforcement/check-engineering-register.py docs/domains/risers/worked-configurations/scr-slwr-worked-configurations.md
```

Each generated input also uses the existing router:

```bash
""" + "\n".join(f"PYTHONPATH=src uv run --no-sync python -m digitalmodel examples/workflows/riser-worked-configurations/{c['case_id']}.yml" for c in cases) + """
```

The router writes detailed JSON/CSV under `results/riser-worked-configurations/<case>/` and includes its own runtime metadata. The builder retains deterministic evidence without `runtime_s`; `--check` compares all six YAMLs, JSON, Markdown and six SVGs without writing them. Generated YAML changes are detected by `--check`; extend the builder's `CASES`, `SIZES` and `settings_for` to change the generated basis.

## Run-it-yourself prompt

> In this repository, read the engineering register and the SCR/SLWR worked-configuration builder. Rerun the builder, its --check mode, the riser capability and screen tests, and the register check using the commands above. Extend one generic case in the builder without changing existing source modules or loosening screening limits. Recompute weight and tension limits, retain violated candidates, compare independent equilibrium values, regenerate the report and SVGs, and name every missing qualification. Use no project data, network, commits or pushes.
"""


def render_report(data):
    """Render the report from the same unrounded screen records as the JSON."""
    cases = data["cases"]
    text = "# SCR and SLWR worked configurations — static screening tier\n\n## Summary\n\n"
    text += ("Six generic worked configurations are evaluated with the existing open-source static screen. "
             "Inputs are assumed for demonstration; no project data are used. All results are provisional under "
             "`worked-config-1`. Feasibility requires every applicable screening constraint to pass at every offset.\n\n")
    rows = []
    for case in cases:
        limiting = sorted((c for c in case["constraint_envelope"].values() if c["margin"] is not None),
                          key=lambda c: c["margin"] / max(abs(c["criterion"]["limit"]), 1e-12))
        evidence = [c for c in limiting if c["status"] == "violation"] or limiting[:1]
        descriptions = [f"{c['criterion']['name']}: computed {number(c['computed'])}, limit {c['criterion']['relation']} {number(c['criterion']['limit'])} {c['criterion']['unit']}, margin {number(c['margin'])} at {offset(c['offset_m'])}"
                        for c in evidence]
        rows.append([case["case_id"], f"{case['feasible_count']} / {case['grid_size']}",
                     "feasible under stated screening limits" if case["feasible"] else "not feasible under stated screening limits",
                     "; ".join(descriptions) or case["missing_evidence"]])
    text += table(["Case", "Feasible / swept", "Rank-1 disposition", "Governing criterion evidence"], rows,
                  "Table 1. Feasibility against the stated limits; a zero feasible count retains the rank-1 violated candidate without tuning limits.")
    text += ("## Scope and limitations\n\nThe method is an inextensible static cable screen with frictionless mobile touchdown. "
             "No dynamics, fatigue or code compliance is established. The screen's own limitations are quoted verbatim:\n\n> "
             + cases[0]["limitations"] + "\n\n"
             "The single pressure pair is internal design pressure 20.0e6 Pa and external pressure 0 Pa at surface hang-off. "
             "The screen applies it to the whole line; depth-dependent internal and external hydrostatics are not established. "
             "Minimum radius uses the screen's touchdown/sag/hog local radii. Utilisation uses sampled thin-wall von Mises "
             "stress including elastic curvature bending and pressure end force; it is not a code check.\n\n"
             "Rank 1 follows the screen's ascending maximum touchdown/sag excursion-curvature range among feasible candidates; "
             "if none is feasible, rank 1 follows the screen's violated-candidate ordering. The curvature-range objective is a "
             "stress-range proxy, not fatigue damage. A governing offset is selected by the smallest signed margin divided "
             "by the absolute limit (floor 1e-12); ties retain offset order. Each constraint also retains its own controlling "
             "offset, because one offset need not control every limit. All four offset rows and input hashes are in "
             "[scr-slwr-results.json](scr-slwr-results.json). Floats are serialized to six significant figures; comparator "
             "dispositions use unrounded values. Raw workflow runtime is discarded.\n\n"
             "SCR comparator: H = wz sinθ/(1 − sinθ), Ttop = H + wz, s = √(z² + 2Hz/w), "
             "x = (H/w) asinh(s/(H/w)), Rtd = H/w. θ is the selected input angle from vertical at zero offset; "
             "at other offsets it is the exposed top angle, so those checks are conditional on that angle. "
             "SLWR comparator: Vtop = w Sbare + w(1 − factor) Sbuoyed, Ttop = hypot(H, Vtop), "
             "Rsag = H/w and Rhog = H/|w(1 − factor)|. Bare suspended material is split before and after "
             "the exposed buoyed interval; the grounded tail is excluded. SLWR horizontal tension is a supplied comparator "
             "input; independent agreement of H and suspended geometry is not established by this equilibrium check.\n\n"
             )
    text += stress_scope(data) + "## Inputs and basis\n\n" + inputs_section(cases) + "## Results per case\n\n"
    for case in cases:
        coordinates = ", ".join(f"{key}={number(value)}" for key, value in case["configuration"].items())
        text += f"### {case['case_id']}\n\nSelected grid coordinates: {coordinates}.\n\n"
        text += f"![{case['case_id']} elevation schematic](figures/{case['case_id']}.svg)\n\n"
        text += f"Figure {case['case_id']}. Zero-offset elevation from screen shape points; the buoyed interval uses a dashed stroke. Uniform x/z scale; the grounded tail is excluded.\n\n"
        text += result_tables(case)
    return text + sensitivity_section(cases) + reproduce_section(cases)


def render_svg(case):
    """Draw the actual zero-offset material coordinates with equal x/z scales."""
    index = case["zero_offset_index"]
    title = case["case_id"]
    if index is None:
        return f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 900 120"><text x="20" y="60" fill="currentColor">{title}: shape not established</text></svg>\n'
    row = case["offset_results"][index]
    depth, span = case["inputs"]["water_depth_m"], row["horizontal_span_m"]
    scale = min(680 / span, 400 / depth)
    x0, bottom = 100., 480.
    project = lambda x, z: (x0 + scale*x, bottom - scale*z)
    points = lambda shape: " ".join(f"{project(x, z)[0]:.3f},{project(x, z)[1]:.3f}" for _, x, z in shape)
    top = bottom - scale*depth
    end = x0 + scale*span
    lines = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 900 610" role="img" aria-labelledby="title desc">',
             f'<title id="title">{title}: zero-offset elevation</title>',
             '<desc id="desc">Static riser from hang-off at sea surface to seabed touchdown. Dashed riser stroke denotes buoyed material.</desc>',
             '<g fill="currentColor" stroke="currentColor" font-family="sans-serif" font-size="17">',
             f'<text x="100" y="28" stroke="none">{title} — elevation above seabed (m)</text>',
             f'<path d="M80 {top:.3f} H820 M80 480 H820" fill="none" stroke-width="1.5"/>',
             f'<text x="600" y="{top+24:.3f}" stroke="none">Sea surface: {depth:.0f} m</text>',
             '<text x="600" y="505" stroke="none">Seabed: 0 m</text>',
             f'<polyline points="{points(row["shape"])}" fill="none" stroke-width="3"/>']
    start, length = row["buoyed_start_m"], row["buoyed_length_m"]
    if start is not None:
        buoyed = [p for p in row["shape"] if start-1e-7 <= p[0] <= start+length+1e-7]
        lines += [f'<polyline points="{points(buoyed)}" fill="none" stroke-width="8" stroke-dasharray="8 8"/>',
                  '<text x="380" y="555" stroke="none">Dashed: buoyed section</text>']
    for point, label, dx, dy in [(row["shape"][0], "Hang-off", 12, -12),
                                 (row["shape"][-1], "Touchdown", 12, -12)]:
        x, y = project(point[1], point[2])
        lines += [f'<circle cx="{x:.3f}" cy="{y:.3f}" r="4"/>',
                  f'<text x="{x+dx:.3f}" y="{y+dy:.3f}" stroke="none">{label}</text>']
    scale_m = 100 * max(1, math.floor(depth / 500))
    lines += [f'<path d="M100 540 h{scale_m*scale:.3f}" fill="none" stroke-width="3"/>',
              f'<text x="100" y="567" stroke="none">Scale: {scale_m} m; x/z equal</text>',
              f'<text x="100" y="600" stroke="none">Horizontal distance (m): 0 to {span:.1f}; suspended span</text>',
              f'<text x="{end-5:.3f}" y="527" stroke="none">{span:.1f} m</text>', '</g>', '</svg>']
    return "\n".join(lines) + "\n"
