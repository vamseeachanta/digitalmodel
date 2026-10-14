# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Deterministic finished analysis using the shared reporting renderer."""
from __future__ import annotations

import base64
from hashlib import sha256
from html import escape
from io import BytesIO
import json
from pathlib import Path
from typing import Any

import numpy as np
import yaml

from digitalmodel.reporting._renderer import ReportRenderer

TITLE = ("Mooring analysis: IEA 15 MW reference turbine on the VolturnUS-S "
         "semi-submersible, static offsets and quasi-static line tensions")
BASIS = "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml"
INPUT = "examples/workflows/fowt-volturnus-mooring/input.yml"
SOURCES = "docs/domains/orcaflex/wind/fowt-volturnus/SOURCES.md"
AXES = ("Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw")
CSS = """
body{font:16px/1.55 Georgia,serif;color:#18303b;background:#eef2f2;margin:0}
.container{max-width:1120px;margin:auto;background:white;padding:4rem}
h1{font-size:2.25rem;line-height:1.2;max-width:950px}h2{color:#135767;
 border-top:2px solid #aec7c9;padding-top:1rem;margin-top:3rem}
h3{font-size:1.15rem;color:#135767}p{max-width:95ch}table{border-collapse:collapse;
 width:100%;font:13px/1.4 system-ui,sans-serif;margin-top:1.5rem}
th,td{text-align:right;padding:.55rem;border-bottom:1px solid #d4dfe0}
th:first-child,td:first-child{text-align:left}th{background:#edf4f4}
.caption{font:13px system-ui,sans-serif;color:#526c73;margin:.6rem 0 1.5rem}
.cover{padding-bottom:2rem;border-bottom:5px solid #135767}.eyebrow{font:14px
 system-ui,sans-serif;text-transform:uppercase;letter-spacing:.12em}img{width:100%}
.note{border-left:4px solid #bd904d;padding-left:1rem} @media print{body{background:white}
.container{padding:0}h2{break-after:avoid}table,img{break-inside:avoid}}
"""


def number(value: Any) -> str:
    """Format finite engineering values to at most four significant figures."""
    value = float(value)
    if not np.isfinite(value):
        raise ValueError("A finite report value is required")
    return "0" if value == 0 else f"{value:.4g}"


def delta(value: float, reference: float) -> str:
    """Format a signed comparison against its named reference to 0.1 percent."""
    return f"{100*(value/reference-1):+.1f} %"


def table(headers: tuple, rows: list, caption: str) -> str:
    """Render an escaped table with its caption below the table."""
    head = "".join(f"<th>{escape(str(h))}</th>" for h in headers)
    body = "".join("<tr>"+"".join(f"<td>{escape(str(v))}</td>" for v in row)+"</tr>" for row in rows)
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table><p class='caption'>{escape(caption)}</p>"


def section(index: int, title: str, body: str) -> str:
    """Render one numbered report section."""
    return f"<section id='section-{index}'><h2>{index} {title}</h2>{body}</section>"


def _plots(results: dict, phase: bool = False) -> str:
    """Embed deterministic six-degree amplitude and phase figures in memory."""
    import matplotlib
    matplotlib.use("Agg")
    from matplotlib.figure import Figure
    from matplotlib.backends.backend_agg import FigureCanvasAgg
    omega = np.asarray(results["omega"])
    mask = (omega >= 0.05) & (omega <= 2.0)
    figures = []
    quantities = (("rao_phase", "Phase [rad]"),) if phase else (("rao_amplitude", "Amplitude"),)
    for key, label in quantities:
        values = np.asarray(results[key], dtype=float).copy()
        if phase:
            amplitude = np.asarray(results["rao_amplitude"])
            values[amplitude < 1e-6*np.max(amplitude)] = np.nan
        figure = Figure(figsize=(11, 9), layout="constrained")
        FigureCanvasAgg(figure)
        for axis, plot in enumerate(figure.subplots(3, 2).flat):
            for index, heading in enumerate((0, 30, 60)):
                plot.plot(omega[mask], values[mask, index, axis], label=f"{heading}°")
            unit = "m/m" if axis < 3 else "rad/m"
            plot.set(title=AXES[axis], xlabel="Frequency [rad/s]",
                     ylabel=f"{label} [{unit}]" if key == "rao_amplitude" else label)
            plot.grid(alpha=0.25)
            plot.legend(fontsize=8)
            plot.ticklabel_format(style="sci", axis="y", scilimits=(-3, 4))
        output = BytesIO()
        figure.savefig(output, format="png", dpi=125, metadata={"Software": "Matplotlib"})
        encoded = base64.b64encode(output.getvalue()).decode("ascii")
        figures.append(f"<img alt='Six degree response {label.lower()}' src='data:image/png;base64,{encoded}'><p class='caption'>Six-degree response {label.lower()}, adopted inertia and 5 % additional damping; wave elevation amplitude is the input.</p>")
        if phase:
            figures.append("<p>Phase is omitted where response amplitude is below 1e-6 of the global peak amplitude across modes, headings and frequencies. Phase of a numerically zero response is undefined; this is a display criterion only.</p>")
        figure.clear()
    return "".join(figures)


def _design_data(basis: dict, results: dict) -> str:
    """State the sourced design basis and distinguish every applied assumption."""
    env, geometry, mooring = basis["environment"], basis["geometry"], basis["mooring"]
    rows = []
    for name in ("operating", "extreme"):
        sea = env[name]
        rows.append((name.title(), *(number(sea[k]["value"]) for k in
                    ("significant_wave_height", "peak_period", "jonswap_gamma", "wind_speed"))))
    seas = table(("Sea", "Hs [m]", "Tp [s]", "γ [–]", "Wind [m/s]"), rows,
                 "Table 1. Published environment, Allen et al. (2020), Table 12, printed page 27; operating DLC 1.1 at 12 m/s and extreme DLC 6.1.")
    dimensions = table(("Quantity", "Value", "Unit"), [
        (key.replace("_", " ").title(), number(geometry[key]["value"]), geometry[key]["unit"])
        for key in ("central_column_diameter", "outer_column_diameter", "outer_column_radius", "draft", "freeboard", "pontoon_width", "pontoon_height")],
        "Table 2. Public reference dimensions. Three radial rectangular pontoons join one central and three outer columns; internal overlap is removed.")
    line_data = table(("Quantity", "Value", "Unit"), [
        (key.replace("_", " ").title(), str(mooring[key]["value"]) if key == "line_type" else number(mooring[key]["value"]), mooring[key]["unit"])
        for key in ("line_count", "line_type", "nominal_diameter", "mass_per_length", "axial_stiffness", "minimum_breaking_load", "unstretched_length")],
        "Table 3. Mooring properties, Allen et al. (2020), Table 6, printed page 13.")
    coordinates = table(("Line", "Fairlead x,y,z [m]", "Anchor x,y,z [m]"), [
        (index+1, ", ".join(map(number, fairlead)), ", ".join(map(number, anchor)))
        for index, (fairlead, anchor) in enumerate(zip(mooring["fairleads"]["value"], mooring["anchors"]["value"]))],
        "Table 4. Fixed reference coordinates at still water level; x lies in the column and line symmetry plane, z is positive upward.")
    turbine = basis["turbine"]
    return (f"<h3>3.1 Site data</h3><p>Water depth is {number(env['water_depth']['value'])} m. The environment is a public reference envelope without a geographical site.</p>"
            f"<h3>3.2 Structure data: turbine</h3><p>The rated reference turbine is 15 MW. Rated horizontal thrust is {number(turbine['rated_rotor_thrust']['value']/1e6)} MN at hub height {number(turbine['hub_height']['value'])} m above still water. Aggregated mass is {number(results['system_mass']/1e6)} million kg; centre of gravity is ({', '.join(map(number, results['system_cg']))}) m.</p>"
            "<h3>3.3 Environmental data</h3>"+seas+
            "<p>Mean current drag is not included. Rated thrust is applied to both seas as an envelope; the extreme combination does not represent parked aerodynamic loading.</p>"
            "<h3>3.4 Vessel data: platform and mooring</h3>"+dimensions+line_data+coordinates+
            "<h3>3.5 Criteria</h3><p>Utilisation is tension multiplied by a factor and divided by minimum breaking load; the limit is 1.0. Intact factor 2 and damaged factor 1.43 are assumed, based on previous experience. No factor is attributed to a standard. No offset limit is established; tension utilisation alone does not establish stationkeeping acceptance.</p>"
            "<h3>3.6 Method</h3><p>The method and assumptions are defined in Section 4.</p>"
            "<h3>3.7 Load cases</h3><p>Three headings × two sea states × two configurations give 12 cases. Headings 0°, 30° and 60° span half the 120° sector of the three-line geometry. RNA orientation remains on the reference axis while the applied horizontal load and wave headings vary. An all-heading response envelope of the asymmetric turbine–platform system is not established. The damaged configuration removes the most loaded intact line for each sea and heading.</p>")


def _validation(basis: dict, results: dict) -> str:
    """Compare computed hydrostatics and modal periods with published quantities."""
    hydro, published = results["hydrostatics"], basis["published"]
    rows: list[tuple] = [("Displacement [m³]", number(hydro["volume"]), number(published["displacement"]["value"]), delta(hydro["volume"], published["displacement"]["value"])),
            ("Waterplane area [m²]", number(hydro["waterplane_area"]), number(published["waterplane_area"]["value"]), delta(hydro["waterplane_area"], published["waterplane_area"]["value"]))]
    if results.get("hydrostatic_validation"):
        rows.extend((f"{r['quantity']} [{r['unit']}]", number(r["computed"]),
                     number(r["published"]), delta(r["computed"], r["published"]))
                    for r in results["hydrostatic_validation"])
    else:
        rows.append(("Heave stiffness [N/m]", number(hydro["stiffness"][2][2]),
                     number(published["hydrostatic_stiffness"]["value"][2]),
                     delta(hydro["stiffness"][2][2], published["hydrostatic_stiffness"]["value"][2])))
    output = table(("Quantity", "Computed", "Published", "Difference"), rows,
                   "Table 5. Hydrostatic validation against Allen et al. (2020), Tables 2 and 3. Published waterplane area is derived from heave stiffness divided by water density and gravity. Rotational validation uses geometry-only stiffness, with mass–gravity–CG restoring removed and both quantities expressed about the same origin.")
    coarse = results["coarse_hydrostatics"]
    output += f"<p>Mesh volume differs from the analytical primitive-union volume by {delta(hydro['volume'], results['analytical_volume'])}. The coarse surface has {coarse['panels']} panels at nominal spacing {number(coarse['panel_size'])} m; the fine surface has {hydro['panels']} panels at {number(hydro['panel_size'])} m. Maximum hull-panel radii are {number(coarse['maximum_panel_radius'])} m and {number(hydro['maximum_panel_radius'])} m, respectively; the fine lid maximum is {number(hydro['maximum_lid_panel_radius'])} m.</p>"
    output += table(("Axis", "Total hydrostatic stiffness [SI]", "Mooring stiffness [SI]"), [
        (AXES[i], number(hydro["stiffness"][i][i]), number(results["mooring_stiffness"][i][i]))
        for i in range(6)],
        "Computed restoring stiffness diagonal at the zero-offset intact condition; translations use N/m and rotations N m/rad. Rotational hydrostatic terms include current-system gravitational restoring; the published Table 3 terms exclude gravitational restoring and are not directly equivalent.")
    output += table(("Inertia reading", "Ixx [kg m²]", "Iyy [kg m²]", "Izz [kg m²]", "Ixy [kg m²]", "Ixz [kg m²]", "Iyz [kg m²]"), [
        ("CG" if key == "at_cg" else "Tower top (assumed)",
         *(number(basis["system"]["inertia_readings"][key]["value"][i][j])
           for i, j in ((0, 0), (1, 1), (2, 2), (0, 1), (0, 2), (1, 2))))
        for key in ("at_cg", "at_tower_top")],
        "Full-system inertia tensor about the still-water origin before reflection projection, aggregated from public component properties under both RNA origin interpretations.")
    rows = [(AXES[i], number(published["natural_periods"]["value"][i]), number(results["periods"]["at_tower_top"][i]), delta(results["periods"]["at_tower_top"][i], published["natural_periods"]["value"][i]), number(results["periods"]["at_cg"][i]), delta(results["periods"]["at_cg"][i], results["periods"]["at_tower_top"][i])) for i in range(6)]
    output += table(("Mode", "Published [s]", "Adopted [s]", "Validation difference", "Other reading [s]", "Reading effect"), rows,
                    "Table 6. Natural-period validation against Allen et al. (2020), Table 11, printed page 21, with inertia interpretation sensitivity. The published periods are reciprocals of tabulated frequencies.")
    output += table(("Quantity", "Peak ω [rad/s]", "Heading [°]", "Fine value [SI]", "Absolute difference"), [(r["quantity"], number(r["omega"]), number(r["heading"]) if "heading" in r else "heading-independent", number(r["fine"]), f"{r['difference_percent']:.1f} %") for r in results["convergence"]],
                    "Table 7. Two-resolution comparison at the fine-resolution peak; added mass is in kg or kg m², excitation per unit wave amplitude in N/m or N m/m. Heading identifies the excitation peak.")
    if results.get("resolution_warning"):
        output += f"<p class='note'>{escape(results['resolution_warning'])}</p>"
    return output


def _case_tables(results: dict) -> str:
    """Render the complete horizontal envelope and every line result."""
    motions, lines, reactions = [], [], []
    for case in results["cases"]:
        name = _case_name(case)
        stats = case["statistics"]
        motions.append((name, *(number(case["mean"][i]) for i in (0, 1)),
                        *(number(s["significant"]) for s in stats), *(number(s["maximum"]) for s in stats),
                        *(number(case["design"][i]) for i in (0, 1))))
        for line in case["lines"]:
            lines.append((name, line["line"], number(line["tension"]/1e6), number(line["utilisation"]), number(line["grounded_length"]), number(line["anchor_angle"])))
        reactions.append((name, *(number(x) for x in case["constraint_reaction"]), number(case["mean_mooring_vertical"])))
    return (table(("Case", "Mean x [m]", "Mean y [m]", "Significant x [m]", "Significant y [m]", "MPM x [m]", "MPM y [m]", "Design x [m]", "Design y [m]"), motions,
                  "Table 8. Horizontal offsets; significant values are single amplitudes 2√m₀. MPM denotes the three-hour most probable maximum. Design coordinates identify the governing four-sign combination.")+
            table(("Case", "Line", "Tension [MN]", "Utilisation [–]", "Grounded length [m]", "Anchor angle [°]"), lines,
                  "Table 9. All surviving line results at the governing design offset. Anchor angle is the upward angle of the line force at the anchor.")+
            table(("Case", "Heave reaction [N]", "Roll reaction [N m]", "Pitch reaction [N m]", "Yaw reaction [N m]", "Mooring vertical [N]"), reactions,
                  "Table 10. Mean reactions required to hold heave and attitude fixed; these external constraints are assumptions, not free-floating equilibrium results."))


def _case_name(case: dict) -> str:
    """Identify the heading, sea, configuration and removed line explicitly."""
    name = f"{number(case['heading'])}° {case['sea']} {case['state']}"
    if case["state"] == "damaged":
        name += f" (line {case['removed_line']} removed)"
    return name


def _sensitivity(results: dict) -> str:
    """Quantify inertia and damping effects without asserting uncomputed acceptance."""
    rows = [("CG" if r["reading"] == "at_cg" else "Tower top (assumed)", number(100*r["damping"]), number(r["surge_peak"]), number(r["heave_peak"]), number(r["pitch_peak"])) for r in results["sensitivity"]]
    output = table(("Tensor interpretation", "Extra damping [%]", "Peak surge [m/m]", "Peak heave [m/m]", "Peak pitch [rad/m]"), rows,
                   "Table 11. Response sensitivity to both published inertia readings and additional damping.")
    adopted = next(r for r in results["sensitivity"] if r["reading"] == "at_tower_top" and r["damping"] == 0.05)
    other = next(r for r in results["sensitivity"] if r["reading"] == "at_cg" and r["damping"] == 0.05)
    output += "<p>At 5 % additional damping, the other inertia reading changes peak surge, heave and pitch response by "+", ".join(delta(other[k], adopted[k]) for k in ("surge_peak", "heave_peak", "pitch_peak"))+", respectively. These are global peaks over 0.05–2 rad/s and the evaluated headings.</p>"
    inertia_cases = results.get("inertia_case_sensitivity")
    if inertia_cases:
        output += table(("Configuration", "Adopted utilisation [–]", "Other reading [–]", "Reading effect"), [
            (state.title(), number(inertia_cases["adopted"][state]), number(inertia_cases["alternative"][state]),
             delta(inertia_cases["alternative"][state], inertia_cases["adopted"][state]))
            for state in ("intact", "damaged")],
            "Inertia sensitivity of governing line utilisation, with identical final line-length configurations.")
        conclusion = ("The utilisation conclusion relative to 1.0 depends on the inertia reading."
                      if inertia_cases["acceptance_changes"] else
                      "Both inertia readings give the same utilisation disposition relative to 1.0 for the evaluated final configurations.")
        output += f"<p>{conclusion} The source inconsistency remains unresolved.</p>"
    else:
        output += "<p>Line-tension acceptance is conditional on the adopted inertia reading. The peak-response comparison alone does not establish whether each line-utilisation conclusion is invariant to the inertia interpretation.</p>"
    projection = results.get("projection_sensitivity")
    if projection:
        output += "<p>Retaining the unprojected mass properties changes peak surge, heave and pitch responses relative to the reflection-symmetric calculation by "+", ".join(f"{projection[k]:+.1f} %" for k in ("surge_peak_difference_percent", "heave_peak_difference_percent", "pitch_peak_difference_percent"))+", respectively. This quantifies the symmetry approximation rather than establishing exact physical symmetry.</p>"
    return output


def _remedies(results: dict) -> str:
    """Require and report a recomputed solution for every over-limit case."""
    rows = []
    for case in results["cases"]:
        if case["governing"]["utilisation"] <= 1:
            continue
        solution = case.get("solution")
        if not solution or solution["governing"]["utilisation"] > 1:
            raise ValueError("Every over-limit case requires a recomputed solution within unity")
        rows.append((_case_name(case), number(case["governing"]["utilisation"]), number(solution["length_change"]), number(solution["governing"]["tension"]/1e6), number(solution["governing"]["utilisation"])))
    if not rows:
        return "<p>No evaluated utilisation exceeds 1.0 under the stated factors and assumptions.</p>"
    return table(("Case", "Original utilisation [–]", "Line length change [m]", "Recomputed tension [MN]", "Recomputed utilisation [–]"), rows,
                 "Table 12. Recomputed line-length solutions for each original over-limit case; further coupled assessment remains necessary.")


def _contents() -> str:
    """Provide document navigation without adding a numbered main section."""
    titles = ("Executive Summary", "Introduction", "Design Data", "Analysis Methodology",
              "Detailed Results", "Key Findings", "Limitations", "Recommendations",
              "References", "Acronyms and Symbols")
    links = "".join(f"<li><a href='#section-{i}'>{i} {title}</a></li>"
                    for i, title in enumerate(titles, 1))
    return ("<nav aria-label='Contents'><h3>Contents</h3><ol style='list-style:none'>"+
            links+"<li><a href='#appendices'>Appendices</a></li></ol></nav>")


def _findings(results: dict, intact: dict, damaged: dict) -> str:
    """Bind numbered findings to their governing cases and stated criteria."""
    output = "<h3>6.1 Governing line demand</h3>"
    for case in (intact, damaged):
        output += (f"<p>For {escape(_case_name(case))}, line {case['governing']['line']} "
                   f"governs at utilisation {number(case['governing']['utilisation'])} "
                   "against the limit 1.0 under the factors in Section 3.5.</p>")
    output += ("<p>No offset limit is established; tension utilisation alone does not "
               "establish stationkeeping acceptance.</p>"
               "<h3>6.2 Validation and applicability</h3><p>Hydrostatic and natural-period "
               "comparisons in Tables 5–6 are the validation of this model; the stated "
               "differences are retained rather than treated as equivalence. Static offsets "
               "in Allen et al. Section 3.1 are not tabulated and are not compared.</p>"
               "<h3>6.3 Published inertia inconsistency</h3><p>The public RNA tensor "
               "cells F24:K24 and L24:Q24 disagree with the parallel-axis identity by "
               "27.2 %, 34.2 % and 35.9 % on the three diagonals. The tower-top reading "
               "is adopted as assumed because it gives the smaller identity residual; "
               "the inconsistency remains unresolved. Appendix A quantifies the effects "
               "of both readings and of additional damping.</p>"
               f"<h3>6.4 Symmetry approximation</h3><p>The 0° sway, roll and yaw response "
               f"ratio is {number(results['symmetry_ratio'])} relative to peak response "
               "under reflection projection. This ratio concerns the assumed symmetric "
               "model; it does not establish exact symmetry of the published mass tensor.</p>")
    return output+_excursion_finding(results)+_remedies(results)


def _excursion_finding(results: dict) -> str:
    """Identify the large damaged excursion and its unresolved design criterion."""
    governing = {}
    for state in ("intact", "damaged"):
        case = max((c for c in results["cases"] if c["state"] == state),
                   key=lambda c: np.linalg.norm(c["design"][:2]))
        governing[state] = (case, float(np.linalg.norm(case["design"][:2])))
    case, radius = governing["damaged"]
    intact = governing["intact"][1]
    return ("<h3>6.5 Large damaged excursion</h3>"
            f"<p>The greatest damaged design excursion is {number(radius)} m "
            f"for {escape(_case_name(case))}, compared with {number(intact)} m "
            f"for the greatest intact excursion, a ratio of {radius/intact:.1f}. "
            "The calculated two-line equilibrium lies at a large displacement. "
            "A mooring-layout and allowable-excursion assessment is required before "
            "stationkeeping acceptance; no allowable-excursion criterion is established.</p>")


def validate_cases(cases: list[dict]) -> None:
    """Require each prescribed heading, sea and configuration exactly once."""
    expected = {(heading, sea, state) for heading in (0, 30, 60)
                for sea in ("operating", "extreme")
                for state in ("intact", "damaged")}
    try:
        combinations = {(case["heading"], case["sea"], case["state"])
                        for case in cases}
    except (KeyError, TypeError) as error:
        raise ValueError("Twelve unique prescribed case combinations are required") from error
    if len(cases) != 12 or combinations != expected:
        raise ValueError("Twelve unique prescribed case combinations are required")


def build_report(root: Path, results: dict, output_dir: Path) -> tuple[Path, Path]:
    """Build a deterministic HTML analysis and its byte-bound input manifest."""
    root, output_dir = Path(root), Path(output_dir)
    basis = yaml.safe_load((root/BASIS).read_text())
    validate_cases(results["cases"])
    sections = _document_sections(basis, results)
    notice = (root/SOURCES).read_text().split("## Publisher data notice\n", 1)[1].split("\n## ", 1)[0].strip()
    sections[-1] = sections[-1].removesuffix("</section>")+(
        "<h3 id='appendix-c'>Appendix C. Public data terms</h3>"
        "<pre style='white-space:pre-wrap;font:13px/1.5 Georgia,serif'>"
        +escape(notice)+"</pre></section>")
    renderer = ReportRenderer()
    html = renderer.render_html(sections, title=TITLE, css=CSS).rstrip()+"\n"
    output = renderer.write_html(html, output_dir/"fowt-volturnus-mooring-analysis.html")
    manifest = {"html_sha256": sha256(output.read_bytes()).hexdigest(),
                "input_digests": {name: sha256((root/name).read_bytes()).hexdigest() for name in (BASIS, INPUT, SOURCES)},
                "results": results}
    manifest_path = output.with_suffix(".manifest.json")
    manifest_path.write_text(json.dumps(manifest, sort_keys=True, indent=2, allow_nan=False)+"\n")
    return output, manifest_path


def _methodology(results: dict) -> str:
    """Describe the numerical method and explicitly adopted assumptions."""
    methodology: str = ("<p>First-order added mass, radiation damping and excitation are calculated with Capytaine "+escape(results["versions"]["Capytaine"])+". Elastic catenary equilibrium, tangent stiffness and line tensions are calculated with MoorPy "+escape(results["versions"]["MoorPy"])+". Only horizontal mean equilibrium is solved. Mean heave and attitude remain fixed at the reference draft; reaction forces and moments are reported in Table 10.</p>"
        "<p>The submerged boundary is the union of circular columns and rectangular radial pontoons with an irregular-frequency lid. Two panel resolutions are evaluated. Natural periods solve det[K−ω²(M+A(ω))]=0 with six-degree coupling and zero-offset mooring stiffness for the intact system. Radiation damping is supplemented in surge, heave and pitch by 5 % critical damping, assumed, based on previous experience; 2 % and 10 % sensitivities are carried. The additional coefficient is Bextraᵢ(ω)=2ζ√[(Mᵢᵢ+Aᵢᵢ(ω))Kᵢᵢ]; its frequency dependence is retained, and the response remains linear in motion amplitude.</p>"
        "<p>The response is obtained from [K−ω²(M+A)−iω(B+Bextra)]q=F over 0.05–2 rad/s, consistent with the exp(−iωt) excitation convention. The intact system uses zero-offset stiffness; the damaged system uses stiffness at its constrained mean equilibrium. Aligned load and wave headings are assumed. The xz plane is a reflection plane; lateral mass-centre offset and inertia cross-products that break this reflection are projected to zero, an assumed symmetry approximation.</p>"
        "<p>For response spectral density |RAO|²S, m₀ and m₂ are frequency moments. Significant single amplitude is 2√m₀; the Rayleigh three-hour maximum is √[2m₀ ln N], where N=T/(2π√(m₀/m₂)), following Longuet-Higgins (1952). Each horizontal maximum is added independently with both signs to the static mean, and the largest tension of four combinations is retained. This envelope is an assumption about concurrent horizontal excursions; the two maxima need not occur simultaneously. Elastic catenary relations follow Irvine (1981). Seabed friction is assumed zero; anchor holding capacity is not established by line-strength utilisation.</p>"
        "<p>The published RNA tensors do not satisfy Iref=ICG+m[(r·r)I−rrᵀ]. Cells F24:K24 and L24:Q24 in the public tabular data give diagonal discrepancies of 27.2 %, 34.2 % and 35.9 %. Treating F24:K24 as about the RNA centre of gravity or about tower top produces the two analysed readings. The tower-top reading is adopted as assumed; exchanging the tensor labels reduces the full-tensor relative residual from 31.7 % to 23.1 %. Retaining F24:K24 as the trusted endpoint is a further assumption; neither reading resolves the published inconsistency. Published RNA mass and centre of gravity are retained separately.</p>")
    return methodology


def _references() -> str:
    """Provide public source citations and the required data credit."""
    references = ("<p>Allen, C. et al. (2020). Definition of the UMaine VolturnUS-S Reference Platform Developed for the IEA Wind 15-Megawatt Offshore Reference Wind Turbine. NREL/TP-5000-76773. Tables 1–3, 6, 11 and 12; Section 3.1.</p>"
        "<p>Gaertner, E. et al. (2020). IEA Wind TCP Task 37: Definition of the IEA 15-Megawatt Offshore Reference Wind Turbine. NREL/TP-5000-75698.</p>"
        "<p>IEA-15-240-RWT public reference inputs, Apache-2.0; the source attribution and exact revision accompany the numerical design basis. The U.S. Department of Energy, National Laboratory of the Rockies, and ALLIANCE are credited for the published reference data. Reference-design contributors are credited for the openly licensed inputs.</p>"
        "<p>Capytaine and MoorPy: open-source boundary-element and quasi-static mooring formulations, versions identified in Section 4.</p>"
        "<p>Longuet-Higgins, M. S. (1952). On the statistical distribution of the height of sea waves. JMR 11, 245–266. Irvine (1981). Cable Structures; elastic catenary formulation.</p>"
        "<p>API RP 2SK (2005), Design and Analysis of Stationkeeping Systems. DNVGL-OS-E301 (2018), Position Mooring, the cited edition of DNV-OS-E301. Clause titles are not established from the accessible source evidence. These are method references only; no clause content or tabulated factor is reproduced or attributed to them.</p>")
    return references


def _document_sections(basis: dict, results: dict) -> list[str]:
    """Assemble the ordered report sections from validated results."""
    remedies = _remedies(results)
    intact = max((c for c in results["cases"] if c["state"] == "intact"), key=lambda c: c["governing"]["utilisation"])
    damaged = max((c for c in results["cases"] if c["state"] == "damaged"), key=lambda c: c["governing"]["utilisation"])
    summary = "<p>"
    for case in (intact, damaged):
        summary += (f"The governing {case['state']} design offset is ({number(case['design'][0])}, {number(case['design'][1])}) m, with line utilisation {number(case['governing']['utilisation'])} at {number(case['heading'])}° in the {case['sea']} sea; ")
    summary = summary.rstrip("; ")+". Coupled dynamics and mean wave drift are not included. No allowable excursion or anchor holding criterion is established; tension strength alone does not establish stationkeeping acceptance.</p>"+remedies
    cover = f"<header class='cover'><p class='eyebrow'>AceEngineer Inc. · Floating wind and mooring</p><h1>{TITLE}</h1><p>14 October 2026 · Revision A</p></header>"
    methodology = _methodology(results)
    references = _references()
    sections = [
        cover+_contents(),
        section(1, "Executive Summary", summary),
        section(2, "Introduction", "<p>This analysis quantifies first-order motions, constrained static offsets and quasi-static mooring demand of the public IEA 15 MW reference turbine on the VolturnUS-S semi-submersible. It supports selection of subsequent coupled assessments; it does not establish a site-specific design.</p>"),
        section(3, "Design Data", _design_data(basis, results)),
        section(4, "Analysis Methodology", methodology),
        section(5, "Detailed Results", _validation(basis, results)+_plots(results)+_case_tables(results)+remedies+"<p>Additional damping and inertia sensitivities are in <a href='#appendix-a'>Appendix A</a>; phase response is in <a href='#appendix-b'>Appendix B</a>.</p>"),
        section(6, "Key Findings", _findings(results, intact, damaged)),
        section(7, "Limitations", "<p>The calculation is quasi-static, at one draft, for a public reference platform with no site. Mean drift, low-frequency response, line dynamics, coupled aero-hydro-servo dynamics, aerodynamic damping and control are absent. Additional linear damping, symmetry projection, aligned headings, independently combined horizontal maxima and fixed mean attitude are assumed. The required constraint reactions are external to the free-floating system. The extreme sea combined with rated thrust is an envelope, not a parked load case. Inertia ambiguity and period-validation differences limit quantitative confidence; line acceptance remains conditional on that basis. No offset limit is established; tension utilisation alone does not establish stationkeeping acceptance. Linearisation and the evaluated frequency band cannot represent every response outside this model.</p>"),
        section(8, "Recommendations", "<p>The next campaign should include coupled time-domain statics, six free decays, and operating and parked cases using a licensed coupled solver. It requires the controller source, a load-case table with source attribution and an execution manifest. Mean drift should be obtained from a second-order solution and evaluated with low-frequency response. A site-specific environment, resolved RNA inertia origin, free mean attitude, line dynamics and a physical pretension/design assessment are required before design acceptance.</p>"),
        section(9, "References", references),
        section(10, "Acronyms and Symbols", "<p>RNA: rotor-nacelle assembly. CG: centre of gravity. RAO: response amplitude operator. MPM: most probable maximum. DLC: design load case. Hs: significant wave height. Tp: peak period. γ: JONSWAP peak-enhancement factor. ω: angular frequency. M: rigid mass matrix. A: added mass. B: radiation damping. K: hydrostatic plus mooring stiffness. m₀ and m₂: response spectral moments. SWL: still water level.</p>"),
        "<section id='appendices'><h2>Appendices</h2><h3 id='appendix-a'>Appendix A. Inertia and damping sensitivities</h3>"+_sensitivity(results)+"<h3 id='appendix-b'>Appendix B. Phase response</h3>"+_plots(results, phase=True)+"</section>",
    ]
    return sections
