"""Emit the human-facing spike note from the committed compact result artifact."""
import argparse
import html
import json
from pathlib import Path


def table(headers, rows, caption):
    header = "".join(f"<th>{html.escape(str(x))}</th>" for x in headers)
    body = "".join("<tr>"+"".join(f"<td>{html.escape(str(x))}</td>" for x in row)+"</tr>"
                   for row in rows)
    return f"<table><thead><tr>{header}</tr></thead><tbody>{body}</tbody></table><p>{caption}</p>"


def method_text(data):
    b = data["benchmark"]
    return f'''<h2>Benchmark and method</h2>
<p>The fixed flaw has a = c = {b['a_mm']:g} mm, t = {b['t_mm']:g} mm,
plate full width = {2*b['width_mm']:g} mm and full length = {2*data['half_length_mm']:g} mm.
Thus a/t = {b['a_mm']/b['t_mm']:g}, a/c = {b['a_mm']/b['c_mm']:g},
c/b = {b['c_mm']/b['width_mm']:g}, full width/c = {2*b['width_mm']/b['c_mm']:g},
and full length/c = {2*data['half_length_mm']/b['c_mm']:g}.
Remote membrane stress is {b['sigma_m_mpa']:g} MPa, with no bending. The synthetic,
isotropic linear-elastic material has E = {data['material']['e_mpa']:g} MPa and
nu = {data['material']['nu']:g}; no yield or toughness assumption is used.</p>
<p>The semicircle is a member of the semi-elliptical family. It makes the radial
sampling rays normal to the front and avoids a geometric interpretation parameter.
The in-repo Newman–Raju implementation requires positive a/c no greater than unity,
a below t and c/b below its stated bound; this benchmark is within those bounds.
The public NASA memorandum supports this shape and finite-thickness regime.
The benchmark was selected before solver results and was unchanged afterwards.</p>
<p>Dimensional seed inputs a, b and remote stress come from the public synthetic
Newman–Raju tests (a = 5, c = 25, stress = 100 in their respective cases),
with t = 2a and the benchmark c = a. E and nu come from the synthetic STEEL
fixture in the CalculiX tests. The refinement seed is the six beam DOFs in those
tests, doubled successively. These are numerical design choices derived from
repository fixtures, not measured material or geometry. File digests and the
source revision are in the JSON artifact; repository terms are MIT License.</p>''' + mesh_text() + field_text()


def mesh_text():
    return '''<p>A quarter plate is meshed explicitly, without installing or invoking a mesher.
The x-z cross-section uses polar layers ending on a rectangle, with the rectangle
corner angle included. Crack-front nodes lie on r = a; faces between them are
chords, whose angular resolution is refined. The y-direction is extruded to the
loaded end. The radial coordinates inside the flaw are a[1-(1-i/n)^2]; outside
the flaw they are a+(R(phi)-a)(j/n)^2, where R terminates at the plate perimeter.
Extrusion coordinates are L(j/n)^2. Linear fully integrated C3D8 elements surround
the front; C3D6 wedges close the polar centre. No quarter-point elements are used.</p>
<p>The x = 0 symmetry plane has ux = 0. The intact part of y = 0 has uy = 0;
the crack face is traction-free. One uz anchor at the loaded-end origin removes
rigid translation. The surface and back face remain traction-free. Negative P2
pressure imposes tensile loading at the extrusion end, following manual section
7.43 face numbering. Native summed y reactions are compared with stress times
the quarter-plate end area. Tests check mesh orientation, rectangular area,
front radii, crack opening and intact-face closure.</p>'''


def field_text():
    return '''<p>CalculiX 2.21 manual section 6.9.27 (pp. 368–372), section 10.14 and the
*CRACK PROPAGATION entry describe an empirical growth route that interpolates
uncracked stresses and applies shape factors. That route is not an independently
solved cracked-body contour integral. No built-in contour-integral route usable
for this benchmark was established by the manual inspection. Its empirical
K route is not used to validate another empirical K solution.</p>
<p>The cracked-body FRD nodal stress syy is read through the unchanged repository
parser. Roylance's public Westergaard opening-mode field (p. 7, Eq. 4) reduces on
theta = 0 to sigma_yy = K/sqrt(2 pi r). Consequently apparent K is
sigma_yy sqrt(2 pi r/1000) when r is in mm and K is in MPa sqrt(m).
The first three exterior-ring nodes at each front station are sampled. A least
squares line apparent K = K0 + slope*r supplies the intercept. The linear fit is
a numerical spike choice, not a textbook guarantee of 3D extraction accuracy.
The individual samples and their spread are retained. The cited leading-field
range r no greater than 0.1a is flagged separately for each station. The synthetic
test generates the leading stress field from a known K and also checks a derived
linear apparent-K correction; it does not establish FE accuracy.</p>
<p>Reference values at every sampled parametric angle are obtained by calling
newman_raju_k from the unchanged in-repo source file. Direct file loading avoids
eager imports of unrelated assessment modules; the wrapper test compares against
an independent import of the unchanged file. No coefficients or results are transcribed from a paid
fitness-for-service standard.</p>'''


def sources_text():
    return '''<h2>Sources and reuse terms</h2>
<p>Retrieval date for the following sources: 2026-10-11 UTC. HTTP requests were
sequential, bounded by a timeout, without automatic retries and with a repository
URL in the User-Agent. Same-site requests were separated by more than two seconds.
No fetcher is part of the committed scripts; all tests and regeneration are offline.</p>
<ul>
<li><a href="https://www.dhondt.de/ccx_2.21.pdf">CalculiX CrunchiX manual 2.21</a>,
sections 6.2.1/6.2.9 (elements), 6.9.27, 7.43 and 10.14. The
<a href="https://www.dhondt.de/">project site</a> links the
<a href="https://www.dhondt.de/gpl-2.0.txt">GNU GENERAL PUBLIC LICENSE Version 2</a>.
The installed package also identifies GPL-2. A distinct manual-content licence
is terms not stated; no manual numerical engineering constants are reused.
Feature descriptions and section citations are recorded.</li>
<li>David Roylance,
<a href="https://ocw.mit.edu/courses/3-11-mechanics-of-materials-fall-1999/e5bca953e1dbd0cd0376f0f77033dc0d_MIT3_11F99_frac.pdf">Introduction to Fracture Mechanics</a>,
p. 7, Eq. (4) and the following near-field discussion. MIT OCW
<a href="https://ocw.mit.edu/pages/privacy-and-terms-of-use/">terms</a> identify
<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International</a>. Intended use is this
noncommercial research spike; no source prose, figure or dataset is redistributed.
The mathematical field is independently expressed in the tests.</li>
<li>Newman and Raju,
<a href="https://ntrs.nasa.gov/api/citations/19810023035/downloads/19810023035.pdf">NASA TM-83200</a>,
Semi-elliptical Surface Crack section, pp. 11–13, Eqs. (16)–(21).
The <a href="https://ntrs.nasa.gov/citations/19810023035">source record and rights
statement</a> states “Work of the US Gov. Public Use Permitted.”
This is the public companion to the implementation's Newman–Raju (1981) citation,
<a href="https://doi.org/10.1016/0013-7944(81)90116-8">journal DOI</a>.
The journal record has no available download and does not establish reuse rights;
no journal values were extracted. All reference values are computed by the module.</li>
<li>Repository source and synthetic fixture paths, revision and SHA-256 digests
are recorded in the result JSON. Terms: MIT License, as stated in the repository
<a href="../../../LICENSE">LICENSE</a>; no fixture containing client data is used.</li>
</ul>''' + fetch_text()


def fetch_text():
    return '''<p>The manual PDF, the NASA PDF and their extracted text exceed the non-source
commit limit and remain in the job scratch area on the data disk of the Linux
workstation. All retained source and solver files, their paths relative to that
scratch root, byte sizes and SHA-256 digests are listed in the JSON manifest.
Local retention is not an archive replica or a backup. No paid standards, licensed
solver receipts or client data were accessed.</p>
<p>Actual fetches returned HTTP 200 for the versioned manual, solver homepage and
GPL text, Roylance PDF and MIT terms, NASA memorandum and its metadata. An initial
NASA citation locator returned HTTP 404; the valid public memorandum locator was
subsequently identified from search. No refusal or credential challenge occurred.
The existing source catalog and domain index were inspected; no matching solver
or Roylance source identity was found in the scoped search. Catalog registration
and authoritative result archival remain gaps; no replacement registry is created.</p>'''


def conclusion(data):
    runs = data["runs"]
    tol = data["comparison_tolerance"]
    qualified = sum(r["max_abs_front_difference"] <= tol and
                    all(p["all_samples_near_field"] for p in r["front"]) for r in runs)
    deepest = ", ".join(f'{100*r["deepest_difference"]:.3f}%' for r in runs)
    surface = ", ".join(f'{100*r["surface_difference"]:.3f}%' for r in runs)
    maximum = 100*runs[-1]["max_abs_front_difference"]
    return f"""<h2>Interim result</h2><p>{len(runs)} native runs completed;
{qualified} qualified benchmark results against the {100*tol:g}% research comparator.
Deepest-point signed differences are {deepest}; surface differences are {surface}.
The finest maximum absolute front difference is {maximum:.3f}%.
The tested linear-element stress-extrapolation route misses the comparator at the
finest mesh. The deepest error decreases, but the surface error is nonmonotonic.
No tuned agreement is claimed. The native campaign stops after the predeclared
refinement sequence. This conclusion covers CalculiX only.</p>"""


def render(data):
    summary, details = [], []
    for r in data["runs"]:
        summary.append([r["refinement"], r["element_types"], r["elements"], r["nodes"],
                        r["front_segments"], f'{r["wall_seconds"]:.2f}', r["peak_rss_kib"],
                        f'{100*r["deepest_difference"]:.3f}',
                        f'{100*r["surface_difference"]:.3f}',
                        f'{100*r["max_abs_front_difference"]:.3f}',
                        f'{r["equilibrium_relative_residual"]:.3g}'])
        for p in r["front"]:
            details.append([r["refinement"], f'{p["phi_deg"]:.3f}',
                            f'{p["reference_k"]:.6f}', f'{p["k_mpa_sqrt_m"]:.6f}',
                            f'{100*p["relative_difference"]:.3f}',
                            f'{p["sampling_spread_mpa_sqrt_m"]:.6f}',
                            str(p["all_samples_near_field"]).lower()])
    tables = table(["n", "Elements", "Count", "Nodes", "Front segments (quarter)",
                    "Time (s)", "Peak RSS (KiB)", "Deepest difference (%)",
                    "Surface difference (%)", "Max |front difference| (%)", "Force residual"],
                   summary, "Table 1. Native costs and signed differences relative to the in-repo reference.")
    tables += table(["n", "phi (deg)", "Reference K (MPa sqrt(m))", "Extracted K (MPa sqrt(m))",
                     "Difference (%)", "Sample spread (MPa sqrt(m))", "All r within near field"],
                    details, "Table 2. All quarter-front stations, including the free-surface endpoint.")
    return '''<!doctype html><html lang="en"><meta charset="utf-8">
<title>CalculiX surface-crack spike: one-solver leg</title>
<style>body{font:16px system-ui;max-width:1200px;margin:2em auto;padding:1em;color:#17212b}
table{border-collapse:collapse;font-size:13px;width:100%}td,th{border:1px solid #bbb;padding:.4em}
th{background:#e9eef2}h1,h2{line-height:1.2}p{line-height:1.55}code{background:#eee}</style>
<h1>CalculiX surface-crack spike: one-solver leg</h1>
<p>Scope: <a href="https://github.com/vamseeachanta/digitalmodel/issues/2178">issue 2178</a>.
The two-solver verdict remains open. No production module is changed.</p>''' + conclusion(data) + method_text(data) + '<h2>Results and cost</h2>' + tables + limitations_text() + sources_text() + reproduction_text()


def limitations_text():
    return '''<p>There are no wholly near-field stations on the coarse mesh; only part of the
intermediate front meets the cited sampling range. All finest-mesh stations meet
that distance bound, but agreement still misses the comparator. Improvement at the
deepest point is evidence of a trend, not demonstrated convergence to the reference.</p>
<p>Each run uses at most four solver threads, a bounded runtime and an address-space
ceiling set by the job. Peak RSS is measured by the time tool; it is not total host
memory. Runs are sequential. Meshing and file-import time are excluded from solver
time. The explicit mesh avoids an external mesher setup cost, but does not establish
cost or accuracy for singular quadratic or arbitrary elliptical meshes.</p>
<h2>Limitations, recommendation and remaining leg</h2>
<p>FRD stresses are extrapolated and averaged at nodes; the near-front result is
sensitive to that recovery. Linear elements do not reproduce the square-root
singularity. A fit to three distances is not a contour-independence check. The
free-surface vertex has a three-dimensional corner field, so its extracted scalar
K is an apparent comparison value rather than a certified endpoint SIF.
Finite-length sensitivity and extraction-window sensitivity are not qualified.
Only elastic mode I is tested: no J, plasticity, mixed mode, residual stress,
material qualification or fitness-for-service acceptance follows from this spike.</p>
<p>Interim recommendation covering CalculiX only: retain the installed solver as
an elastic displacement/stress engine, but do not promote this stress-extrapolation
route to a Level 3 driving-force implementation. A separate investigation could
compare quadratic singular elements and a domain/interaction-integral extraction.
That proposed investigation is outside this time box.</p>
<p>The Code_Aster leg will need the unchanged geometry, load, elastic material,
reference wrapper and front angles, plus native driving-force readback, contour
sensitivity, refinement costs and the same comparator. Code_Aster is not installed
and is not installed by this job. Licensed-solver receipts are unavailable and
unused. The two-solver selection and the program note's recommendation wait for
that leg and second-provider review. No follow-on issue is opened.</p>'''


def reproduction_text():
    return '''<h2>Offline reproduction</h2>
<p>From the repository root, with an existing Python environment and PYTHONPATH=src:</p>
<pre>python scripts/studies/surface_crack_post.py --scratch &lt;job-scratch&gt; --output docs/domains/asset-integrity/surface-crack-calculix-results.json
python scripts/studies/surface_crack_report.py --results docs/domains/asset-integrity/surface-crack-calculix-results.json --output docs/domains/asset-integrity/surface-crack-calculix-spike-2026-10-11.html</pre>
<p>The first command reads retained native results without rerunning the solver.
For new runs use --run with a fresh scratch directory. Inputs are intentionally
fixed. Neither script performs network access. The compact JSON and this note
are exact generated outputs; detailed native outputs remain in scratch.</p>
</html>'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output.write_text(render(json.loads(args.results.read_text())))


if __name__ == "__main__":
    main()
