# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build a reproducible diffraction external matrix analysis without a solver.

Run with PYTHONPATH=src, then python this_script --output-dir <report-dir>
--scratch <temporary-native-input-dir>. All coefficients are synthetic inputs;
the polynomial expansion in the report supplies the independent answer.
"""

from __future__ import annotations

import argparse
import json
from html import escape
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
from digitalmodel.hydrodynamics.diffraction.external_matrices import (
    prepare_matrix,
    transfer_matrix,
)
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend


CHANGES = {
    "document": "diffraction-external-matrix-analysis",
    "revision": 1,
    "date": "2026-10-22",
    "changes": [{"section": "external-matrices", "description":
                 "Initial unit, reference-transfer and native-input analysis; "
                 "explicit contracts replace ambiguous matrix emission."}],
}

SOURCES = [
    ("OrcaWave help, Constraints, external stiffness and damping block equations "
     "(accessed 2026-10-22; online edition)",
     "https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Constraints.htm"),
    ("OrcaWave help, Model, Units: SI force is kN, length m, time s "
     "(accessed 2026-10-22; online edition)",
     "https://www.orcina.com/webhelp/OrcaWave/Content/html/Data,Model.htm#Units"),
    ("Handbook of Marine Craft Hydrodynamics and Motion Control, second edition "
     "(2021), public marine craft model: skew cross-product matrix and CoG offset",
     "https://www.fossen.biz/html/marineCraftModel.html"),
    ("Aqwa Reference Manual, Release 2024 R2, 1.2.1 Axes Systems: local axes at CoG",
     "https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/aqwa_ref/aqwaref_intro_axes.html"),
    ("Aqwa Reference Manual, Release 2024 R2, 1.4 Units: N, m, kg, s; matrix angles radians",
     "https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/aqwa_ref/aqwaref_unitsconv.html"),
    ("Aqwa Reference Manual, Release 2024 R2, 11.6.1 FIAM/FIDP: additive damping matrix",
     "https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/aqwa_ref/aqwaref_deck7_fiam_fidp.html"),
    ("Aqwa Reference Manual, Release 2024 R2, 11.7.1 SSTF: global-frame structural stiffness",
     "https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/aqwa_ref/aqwaref_deck7_sstf.html"),
    ("BIPM SI prefixes table: kilo is exactly ten cubed (accessed 2026-10-22)",
     "https://www.bipm.org/en/measurement-units/si-prefixes"),
]


def table(headers, rows):
    head = "".join(f'<th scope="col">{escape(str(x))}</th>' for x in headers)
    body = "".join("<tr>" + "".join(f"<td>{escape(str(x))}</td>" for x in row)
                   + "</tr>" for row in rows)
    return f"<table><thead><tr>{head}</tr></thead><tbody>{body}</tbody></table>"


def matrix_table(matrix):
    return table(["Row / column", "surge", "sway", "heave", "roll", "pitch", "yaw"],
                 [[i + 1, *[f"{value:.1f}" for value in row]]
                  for i, row in enumerate(matrix)])


def figure(matrix):
    """Derive every cell shade and label from the computed matrix values."""
    peak = float(np.max(np.abs(matrix)))
    cells = []
    # Pixel sizes are presentation choices, not engineering coefficients.
    for i, row in enumerate(matrix):
        for j, value in enumerate(row):
            lightness = 98 - 50 * abs(float(value)) / peak
            hue = 10 if value < 0 else 210
            x, y = j * 90, i * 60
            cells.append(f'<rect x="{x}" y="{y}" width="90" height="60" '
                         f'fill="hsl({hue},50%,{lightness:.1f}%)" stroke="white"/>')
            cells.append(f'<text x="{x+45}" y="{y+35}" text-anchor="middle">'
                         f'{float(value):.1f}</text>')
    return ('<figure><svg viewBox="0 0 540 360" role="img" '
            'aria-label="Computed body-origin matrix; numerical values also tabulated">'
            + "".join(cells) + '</svg><figcaption>Computed body-origin matrix. '
            'Each block retains the units in the unit table.</figcaption></figure>')


def calculate(scratch):
    """Compute the transfer, both native inputs and explicit refusal evidence."""
    source = np.diag([2000., 3000., 5000., 7000., 11000., 13000.])
    offset = [0., 0., 2.]
    origin = transfer_matrix(source, offset)
    spec = DiffractionSpec.model_validate({
        "vessel": {"name": "synthetic", "geometry": {"mesh_file": "synthetic.gdf"},
                   "inertia": {"mass": 1000., "centre_of_gravity": offset,
                               "radii_of_gyration": [1., 1., 1.]},
                   "external_damping": origin.tolist(), "external_damping_units": "SI",
                   "external_damping_reference_point": "body_origin"},
        "environment": {"water_depth": 100.},
        "frequencies": {"values": [1.]}, "wave_headings": {"values": [0.]},
    })
    path = OrcaWaveBackend().generate_single(spec, scratch)
    native_yaml = path.read_text(encoding="utf-8")
    # Retain the actual external matrix block and origin line from the emitter.
    lines = native_yaml.splitlines()
    start = next(i for i, line in enumerate(lines) if "BodyExternalDampingMatrixx," in line)
    end = next(i for i in range(start, len(lines))
               if "BodyExternalDampingMatrixOriginType" in lines[i])
    aqwa_text = "\n".join(AQWABackend().build_deck7(spec))
    stiffness_body = spec.vessel.model_copy(deep=True)
    stiffness_body.external_stiffness = origin.tolist()
    stiffness_body.external_stiffness_units = "SI"
    stiffness_body.external_stiffness_reference_point = "body_origin"
    try:
        prepare_matrix(stiffness_body, "stiffness", "aqwa")
    except ValueError as error:
        refusal = str(error)
    else:
        raise AssertionError("AQWA stiffness qualification unexpectedly changed")
    return {"cog_matrix": source.tolist(), "offset_m": offset,
            "body_origin_matrix": origin.tolist(),
            "orcawave_text": "\n".join(lines[start:end+1]),
            "aqwa_text": aqwa_text, "aqwa_stiffness_refusal": refusal,
            "orcawave_record": prepare_matrix(spec.vessel, "damping", "orcawave"),
            "aqwa_record": prepare_matrix(spec.vessel, "damping", "aqwa")}


def render(data):
    units = table(["Load rows / motion columns", "Stiffness", "Damping"], [
        ["Translation / translation", "N/m", "N.s/m"],
        ["Translation / rotation", "N/rad", "N.s/rad"],
        ["Rotation / translation", "N.m/m", "N.m.s/m"],
        ["Rotation / rotation", "N.m/rad", "N.m.s/rad"],
    ])
    history = table(["Revision", "Date", "What changed and why"], [
        [CHANGES["revision"], CHANGES["date"], CHANGES["changes"][0]["description"]]])
    source_list = "".join(f'<li><a href="{escape(url)}">{escape(title)}</a></li>'
                          for title, url in SOURCES)
    offset = ", ".join(f"{x:.1f}" for x in data["offset_m"])
    policy = table(["Numeric policy field", "Valid domain", "Reason"], [
        ["min_samples", "Integer >= 2; excludes booleans", "Two points define an interval"],
        ["min_coverage", "None or finite real in (0,1]; excludes booleans", "Ratio of spans"],
        ["max_relative_gap", "Finite real > 0; excludes booleans", "Bounds frequency gap / frequency"],
    ])
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>Diffraction external matrix analysis</title><style>
body{{font:16px system-ui,sans-serif;color:#172b3a;background:#f5f7fa;margin:0}}
main{{max-width:1100px;margin:auto;padding:2rem}}section{{background:white;padding:1.5rem;margin:1rem 0}}
table{{border-collapse:collapse;width:100%;margin:1rem 0}}th,td{{border:1px solid #cad5df;padding:.5rem;text-align:right}}
th:first-child,td:first-child{{text-align:left}}pre{{overflow:auto;background:#edf2f6;padding:1rem}}
svg{{width:100%;max-width:700px}}nav a{{margin-right:1rem}}a{{color:#125a91}}
</style></head><body><main><h1>Diffraction external matrix analysis</h1>
<nav aria-label="Contents"><a href="#summary">Summary</a><a href="#inputs">Inputs</a>
<a href="#method">Method</a><a href="#example">Worked example</a><a href="#limits">Limits of use</a></nav>
<section id="summary"><h2>Summary</h2><p>External matrices require declared SI units and
a body-origin or centre-of-gravity reference. The computed example transfers a reciprocal
matrix and emits physically equivalent damping inputs with explicit native units.
AQWA nonzero external stiffness is refused pending native semantic qualification.</p>
<h3>Revision history</h3>{history}</section>
<section id="inputs"><h2>Inputs</h2><p>This is a synthetic arithmetic example.
The six coordinates are surge, sway, heave, roll, pitch and yaw, in right-handed parallel
body axes; translations are metres and rotations radians. CoG offset from body origin:
({offset}) m. Coefficients are independently chosen inputs, with no physical acceptance claim.</p>
{matrix_table(data["cog_matrix"])}<h3>Canonical unit contract by matrix block</h3>{units}
<p>Each matrix requires its own external_stiffness_units or external_damping_units = SI
and external_stiffness_reference_point or external_damping_reference_point.
Accepted points are body_origin and centre_of_gravity. Missing declarations are rejected.
Legacy inputs require a documented unit and origin audit before migration.</p></section>
<section id="method"><h2>Method</h2><p>Source 1 supplies matrix block dimensions.
Source 2 defines OrcaWave SI force as kN; source 8 defines the kilo prefix,
so every N-based block is divided by 1000.
Sources 4–6 define the AQWA local CoG axes, SI units and additive FIDP damping.</p>
<p>Source 3 supplies S(r)a = r cross a. For r from target to source,
u_source = u_target + theta cross r, T = [[I,-S(r)],[0,I]].
Substitution in quadratic energy or virtual work gives K_target = Tᵀ K_source T.
The same relation holds for damping using velocity and power.</p>
<p>Exact symmetry is the supported reciprocal linear-input rule for both matrices;
no physical tolerance is introduced. Positivity and passivity are NOT_EVALUATED.</p>
<h3>Nested numeric policy inventory</h3>{policy}<p>Physical defaults are unchanged;
custom thresholds require nonblank custom justification. Nonfinite numbers never disable a check.</p></section>
<section id="example"><h2>Worked example</h2><p>Independently expand twice the energy divided by 1000:
2(x+2 pitch)² + 3(y−2 roll)² + 5z² + 7 roll² + 11 pitch² + 13 yaw².
This yields the body-origin matrix below. The test docstring records each diagonal and coupling.</p>
{matrix_table(data["body_origin_matrix"])}{figure(data["body_origin_matrix"])}
<p>The same body-origin matrix is used as damping for both input emitters.
OrcaWave keeps the declared point; AQWA transfers it to CoG and preserves N-based units.</p>
<h3>OrcaWave emitted text</h3><pre>{escape(data["orcawave_text"])}</pre>
<h3>AQWA emitted text</h3><pre>{escape(data["aqwa_text"])}</pre>
<h3>AQWA stiffness disposition</h3><pre>{escape(data["aqwa_stiffness_refusal"])}</pre>
<p>Each generated input directory receives external-matrix-provenance.json with
canonical, normalized and native values, units, points, CoG offset and conversion revision.</p></section>
<section id="limits"><h2>Limits of use</h2><p>Small rotations, parallel axes and
constant reciprocal linear matrices are required. The transfer excludes orientation changes,
preload-dependent geometric stiffness and nonlinear damping. AQWA supports damping for one
unrotated body with coincident mesh and body origins; rotated, positioned and coupled
cases are refused. CoG-dependent free-floating matrices
are refused because their CoG is determined by the solver.</p>
<h3>NOT_EVALUATED</h3><ul><li>Native solver loading, execution and physical response.</li>
<li>AQWA structural stiffness: source 7 specifies global-frame SSTF semantics;
the old FISK emission has no confirmed contract. Restoring composition remains separate.</li>
<li>Positive definiteness, passivity, physical acceptance and field calibration.</li>
<li>Legacy matched-case matrix migration without a confirmed data basis.</li>
<li>Nonlinear, preloaded, multibody and rotated-body external matrix behaviour.</li></ul>
<p>Way forward: qualify native stiffness coordinates and composition, audit legacy units
against their sources, then perform independently reviewed solver benchmarks.</p></section>
<section><h2>Sources</h2><ol>{source_list}</ol><h3>Reproducibility</h3>
<pre>PYTHONPATH=src python scripts/reporting/build_diffraction_external_matrices_report.py --output-dir docs/reports/modules/diffraction/external-matrices --scratch "$TMPDIR/native-report"</pre>
<p>The builder computes every figure and table, extracts actual emitted input text,
and writes report.html, results.json and changes.json. The offline report test rebuilds
twice and compares both results with the committed bytes. No solver is imported or started.</p>
</section></main></body></html>
'''


def build(output_dir, scratch):
    output_dir, scratch = Path(output_dir), Path(scratch)
    scratch.mkdir(parents=True, exist_ok=True)
    data = calculate(scratch)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "report.html").write_text(render(data), encoding="utf-8")
    for name, value in (("results.json", data), ("changes.json", CHANGES)):
        (output_dir / name).write_text(json.dumps(value, indent=2, allow_nan=False) + "\n",
                                       encoding="utf-8")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--scratch", type=Path, required=True)
    args = parser.parse_args()
    build(args.output_dir, args.scratch)


if __name__ == "__main__":
    main()
