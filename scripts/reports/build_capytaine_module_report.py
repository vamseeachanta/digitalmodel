# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the Capytaine module analysis using supplied synthetic coefficients.

H1: Independent force balance m*qdd+b*qd+k*q=F with exp(-i*omega*t).
The specified controls use omega=1 rad/s, M=2 kg, A=1 kg, K=5 N/m,
B=1 N*s/m, F=+/-2 N and extra damping either absent or 1 N*s/m.
Thus Z=2-i and q=+/-(4+2i)/5 m, or Z=2-2i and q=+/-(1+i)/2 m.
H2: The existing coupled hand control in test_rao_manual_sign.py uses
omega=1, M=I, A=0, B=I, K=[[2,1],[1,2]], F=[1,i]. Direct multiplication
confirms q=[(3+4i)/5,(-2-i)/5]. No numerical reference solve is used.
The 1e-12 absolute criteria allow floating arithmetic, not physical error.
Other numeric literals describe array dimensions, formatting or metadata.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import math
from pathlib import Path
import re
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
JsonObject = dict[str, Any]
DESTINATION = ROOT / "docs/reports/modules/capytaine"
GENERATOR = "scripts/reports/build_capytaine_module_report.py"
MODULE = "src/digitalmodel/hydrodynamics/capytaine/"
TESTS = "tests/hydrodynamics/capytaine/"
SOURCES = (
    GENERATOR, MODULE + "__init__.py", MODULE + "models.py",
    MODULE + "mesh_adapter.py", MODULE + "lid.py", MODULE + "solver.py",
    MODULE + "rao.py", MODULE + "results.py",
    TESTS + "test_module_report_hand_values.py", TESTS + "test_rao_manual_sign.py",
    TESTS + "test_rao_manual_input_contract.py", TESTS + "test_result_coordinates.py",
    "tests/reporting/test_capytaine_module_report.py", "docs/api/_assets/brand.css",
)
CHANGES = [{
    "revision": "A", "date": "2026-10-28",
    "change": "The manual harmonic-response controls, public method review and "
              "reproducible report are added to distinguish algebra evidence "
              "from unperformed native accuracy and convergence checks.",
}]
# H1 and H2 above are the complete synthetic input authority; no measured data.
INPUTS: JsonObject = {
    "single_degree": {
        "omega_rad_s": [1.0], "headings_rad": [0.0, math.pi],
        "dof_names": ["Heave"], "mass_kg": [[2.0]],
        "added_mass_kg": [[[1.0]]], "stiffness_n_per_m": [[5.0]],
        "radiation_damping_n_s_per_m": [[[1.0]]],
        "force_real_n": [[[2.0], [-2.0]]],
        "extra_damping_n_s_per_m": [[1.0]], "source": "H1",
    },
    "coupled": {
        "omega_rad_s": [1.0], "headings_rad": [0.0],
        "dof_names": ["Surge", "Heave"], "mass_kg": [[1.0, 0.0], [0.0, 1.0]],
        "added_mass_kg": [[[0.0, 0.0], [0.0, 0.0]]],
        "stiffness_n_per_m": [[2.0, 1.0], [1.0, 2.0]],
        "radiation_damping_n_s_per_m": [[[1.0, 0.0], [0.0, 1.0]]],
        "force_real_n": [[[1.0, 0.0]]], "force_imag_n": [[[0.0, 1.0]]],
        "source": "H2",
    },
}
UNITS = {
    "omega": "rad/s", "heading": "rad", "period": "s", "force": "N",
    "mass": "kg", "added_mass": "kg", "stiffness": "N/m",
    "radiation_damping": "N s/m", "extra_damping": "N s/m",
    "response": "m", "amplitude": "m", "phase": "deg",
}
METHODS = (
    ("models.py", "MeshFormat; DOF.all_dofs/translational/rotational; MeshConfig; "
     "BodyConfig.rotation_center_or_com; SolverConfig; BEMResult; RAOResult",
     "STATIC_REVIEW; array containers do not enforce units or all metadata contracts."),
    ("models.py", "WaveConditions; omega_array; period_array; from_period_range; from_omega_range",
     "PARTIAL_SYNTHETIC; angular-frequency/period conversion is used by the worked controls. "
     "Range constructors and invalid-input coverage remain NOT_EVALUATED here."),
    ("mesh_adapter.py", "create_floating_body",
     "NOT_EVALUATED; imports the optional backend, loads/converts geometry, applies symmetry, "
     "immerses the body and may generate a lid. Geometry and conversion evidence are absent."),
    ("solver.py", "CapytaineSolver.__init__; validate_inputs; solve; dataset; body; run_bem_analysis",
     "NOT_EVALUATED for native execution. Coordinate extraction has a separate synthetic "
     "regression; it does not establish BEM coefficient accuracy."),
    ("rao.py", "compute_rao_manual",
     "SYNTHETIC_CONTROLS; the computed verdict below applies only to the named controls. Finite-shape contracts and coupled "
     "sign regressions are separate evidence. Singular least squares is not an acceptance criterion."),
    ("rao.py", "compute_rao",
     "NOT_EVALUATED; delegates to optional native post-processing and requires dataset inertia "
     "and hydrostatic stiffness. Native axis ordering and normalization need readback."),
    ("results.py", "export_netcdf",
     "NOT_EVALUATED; delegates to native export. Exact complex coefficient/coordinate round-trip "
     "evidence and backend-version compatibility are absent."),
    ("results.py", "added_mass_table; excitation_force_table",
     "PARTIAL_SYNTHETIC; existing excitation-table regressions cover reordered labeled coordinates. "
     "Added-mass tabulation is positional in frequency and remains unqualified."),
    ("results.py", "plot_added_mass_damping; plot_excitation_force; plot_rao",
     "NOT_EVALUATED; coefficient plots pair stored frequency order with periods; rotational "
     "response labels require explicit angular and wave-amplitude unit confirmation."),
)


def _validate_response_contract(result: Any, coordinates: JsonObject) -> None:
    """Require complete response axes and exact labels before qualification."""
    import numpy as np

    shape = (len(coordinates["omegas"]), len(coordinates["headings"]), len(coordinates["dof_names"]))
    for name in ("rao", "rao_amplitude", "rao_phase"):
        values = np.asarray(getattr(result, name))
        if values.shape != shape or not np.isfinite(values).all():
            raise ValueError(f"Returned response contract violated: {name}")
    for name, expected in coordinates.items():
        if not np.array_equal(getattr(result, name), expected):
            raise ValueError(f"Returned response contract violated: {name}")


def _run_control(inputs: JsonObject, check_id: str, expected: list[complex],
                 extra: list[list[float]] | None = None) -> JsonObject:
    """Call the public algebra route and compare with independent hand values."""
    import numpy as np
    from digitalmodel.hydrodynamics.capytaine.models import BEMResult, WaveConditions
    from digitalmodel.hydrodynamics.capytaine.rao import compute_rao_manual

    waves = WaveConditions(omegas=inputs["omega_rad_s"], headings=inputs["headings_rad"])
    # Snapshot immutable expected labels before the public call. Output metadata
    # must not alias its own comparison basis or the authoritative inputs.
    coordinates = {
        "omegas": tuple(inputs["omega_rad_s"]), "headings": tuple(inputs["headings_rad"]),
        "periods": tuple(2 * math.pi / omega for omega in inputs["omega_rad_s"]),
        "dof_names": tuple(inputs["dof_names"]),
    }
    force = np.asarray(inputs["force_real_n"], dtype=complex)
    if "force_imag_n" in inputs:
        force += 1j * np.asarray(inputs["force_imag_n"])
    bem = BEMResult(
        added_mass=np.asarray(inputs["added_mass_kg"]),
        radiation_damping=np.asarray(inputs["radiation_damping_n_s_per_m"]),
        excitation_force=force, omegas=waves.omega_array, periods=waves.period_array,
        headings=waves.headings, dof_names=list(inputs["dof_names"]),
    )
    result: Any = compute_rao_manual(bem, inputs["mass_kg"], inputs["stiffness_n_per_m"], extra)
    _validate_response_contract(result, coordinates)
    responses, complex_errors, amplitude_errors, phase_errors = [], [], [], []
    for index, q in zip(np.ndindex(result.rao.shape), expected, strict=True):
        frequency, heading, dof = index
        actual = result.rao[index]
        amplitude, phase = float(result.rao_amplitude[index]), float(result.rao_phase[index])
        expected_amplitude, expected_phase = abs(q), math.degrees(math.atan2(q.imag, q.real))
        complex_errors.append(float(abs(actual - q)))
        amplitude_errors.append(abs(amplitude - expected_amplitude))
        phase_errors.append(abs(phase - expected_phase))
        responses.append({
            "omega_rad_s": float(result.omegas[frequency]),
            "period_s": float(result.periods[frequency]),
            "heading_rad": float(result.headings[heading]), "dof": result.dof_names[dof],
            "real_m": float(actual.real), "imag_m": float(actual.imag),
            "amplitude_m": amplitude, "phase_deg": phase,
        })
    errors = {"complex_m": max(complex_errors), "amplitude_m": max(amplitude_errors),
              "phase_deg": max(phase_errors)}
    return {
        "id": check_id, "source": inputs["source"], "responses": responses,
        "status": "PASS" if all(error <= 1e-12 for error in errors.values()) else "FAIL",
        "maximum_absolute_errors": errors,
        "coordinates": {"axis_order": ["omega", "heading", "dof"],
                        "omegas_rad_s": result.omegas.tolist(),
                        "headings_rad": result.headings.tolist(), "dof_names": result.dof_names},
    }


def calculate() -> JsonObject:
    """Return deterministic inputs and public-function results; no native solve."""
    # H1: multiplying the conjugate of each scalar impedance gives these fractions.
    single, coupled = INPUTS["single_degree"], INPUTS["coupled"]
    q, damped_q = (4 + 2j) / 5, (1 + 1j) / 2
    checks = [
        _run_control(single, "single_radiation", [q, -q]),
        _run_control(single, "single_extra_damping", [damped_q, -damped_q],
                     single["extra_damping_n_s_per_m"]),
        _run_control(coupled, "coupled_existing_hand", [(3 + 4j) / 5, (-2 - 1j) / 5]),
    ]
    evidence = {"manual_rao": {
        "status": "PASS" if all(case["status"] == "PASS" for case in checks) else "FAIL",
        "basis": "H1 and H2 synthetic algebra controls only; no native coefficient generation.",
    }}
    missing = {
        "native_bem": "A public worked benchmark with published geometry, environment, "
                      "coefficient tables, normalization and comparison criteria, plus native output readback.",
        "mesh_conversion": "Original and converted public meshes with checked units, normals, "
                           "waterline, wetted surface and DOF centers.",
        "mesh_convergence": "A common public benchmark with successively refined meshes, "
                            "coefficient/response changes and a predefined convergence criterion.",
        "radiation_diffraction_accuracy": "Independent published radiation/diffraction coefficients "
                                          "at matching frequencies, headings, DOFs and physical conditions.",
        "delegated_rao": "Native post-processing output with inertia/stiffness, complex response, "
                         "dimension order and units checked against the same independent hand problem.",
        "native_netcdf": "Native exported and reopened datasets preserving complex coefficients, "
                         "dimensions, coordinate order, units and exact benchmark inputs.",
    }
    evidence.update({name: {"status": "NOT_EVALUATED", "source_needed": need}
                     for name, need in missing.items()})
    return {"inputs": INPUTS, "checks": checks, "units": UNITS, "evidence": evidence,
            "revision_history": CHANGES, "methods": METHODS,
            "absolute_criteria": {"complex_m": 1e-12, "amplitude_m": 1e-12, "phase_deg": 1e-12,
                                  "source": "H1/H2; double-precision roundoff allowance only"}}


def _table(headers: tuple[str, ...], rows: Any, caption: str) -> str:
    """Escape all values and place the caption below its table."""
    heading = "".join(f"<th>{html.escape(str(value))}</th>" for value in headers)
    body = "".join("<tr>" + "".join(f"<td>{html.escape(str(value))}</td>" for value in row)
                   + "</tr>" for row in rows)
    return f"<table><thead><tr>{heading}</tr></thead><tbody>{body}</tbody></table><p>{html.escape(caption)}</p>"


def _result_tables(data: JsonObject) -> str:
    """Format the actual returned responses, with source and numerical criteria."""
    sections = []
    for case in data["checks"]:
        rows = [(f'{row["omega_rad_s"]:.1f}', f'{math.degrees(row["heading_rad"]):.1f}',
                 row["dof"], f'{row["real_m"]:.3f}', f'{row["imag_m"]:.3f}',
                 f'{row["amplitude_m"]:.3f}', f'{row["phase_deg"]:.1f}', case["source"])
                for row in case["responses"]]
        sections.append(f'<h3>{html.escape(case["id"])}</h3>' + _table(
            ("Omega (rad/s)", "Heading (deg)", "DOF", "Real (m)", "Imaginary (m)",
             "Amplitude (m)", "Phase (deg)", "Source"), rows,
            "Computed harmonic displacements; headings are displayed in degrees from radian coordinates."))
        errors = case["maximum_absolute_errors"]
        sections.append(f'<p>{case["status"]}: maximum absolute complex error '
                        f'{errors["complex_m"]:.1e} m; amplitude error {errors["amplitude_m"]:.1e} m; '
                        f'phase error {errors["phase_deg"]:.1e} deg. '
                        'Each is compared with its absolute criterion of 1e-12 in the same unit [H1, H2].</p>')
    return "\n".join(sections)


def render(data: JsonObject) -> str:
    """Render the established ten sections around the calculation payload."""
    history = _table(("Revision", "Date", "Change and reason"),
                     [(item["revision"], item["date"], item["change"]) for item in data["revision_history"]],
                     "Revision history; earlier issues will remain unchanged on re-issue.")
    method_table = _table(("Source file", "Public entry points", "Review and qualification"),
                          data["methods"], "Method inventory; review is distinct from numerical qualification.")
    evidence_table = _table(("Method or criterion", "Status", "Basis or source needed"),
                            [(name, entry["status"], entry.get("basis", entry.get("source_needed")))
                             for name, entry in data["evidence"].items()],
                            "Evidence status applies only to the stated method and criterion.")
    single = data["inputs"]["single_degree"]
    input_rows = [(key, value, data["units"][key], "H1") for key, value in (
        ("omega", single["omega_rad_s"]),
        ("heading", single["headings_rad"]),
        ("mass", single["mass_kg"]),
        ("added_mass", single["added_mass_kg"]),
        ("stiffness", single["stiffness_n_per_m"]),
        ("radiation_damping", single["radiation_damping_n_s_per_m"]),
        ("force", single["force_real_n"]),
        ("extra_damping", single["extra_damping_n_s_per_m"]),
    )]
    # Radian headings use the exact symbolic half turn in the visible data table.
    input_rows[1] = ("heading", "[0, pi]", "rad", "H1; pi is the half-turn definition")
    design = _table(("Quantity", "Supplied value", "Unit", "Source"), input_rows,
                    "Single-degree synthetic design data; coefficients are supplied controls.")
    brand = re.sub(r"/\*.*?\*/", "", (ROOT / "docs/api/_assets/brand.css").read_text(), flags=re.S)
    source_items = "".join(f"<li><code>{html.escape(path)}</code></li>" for path in SOURCES)
    payload = json.dumps(data, sort_keys=True, allow_nan=False).replace("<", "\\u003c")
    page = f'''<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Capytaine module analysis</title><style>{brand}
main{{max-width:70rem;margin:auto;padding:2rem}} section{{margin-block:2rem}}
table{{border-collapse:collapse;width:100%;margin-top:1rem}} th,td{{border:1px solid var(--edge);padding:.5rem;text-align:left;vertical-align:top}}
th{{background:var(--panel-2)}} pre{{white-space:pre-wrap;overflow-wrap:anywhere}} code{{overflow-wrap:anywhere}}
@media(max-width:50rem){{table{{display:block;overflow-x:auto}}}}
</style></head><body><main>
<header><h1>Capytaine module analysis</h1><p>Supplied-coefficient harmonic-response controls and open-source wrapper review.</p></header>
<section><h2 id="s1">1. Executive Summary</h2>
<p>The manual response method is {data["evidence"]["manual_rao"]["status"]} against the independent complex-displacement, amplitude and phase controls [H1, H2].
The synthetic coefficients are supplied rather than BEM-computed; they establish neither mesh convergence nor physical accuracy of radiation/diffraction.
Native execution, delegated response and NetCDF export remain NOT_EVALUATED against their required evidence.</p></section>
<section id="revision-history"><h3>Revision history</h3>{history}</section>
<details id="changes-panel"><summary>Engineering changes</summary><p>{html.escape(CHANGES[-1]["change"])}</p></details>
<section><h2 id="s2">2. Introduction</h2>
<p>The wrapper exposes configuration, mesh conversion, radiation/diffraction solve, manual and delegated response calculation, tables, export and plots [module sources below].
This analysis qualifies the stated algebra controls only. No licensed interface is imported and no native boundary-element solve is performed.</p>
{method_table}</section>
<section><h2 id="s3">3. Design Data and Basis</h2>{design}
<p>H1 is a synthetic single-degree translational oscillator with positive and negative real force at opposite heading labels.
The heading labels do not model a physical diffraction pattern. Extra damping is absent in the first control and supplied in the second.
H2 is an explicitly separate coupled translational hand example, retained from the existing sign regression.</p>
<p>Response is dimensional displacement in m for the supplied force in N. It is not a motion-to-wave ratio in m/m:
no incident wave amplitude is supplied for normalization. The numeric containers do not enforce units.
Array axes are omega, heading, DOF; matrix rows are influenced DOFs and columns are radiating DOFs. Headings use rad and phases use deg [models.py, H1].</p></section>
<section><h2 id="s4">4. Analysis Methodology</h2>
<p>With exp(-i*omega*t), differentiation gives qdot=-i*omega*q and qddot=-omega^2*q.
Force balance therefore gives Z=K-omega^2*(M+A)-i*omega*(B+B_extra), and Z*q=F [H1].
The public function <code>compute_rao_manual</code> is called with independently constructible <code>BEMResult</code> arrays.
Amplitude and phase are returned by that function; periods are obtained through <code>WaveConditions.period_array</code>.</p>
<h3>Independent single-degree calculations [H1]</h3>
<p>For radiation damping alone, Z=5-(2+1)-i=2-i N/m.
Thus q=2/(2-i)=(4+2i)/5 m, |q|=2/sqrt(5) m and phase=atan(1/2) radians converted to degrees.
With extra damping, Z=2-2i N/m and q=(1+i)/2 m, |q|=1/sqrt(2) m and phase=atan(1) radians converted to degrees.
Reversing the real force reverses the complex response; its amplitude is unchanged and the principal phase decreases by a half turn.</p>
<h3>Independent coupled calculation [H2]</h3>
<p>M=I kg, A=0 kg, B=I N s/m, K=[[2,1],[1,2]] N/m, omega=1 rad/s and F=[1,i] N.
Hence Z=[[1-i,1],[1,1-i]] N/m; direct multiplication gives
Z*[(3+4i)/5,(-2-i)/5]=[1,i] N. This reuses the existing coupled sign evidence and is separate from H1.</p>
<p>Each absolute complex, amplitude and phase error is compared with 1e-12 in m, m and deg respectively.
This is a double-precision algebra criterion, not an engineering acceptance threshold. Expected values use literal fractions and elementary angles; no second numerical solve is used [H1, H2].</p></section>
<section><h2 id="s5">5. Detailed Results</h2>{_result_tables(data)}</section>
<section><h2 id="s6">6. Key Findings</h2>{evidence_table}
<p>The existing coordinate regression covers requested frequency, heading and DOF ordering in extracted synthetic datasets,
and the excitation table uses labeled selection. This does not qualify native BEM accuracy or every table and plot.</p>
<p>Static review observations remain unresolved: solver engine/method fields do not select alternate implementations;
unknown Green-function names fall back to a default; configured hydrostatic stiffness is not consumed;
native input validation does not establish rejection of all nonfinite inputs; and hydrostatic failures may leave partial results.
Delegated response ordering and normalization need readback. Added-mass tables and coefficient plots can pair stored frequency order positionally with periods.
Rotational plot labels require angular-unit confirmation. The manual singular-system fallback does not enforce a residual or condition criterion [solver.py, rao.py, results.py].
These observations are not independently reproduced defects fixed by this report.</p></section>
<section><h2 id="s7">7. Recommendations</h2>
<p>The next qualification step should obtain the public benchmark source described in the evidence table, including its geometry, fluid conditions,
coordinate definitions, coefficient normalization and published numerical values. Native readback should then compare matching coordinates
and execute a defined mesh-refinement sequence before physical use. No benchmark outcome is asserted.</p>
<p>Targeted independent regressions should resolve the static API observations before affected methods are relied on.
No BEM accuracy setting, screening threshold, default geometry or engineering acceptance criterion is changed.</p></section>
<section><h2 id="s8">8. Conclusion</h2>
<p>The specified synthetic algebra controls are {data["evidence"]["manual_rao"]["status"]} against their absolute criteria.
Physical radiation/diffraction accuracy, convergence and native export qualification cannot be established without the listed sources and retained native outputs.
The remaining module qualification scope stays open.</p></section>
<section><h2 id="s9">9. Data Register and Provenance</h2>
<p>H1: independent force-balance derivation stated in the builder and <code>test_module_report_hand_values.py</code>.
H2: existing coupled example in <code>test_rao_manual_sign.py</code>.
All numeric engineering inputs are synthetic controls from H1 or H2; mathematical constants and degree conversion are definitions.
No measured data or licensed-publication values are used. The authoritative inputs are the builder's committed INPUTS mapping.</p>
<ul>{source_items}</ul>
<p><code>manifest.json</code> records SHA-256 source and artifact digests. The manifest excludes its own digest to avoid self-reference;
the rebuild test compares all deliverables, including the manifest, byte for byte. Exact machine-readable values are embedded below;
visible values are rounded for presentation and comparisons use unrounded values.</p>
<p>Rebuild: <code>PYTHONPATH=src python scripts/reports/build_capytaine_module_report.py --scratch "$TMPDIR/capytaine-work"</code>.
The optional <code>--output-dir</code> selects an external delivery directory. Scratch is resolved outside the checkout;
the calculations remain in memory. No runtime timestamp or environment path enters the artifacts.</p></section>
<section><h2 id="s10">10. Acronyms and Review Notes</h2>
<p>BEM means boundary element method; DOF means degree of freedom; RAO means response amplitude operator.
PASS denotes only agreement with the stated synthetic criterion. NOT_EVALUATED means the evidence needed to calculate or accept that method's result is absent.
Independent review should address the unresolved methods and their evidence requirements before physical application.</p></section>
<script id="calculation" type="application/json">{payload}</script>
</main></body></html>
'''
    return "\n".join(line.rstrip() for line in page.splitlines()) + "\n"


def _json_bytes(value: Any) -> bytes:
    """Encode stable UTF-8 with a final newline on every platform."""
    return (json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False, allow_nan=False) + "\n").encode("utf-8")


def build_report(*, scratch: str | Path, output_dir: str | Path = DESTINATION) -> JsonObject:
    """Validate destinations before writing the four deterministic deliverables."""
    scratch, output = Path(scratch).resolve(), Path(output_dir).resolve()
    if scratch == ROOT or ROOT in scratch.parents:
        raise ValueError("Calculation scratch must be outside the checkout")
    if (output == ROOT or ROOT in output.parents) and output != DESTINATION:
        raise ValueError("Output inside the checkout must be the designated report directory")
    if scratch == output or scratch in output.parents or output in scratch.parents:
        raise ValueError("Scratch and output must be separate directories without nesting")
    for name in ("index.html", "changes.json", "README.md", "manifest.json"):
        if (output / name).is_symlink():
            raise ValueError("Report deliverables must not be symbolic links")
    data = calculate()
    artifacts = {
        "index.html": render(data).encode("utf-8"),
        "changes.json": _json_bytes(CHANGES),
        "README.md": (
            "# Capytaine module analysis\n\n"
            "[Read the module analysis](index.html). Synthetic coefficients are supplied, not BEM-computed. "
            "The single-degree and existing coupled force-balance checks qualify only manual algebra. "
            "Native solve, mesh convergence, physical accuracy, delegated RAO and NetCDF remain NOT_EVALUATED.\n\n"
            "Rebuild from the repository root with NumPy, SciPy and xarray available:\n\n"
            "```sh\nPYTHONPATH=src python scripts/reports/build_capytaine_module_report.py "
            '--scratch "$TMPDIR/capytaine-work"\n```\n\n'
            "Scratch must resolve outside the checkout. Use `--output-dir` for an external copy; "
            "in-checkout output is limited to this report directory. Calculations run in memory; "
            "no intermediate file is saved in the checkout. The optional native solver is not required.\n\n"
            "The dimensional response is in m for force in N; no wave-amplitude normalization is supplied. "
            "Phase uses deg, heading uses rad and omega uses rad/s. Inputs and hand derivations are "
            "committed in the builder and independent tests, as identified by the report source register.\n\n"
            "`manifest.json` binds the builder, local method and test sources, palette and the other "
            "three report files using SHA-256. Its own digest is excluded to avoid self-reference. "
            "The deterministic rebuild test checks all four exact deliverable byte streams.\n\n"
            "Run the report controls:\n\n```sh\nPYTHONPATH=src python -m pytest "
            "tests/reporting/test_capytaine_module_report.py\n```\n"
        ).encode("utf-8"),
    }
    manifest = {
        "schema_version": 1, "generator": GENERATOR,
        "digest_convention": "SHA-256 over exact UTF-8/LF bytes; manifest self-digest is excluded "
                             "to avoid self-reference. Rebuild checks include manifest bytes.",
        "sources": {name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest() for name in SOURCES},
        "artifacts": {name: hashlib.sha256(content).hexdigest() for name, content in artifacts.items()},
    }
    artifacts["manifest.json"] = _json_bytes(manifest)
    scratch.mkdir(parents=True, exist_ok=True)
    output.mkdir(parents=True, exist_ok=True)
    for name, content in artifacts.items():
        (output / name).write_bytes(content)
    return manifest


def main() -> None:
    """Accept explicit scratch and optional report destination paths."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", required=True, type=Path)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    args = parser.parse_args()
    build_report(scratch=args.scratch, output_dir=args.output_dir)
    print("Written: index.html, changes.json, manifest.json, README.md")


if __name__ == "__main__":
    main()
