"""Standard-report adapter and reproducible generic berth worked example."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path

from digitalmodel.hydrodynamics.berth_mooring import (
    SOURCE_URLS,
    drag_load,
    load_spec,
    write_campaign,
)
from digitalmodel.reporting import (
    DocumentMeta,
    Provenance,
    ReportSpec,
    Section,
    TableBlock,
    TextBlock,
    render_html,
)


def build_report(spec: dict, manifest: dict) -> ReportSpec:
    """Build a standard report with a revision table directly after summary.

    Native preparation is separate from coupled response. Force peaks are
    sampled applied-load peaks, not line/fender loads and not continuous peaks.
    """
    wind, current = drag_load(spec["wind"]), drag_load(spec["current"])
    total = [a + b for a, b in zip(wind, current)]

    # BIPM SI Brochure, 9th ed. (2019), SI prefixes: kilo=10^3.
    def kn(value):
        return f"{value / 1000:.1f}"

    def section(key, title, *blocks):
        return Section(key=key, title=title, blocks=list(blocks))

    rows = [
        [
            c["name"],
            f"{c['speed_m_s']:.1f}",
            f"{c['separation_m']:.1f}",
            str(c["wind"]),
            str(c["current"]),
            kn(c["peak_force_n"]["surge_n"]),
            kn(c["peak_force_n"]["sway_n"]),
            kn(c["peak_yaw_nm"]),
            "NOT_EVALUATED",
        ]
        for c in manifest["cases"]
    ]
    source_lines = "\n\n".join(f"{key}: {url}" for key, url in SOURCE_URLS.items())
    assumptions = [[key, value] for key, value in spec["basis"].items()]
    response_rows = [
        [
            r["case"],
            r["object"],
            "—" if r["peak_kn"] is None else f"{r['peak_kn']:.1f}",
            "—" if r["allowable_kn"] is None else f"{r['allowable_kn']:.1f}",
            "—" if r["utilisation"] is None else f"{r['utilisation']:.1f}",
            r["status"],
            r["criterion_source"] or "not supplied",
        ]
        for r in manifest["response"]
    ]
    version = manifest["solver_version"] or "not supplied"
    response_status = (
        "NOT_EVALUATED"
        if manifest["solver_version"] is None
        else "exported samples evaluated against supplied criteria"
    )
    calculations = []
    for key, load in (("wind", wind), ("current", current)):
        data = spec[key]
        speed = math.hypot(*data["velocity_m_s"])
        calculations.append(
            f"{key}: |F| = 0.5 x {data['density_kg_m3']:.1f} x {data['area_m2']:.1f} "
            f"x {data['coefficient']:.1f} x {speed:.1f} squared = "
            f"{kn(math.hypot(*load[:2]))} kN; "
            f"Mz = {data['application_m'][0]:.1f} x {kn(load[1])} "
            f"- {data['application_m'][1]:.1f} x {kn(load[0])} = {kn(load[2])} kN m."
        )
    return ReportSpec(
        document=DocumentMeta(
            number="B0000-RPT-013-00",
            revision="00",
            title="Generic berth mooring analysis",
            project="Generic example",
            client="Not applicable",
            date="2026-10-20",
        ),
        provenance=Provenance().add(
            "calculation",
            "generic-berth-input",
            digest="sha256:" + manifest["input_sha256"],
            description="Explicit assumed inputs; no restricted model values",
        ),
        input_echo=spec,
        sections=[
            section(
                "summary",
                "Summary",
                TextBlock(
                    markdown=(
                        f"{len(manifest['cases'])} generic berth cases are prepared. Steady combined sway is **{kn(total[1])} kN** "
                        f"and yaw moment **{kn(total[2])} kN m**. These are applied loads. "
                        f"Line/fender response: **{response_status}**. Equilibrium and applied-load wiring "
                        "are **NOT_EVALUATED**. Preparation alone does not establish design acceptance."
                    )
                ),
            ),
            section(
                "revision",
                "Revision history",
                TableBlock(
                    title="",
                    columns=["Revision", "Date", "What changed and why"],
                    rows=[
                        [
                            "00",
                            "2026-10-20",
                            "Initial generic preparation to expose inputs, methods and pending response.",
                        ]
                    ],
                ),
            ),
            section(
                "objective",
                "Objective",
                TextBlock(
                    markdown=(
                        "Prepare a generic moored body at a berth with mooring links and fenders, "
                        "wind, current and a parallel passing-load history. The result is a "
                        "review package for later load wiring and licensed response analysis."
                    )
                ),
            ),
            section(
                "inputs",
                "Design data and assumptions",
                TableBlock(
                    title="", columns=["Input group", "Basis"], rows=assumptions
                ),
                TextBlock(
                    markdown=(
                        "The complete input echo follows the report. All physical inputs are "
                        "assumed, based on previous experience. No value from the provenance-unknown "
                        "mooring/fender model group is used. The vendor fender example was inspected "
                        "for object conventions only; no vendor model is redistributed. "
                        "The existing passing sample is a layout reference, not a parameter source."
                    )
                ),
            ),
            section(
                "method",
                "Analysis methodology",
                TextBlock(
                    markdown=(
                        "**Steady load:** F = rho Cd A |v| v / 2; Mz = x Fy - y Fx. "
                        "NASA Drag Equation, public web edition retrieved 2026-10-20. "
                        "The single supplied area and coefficient form an isotropic screen; "
                        "directional wind/current coefficients are not inferred. Velocity points toward the force.\n\n"
                        "**Passage:** stagger(t) = initial_stagger + speed t. The existing closed-form "
                        "passing-force implementation uses parabolic sectional areas and the finite-depth "
                        "image sum documented in the repository's 1975-method note. Its numerical settings "
                        "are explicit inputs. The passing track is +y and the quay is -y; yaw is right handed. "
                        "Forces are summed in a fixed initial body frame. Bank/channel effects, "
                        "current-passage interaction and moving-hull feedback are excluded.\n\n"
                        "**Mooring links:** T = EA (length - unstretched_length) / unstretched_length "
                        "in tension, otherwise zero. **Fenders:** linear interpolation of supplied "
                        "length/tension points, with negative tension indicating compression. Orcina Links: Data, "
                        "public web edition retrieved 2026-10-20. Zero damping is an explicit exclusion.\n\n"
                        "**Acceptance:** sampled maximum tensile/compressive magnitude divided by a supplied "
                        "allowable is utilisation. PASS requires peak <= allowable. Every allowable needs a "
                        "source. Missing allowable or response is NOT_EVALUATED. No licensed-standard limit is inferred."
                    )
                ),
            ),
            section(
                "answer",
                "Worked answer",
                TableBlock(
                    title="",
                    columns=["Steady contribution", "Surge kN", "Sway kN", "Yaw kN m"],
                    rows=[
                        [name, *[kn(x) for x in load]]
                        for name, load in [
                            ("Wind", wind),
                            ("Current", current),
                            ("Combined", total),
                        ]
                    ],
                ),
                TextBlock(
                    markdown=(
                        "Hand calculation from the supplied inputs (display rounded to one decimal): "
                        + " ".join(calculations)
                        + f" Combined sway = {kn(total[1])} kN and yaw = {kn(total[2])} kN m. "
                        "All numbers are example inputs, not recommended coefficients. "
                        "The table below adds computed passing loads at the specified samples. "
                        "Each component peak may occur at a different time."
                    )
                ),
            ),
            section(
                "cases",
                "Case table and sampled applied loads",
                TableBlock(
                    title="",
                    columns=[
                        "Case",
                        "Speed m/s",
                        "Separation m",
                        "Wind",
                        "Current",
                        "Peak surge kN",
                        "Peak sway kN",
                        "Peak yaw kN m",
                        "Coupled response",
                    ],
                    rows=rows,
                ),
            ),
            section(
                "response",
                "Stored line and fender loads",
                TextBlock(
                    markdown=f"Recorded solver version: {version}. Results are exported sample magnitudes."
                ),
                TableBlock(
                    title="Response against supplied criteria",
                    columns=[
                        "Case",
                        "Object",
                        "Peak kN",
                        "Allowable kN",
                        "Utilisation",
                        "Status",
                        "Criterion source",
                    ],
                    rows=response_rows,
                ),
            ),
            section(
                "validation",
                "Validation status",
                TextBlock(
                    markdown=(
                        "Independent hand checks cover steady drag, moment, time mapping, superposition "
                        "and utilisation. Passing-force symmetry and speed-squared scaling are checked; "
                        "the inherited module retains its existing regression tests. Generated native review "
                        "models pass repository structural YAML validation. This is not solver load attestation "
                        "or physical validation of the example. Sampling and image-sum convergence for this "
                        "example are NOT_EVALUATED. Stored results, when supplied, require the exact input digest "
                        "and one solver version; this checks consistency, not authenticity or solver convergence."
                    )
                ),
            ),
            section(
                "forward",
                "Way forward",
                TextBlock(
                    markdown=(
                        "Supply sourced allowable line tensions, fender reactions and any displacement or "
                        "compression limits. Qualify hydrodynamic inertia/damping and initial equilibrium. "
                        "Wire the CSV loads to vessel applied loads, converting N to kN and N m to kN m, "
                        "and retain the fixed global axes during rotations. Load and solve the twelve cases "
                        "only under separate authorisation on a recorded solver version. Export nonnegative "
                        "line tensile/fender compression magnitudes in kN for every case and object; the "
                        "stored-result evaluator checks completeness and supplied limits. Record sampling, "
                        "time-step and image-sum convergence before treating sampled peaks as governing."
                    )
                ),
            ),
            section("refs", "Sources", TextBlock(markdown=source_lines)),
        ],
    )


def write_example(
    input_path: Path, destination: Path, *, stored: dict | None = None
) -> None:
    """Build the campaign, case table and report without any solver binding."""
    spec = load_spec(input_path)
    destination = Path(destination)
    manifest = write_campaign(spec, destination, stored=stored)
    with (destination / "case-table.csv").open("w", newline="") as stream:
        fields = [
            "name",
            "speed_m_s",
            "separation_m",
            "wind",
            "current",
            "model",
            "history",
            "coupled_response",
            "applied_load_wiring",
        ]
        writer = csv.DictWriter(
            stream, fieldnames=fields, extrasaction="ignore", lineterminator="\n"
        )
        writer.writeheader()
        writer.writerows(manifest["cases"])
    (destination / "report.html").write_text(render_html(build_report(spec, manifest)))
    # The report and case table are also content-addressed; no absolute paths.
    for name in ("report.html", "case-table.csv"):
        manifest["artifacts"][name] = hashlib.sha256(
            (destination / name).read_bytes()
        ).hexdigest()
    (destination / "manifest.json").write_text(
        json.dumps(manifest, indent=2, allow_nan=False) + "\n"
    )


def main() -> None:
    """Write the example to an explicitly selected output folder."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument(
        "--results", type=Path, help="Exported result JSON; no solver is invoked"
    )
    args = parser.parse_args()
    stored = None if args.results is None else json.loads(args.results.read_text())
    write_example(args.input, args.output, stored=stored)


if __name__ == "__main__":
    main()
