# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Render synthetic tank and pressure-vessel examples through the house engine.

Run with --output DIRECTORY; --compact produces small repository pages whose
charts load the installed Plotly release's JavaScript from its public CDN.
Without --compact, reports embed JavaScript and work without network access.
Neither mode fetches any data or starts a browser or an engineering solver.

No API material or floor table is reproduced. Input values without a qualified
source are labelled assumed and retain NOT_EVALUATED code qualification.
"""

from __future__ import annotations

import argparse
from dataclasses import asdict
import hashlib
import json
from pathlib import Path

from plotly.offline.offline import get_plotlyjs_version

from digitalmodel.ansys.pressure_vessel import DesignConditions, VesselGeometry
from digitalmodel.ansys.pressure_vessel_report import vessel_report
from digitalmodel.code_checks.storage_tanks import (
    MaterialAllowables,
    OneFootBasis,
    TankDesign,
    design_shell,
)
from digitalmodel.code_checks.storage_tanks.report import tank_report
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.figures import inline_plotlyjs
from digitalmodel.reporting.spec import DocumentMeta, RevisionRow, Section, TextBlock


ASSUMED = (
    "assumed, based on previous experience; illustrative input, not a code-table value"
)


def _document(title: str, sequence: str, *, revision: str) -> DocumentMeta:
    return DocumentMeta(
        number=f"T0000-RPT-{sequence}-{revision}",
        revision=revision,
        title=title,
        project="Generic shell sizing",
        client="Public illustration",
        date="2026-10-21",
        revision_history=[
            RevisionRow(
                rev="00",
                date="2026-10-19",
                description=(
                    "Initial illustration of shell sizing, sources, hand arithmetic and governing cases."
                ),
            ),
            *(
                [RevisionRow(
                    rev="01",
                    date="2026-10-19",
                    description=(
                        "Use design liquid level in both sizing cases and distinguish "
                        "pressure-governed utilisation from the nominal floor."
                    ),
                )]
                if sequence == "001" else []
            ),
            RevisionRow(
                rev=revision,
                date="2026-10-21",
                description=(
                    "Show separate applicability evidence and selected plate margins in chart hover data."
                    if sequence == "001" else
                    "Show shell and head nominal limits and margins in chart hover data."
                ),
            ),
        ],
    )


def example_specs():
    """Return report views and the unrounded synthetic tank calculation.

    Method c=4.9 and a=0.3 m: public issue 2958, part A.1 (2026-10-19;
    API 650 5.6.3, edition unverified). All other numbers below are assumed
    scenario inputs, never a claim about A36 or an API acceptance limit.
    """
    inputs = TankDesign(
        diameter_m=30,
        course_heights_m=(2.25,) * 8,
        design_level_m=18,
        test_level_m=18,
        specific_gravity=1,
        corrosion_allowance_mm=1.5,
        density_kg_m3=7800,
        plate_sizes_mm=(6, 8, 10, 12, 14, 16, 18, 20),
        material=MaterialAllowables("Illustrative steel", 150, 160, ASSUMED),
        basis=OneFootBasis(
            4.9, 0.3, 60, 6, "Public issue 2958 equation; limits and floor " + ASSUMED
        ),
    )
    result = design_shell(inputs)
    tank = tank_report(
        result,
        _document("Atmospheric tank shell analysis", "001", revision="02"),
        water_unit_weight_kn_m3=9.8,
        pressure_source=ASSUMED,
    )
    tank.sections.insert(
        4,
        Section(
            key="hand-check",
            title="Independent hand calculation",
            blocks=[
                TextBlock(
                    markdown=(
                        "This 30 m diameter, 18 m liquid-height example uses eight equal 2.25 m courses. "
                        "The design allowable is an assumed 150 MPa and the test allowable an assumed "
                        "160 MPa; neither is an A36 material-table lookup. The 6.000 mm floor and "
                        "60 m limit are assumed illustration inputs, not established API limits.\n\n"
                        "Bottom course: H = 18 m. Design wall = 4.9 * 30 * (18 - 0.3) * 1 / 150 "
                        "+ 1.500 = 18.846 mm. Test wall = 4.9 * 30 * (18 - 0.3) / 160 = 16.262 mm. "
                        "Design governs the 6.000 mm nominal floor, and the next supplied plate is 20.000 mm.\n\n"
                        "Course 7 starts at 6 * 2.25 = 13.5 m, so H = 4.5 m. Design wall = "
                        "4.9 * 30 * (4.5 - 0.3) / 150 + 1.500 = 5.616 mm. Test wall = "
                        "4.9 * 30 * (4.5 - 0.3) / 160 = 3.859 mm. The 6.000 mm floor governs.\n\n"
                        "At 3 significant figures, the hand and program design values are 18.8 mm "
                        "and 5.62 mm respectively; the unrounded arithmetic is checked in the tests. "
                        "The selected course plates sum to 100.000 mm. Nominal shell mass = "
                        "pi * 30 * 2.25 * (100 / 1000) * 7800 kg; this is a thin-shell estimate.\n\n"
                        "Way forward: obtain an applicable API 650 edition, its diameter limit, "
                        "the floor for this diameter, and material allowables with temperature and "
                        "thickness applicability. Supply resolved citations before using the result "
                        "as a qualified engineering input."
                    )
                ),
            ],
        ),
    )
    vessel = vessel_report(
        geometry=VesselGeometry(
            inner_diameter_mm=200,
            shell_length_mm=500,
            wall_thickness_mm=12,
            head_thickness_mm=12,
            corrosion_allowance_mm=2,
        ),
        conditions=DesignConditions(
            design_pressure_mpa=10,
            allowable_stress_mpa=106,
            joint_efficiency=1,
            material_name="Illustrative steel",
        ),
        document=_document("Pressure vessel shell analysis", "002", revision="01"),
        test_factor=1.5,
        test_allowable_mpa=120,
        pressure_points_mpa=(0, 2, 4, 6, 8, 10),
        source=ASSUMED
        + "; equations inherited from ASMEThicknessCalc (edition unspecified)",
    )
    return {
        "one-foot-worked-example.html": tank,
        "pressure-vessel-example.html": vessel,
    }, result


def generate(output: Path, *, compact: bool = False) -> list[Path]:
    """Write two report views and digests; deterministic, with no current clock."""
    output = Path(output).resolve()
    output.mkdir(parents=True, exist_ok=True)
    specs, result = example_specs()
    paths, digests = [], {}
    for filename, spec in specs.items():
        delivery = (
            (
                "Compact distribution: interactive charts require access to the public Plotly CDN. "
                "The default generator output embeds JavaScript for offline use."
            )
            if compact
            else ("Self-contained distribution: charts require no external JavaScript.")
        )
        spec.sections.append(
            Section(
                key="distribution",
                title="Distribution",
                blocks=[
                    TextBlock(markdown=delivery),
                ],
            )
        )
        rendered = render_html(spec)
        if compact:
            bundle = inline_plotlyjs()
            tag = f'<script type="text/javascript">{bundle}</script>'
            if rendered.count(tag) != 1:
                raise ValueError("expected exactly one embedded Plotly bundle")
            # Version comes from the installed dependency, never a guessed CDN release.
            url = f"https://cdn.plot.ly/plotly-{get_plotlyjs_version()}.min.js"
            rendered = rendered.replace(tag, f'<script src="{url}"></script>', 1)
        path = output / filename
        path.write_bytes(rendered.encode("utf-8"))
        if path.read_bytes() != rendered.encode("utf-8"):
            raise OSError("report readback differs from rendered bytes")
        digests[filename] = hashlib.sha256(path.read_bytes()).hexdigest()
        paths.append(path)
    manifest = {
        "schema": "shell-thickness-examples/v1",
        "status": "NOT_EVALUATED",
        "data_basis": ASSUMED,
        "source": "Public issue 2958; standard edition unverified",
        "qualified_engineering_input": False,
        "compact": compact,
        "inputs": specs["one-foot-worked-example.html"].input_echo,
        "courses": [asdict(row) for row in result.courses],
        "shell_mass_kg": result.shell_mass_kg,
        "citations": result.citation_sidecar()["citations"],
        "not_evaluated": list(result.not_evaluated),
        "sha256": digests,
    }
    path = output / "shell-thickness-examples.json"
    path.write_text(
        json.dumps(manifest, indent=2, sort_keys=True, allow_nan=False) + "\n",
        encoding="utf-8",
        newline="\n",
    )
    if json.loads(path.read_text(encoding="utf-8")) != manifest:
        raise OSError("manifest readback differs from source values")
    return [*paths, path]


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--compact", action="store_true")
    arguments = parser.parse_args()
    generate(arguments.output, compact=arguments.compact)
