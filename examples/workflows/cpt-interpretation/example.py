# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Print a synthetic clay-over-sand CPT profile; no files or solvers are opened."""

from digitalmodel.geotechnical.cpt import (
    CPTData,
    DrainageBands,
    ProfileProvenance,
    SandConditions,
    assess_drainage,
    correct_cpt,
    normalise_cpt,
    sand_state,
    to_database_rows,
    undrained_strength,
)
from digitalmodel.geotechnical.database_schema import Source


def build_example(*, bands: DrainageBands | None = None) -> dict:
    """Calculate 20 point observations; every geological input is synthetic.

    All numbers below define this illustration, not default engineering values.
    The equations and their coefficients are cited by the interpretation module.
    Undrained clay and drained sand are explicit, unverified assumptions used
    only for conditional strength/state estimates, independently of the V flag.
    """
    depths = tuple(range(1, 21))
    clay = tuple(z <= 10 for z in depths)
    data = CPTData(
        depth_m=depths,
        qc=[68 * z if fine else 8000 + 300 * z for z, fine in zip(depths, clay)],
        fs=[3 * z if fine else 20 for z, fine in zip(depths, clay)],
        u2=[50 * z if fine else 10 * z for z, fine in zip(depths, clay)],
        area_ratio=0.8,
        pressure_unit="kPa",
    )
    corrected = correct_cpt(
        data,
        total_stress_pa=[18000 * z for z in depths],
        equilibrium_pore_pressure_pa=[10000 * z for z in depths],
    )
    normalised = normalise_cpt(corrected)
    drainage = tuple(
        assess_drainage(
            row,
            penetration_rate_m_s=0.02,
            cone_diameter_m=0.04,
            consolidation_coefficient_m2_s=1e-7 if fine else 0.01,
            consolidation_basis="vertical",
            bands=bands,
        )
        for row, fine in zip(corrected, clay)
    )
    strengths = tuple(
        undrained_strength(row, nkt=15, undrained=fine)
        for row, fine in zip(corrected, clay)
    )
    states = tuple(
        sand_state(
            point,
            conditions=SandConditions(
                clean=not fine,
                quartz=not fine,
                uncemented=not fine,
                unaged=not fine,
                rounded=not fine,
                drained=not fine,
                medium_compressibility=not fine,
            ),
        )
        for point, fine in zip(normalised, clay)
    )
    source = Source(
        source_id="synthetic-cpt-example",
        citation="Synthetic 20 m clay-over-sand CPT input profile",
        access="examples/workflows/cpt-interpretation/example.py",
        reuse_terms="Illustrative numerical assumptions; no additional data reuse terms stated",
        redistribution="not-stated",
        read_status="read",
    )
    provenance = ProfileProvenance(
        row_prefix="synthetic-cpt",
        soil_unit="synthetic clay over sand",
        site_or_region="synthetic",
        source=source,
        locator="build_example inputs",
        licence="NS",
        confidence_grade="C",
    )
    rows = to_database_rows(
        normalised,
        drainage=drainage,
        provenance=provenance,
        strengths=strengths,
        sand_states=states,
    )
    return dict(
        data=data,
        corrected=corrected,
        normalised=normalised,
        drainage=drainage,
        strengths=strengths,
        sand_states=states,
        database_rows=rows,
    )


def render_example(example: dict) -> str:
    """Print inputs, steps and rounded answers; retain full precision in results."""
    lines = [
        "CPT interpretation analysis: synthetic 20 m profile",
        "All geological inputs are illustrative assumptions, not measured site data.",
        "Inputs: depth m; qc, fs, u2, total stress and u0 in kPa; area ratio=0.8.",
        "z | qc | fs | u2 | total | u0",
    ]
    for row in example["corrected"]:
        lines.append(
            " | ".join(
                f"{v:.1f}"
                for v in (
                    row.depth_m,
                    row.qc_pa / 1000,
                    row.fs_pa / 1000,
                    row.u2_pa / 1000,
                    row.total_stress_pa / 1000,
                    row.equilibrium_pore_pressure_pa / 1000,
                )
            )
        )
    lines.extend(
        [
            "Method: qt=qc+u2(1-a); qn=qt-total; Rf=100fs/qt; Bq=(u2-u0)/qn.",
            "Method: Qtn=(qn/pa)(pa/effective)^n; Fr=100fs/qn; iterate Ic and n.",
            "Method: V=v*d/cv; v=20.0 mm/s; d=40.0 mm; cv=1.0e-7 clay, 1.0e-2 sand m2/s.",
            (
                "Method: no published bands supplied; drainage NOT_EVALUATED."
                if all(check.drained_max is None for check in example["drainage"])
                else "Method: regimes use explicitly supplied drainage bands and their stated basis."
            ),
            "Method: no readable published backbone fit; correction NOT_EVALUATED.",
            "Method: conditional Su=qn/Nkt (nkt=15.0); Dr=sqrt(Qtn/350); phi=17.6+11log10(Qtn).",
            "Undrained clay and drained eligible clean sand are unverified assumptions for conditional estimates.",
            "Answer: unrounded values select zones; printed values have one decimal place.",
            "z m | qt kPa | qn kPa | Rf % | Bq | Qtn | Fr % | Ic | n | zone | V | drainage | conditional Su kPa | conditional Dr % | conditional phi deg",
        ]
    )

    def number(value: float | None) -> str:
        return "NOT_EVALUATED" if value is None else f"{value:.1f}"

    for point, check, strength, state in zip(
        example["normalised"],
        example["drainage"],
        example["strengths"],
        example["sand_states"],
    ):
        row = point.corrected
        values = [
            row.depth_m,
            row.qt_pa / 1000,
            row.qn_pa / 1000,
            row.rf_percent,
            row.bq,
            point.qtn,
            point.fr_percent,
            point.ic,
            point.stress_exponent,
        ]
        cells = [number(value) for value in values]
        cells.extend(
            [point.zone.split(":")[0], number(check.normalised_velocity), check.regime]
        )
        cells.extend(
            number(v)
            for v in (
                None if strength.value is None else strength.value / 1000,
                None
                if state.relative_density.value is None
                else 100 * state.relative_density.value,
                state.friction_angle.value,
            )
        )
        lines.append(" | ".join(cells))
    lines.append(
        f"Database: {len(example['database_rows'])} provenance-bearing parameter rows; point observations only."
    )
    lines.append(
        "Way forward: qualify site stresses, material assumptions, Nkt, drainage bands and backbone fit before design use."
    )
    return "\n".join(lines) + "\n"


if __name__ == "__main__":
    print(render_example(build_example()), end="")
