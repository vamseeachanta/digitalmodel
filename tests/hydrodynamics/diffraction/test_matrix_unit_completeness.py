"""Every cell of an exported 6x6 matrix must carry a unit (#1550 W4).

A 6x6 added-mass matrix has three dimensionally distinct blocks:

    rows 0-2, cols 0-2   linear-linear     9 cells    kg
    rows 0-2, cols 3-5   linear-angular   18 cells    kg.m     <- the coupling
    rows 3-5, cols 0-2                                            block
    rows 3-5, cols 3-5   angular-angular   9 cells    kg.m^2

`polars_exporter._build_matrix_records` derives exactly that three-way key
("linear" / "angular" / "coupling") and looks it up with
``matrix.units.get(unit_key, "")``.

Producers disagree on the dict shape, so that default silently fires:

    orcawave_runner, orcawave_to_orcaflex   {"linear", "angular"}
        -> the 18 coupling cells export with unit ""
    run_3way_benchmark, validate_owd_vs_spec {"coupling"}
        -> the 18 linear/angular cells export with unit ""
    aqwa_converter, solver.orcawave_converter {"linear-linear",
                                               "linear-angular",
                                               "angular-angular"}
        -> NO key the exporter looks for; all 36 cells export with unit ""

`orcaflex_exporter._get_added_mass_unit(i, j)` already implements the correct
block semantics and is the reference this converges on.

This module asserts completeness at the producer/consumer seam rather than
pinning one dict shape, so it stays true however the convergence is spelt.
"""
from __future__ import annotations

from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.orcawave_runner import OrcaWaveRunner
from digitalmodel.hydrodynamics.diffraction.polars_exporter import (
    PolarsExporter,
)


N_FREQ = 3
N_HEAD = 2


def _fake_diffraction() -> SimpleNamespace:
    added_mass = np.zeros((N_FREQ, 6, 6), dtype=float)
    damping = np.zeros((N_FREQ, 6, 6), dtype=float)
    for k in range(N_FREQ):
        # Populate every block so no cell is trivially skippable.
        added_mass[k] = np.full((6, 6), 100.0 + k)
        damping[k] = np.full((6, 6), 5.0 + k)
    return SimpleNamespace(
        frequencies=np.array([0.30, 0.20, 0.10], dtype=float),
        headings=np.array([0.0, 90.0], dtype=float),
        addedMass=added_mass,
        damping=damping,
        displacementRAOs=np.zeros((N_HEAD, N_FREQ, 6), dtype=complex),
    )


@pytest.fixture
def exporter() -> PolarsExporter:
    runner = object.__new__(OrcaWaveRunner)
    runner._result = SimpleNamespace(spec_name="UnitCompletenessVessel")
    runner._water_depth = 200.0
    runner._extract_hydrostatics = lambda *a, **k: None
    results = runner._build_results_from_object(_fake_diffraction(), [])
    return PolarsExporter(results)


@pytest.mark.parametrize("matrix_type", ["added_mass", "damping"])
class TestExportedUnitsAreComplete:
    """No exported cell may carry an empty unit string."""

    def test_no_cell_exports_a_blank_unit(
        self, exporter: PolarsExporter, matrix_type: str,
    ) -> None:
        records = exporter._build_matrix_records(matrix_type)
        blank = [r for r in records if not r["unit"]]

        assert not blank, (
            f"{len(blank)} of {len(records)} {matrix_type} cells exported with "
            f'unit="" - the producer omits a key the exporter looks up. '
            f"Example: {blank[0]['dof_i']}-{blank[0]['dof_j']}"
        )

    def test_coupling_block_is_distinctly_labelled(
        self, exporter: PolarsExporter, matrix_type: str,
    ) -> None:
        # The linear-angular block is dimensionally distinct from both
        # diagonals; labelling it as either one is wrong, not just terse.
        records = {
            (r["dof_i"], r["dof_j"]): r["unit"]
            for r in exporter._build_matrix_records(matrix_type)
        }
        linear = records[("SURGE", "SURGE")]
        angular = records[("ROLL", "ROLL")]
        coupling = records[("SURGE", "ROLL")]

        assert coupling not in ("", linear, angular), (
            f"coupling unit {coupling!r} must differ from linear {linear!r} "
            f"and angular {angular!r}"
        )

    def test_all_three_blocks_are_internally_consistent(
        self, exporter: PolarsExporter, matrix_type: str,
    ) -> None:
        records = exporter._build_matrix_records(matrix_type)
        by_block: dict[str, set[str]] = {"linear": set(), "angular": set(), "coupling": set()}
        order = ["SURGE", "SWAY", "HEAVE", "ROLL", "PITCH", "YAW"]
        for r in records:
            i = order.index(r["dof_i"])
            j = order.index(r["dof_j"])
            key = (
                "linear" if i < 3 and j < 3
                else "angular" if i >= 3 and j >= 3
                else "coupling"
            )
            by_block[key].add(r["unit"])

        for block, units in by_block.items():
            assert len(units) == 1, (
                f"{matrix_type} {block} block exported inconsistent units: {units}"
            )
