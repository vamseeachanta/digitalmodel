"""Native-unit invariant for the direct-from-OrcFxAPI report path (#1550 W5).

There are two distinct paths out of OrcFxAPI, and after #1550 they deliberately
use different unit bases:

  orcawave_runner._build_results_from_object  -> DiffractionResults, SI/kg
      converts te -> kg, because DiffractionResults is an SI type shared with
      aqwa_converter, solver.orcawave_converter and wamit_reference_loader.

  solver.report_extractors.extract_report_data_from_owr -> report dataclasses
      stays in OrcFxAPI-native te / kN. It never constructs DiffractionResults.

Native is correct for the report path *provided it stays internally consistent*.
`compute_natural_periods` evaluates

    T_n = 2*pi * sqrt((M_ii + A_ii) / C_ii)

where M_ii is the hydrostatic inertia matrix and A_ii the added mass. Those are
SUMMED, so they must share a basis. C_ii then divides the sum, and the ratio is
basis-independent only because te/kN carries the same 1000 factor as kg/N.

The hazard this module exists to catch: converting added mass to kg (as W2 does
on the other path) while leaving the hydrostatic inertia in te. That is not a
mislabel - it is a numerically wrong natural period, and nothing else would
signal it.
"""
from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.report_computations import (
    compute_natural_periods,
)


# Grid chosen so the sweep intersects exactly at 1.0 rad/s => T = 2*pi s.
FREQ_RAD_S = [0.5, 1.0, 1.5]

# Heave, consistent OrcFxAPI-native tonnes.
M_HEAVE_TE = 1000.0
A_HEAVE_TE = [600.0, 500.0, 400.0]
# c = m + a(omega_n) so that omega_n_est[1] == 1.0 exactly.
C_HEAVE = M_HEAVE_TE + A_HEAVE_TE[1]

EXPECTED_T_HEAVE = 2.0 * math.pi  # 6.2832 s


class _Hydrostatics:
    """Minimal stand-in exposing the two matrices the computation reads."""

    def __init__(self, inertia_scale: float = 1.0) -> None:
        inertia = np.zeros((6, 6), dtype=float)
        inertia[2, 2] = M_HEAVE_TE * inertia_scale
        restoring = np.zeros((6, 6), dtype=float)
        restoring[2, 2] = C_HEAVE
        self.inertia_matrix = inertia
        self.restoring_matrix = restoring


def _added_mass_diag(scale: float = 1.0):
    return {
        "surge": [0.0] * 3,
        "sway": [0.0] * 3,
        "heave": [a * scale for a in A_HEAVE_TE],
        "roll": [0.0] * 3,
        "pitch": [0.0] * 3,
        "yaw": [0.0] * 3,
    }


class TestNaturalPeriodConsistentBasis:
    """With one consistent basis the period matches the closed form."""

    def test_heave_period_matches_analytic(self) -> None:
        periods = compute_natural_periods(
            _Hydrostatics(), _added_mass_diag(), FREQ_RAD_S,
        )
        assert periods["heave"] == pytest.approx(EXPECTED_T_HEAVE, rel=1e-9)

    def test_fully_converting_to_si_leaves_the_period_unchanged(self) -> None:
        # The real invariant: convert M, A *and* C together (te -> kg,
        # kN -> N) and the 1000 cancels in (M + A) / C.
        hydro = _Hydrostatics(inertia_scale=1000.0)
        hydro.restoring_matrix = hydro.restoring_matrix * 1000.0

        si = compute_natural_periods(
            hydro, _added_mass_diag(scale=1000.0), FREQ_RAD_S,
        )
        assert si["heave"] == pytest.approx(EXPECTED_T_HEAVE, rel=1e-9)

    def test_converting_mass_terms_but_not_restoring_matrix_breaks_it(
        self,
    ) -> None:
        # Partial conversion in the other direction: M and A go to SI, C is
        # left in kN. The sum grows 1000x against an unchanged denominator.
        partial = compute_natural_periods(
            _Hydrostatics(inertia_scale=1000.0),
            _added_mass_diag(scale=1000.0),
            FREQ_RAD_S,
        )
        assert partial["heave"] != pytest.approx(EXPECTED_T_HEAVE, rel=1e-6)


class TestMixedBasisIsDetected:
    """The W5 hazard: convert added mass, forget the hydrostatic inertia."""

    def test_added_mass_in_kg_against_inertia_in_te_changes_the_period(
        self,
    ) -> None:
        mixed = compute_natural_periods(
            _Hydrostatics(),                 # inertia still tonnes
            _added_mass_diag(scale=1000.0),  # added mass converted to kg
            FREQ_RAD_S,
        )

        # The sweep collapses to the lowest grid frequency: omega_n_est falls
        # ~30x, so the nearest grid point is 0.5 rad/s rather than 1.0.
        assert mixed["heave"] == pytest.approx(2.0 * math.pi / 0.5, rel=1e-9)
        assert mixed["heave"] != pytest.approx(EXPECTED_T_HEAVE, rel=1e-6)

    def test_inertia_in_kg_against_added_mass_in_te_changes_the_period(
        self,
    ) -> None:
        # The mirror error, in case only the hydrostatics get converted.
        mixed = compute_natural_periods(
            _Hydrostatics(inertia_scale=1000.0),
            _added_mass_diag(),
            FREQ_RAD_S,
        )
        assert mixed["heave"] != pytest.approx(EXPECTED_T_HEAVE, rel=1e-6)


class TestReportPathStaysNative:
    """Structural fence: the report path must not import the SI converter."""

    def test_report_extractors_does_not_convert_mass(self) -> None:
        from digitalmodel.hydrodynamics.diffraction.solver import (
            report_extractors,
        )

        source = report_extractors.__file__
        with open(source, "r", encoding="utf-8") as handle:
            text = handle.read()

        # If a future change converts added mass here, it must also convert the
        # hydrostatic inertia and the restoring matrix - and update this test
        # plus the module docstring's units contract. Failing loudly is the
        # point; a silent half-conversion is the defect.
        assert "tonnes_to_kg" not in text, (
            "report_extractors reads OrcFxAPI directly and is native-units "
            "(te/kN). Introducing a mass conversion here without also "
            "converting hydrostaticResults' inertiaMatrix and restoringMatrix "
            "produces a numerically wrong natural period. See #1550 W5."
        )
