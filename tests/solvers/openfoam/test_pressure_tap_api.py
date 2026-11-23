#!/usr/bin/env python3
"""
ABOUTME: Source-neutral public-surface tests for the OpenFOAM package (dm#1574).
Pins the exported namespace to an exact tuple so a project-coded symbol cannot be
reintroduced without failing by construction, and covers the neutral rectangular
tank wall-tap factory that replaced the project-coded default-tap helper. All
geometry here is synthetic and encodes no real tank.
"""

import math

import pytest

import digitalmodel.solvers.openfoam as openfoam
from digitalmodel.solvers.openfoam.pressure_taps import rectangular_tank_wall_taps

# The exact public surface of the package. This is an identity assertion, not a
# containment check: any added, removed or renamed export fails here, which is
# what makes a reintroduced project-coded symbol impossible to land silently.
EXPECTED_ALL = (
    "BoundaryCondition",
    "BoundaryType",
    "CaseType",
    "DomainConfig",
    "DomainBuilder",
    "MotionType",
    "OpenFOAMCase",
    "OpenFOAMCaseBuilder",
    "PrescribedMotion",
    "render_dynamic_mesh_dict",
    "render_dynamic_mesh_dict_body",
    "write_dynamic_mesh_dict",
    "OpenFOAMRunConfig",
    "OpenFOAMRunner",
    "OpenFOAMRunResult",
    "OpenFOAMRunStatus",
    "StageResult",
    "SolverConfig",
    "TurbulenceModel",
    "TurbulenceType",
    "SloshingFrequencyResult",
    "SpectralPeak",
    "SpectrumResult",
    "compute_fft_spectrum",
    "compute_welch_spectrum",
    "extract_natural_frequency",
    "prismatic_tank_natural_frequency",
    "PressureTap",
    "PressureTapStatistics",
    "rectangular_tank_wall_taps",
    "compute_tap_statistics",
    "point_tap_names",
    "read_tap_statistics",
    "render_patch_probes_entry",
    "render_pressure_tap_functions",
    "render_probes_entry",
    "render_surface_entry",
    "sloshing_natural_frequency",
    "CouplingStrengthReport",
    "FillDampingResult",
    "MomentCoefficients",
    "SloshingCase",
    "SloshingCouplingModel",
    "TuningReport",
    "ConduitGeometry",
    "G_STANDARD",
    "GravityExchangeResult",
    "TankState",
    "check_transfer_feasibility",
    "conduit_flow_rate",
    "signed_hydrostatic_head",
    "simulate_gravity_exchange",
    "simulate_inertial_exchange",
    "utube_natural_frequency",
    "NaturalPeriodResult",
    "SweepCase",
    "SweepConfig",
    "SweepManifest",
    "first_sloshing_natural_period",
    "generate_sweep",
    "period_ratio",
    "ExtractionConfig",
    "RawCFDOutputs",
    "SynchronizedTimeHistory",
    "TimeSeriesChannel",
    "ValidationFlags",
    "extract_time_history",
    "phase_lag_deg",
    "validate_synchronized_time",
    "REQUIRED_SECTIONS",
    "render_aggregate_report",
    "render_case_report",
    "CaseManifest",
    "CouplingSpec",
    "SyntheticCase",
    "VerificationResult",
    "build_case_manifest",
    "emit_synthetic_gravity_exchange_case",
    "map_sweep_case_to_openfoam_case",
    "peak_flow_rate",
    "synthetic_coupling_spec",
    "synthetic_sweep_case",
    "transfer_volume",
    "verify_coupling",
)

# Suffix of the removed project-coded factory. Naming the suffix rather than the
# coded spelling keeps the protected value out of this repository while still
# detecting the symbol shape.
_REMOVED_FACTORY_SUFFIX = "_default_taps"


class TestNeutralPublicSurface:
    """The package must export no project-coded symbol."""

    def test_coded_export_absent(self):
        """No export whose name carries the removed factory shape."""
        matches = [n for n in openfoam.__all__ if n.endswith(_REMOVED_FACTORY_SUFFIX)]
        assert matches == []

    def test_public_all_matches_expected_tuple(self):
        """__all__ is exactly the expected tuple, ordering included."""
        assert tuple(openfoam.__all__) == EXPECTED_ALL

    def test_no_sensitive_compatibility_alias(self):
        """No module attribute survives with the removed factory shape.

        ``__all__`` alone is not enough: a re-import or a deprecation alias
        would still be reachable as a module attribute without being exported.
        """
        matches = [n for n in dir(openfoam) if n.endswith(_REMOVED_FACTORY_SUFFIX)]
        assert matches == []

    def test_every_exported_name_resolves(self):
        """Each exported name is actually reachable on the package."""
        missing = [n for n in openfoam.__all__ if not hasattr(openfoam, n)]
        assert missing == []


class TestRectangularTankWallTaps:
    """The neutral replacement factory, on synthetic geometry only."""

    def test_emits_two_taps_per_elevation(self):
        taps = rectangular_tank_wall_taps(
            tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(1.0, 2.0, 3.0)
        )
        assert len(taps) == 6

    def test_returns_a_tuple(self):
        taps = rectangular_tank_wall_taps(
            tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(1.0,)
        )
        assert isinstance(taps, tuple)

    def test_names_are_neutral_and_sequential(self):
        taps = rectangular_tank_wall_taps(
            tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(1.0, 2.0)
        )
        assert [t.name for t in taps] == ["wall_1", "wall_2", "wall_3", "wall_4"]

    def test_neutral_tap_api_preserves_geometry(self):
        """Exact synthetic coordinates: mid-length, both side walls, per elevation."""
        taps = rectangular_tank_wall_taps(
            tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(1.0, 2.5)
        )
        assert [t.location for t in taps] == [
            (4.0, 0.0, 1.0),
            (4.0, 4.0, 1.0),
            (4.0, 0.0, 2.5),
            (4.0, 4.0, 2.5),
        ]

    def test_all_taps_are_point_taps(self):
        taps = rectangular_tank_wall_taps(
            tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(1.0, 2.0)
        )
        assert {t.kind for t in taps} == {"point"}

    def test_multiphase_fields_by_default(self):
        taps = rectangular_tank_wall_taps(
            tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(1.0,)
        )
        assert taps[0].fields == ("p", "p_rgh")

    def test_fields_are_honoured(self):
        taps = rectangular_tank_wall_taps(
            tank_length_m=8.0,
            tank_width_m=4.0,
            tap_elevations_m=(1.0,),
            fields=("p",),
        )
        assert taps[0].fields == ("p",)

    def test_is_deterministic(self):
        kwargs = dict(
            tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(1.0, 2.0)
        )
        assert rectangular_tank_wall_taps(**kwargs) == rectangular_tank_wall_taps(
            **kwargs
        )


class TestRectangularTankWallTapsValidation:
    """Each invalid input raises, one exact condition per test."""

    def test_zero_length_raises(self):
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=0.0, tank_width_m=4.0, tap_elevations_m=(1.0,)
            )

    def test_negative_width_raises(self):
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=8.0, tank_width_m=-4.0, tap_elevations_m=(1.0,)
            )

    def test_non_finite_length_raises(self):
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=math.inf, tank_width_m=4.0, tap_elevations_m=(1.0,)
            )

    def test_nan_width_raises(self):
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=8.0, tank_width_m=math.nan, tap_elevations_m=(1.0,)
            )

    def test_empty_elevations_raise(self):
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=()
            )

    def test_floor_elevation_raises(self):
        """Zero is the tank floor and is not strictly interior."""
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(0.0,)
            )

    def test_negative_elevation_raises(self):
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(-1.0,)
            )

    def test_non_finite_elevation_raises(self):
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(math.inf,)
            )

    def test_duplicate_elevations_raise(self):
        with pytest.raises(ValueError):
            rectangular_tank_wall_taps(
                tank_length_m=8.0, tank_width_m=4.0, tap_elevations_m=(1.0, 1.0)
            )
