"""
Performance Benchmark Tests for Marine Engineering Modules

Uses pytest-benchmark for regression testing and performance tracking.
Run with: pytest tests/marine_engineering/test_performance.py --benchmark-only

Targets:
- Wave spectrum generation: <10ms
- OCIMF coefficient lookup: <1ms
- Catenary solver: <10ms per line
- Hydro coefficient interpolation: <5ms
- Complete mooring analysis: <100ms
- End-to-end workflow: <5s
"""

import pytest
import numpy as np
from pathlib import Path
import tempfile

from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra
from digitalmodel.marine_ops.marine_analysis.environmental_loading.ocimf import (
    OCIMFDatabase, EnvironmentalForces, EnvironmentalConditions,
    VesselGeometry, create_sample_database
)
from digitalmodel.marine_ops.marine_analysis.catenary.solver import CatenarySolver, CatenaryInput
from digitalmodel.marine_ops.marine_analysis.hydrodynamic_coefficients.coefficients import (
    CoefficientDatabase, FrequencyDependentMatrix
)


@pytest.fixture(scope="session")
def ocimf_database(tmp_path_factory):
    """Create OCIMF database for testing."""
    db_dir = tmp_path_factory.mktemp("ocimf_data")
    db_path = db_dir / "test_ocimf.csv"
    create_sample_database(str(db_path), num_vessels=5,
                          num_headings=13, num_displacements=3)
    return OCIMFDatabase(str(db_path))


@pytest.fixture(scope="session")
def hydro_database():
    """Create hydrodynamic coefficient database for testing."""
    db = CoefficientDatabase()

    # Generate sample data
    frequencies = np.linspace(0.1, 2.0, 20)
    for freq in frequencies:
        # Added mass matrix
        matrix_a = np.random.rand(6, 6) * 1e6
        matrix_a = (matrix_a + matrix_a.T) / 2  # Make symmetric
        db.added_mass_matrices[freq] = FrequencyDependentMatrix(
            frequency=freq,
            matrix=matrix_a,
            matrix_type='added_mass'
        )

        # Damping matrix
        matrix_d = np.random.rand(6, 6) * 1e5
        matrix_d = (matrix_d + matrix_d.T) / 2
        db.damping_matrices[freq] = FrequencyDependentMatrix(
            frequency=freq,
            matrix=matrix_d,
            matrix_type='damping'
        )

    db.frequencies = frequencies
    return db


class TestWaveSpectrumPerformance:
    """Benchmark tests for wave spectrum generation."""

    def test_jonswap_spectrum_generation(self, benchmark):
        """Benchmark JONSWAP spectrum generation (<10ms target)."""
        ws = WaveSpectra()

        def run_spectrum():
            omega, S = ws.jonswap(hs=3.5, tp=10.0, gamma=3.3, n_points=100)
            return S

        result = benchmark(run_spectrum)

        # Verify target
        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 10.0, f"Wave spectrum too slow: {mean_time_ms:.3f}ms > 10ms"

    def test_spectral_statistics(self, benchmark):
        """Benchmark spectral statistics calculation."""
        ws = WaveSpectra()
        omega, S = ws.jonswap(hs=3.5, tp=10.0, gamma=3.3, n_points=100)

        def run_stats():
            return ws.spectrum_statistics(omega, S)

        result = benchmark(run_stats)

        # Should be very fast
        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 5.0, f"Spectral stats too slow: {mean_time_ms:.3f}ms > 5ms"

    def test_pierson_moskowitz_spectrum(self, benchmark):
        """Benchmark Pierson-Moskowitz spectrum generation."""
        ws = WaveSpectra()

        def run_spectrum():
            omega, S = ws.pierson_moskowitz(hs=3.5, tp=10.0, n_points=100)
            return S

        result = benchmark(run_spectrum)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 10.0, f"PM spectrum too slow: {mean_time_ms:.3f}ms > 10ms"


class TestOCIMFPerformance:
    """Benchmark tests for OCIMF coefficient lookup."""

    def test_coefficient_lookup(self, benchmark, ocimf_database):
        """Benchmark single coefficient lookup (<1ms target)."""
        def run_lookup():
            return ocimf_database.get_coefficients(90.0, 200000.0)

        result = benchmark(run_lookup)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 1.0, f"OCIMF lookup too slow: {mean_time_ms:.6f}ms > 1ms"

    def test_database_load_time(self, tmp_path):
        """Test database loading performance."""
        db_path = tmp_path / "load_test.csv"
        create_sample_database(str(db_path), num_vessels=5,
                              num_headings=13, num_displacements=3)

        import time
        start = time.perf_counter()
        db = OCIMFDatabase(str(db_path))
        load_time_ms = (time.perf_counter() - start) * 1000

        # Should load quickly
        assert load_time_ms < 100.0, f"Database load too slow: {load_time_ms:.1f}ms > 100ms"

    def test_force_calculation(self, benchmark, ocimf_database):
        """Benchmark environmental force calculation."""
        env_forces = EnvironmentalForces(ocimf_database)

        conditions = EnvironmentalConditions(
            wind_speed=15.0,
            wind_direction=45.0,
            current_speed=1.5,
            current_direction=90.0
        )

        geometry = VesselGeometry(
            loa=300.0,
            beam=50.0,
            draft=18.0
        )

        def run_calculation():
            return env_forces.calculate_total_forces(conditions, geometry, 250000.0)

        result = benchmark(run_calculation)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 10.0, f"Force calc too slow: {mean_time_ms:.3f}ms > 10ms"


class TestCatenaryPerformance:
    """Benchmark tests for catenary solver."""

    def test_catenary_solve_single_line(self, benchmark):
        """Benchmark single line catenary solution (<10ms target)."""
        solver = CatenarySolver(tolerance=1e-6, max_iterations=200)

        params = CatenaryInput(
            length=500.0,
            horizontal_span=450.0,
            vertical_span=50.0,
            weight_per_length=800.0,
            ea_stiffness=1.5e8
        )

        def run_solver():
            return solver.solve(params)

        result = benchmark(run_solver)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 10.0, f"Catenary solver too slow: {mean_time_ms:.3f}ms > 10ms"

    def test_catenary_solve_steep_line(self, benchmark):
        """Benchmark steep catenary configuration."""
        solver = CatenarySolver(tolerance=1e-6, max_iterations=200)

        params = CatenaryInput(
            length=200.0,
            horizontal_span=100.0,
            vertical_span=150.0,
            weight_per_length=600.0,
            ea_stiffness=1.0e8
        )

        def run_solver():
            return solver.solve(params)

        result = benchmark(run_solver)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 10.0, f"Steep catenary too slow: {mean_time_ms:.3f}ms > 10ms"

    def test_catenary_multi_line(self, benchmark):
        """Benchmark multi-line mooring analysis."""
        solver = CatenarySolver(tolerance=1e-6, max_iterations=200)

        # 8-line mooring system
        line_configs = [
            CatenaryInput(500.0, 450.0, 50.0, 800.0, 1.5e8),
            CatenaryInput(500.0, 450.0, 50.0, 800.0, 1.5e8),
            CatenaryInput(500.0, 450.0, 50.0, 800.0, 1.5e8),
            CatenaryInput(500.0, 450.0, 50.0, 800.0, 1.5e8),
            CatenaryInput(500.0, 450.0, 50.0, 800.0, 1.5e8),
            CatenaryInput(500.0, 450.0, 50.0, 800.0, 1.5e8),
            CatenaryInput(500.0, 450.0, 50.0, 800.0, 1.5e8),
            CatenaryInput(500.0, 450.0, 50.0, 800.0, 1.5e8),
        ]

        def run_multi_line():
            results = []
            for params in line_configs:
                results.append(solver.solve(params))
            return results

        result = benchmark(run_multi_line)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        # 8 lines × 10ms = 80ms target
        assert mean_time_ms < 100.0, f"Multi-line too slow: {mean_time_ms:.1f}ms > 100ms"


class TestHydrodynamicPerformance:
    """Benchmark tests for hydrodynamic coefficient interpolation."""

    def test_single_coefficient_interpolation(self, benchmark, hydro_database):
        """Benchmark single coefficient interpolation."""
        def run_interpolation():
            return hydro_database.get_added_mass(1.0, 0, 0)

        result = benchmark(run_interpolation)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 1.0, f"Single coeff interp too slow: {mean_time_ms:.6f}ms > 1ms"

    def test_full_matrix_interpolation(self, benchmark, hydro_database):
        """Benchmark full 6×6 matrix interpolation (<5ms target)."""
        def run_interpolation():
            added_mass = hydro_database.get_added_mass_matrix(1.0)
            damping = hydro_database.get_damping_matrix(1.0)
            return added_mass, damping

        result = benchmark(run_interpolation)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 5.0, f"Matrix interp too slow: {mean_time_ms:.3f}ms > 5ms"

    def test_multiple_frequencies(self, benchmark, hydro_database):
        """Benchmark interpolation at multiple frequencies."""
        frequencies = np.linspace(0.1, 2.0, 10)

        def run_multi_freq():
            results = []
            for freq in frequencies:
                added_mass = hydro_database.get_added_mass_matrix(freq)
                damping = hydro_database.get_damping_matrix(freq)
                results.append((added_mass, damping))
            return results

        result = benchmark(run_multi_freq)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        # 10 frequencies × 5ms = 50ms target
        assert mean_time_ms < 50.0, f"Multi-freq interp too slow: {mean_time_ms:.1f}ms > 50ms"


class TestEndToEndPerformance:
    """Benchmark tests for complete workflows."""

    def test_complete_mooring_analysis(self, benchmark, ocimf_database):
        """Benchmark complete mooring analysis workflow (<100ms target)."""
        solver = CatenarySolver(tolerance=1e-6, max_iterations=200)
        env_forces = EnvironmentalForces(ocimf_database)

        def run_complete_analysis():
            # Environmental forces
            conditions = EnvironmentalConditions(
                wind_speed=15.0,
                wind_direction=45.0,
                current_speed=1.5,
                current_direction=90.0
            )

            geometry = VesselGeometry(loa=300.0, beam=50.0, draft=18.0)
            forces = env_forces.calculate_total_forces(conditions, geometry, 250000.0)

            # Mooring line analysis (4 lines)
            line_results = []
            for _ in range(4):
                params = CatenaryInput(
                    length=500.0,
                    horizontal_span=450.0,
                    vertical_span=50.0,
                    weight_per_length=800.0,
                    ea_stiffness=1.5e8
                )
                line_results.append(solver.solve(params))

            return forces, line_results

        result = benchmark(run_complete_analysis)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 100.0, f"Complete analysis too slow: {mean_time_ms:.1f}ms > 100ms"

    def test_wave_spectrum_workflow(self, benchmark):
        """Benchmark complete wave spectrum workflow."""
        ws = WaveSpectra()

        def run_wave_workflow():
            # Generate spectrum
            omega, S = ws.jonswap(hs=3.5, tp=10.0, gamma=3.3, n_points=100)

            # Calculate statistics
            stats = ws.spectrum_statistics(omega, S)

            # Multiple spectra (sea states)
            spectra = []
            for Hs in [2.0, 3.0, 4.0, 5.0]:
                omega_i, S_i = ws.jonswap(hs=Hs, tp=10.0, gamma=3.3, n_points=100)
                spectra.append(S_i)

            return S, stats, spectra

        result = benchmark(run_wave_workflow)

        stats = benchmark.stats
        mean_time_ms = stats['mean'] * 1000
        assert mean_time_ms < 50.0, f"Wave workflow too slow: {mean_time_ms:.1f}ms > 50ms"


# Pytest configuration for benchmarks
def pytest_benchmark_update_json(config, benchmarks, output_json):
    """Custom benchmark output formatting."""
    # Add metadata
    output_json['performance_targets'] = {
        'wave_spectrum': '< 10ms',
        'ocimf_lookup': '< 1ms',
        'catenary_solver': '< 10ms per line',
        'hydro_interpolation': '< 5ms',
        'complete_mooring': '< 100ms',
    }


if __name__ == '__main__':
    pytest.main([__file__, '--benchmark-only', '-v'])
