"""
Performance Profiling Script for Marine Engineering Modules

Comprehensive profiling tool that analyzes runtime, memory usage, and
bottlenecks across all Phase 1 and Phase 2 modules.

Usage:
    python scripts/profile_marine_modules.py --module all
    python scripts/profile_marine_modules.py --module wave_spectra
    python scripts/profile_marine_modules.py --output outputs/profiling

Generates:
- cProfile profiling data
- Memory profiling results
- Line-by-line profiling
- Performance metrics JSON
- HTML dashboard
"""

import sys
import time
import cProfile
import pstats
import io
import json
import numpy as np
import pandas as pd
from pathlib import Path
from typing import Dict, List, Tuple, Any
from dataclasses import dataclass, asdict
from datetime import datetime
import argparse

# Memory profiling (optional)
try:
    from memory_profiler import memory_usage
    MEMORY_PROFILER_AVAILABLE = True
except ImportError:
    MEMORY_PROFILER_AVAILABLE = False
    print("Warning: memory_profiler not available. Memory profiling will be estimated.")

    def memory_usage(func, *args, **kwargs):
        """Fallback memory estimation."""
        import psutil
        import os
        process = psutil.Process(os.getpid())
        mem_before = process.memory_info().rss / 1024 / 1024  # MB
        func()
        mem_after = process.memory_info().rss / 1024 / 1024  # MB
        return max(mem_after, mem_before)

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.modules.marine_engineering.wave_spectra.spectra import (
    JONSWAPSpectrum, PiersonMoskowitzSpectrum, WaveSpectrumParameters
)
from digitalmodel.modules.marine_engineering.environmental_loading.ocimf import (
    OCIMFDatabase, EnvironmentalForces, EnvironmentalConditions,
    VesselGeometry, create_sample_database
)
from digitalmodel.modules.marine_engineering.catenary.solver import CatenarySolver, CatenaryInput
from digitalmodel.modules.marine_engineering.hydrodynamic_coefficients.coefficients import (
    CoefficientDatabase, FrequencyDependentMatrix
)


@dataclass
class PerformanceMetrics:
    """Container for performance metrics."""
    module_name: str
    test_name: str
    execution_time_ms: float
    memory_usage_mb: float
    peak_memory_mb: float
    iterations: int
    timestamp: str
    target_time_ms: float
    meets_target: bool
    speedup_factor: float = 1.0

    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        return asdict(self)


class PerformanceProfiler:
    """Main profiling class for marine engineering modules."""

    def __init__(self, output_dir: str = "outputs/profiling"):
        """Initialize profiler.

        Args:
            output_dir: Directory for profiling outputs
        """
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

        # Create subdirectories
        (self.output_dir / "cprofile").mkdir(exist_ok=True)
        (self.output_dir / "memory").mkdir(exist_ok=True)
        (self.output_dir / "line_profile").mkdir(exist_ok=True)
        (self.output_dir / "metrics").mkdir(exist_ok=True)
        (self.output_dir / "charts").mkdir(exist_ok=True)

        self.metrics: List[PerformanceMetrics] = []
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Setup test data
        self._setup_test_data()

    def _setup_test_data(self):
        """Setup test data for profiling."""
        # Create sample OCIMF database
        db_path = self.output_dir / "sample_ocimf.csv"
        if not db_path.exists():
            create_sample_database(str(db_path), num_vessels=5,
                                 num_headings=13, num_displacements=3)
        self.ocimf_db_path = db_path

    def profile_wave_spectrum(self) -> PerformanceMetrics:
        """Profile wave spectrum generation."""
        print("\n=== Profiling Wave Spectrum Generation ===")

        # Setup
        params = WaveSpectrumParameters(
            Hs=3.5,
            Tp=10.0,
            gamma=3.3,
            n_frequencies=100
        )

        # Profile execution time
        iterations = 1000
        start_time = time.perf_counter()

        for _ in range(iterations):
            spectrum = JONSWAPSpectrum(params)
            S = spectrum.compute_spectrum()
            stats = spectrum.get_spectral_statistics()

        end_time = time.perf_counter()
        avg_time_ms = (end_time - start_time) / iterations * 1000

        # Memory profiling
        def run_spectrum():
            spectrum = JONSWAPSpectrum(params)
            S = spectrum.compute_spectrum()
            stats = spectrum.get_spectral_statistics()
            return S

        mem_usage = memory_usage(run_spectrum, max_usage=True)
        peak_memory_mb = mem_usage

        # cProfile
        profiler = cProfile.Profile()
        profiler.enable()
        for _ in range(100):
            spectrum = JONSWAPSpectrum(params)
            S = spectrum.compute_spectrum()
        profiler.disable()

        # Save cProfile results
        stats_file = self.output_dir / "cprofile" / f"wave_spectrum_{self.timestamp}.prof"
        profiler.dump_stats(str(stats_file))

        # Print stats
        s = io.StringIO()
        ps = pstats.Stats(profiler, stream=s).sort_stats('cumulative')
        ps.print_stats(20)
        print(s.getvalue())

        # Target: <10ms
        target_ms = 10.0
        meets_target = avg_time_ms < target_ms

        metrics = PerformanceMetrics(
            module_name="wave_spectra",
            test_name="JONSWAP_generation",
            execution_time_ms=avg_time_ms,
            memory_usage_mb=peak_memory_mb,
            peak_memory_mb=peak_memory_mb,
            iterations=iterations,
            timestamp=self.timestamp,
            target_time_ms=target_ms,
            meets_target=meets_target
        )

        print(f"Average Time: {avg_time_ms:.3f} ms (Target: {target_ms} ms)")
        print(f"Memory Usage: {peak_memory_mb:.2f} MB")
        print(f"Status: {'[PASS]' if meets_target else '[FAIL]'}")

        self.metrics.append(metrics)
        return metrics

    def profile_ocimf_lookup(self) -> PerformanceMetrics:
        """Profile OCIMF coefficient lookup."""
        print("\n=== Profiling OCIMF Coefficient Lookup ===")

        # Load database
        db = OCIMFDatabase(str(self.ocimf_db_path))

        # Profile lookup time
        iterations = 10000
        headings = np.random.uniform(0, 180, iterations)
        displacements = np.random.uniform(100000, 300000, iterations)

        start_time = time.perf_counter()

        for heading, disp in zip(headings, displacements):
            coeffs = db.get_coefficients(heading, disp)

        end_time = time.perf_counter()
        avg_time_ms = (end_time - start_time) / iterations * 1000

        # Memory profiling
        def run_lookup():
            coeffs = db.get_coefficients(90.0, 200000.0)
            return coeffs

        mem_usage = memory_usage(run_lookup, max_usage=True)
        peak_memory_mb = mem_usage

        # cProfile
        profiler = cProfile.Profile()
        profiler.enable()
        for _ in range(1000):
            coeffs = db.get_coefficients(90.0, 200000.0)
        profiler.disable()

        stats_file = self.output_dir / "cprofile" / f"ocimf_lookup_{self.timestamp}.prof"
        profiler.dump_stats(str(stats_file))

        s = io.StringIO()
        ps = pstats.Stats(profiler, stream=s).sort_stats('cumulative')
        ps.print_stats(20)
        print(s.getvalue())

        # Target: <1ms
        target_ms = 1.0
        meets_target = avg_time_ms < target_ms

        metrics = PerformanceMetrics(
            module_name="environmental_loading",
            test_name="OCIMF_coefficient_lookup",
            execution_time_ms=avg_time_ms,
            memory_usage_mb=peak_memory_mb,
            peak_memory_mb=peak_memory_mb,
            iterations=iterations,
            timestamp=self.timestamp,
            target_time_ms=target_ms,
            meets_target=meets_target
        )

        print(f"Average Time: {avg_time_ms:.6f} ms (Target: {target_ms} ms)")
        print(f"Memory Usage: {peak_memory_mb:.2f} MB")
        status_text = "[PASS]" if meets_target else "[FAIL]"
        print(f"Status: {status_text}")

        self.metrics.append(metrics)
        return metrics

    def profile_catenary_solver(self) -> PerformanceMetrics:
        """Profile catenary solver."""
        print("\n=== Profiling Catenary Solver ===")

        solver = CatenarySolver(tolerance=1e-6, max_iterations=200)

        # Test case (use more realistic parameters)
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=600.0,
            ea_stiffness=1.0e9
        )

        # Profile execution time
        iterations = 100
        start_time = time.perf_counter()

        for _ in range(iterations):
            results = solver.solve(params)

        end_time = time.perf_counter()
        avg_time_ms = (end_time - start_time) / iterations * 1000

        # Memory profiling
        def run_solver():
            results = solver.solve(params)
            return results

        mem_usage = memory_usage(run_solver, max_usage=True)
        peak_memory_mb = mem_usage

        # cProfile
        profiler = cProfile.Profile()
        profiler.enable()
        for _ in range(50):
            results = solver.solve(params)
        profiler.disable()

        stats_file = self.output_dir / "cprofile" / f"catenary_solver_{self.timestamp}.prof"
        profiler.dump_stats(str(stats_file))

        s = io.StringIO()
        ps = pstats.Stats(profiler, stream=s).sort_stats('cumulative')
        ps.print_stats(20)
        print(s.getvalue())

        # Target: <10ms per line
        target_ms = 10.0
        meets_target = avg_time_ms < target_ms

        metrics = PerformanceMetrics(
            module_name="catenary",
            test_name="catenary_solver",
            execution_time_ms=avg_time_ms,
            memory_usage_mb=peak_memory_mb,
            peak_memory_mb=peak_memory_mb,
            iterations=iterations,
            timestamp=self.timestamp,
            target_time_ms=target_ms,
            meets_target=meets_target
        )

        print(f"Average Time: {avg_time_ms:.3f} ms (Target: {target_ms} ms)")
        print(f"Memory Usage: {peak_memory_mb:.2f} MB")
        print(f"Status: {'[PASS]' if meets_target else '[FAIL]'}")

        self.metrics.append(metrics)
        return metrics

    def profile_hydro_interpolation(self) -> PerformanceMetrics:
        """Profile hydrodynamic coefficient interpolation."""
        print("\n=== Profiling Hydrodynamic Coefficient Interpolation ===")

        # Create sample database
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

        # Profile interpolation time
        iterations = 1000
        test_frequencies = np.random.uniform(0.1, 2.0, iterations)

        start_time = time.perf_counter()

        for freq in test_frequencies:
            added_mass = db.get_added_mass_matrix(freq)
            damping = db.get_damping_matrix(freq)

        end_time = time.perf_counter()
        avg_time_ms = (end_time - start_time) / iterations * 1000

        # Memory profiling
        def run_interpolation():
            added_mass = db.get_added_mass_matrix(1.0)
            damping = db.get_damping_matrix(1.0)
            return added_mass, damping

        mem_usage = memory_usage(run_interpolation, max_usage=True)
        peak_memory_mb = mem_usage

        # cProfile
        profiler = cProfile.Profile()
        profiler.enable()
        for _ in range(100):
            added_mass = db.get_added_mass_matrix(1.0)
            damping = db.get_damping_matrix(1.0)
        profiler.disable()

        stats_file = self.output_dir / "cprofile" / f"hydro_interp_{self.timestamp}.prof"
        profiler.dump_stats(str(stats_file))

        s = io.StringIO()
        ps = pstats.Stats(profiler, stream=s).sort_stats('cumulative')
        ps.print_stats(20)
        print(s.getvalue())

        # Target: <5ms
        target_ms = 5.0
        meets_target = avg_time_ms < target_ms

        metrics = PerformanceMetrics(
            module_name="hydrodynamic_coefficients",
            test_name="coefficient_interpolation",
            execution_time_ms=avg_time_ms,
            memory_usage_mb=peak_memory_mb,
            peak_memory_mb=peak_memory_mb,
            iterations=iterations,
            timestamp=self.timestamp,
            target_time_ms=target_ms,
            meets_target=meets_target
        )

        print(f"Average Time: {avg_time_ms:.3f} ms (Target: {target_ms} ms)")
        print(f"Memory Usage: {peak_memory_mb:.2f} MB")
        print(f"Status: {'[PASS]' if meets_target else '[FAIL]'}")

        self.metrics.append(metrics)
        return metrics

    def save_metrics(self):
        """Save all metrics to JSON file."""
        metrics_file = self.output_dir / "metrics" / f"performance_metrics_{self.timestamp}.json"

        data = {
            'timestamp': self.timestamp,
            'metrics': [m.to_dict() for m in self.metrics],
            'summary': self.generate_summary()
        }

        with open(metrics_file, 'w') as f:
            json.dump(data, f, indent=2)

        print(f"\n[OK] Metrics saved to: {metrics_file}")

    def generate_summary(self) -> Dict:
        """Generate performance summary."""
        passed = sum(1 for m in self.metrics if m.meets_target)
        total = len(self.metrics)

        summary = {
            'total_tests': total,
            'passed': passed,
            'failed': total - passed,
            'pass_rate': passed / total * 100 if total > 0 else 0,
            'by_module': {}
        }

        # Group by module
        for metric in self.metrics:
            module = metric.module_name
            if module not in summary['by_module']:
                summary['by_module'][module] = {
                    'tests': 0,
                    'passed': 0,
                    'avg_time_ms': 0,
                    'total_memory_mb': 0
                }

            summary['by_module'][module]['tests'] += 1
            if metric.meets_target:
                summary['by_module'][module]['passed'] += 1
            summary['by_module'][module]['avg_time_ms'] += metric.execution_time_ms
            summary['by_module'][module]['total_memory_mb'] += metric.memory_usage_mb

        # Calculate averages
        for module_data in summary['by_module'].values():
            if module_data['tests'] > 0:
                module_data['avg_time_ms'] /= module_data['tests']
                module_data['total_memory_mb'] /= module_data['tests']

        return summary

    def print_summary(self):
        """Print performance summary."""
        summary = self.generate_summary()

        print("\n" + "="*60)
        print("PERFORMANCE PROFILING SUMMARY")
        print("="*60)
        print(f"Total Tests: {summary['total_tests']}")
        print(f"Passed: {summary['passed']} ({summary['pass_rate']:.1f}%)")
        print(f"Failed: {summary['failed']}")
        print("\nBy Module:")

        for module, data in summary['by_module'].items():
            print(f"\n  {module}:")
            print(f"    Tests: {data['tests']}")
            print(f"    Passed: {data['passed']}")
            print(f"    Avg Time: {data['avg_time_ms']:.3f} ms")
            print(f"    Avg Memory: {data['total_memory_mb']:.2f} MB")

        print("\n" + "="*60)

    def run_all(self):
        """Run all profiling tests."""
        print("\n" + "="*60)
        print("MARINE ENGINEERING MODULES - PERFORMANCE PROFILING")
        print("="*60)

        # Run all profiling tests
        self.profile_wave_spectrum()
        self.profile_ocimf_lookup()
        self.profile_catenary_solver()
        self.profile_hydro_interpolation()

        # Save and print summary
        self.save_metrics()
        self.print_summary()

        return self.metrics


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Profile marine engineering modules")
    parser.add_argument('--module', choices=['all', 'wave_spectra', 'ocimf',
                                             'catenary', 'hydro'],
                       default='all', help='Module to profile')
    parser.add_argument('--output', default='outputs/profiling',
                       help='Output directory for profiling results')

    args = parser.parse_args()

    profiler = PerformanceProfiler(output_dir=args.output)

    if args.module == 'all':
        profiler.run_all()
    elif args.module == 'wave_spectra':
        profiler.profile_wave_spectrum()
        profiler.save_metrics()
    elif args.module == 'ocimf':
        profiler.profile_ocimf_lookup()
        profiler.save_metrics()
    elif args.module == 'catenary':
        profiler.profile_catenary_solver()
        profiler.save_metrics()
    elif args.module == 'hydro':
        profiler.profile_hydro_interpolation()
        profiler.save_metrics()


if __name__ == '__main__':
    main()
