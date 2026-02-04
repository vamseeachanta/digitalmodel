"""
Performance benchmark tests for marine engineering integration workflows.

Validates that integrated workflows meet performance targets:
- Complete workflow < 5s
- Individual module performance
- Memory efficiency
- Scalability with problem size
"""

import pytest
import numpy as np
import time
import matplotlib.pyplot as plt
from pathlib import Path
import tempfile
import psutil
import os

from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra
from digitalmodel.marine_ops.marine_analysis.hydrodynamic_coefficients.coefficients import (
    CoefficientDatabase
)
from digitalmodel.marine_ops.marine_analysis.environmental_loading.ocimf import (
    OCIMFDatabase,
    EnvironmentalConditions,
    VesselGeometry,
    EnvironmentalForces,
    create_sample_database
)
import pandas as pd


class TestPerformanceBenchmarks:
    """Performance and timing benchmarks for integration workflows."""

    @pytest.fixture
    def output_dir(self):
        """Create output directory."""
        output_path = Path(__file__).parent / "charts" / "performance"
        output_path.mkdir(parents=True, exist_ok=True)
        return output_path

    def test_wave_spectrum_performance(self, output_dir):
        """Benchmark wave spectrum calculation performance."""
        frequency_counts = [50, 100, 200, 500, 1000, 2000]
        computation_times = []
        memory_usage = []
        ws = WaveSpectra()

        for n_freq in frequency_counts:
            # Measure time
            start_time = time.perf_counter()
            omega, S = ws.jonswap(hs=5.0, tp=10.0, gamma=3.3, n_points=n_freq)
            stats = ws.spectrum_statistics(omega, S)
            end_time = time.perf_counter()

            computation_times.append((end_time - start_time) * 1000)  # ms

            # Measure memory
            process = psutil.Process(os.getpid())
            memory_usage.append(process.memory_info().rss / 1024 / 1024)  # MB

        # Performance should scale approximately linearly
        # Time for 2000 frequencies should be < 100ms
        assert computation_times[-1] < 100, \
            f"Wave spectrum computation {computation_times[-1]:.1f}ms exceeds 100ms target"

        # Create benchmark chart
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))

        # Computation time
        ax1.plot(frequency_counts, computation_times, 'o-', linewidth=2, markersize=8)
        ax1.set_xlabel('Number of Frequencies')
        ax1.set_ylabel('Computation Time (ms)')
        ax1.set_title('Wave Spectrum Performance')
        ax1.grid(True, alpha=0.3)
        ax1.set_xscale('log')

        # Memory usage
        ax2.plot(frequency_counts, memory_usage, 'o-', linewidth=2,
                markersize=8, color='green')
        ax2.set_xlabel('Number of Frequencies')
        ax2.set_ylabel('Memory Usage (MB)')
        ax2.set_title('Wave Spectrum Memory Usage')
        ax2.grid(True, alpha=0.3)
        ax2.set_xscale('log')

        plt.tight_layout()
        plt.savefig(output_dir / "wave_spectrum_performance.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_ocimf_database_performance(self, tmp_path, output_dir):
        """Benchmark OCIMF database query performance."""
        database_sizes = [10, 50, 100, 200, 500]
        load_times = []
        query_times = []

        for size in database_sizes:
            # Create database
            db_path = tmp_path / f"ocimf_{size}.csv"
            create_sample_database(
                str(db_path),
                num_vessels=max(1, size // 50),
                num_headings=min(37, size // 3),
                num_displacements=min(5, max(1, size // 20))
            )

            # Measure load time
            start_time = time.perf_counter()
            db = OCIMFDatabase(str(db_path))
            load_time = (time.perf_counter() - start_time) * 1000
            load_times.append(load_time)

            # Measure query time (average of 100 queries)
            query_start = time.perf_counter()
            for _ in range(100):
                coeffs = db.get_coefficients(45.0, 300000)
            query_time = (time.perf_counter() - query_start) * 1000 / 100
            query_times.append(query_time)

        # Query time should be < 1ms even for large databases
        assert query_times[-1] < 1.0, \
            f"Database query {query_times[-1]:.3f}ms exceeds 1ms target"

        # Create benchmark chart
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))

        # Load time
        ax1.plot(database_sizes, load_times, 'o-', linewidth=2, markersize=8, color='blue')
        ax1.set_xlabel('Database Size (entries)')
        ax1.set_ylabel('Load Time (ms)')
        ax1.set_title('OCIMF Database Load Performance')
        ax1.grid(True, alpha=0.3)

        # Query time
        ax2.plot(database_sizes, query_times, 'o-', linewidth=2, markersize=8, color='red')
        ax2.set_xlabel('Database Size (entries)')
        ax2.set_ylabel('Query Time (ms)')
        ax2.set_title('OCIMF Database Query Performance')
        ax2.grid(True, alpha=0.3)
        ax2.axhline(1.0, color='green', linestyle='--', label='1ms target')
        ax2.legend()

        plt.tight_layout()
        plt.savefig(output_dir / "ocimf_performance.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_complete_workflow_performance(self, tmp_path, output_dir):
        """Benchmark complete end-to-end workflow performance."""
        # Setup databases
        db_path = tmp_path / "ocimf_bench.csv"
        create_sample_database(str(db_path), num_vessels=3,
                              num_headings=13, num_displacements=3)
        ocimf_db = OCIMFDatabase(str(db_path))

        vessel_geometry = VesselGeometry(loa=320.0, beam=58.0, draft=22.0)

        # Benchmark complete workflow
        workflow_times = []
        num_runs = 100
        ws = WaveSpectra()

        for i in range(num_runs):
            start_time = time.perf_counter()

            # Step 1: Wave spectrum
            wave_omega, wave_S = ws.jonswap(hs=5.0, tp=10.0, gamma=3.3, n_points=100)
            wave_stats = ws.spectrum_statistics(wave_omega, wave_S)

            # Step 2: Environmental forces
            env_conditions = EnvironmentalConditions(
                wind_speed=20.0,
                wind_direction=45.0,
                current_speed=1.5,
                current_direction=30.0
            )

            env_forces = EnvironmentalForces(ocimf_db)
            force_results = env_forces.calculate_total_forces(
                env_conditions, vessel_geometry, 300000
            )

            # Step 3: Simple mooring tension calculation
            num_lines = 8
            line_angles = np.linspace(0, 360, num_lines, endpoint=False)
            tensions = []
            for angle in line_angles:
                angle_rad = np.radians(angle)
                force_component = (
                    force_results.total_fx * np.cos(angle_rad) +
                    force_results.total_fy * np.sin(angle_rad)
                )
                tensions.append(max(0, force_component / num_lines))

            end_time = time.perf_counter()
            workflow_times.append((end_time - start_time) * 1000)  # ms

        workflow_times = np.array(workflow_times)

        # Statistics
        mean_time = np.mean(workflow_times)
        median_time = np.median(workflow_times)
        p95_time = np.percentile(workflow_times, 95)
        p99_time = np.percentile(workflow_times, 99)

        # Performance target: < 5000ms (5s)
        assert p95_time < 5000, \
            f"95th percentile workflow time {p95_time:.1f}ms exceeds 5000ms target"

        # Typical should be much faster
        assert mean_time < 1000, \
            f"Mean workflow time {mean_time:.1f}ms exceeds 1000ms target"

        # Create benchmark chart
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))

        # Histogram
        ax1.hist(workflow_times, bins=30, color='steelblue', alpha=0.7, edgecolor='black')
        ax1.axvline(mean_time, color='green', linestyle='--', linewidth=2,
                   label=f'Mean: {mean_time:.1f}ms')
        ax1.axvline(p95_time, color='orange', linestyle='--', linewidth=2,
                   label=f'95th: {p95_time:.1f}ms')
        ax1.axvline(p99_time, color='red', linestyle='--', linewidth=2,
                   label=f'99th: {p99_time:.1f}ms')
        ax1.set_xlabel('Workflow Time (ms)')
        ax1.set_ylabel('Frequency')
        ax1.set_title(f'Complete Workflow Performance ({num_runs} runs)')
        ax1.legend()
        ax1.grid(True, alpha=0.3)

        # Cumulative distribution
        sorted_times = np.sort(workflow_times)
        cumulative = np.arange(1, len(sorted_times) + 1) / len(sorted_times) * 100

        ax2.plot(sorted_times, cumulative, linewidth=2, color='navy')
        ax2.axhline(95, color='orange', linestyle='--', alpha=0.5, label='95th percentile')
        ax2.axvline(5000, color='red', linestyle='--', alpha=0.5, label='5000ms target')
        ax2.set_xlabel('Workflow Time (ms)')
        ax2.set_ylabel('Cumulative Probability (%)')
        ax2.set_title('Cumulative Performance Distribution')
        ax2.legend()
        ax2.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "workflow_performance.png", dpi=300, bbox_inches='tight')
        plt.close()

        # Create summary report
        summary = {
            'test': 'Complete Workflow Performance',
            'runs': num_runs,
            'mean_ms': float(mean_time),
            'median_ms': float(median_time),
            'p95_ms': float(p95_time),
            'p99_ms': float(p99_time),
            'min_ms': float(np.min(workflow_times)),
            'max_ms': float(np.max(workflow_times)),
            'std_ms': float(np.std(workflow_times)),
            'target_met': p95_time < 5000
        }

        import json
        with open(output_dir / "performance_summary.json", 'w') as f:
            json.dump(summary, f, indent=2)

    def test_scalability_with_problem_size(self, tmp_path, output_dir):
        """Test performance scalability with problem size."""
        # Create OCIMF database
        db_path = tmp_path / "ocimf_scale.csv"
        create_sample_database(str(db_path), num_vessels=3,
                              num_headings=13, num_displacements=3)
        ocimf_db = OCIMFDatabase(str(db_path))

        vessel_geometry = VesselGeometry(loa=320.0, beam=58.0, draft=22.0)

        # Test with varying numbers of mooring lines
        line_counts = [4, 8, 12, 16, 20, 24, 32]
        times_by_line_count = []

        ws_scale = WaveSpectra()
        for num_lines in line_counts:
            run_times = []

            for _ in range(50):  # 50 runs per configuration
                start_time = time.perf_counter()

                # Wave spectrum
                _omega, _S = ws_scale.jonswap(hs=5.0, tp=10.0, gamma=3.3, n_points=100)

                # Environmental forces
                env_conditions = EnvironmentalConditions(
                    wind_speed=20.0, wind_direction=45.0,
                    current_speed=1.5, current_direction=30.0
                )
                env_forces = EnvironmentalForces(ocimf_db)
                force_results = env_forces.calculate_total_forces(
                    env_conditions, vessel_geometry, 300000
                )

                # Mooring tensions (scales with num_lines)
                line_angles = np.linspace(0, 360, num_lines, endpoint=False)
                tensions = []
                for angle in line_angles:
                    angle_rad = np.radians(angle)
                    force_component = (
                        force_results.total_fx * np.cos(angle_rad) +
                        force_results.total_fy * np.sin(angle_rad)
                    )
                    tensions.append(max(0, force_component / num_lines))

                end_time = time.perf_counter()
                run_times.append((end_time - start_time) * 1000)

            times_by_line_count.append(np.mean(run_times))

        # Performance should scale approximately linearly
        # Doubling lines should not double time (mooring calc is small fraction)
        time_ratio = times_by_line_count[-1] / times_by_line_count[0]
        line_ratio = line_counts[-1] / line_counts[0]

        assert time_ratio < line_ratio * 0.5, \
            "Performance should scale sub-linearly with problem size"

        # Create scalability chart
        fig, ax = plt.subplots(figsize=(10, 6))

        ax.plot(line_counts, times_by_line_count, 'o-', linewidth=2, markersize=8)
        ax.set_xlabel('Number of Mooring Lines')
        ax.set_ylabel('Mean Workflow Time (ms)')
        ax.set_title('Performance Scalability with Problem Size')
        ax.grid(True, alpha=0.3)

        # Add linear reference line
        linear_ref = [times_by_line_count[0] * (n / line_counts[0]) for n in line_counts]
        ax.plot(line_counts, linear_ref, '--', linewidth=2, alpha=0.5,
               color='red', label='Linear scaling')
        ax.legend()

        plt.tight_layout()
        plt.savefig(output_dir / "scalability.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_memory_efficiency(self, tmp_path, output_dir):
        """Test memory efficiency of workflows."""
        # Monitor memory during workflow
        db_path = tmp_path / "ocimf_mem.csv"
        create_sample_database(str(db_path), num_vessels=3,
                              num_headings=13, num_displacements=3)

        process = psutil.Process(os.getpid())
        memory_samples = []
        timestamps = []

        # Baseline memory
        baseline_memory = process.memory_info().rss / 1024 / 1024  # MB

        start_time = time.perf_counter()

        # Execute workflow multiple times
        ws_mem = WaveSpectra()
        for i in range(100):
            # Wave spectrum
            wave_omega, wave_S = ws_mem.jonswap(hs=5.0, tp=10.0, gamma=3.3, n_points=100)
            wave_stats = ws_mem.spectrum_statistics(wave_omega, wave_S)

            # Sample memory every 10 iterations
            if i % 10 == 0:
                current_memory = process.memory_info().rss / 1024 / 1024
                memory_samples.append(current_memory)
                timestamps.append(time.perf_counter() - start_time)

        # Final memory
        final_memory = process.memory_info().rss / 1024 / 1024

        # Memory growth should be minimal
        memory_growth = final_memory - baseline_memory
        assert memory_growth < 50, \
            f"Memory growth {memory_growth:.1f}MB exceeds 50MB limit"

        # Create memory usage chart
        fig, ax = plt.subplots(figsize=(10, 6))

        ax.plot(timestamps, memory_samples, linewidth=2, color='green')
        ax.axhline(baseline_memory, color='blue', linestyle='--',
                  label=f'Baseline: {baseline_memory:.1f}MB')
        ax.set_xlabel('Time (s)')
        ax.set_ylabel('Memory Usage (MB)')
        ax.set_title('Memory Efficiency During Workflow Execution')
        ax.legend()
        ax.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "memory_efficiency.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_parallel_execution_potential(self, tmp_path, output_dir):
        """Test potential for parallel execution of independent calculations."""
        db_path = tmp_path / "ocimf_parallel.csv"
        create_sample_database(str(db_path), num_vessels=3,
                              num_headings=13, num_displacements=3)
        ocimf_db = OCIMFDatabase(str(db_path))

        vessel_geometry = VesselGeometry(loa=320.0, beam=58.0, draft=22.0)

        # Test sequential execution
        num_cases = 100
        headings = np.linspace(0, 180, num_cases)

        start_sequential = time.perf_counter()
        sequential_results = []

        for heading in headings:
            env_conditions = EnvironmentalConditions(
                wind_speed=20.0,
                wind_direction=heading,
                current_speed=1.5,
                current_direction=heading
            )

            env_forces = EnvironmentalForces(ocimf_db)
            result = env_forces.calculate_total_forces(
                env_conditions, vessel_geometry, 300000
            )
            sequential_results.append(result)

        sequential_time = time.perf_counter() - start_sequential

        # In actual parallel implementation, we would expect near-linear speedup
        # For now, report potential speedup with 4 cores
        estimated_parallel_time = sequential_time / 4
        speedup_factor = sequential_time / estimated_parallel_time

        # Create report
        fig, ax = plt.subplots(figsize=(10, 6))

        execution_modes = ['Sequential', 'Parallel (4 cores)\n(estimated)']
        times = [sequential_time, estimated_parallel_time]
        colors = ['steelblue', 'green']

        bars = ax.bar(execution_modes, times, color=colors, alpha=0.7)
        ax.set_ylabel('Execution Time (s)')
        ax.set_title(f'Parallel Execution Potential (Speedup: {speedup_factor:.1f}x)')
        ax.grid(True, alpha=0.3, axis='y')

        # Add time labels on bars
        for bar, t in zip(bars, times):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{t:.2f}s', ha='center', va='bottom', fontsize=12)

        plt.tight_layout()
        plt.savefig(output_dir / "parallel_potential.png", dpi=300, bbox_inches='tight')
        plt.close()
