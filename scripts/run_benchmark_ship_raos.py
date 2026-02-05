#!/usr/bin/env python3
"""
Run WRK-031 Benchmark: Ship RAOs - AQWA vs OrcaWave

Uses existing L01_aqwa_benchmark CSV data to demonstrate the
MultiSolverComparator and BenchmarkPlotter framework.

This script:
1. Loads AQWA and OrcaWave RAO data from existing CSVs
2. Converts to DiffractionResults format
3. Runs the benchmark comparison
4. Generates plots and reports
"""

from __future__ import annotations

import csv
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Dict, List

import numpy as np

from digitalmodel.hydrodynamics.diffraction import (
    BenchmarkConfig,
    BenchmarkRunner,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    RAOSet,
    RAOComponent,
    AddedMassSet,
    DampingSet,
    HydrodynamicMatrix,
    FrequencyData,
    HeadingData,
    DOF,
)


def load_rao_csv(csv_path: Path) -> Dict:
    """Load RAO data from CSV file.

    Returns dict with structure:
        {heading: {dof: {'freq': [], 'amp': [], 'phase': []}}}
    """
    data = defaultdict(lambda: defaultdict(lambda: {'freq': [], 'amp': [], 'phase': []}))

    dof_map = {
        'surge': DOF.SURGE,
        'sway': DOF.SWAY,
        'heave': DOF.HEAVE,
        'roll': DOF.ROLL,
        'pitch': DOF.PITCH,
        'yaw': DOF.YAW,
    }

    with open(csv_path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            # Handle both column naming conventions
            if 'frequency_rad_s' in row:
                freq = float(row['frequency_rad_s'])
            else:
                freq = float(row.get('freq_rad_s', row.get('frequency', 0)))

            heading = float(row['heading_deg'])

            for dof_name, dof_enum in dof_map.items():
                amp_key = f'{dof_name}_amp'
                phase_key = f'{dof_name}_phase'

                if amp_key in row and phase_key in row:
                    data[heading][dof_enum]['freq'].append(freq)
                    data[heading][dof_enum]['amp'].append(float(row[amp_key]))
                    data[heading][dof_enum]['phase'].append(float(row[phase_key]))

    return data


def create_diffraction_results(
    rao_data: Dict,
    vessel_name: str,
    solver_name: str,
) -> DiffractionResults:
    """Convert loaded RAO data to DiffractionResults format."""

    # Get unique headings and sort
    headings = sorted(rao_data.keys())

    # Get frequencies from first heading/dof (assumed same for all)
    first_heading = headings[0]
    first_dof = list(rao_data[first_heading].keys())[0]
    frequencies = np.array(sorted(set(rao_data[first_heading][first_dof]['freq'])))

    n_freq = len(frequencies)
    n_head = len(headings)

    # Create FrequencyData and HeadingData
    freq_data = FrequencyData(
        values=frequencies.copy(),
        periods=2.0 * np.pi / frequencies,
        count=n_freq,
        min_freq=float(frequencies[0]),
        max_freq=float(frequencies[-1]),
    )

    heading_arr = np.array(headings)
    heading_data = HeadingData(
        values=heading_arr.copy(),
        count=n_head,
        min_heading=float(heading_arr[0]),
        max_heading=float(heading_arr[-1]),
    )

    # Build RAO components for each DOF
    components = {}
    for dof in DOF:
        # Collect data across all headings
        mag_data = []
        phase_data = []

        for heading in headings:
            if dof in rao_data[heading]:
                dof_data = rao_data[heading][dof]
                # Sort by frequency
                sorted_indices = np.argsort(dof_data['freq'])
                mag_data.append([dof_data['amp'][i] for i in sorted_indices])
                phase_data.append([dof_data['phase'][i] for i in sorted_indices])
            else:
                # Missing data - fill with zeros
                mag_data.append([0.0] * n_freq)
                phase_data.append([0.0] * n_freq)

        # Convert to numpy arrays [nfreq x nheading]
        magnitude = np.array(mag_data).T
        phase = np.array(phase_data).T

        components[dof] = RAOComponent(
            dof=dof,
            magnitude=magnitude,
            phase=phase,
            frequencies=FrequencyData(
                values=frequencies.copy(),
                periods=2.0 * np.pi / frequencies,
                count=n_freq,
                min_freq=float(frequencies[0]),
                max_freq=float(frequencies[-1]),
            ),
            headings=HeadingData(
                values=heading_arr.copy(),
                count=n_head,
                min_heading=float(heading_arr[0]),
                max_heading=float(heading_arr[-1]),
            ),
            unit="",
        )

    now = datetime.now().isoformat()

    rao_set = RAOSet(
        vessel_name=vessel_name,
        analysis_tool=solver_name,
        water_depth=500.0,
        surge=components[DOF.SURGE],
        sway=components[DOF.SWAY],
        heave=components[DOF.HEAVE],
        roll=components[DOF.ROLL],
        pitch=components[DOF.PITCH],
        yaw=components[DOF.YAW],
        created_date=now,
        source_file="benchmark_csv",
    )

    # Create minimal added mass and damping sets
    # Use placeholder matrices since CSV doesn't have this data
    matrices_am = []
    matrices_damp = []
    for i, freq in enumerate(frequencies[:min(10, n_freq)]):
        matrices_am.append(
            HydrodynamicMatrix(
                matrix=np.eye(6) * 1000,
                frequency=float(freq),
                matrix_type="added_mass",
                units={"linear": "kg", "angular": "kg.m^2"},
            )
        )
        matrices_damp.append(
            HydrodynamicMatrix(
                matrix=np.eye(6) * 100,
                frequency=float(freq),
                matrix_type="damping",
                units={"linear": "N.s/m", "angular": "N.m.s/rad"},
            )
        )

    added_mass = AddedMassSet(
        vessel_name=vessel_name,
        analysis_tool=solver_name,
        water_depth=500.0,
        matrices=matrices_am,
        frequencies=freq_data,
        created_date=now,
    )

    damping = DampingSet(
        vessel_name=vessel_name,
        analysis_tool=solver_name,
        water_depth=500.0,
        matrices=matrices_damp,
        frequencies=freq_data,
        created_date=now,
    )

    return DiffractionResults(
        vessel_name=vessel_name,
        analysis_tool=solver_name,
        water_depth=500.0,
        raos=rao_set,
        added_mass=added_mass,
        damping=damping,
        created_date=now,
    )


def main():
    """Run the benchmark comparison."""

    # Paths
    benchmark_dir = Path("docs/modules/orcawave/L01_aqwa_benchmark")
    data_dir = benchmark_dir / "data"
    output_dir = benchmark_dir / "benchmark_results"

    aqwa_csv = data_dir / "aqwa_rao_data.csv"
    orcawave_csv = data_dir / "orcawave_rao_data.csv"

    # Check files exist
    if not aqwa_csv.exists():
        print(f"ERROR: AQWA data not found: {aqwa_csv}")
        return
    if not orcawave_csv.exists():
        print(f"ERROR: OrcaWave data not found: {orcawave_csv}")
        return

    print("=" * 60)
    print("WRK-031 Benchmark: Ship RAOs - AQWA vs OrcaWave")
    print("=" * 60)

    # Load data
    print("\n1. Loading AQWA RAO data...")
    aqwa_data = load_rao_csv(aqwa_csv)
    print(f"   Loaded {len(aqwa_data)} headings")

    print("\n2. Loading OrcaWave RAO data...")
    orcawave_data = load_rao_csv(orcawave_csv)
    print(f"   Loaded {len(orcawave_data)} headings")

    # Find common headings
    common_headings = sorted(set(aqwa_data.keys()) & set(orcawave_data.keys()))
    print(f"\n3. Common headings: {len(common_headings)}")
    print(f"   Headings: {common_headings}")

    if not common_headings:
        print("ERROR: No common headings between AQWA and OrcaWave data")
        return

    # Filter to common headings
    aqwa_filtered = {h: aqwa_data[h] for h in common_headings}
    orcawave_filtered = {h: orcawave_data[h] for h in common_headings}

    # Find common frequencies (within tolerance)
    first_h = common_headings[0]
    first_dof = list(aqwa_filtered[first_h].keys())[0]
    aqwa_freqs = set(aqwa_filtered[first_h][first_dof]['freq'])
    orcawave_freqs = set(orcawave_filtered[first_h][first_dof]['freq'])

    # Find frequencies that are close enough (within 1%)
    common_freqs = []
    for af in sorted(aqwa_freqs):
        for of in orcawave_freqs:
            if abs(af - of) / max(af, of) < 0.01:
                common_freqs.append(af)
                break

    print(f"\n   AQWA frequencies: {len(aqwa_freqs)}")
    print(f"   OrcaWave frequencies: {len(orcawave_freqs)}")
    print(f"   Common frequencies: {len(common_freqs)}")

    if len(common_freqs) < 3:
        print("WARNING: Few common frequencies. Using AQWA frequencies as reference.")
        # Filter OrcaWave data to AQWA frequencies (interpolate if needed)
        common_freqs = sorted(aqwa_freqs)

    # Filter data to common frequencies only
    def filter_to_freqs(data, target_freqs):
        filtered = {}
        for heading, dof_data in data.items():
            filtered[heading] = {}
            for dof, values in dof_data.items():
                # Find indices of target frequencies
                new_freq = []
                new_amp = []
                new_phase = []
                for tf in target_freqs:
                    for i, f in enumerate(values['freq']):
                        if abs(f - tf) / max(f, tf) < 0.05:  # 5% tolerance
                            new_freq.append(tf)
                            new_amp.append(values['amp'][i])
                            new_phase.append(values['phase'][i])
                            break
                filtered[heading][dof] = {
                    'freq': new_freq,
                    'amp': new_amp,
                    'phase': new_phase,
                }
        return filtered

    aqwa_filtered = filter_to_freqs(aqwa_filtered, common_freqs)
    orcawave_filtered = filter_to_freqs(orcawave_filtered, common_freqs)

    # Convert to DiffractionResults
    print("\n4. Converting to DiffractionResults format...")
    vessel_name = "Ship_001_RAOs"

    aqwa_results = create_diffraction_results(
        aqwa_filtered, vessel_name, "AQWA"
    )
    orcawave_results = create_diffraction_results(
        orcawave_filtered, vessel_name, "OrcaWave"
    )

    print(f"   AQWA: {aqwa_results.raos.surge.frequencies.count} frequencies, "
          f"{aqwa_results.raos.surge.headings.count} headings")
    print(f"   OrcaWave: {orcawave_results.raos.surge.frequencies.count} frequencies, "
          f"{orcawave_results.raos.surge.headings.count} headings")

    # Run benchmark
    print("\n5. Running benchmark comparison...")
    solver_results = {
        "AQWA": aqwa_results,
        "OrcaWave": orcawave_results,
    }

    config = BenchmarkConfig(
        output_dir=output_dir,
        tolerance=0.05,
        x_axis="period",
        reference_solver="AQWA",
    )

    runner = BenchmarkRunner(config)
    result = runner.run_from_results(solver_results)

    # Report results
    print("\n" + "=" * 60)
    print("BENCHMARK RESULTS")
    print("=" * 60)

    if result.success:
        print(f"\nStatus: SUCCESS")
        print(f"\nOverall Consensus: {result.report.overall_consensus}")

        print("\nConsensus by DOF:")
        for dof_name, metrics in result.report.consensus_by_dof.items():
            print(f"  {dof_name}: {metrics.consensus_level}")

        print(f"\nJSON Report: {result.report_json_path}")
        print(f"HTML Report: {result.report_html_path}")
        print(f"\nPlots generated: {len(result.plot_paths)}")
        for p in result.plot_paths:
            print(f"  - {p.name}")
    else:
        print(f"\nStatus: FAILED")
        print(f"Error: {result.error_message}")

    print("\n" + "=" * 60)
    print("Benchmark complete!")
    print("=" * 60)


if __name__ == "__main__":
    main()
