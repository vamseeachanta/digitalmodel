#!/usr/bin/env python3
"""
Complete Diffraction Conversion Example

Demonstrates the complete workflow from AQWA diffraction data through
unified schema conversion to OrcaFlex export in all formats.

This example shows:
1. Creating mock AQWA diffraction data
2. Converting to unified DiffractionResults schema
3. Running comprehensive validation
4. Exporting to all OrcaFlex formats (YAML, CSV, Excel)
5. Examining the generated outputs
"""

import numpy as np
from pathlib import Path
from datetime import datetime

# Import Phase 2 modules
from digitalmodel.modules.diffraction import (
    # Data structures
    DiffractionResults,
    RAOSet,
    RAOComponent,
    AddedMassSet,
    DampingSet,
    HydrodynamicMatrix,
    FrequencyData,
    HeadingData,
    DOF,

    # Export and validation
    OrcaFlexExporter,
    validate_results
)


def create_mock_aqwa_data():
    """
    Create realistic mock AQWA diffraction data for a generic FPSO

    Returns:
        Dictionary with mock diffraction data
    """
    print("=" * 70)
    print("STEP 1: Creating Mock AQWA Diffraction Data")
    print("=" * 70)

    # Define frequency range (typical for offshore analysis)
    # 0.05 - 2.0 rad/s covers periods from ~3s to 125s
    frequencies = np.array([
        0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8,
        0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0
    ])

    # Define heading angles
    headings = np.array([0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330])

    nfreq = len(frequencies)
    nhead = len(headings)

    print(f"\nFrequencies: {nfreq} points from {frequencies[0]:.3f} to {frequencies[-1]:.3f} rad/s")
    print(f"Headings: {nhead} directions from {headings[0]:.0f}° to {headings[-1]:.0f}°")

    # Generate realistic RAO data for FPSO
    # Surge RAO: Peaks at low frequencies
    surge_mag = np.zeros((nfreq, nhead))
    surge_phase = np.zeros((nfreq, nhead))

    for i, freq in enumerate(frequencies):
        for j, heading in enumerate(headings):
            # Higher response at low frequencies
            base_mag = 1.5 / (1 + freq)
            # Directional dependency (max in head seas, min in beam)
            dir_factor = abs(np.cos(np.radians(heading)))
            surge_mag[i, j] = base_mag * dir_factor
            surge_phase[i, j] = 180 - 20 * freq

    # Sway RAO: Peaks in beam seas
    sway_mag = np.zeros((nfreq, nhead))
    sway_phase = np.zeros((nfreq, nhead))

    for i, freq in enumerate(frequencies):
        for j, heading in enumerate(headings):
            base_mag = 1.2 / (1 + freq)
            dir_factor = abs(np.sin(np.radians(heading)))
            sway_mag[i, j] = base_mag * dir_factor
            sway_phase[i, j] = 90 - 15 * freq

    # Heave RAO: Less directional, peaks at resonance
    heave_mag = np.zeros((nfreq, nhead))
    heave_phase = np.zeros((nfreq, nhead))

    for i, freq in enumerate(frequencies):
        # Resonance around 0.4 rad/s (typical for FPSO)
        resonance = np.exp(-((freq - 0.4) ** 2) / 0.04)
        for j, heading in enumerate(headings):
            heave_mag[i, j] = 0.8 + 1.5 * resonance
            heave_phase[i, j] = 0 if freq < 0.4 else 180

    # Roll RAO: Beam seas, resonance around 0.3 rad/s
    roll_mag = np.zeros((nfreq, nhead))
    roll_phase = np.zeros((nfreq, nhead))

    for i, freq in enumerate(frequencies):
        resonance = np.exp(-((freq - 0.3) ** 2) / 0.02)
        for j, heading in enumerate(headings):
            dir_factor = abs(np.sin(np.radians(heading)))
            roll_mag[i, j] = (2.0 + 8.0 * resonance) * dir_factor
            roll_phase[i, j] = 0 if freq < 0.3 else 180

    # Pitch RAO: Head/stern seas, resonance around 0.35 rad/s
    pitch_mag = np.zeros((nfreq, nhead))
    pitch_phase = np.zeros((nfreq, nhead))

    for i, freq in enumerate(frequencies):
        resonance = np.exp(-((freq - 0.35) ** 2) / 0.02)
        for j, heading in enumerate(headings):
            dir_factor = abs(np.cos(np.radians(heading)))
            pitch_mag[i, j] = (1.5 + 6.0 * resonance) * dir_factor
            pitch_phase[i, j] = 0 if freq < 0.35 else 180

    # Yaw RAO: Smaller magnitudes, quartering seas
    yaw_mag = np.zeros((nfreq, nhead))
    yaw_phase = np.zeros((nfreq, nhead))

    for i, freq in enumerate(frequencies):
        for j, heading in enumerate(headings):
            # Maximum in quartering seas
            dir_factor = abs(np.sin(2 * np.radians(heading)))
            yaw_mag[i, j] = (0.5 / (1 + freq**2)) * dir_factor
            yaw_phase[i, j] = 45 - 10 * freq

    # Generate added mass matrices (6x6, frequency dependent)
    added_mass_matrices = []

    for freq in frequencies:
        # Diagonal dominant, frequency dependent
        matrix = np.zeros((6, 6))

        # Translation masses (surge, sway, heave)
        matrix[0, 0] = 50000 * (1 + 0.1 / (0.1 + freq))  # Surge
        matrix[1, 1] = 60000 * (1 + 0.1 / (0.1 + freq))  # Sway
        matrix[2, 2] = 80000 * (1 + 0.2 / (0.1 + freq))  # Heave

        # Rotational inertias (roll, pitch, yaw)
        matrix[3, 3] = 5e9 * (1 + 0.15 / (0.1 + freq))   # Roll
        matrix[4, 4] = 3e10 * (1 + 0.15 / (0.1 + freq))  # Pitch
        matrix[5, 5] = 3e10 * (1 + 0.1 / (0.1 + freq))   # Yaw

        # Coupling terms (smaller)
        matrix[0, 4] = matrix[4, 0] = 1e6 * (1 - freq/2)  # Surge-Pitch
        matrix[1, 3] = matrix[3, 1] = -8e5 * (1 - freq/2) # Sway-Roll

        added_mass_matrices.append(matrix)

    # Generate damping matrices (6x6, frequency dependent)
    damping_matrices = []

    for i, freq in enumerate(frequencies):
        # Frequency dependent damping
        matrix = np.zeros((6, 6))

        # Translation damping increases with frequency
        matrix[0, 0] = 2e5 * freq  # Surge
        matrix[1, 1] = 2.5e5 * freq  # Sway
        matrix[2, 2] = 3e5 * freq  # Heave

        # Rotational damping
        matrix[3, 3] = 1e8 * freq  # Roll
        matrix[4, 4] = 5e8 * freq  # Pitch
        matrix[5, 5] = 5e8 * freq  # Yaw

        # Small coupling
        matrix[0, 4] = matrix[4, 0] = 5e6 * freq
        matrix[1, 3] = matrix[3, 1] = 4e6 * freq

        damping_matrices.append(matrix)

    print("\n[OK] Generated mock RAO data for all 6 DOFs")
    print("[OK] Generated added mass matrices (17 frequencies)")
    print("[OK] Generated damping matrices (17 frequencies)")

    return {
        'frequencies': frequencies,
        'headings': headings,
        'raos': {
            'surge': {'magnitude': surge_mag, 'phase': surge_phase},
            'sway': {'magnitude': sway_mag, 'phase': sway_phase},
            'heave': {'magnitude': heave_mag, 'phase': heave_phase},
            'roll': {'magnitude': roll_mag, 'phase': roll_phase},
            'pitch': {'magnitude': pitch_mag, 'phase': pitch_phase},
            'yaw': {'magnitude': yaw_mag, 'phase': yaw_phase}
        },
        'added_mass_matrices': added_mass_matrices,
        'damping_matrices': damping_matrices
    }


def convert_to_unified_schema(mock_data, vessel_name="Example_FPSO", water_depth=1200.0):
    """
    Convert mock AQWA data to unified DiffractionResults schema

    Args:
        mock_data: Dictionary with mock diffraction data
        vessel_name: Name of vessel
        water_depth: Water depth in meters

    Returns:
        DiffractionResults object
    """
    print("\n" + "=" * 70)
    print("STEP 2: Converting to Unified Schema")
    print("=" * 70)

    # Create frequency and heading data
    freq_data = FrequencyData(
        values=mock_data['frequencies'],
        periods=2 * np.pi / mock_data['frequencies'],
        count=len(mock_data['frequencies']),
        min_freq=0.0,
        max_freq=0.0
    )

    heading_data = HeadingData(
        values=mock_data['headings'],
        count=len(mock_data['headings']),
        min_heading=0.0,
        max_heading=0.0
    )

    print(f"\n[OK] Frequency data: {freq_data.count} points")
    print(f"[OK] Heading data: {heading_data.count} directions")

    # Create RAO components for each DOF
    rao_components = {}

    for dof in DOF:
        dof_name = dof.name.lower()
        component = RAOComponent(
            dof=dof,
            magnitude=mock_data['raos'][dof_name]['magnitude'],
            phase=mock_data['raos'][dof_name]['phase'],
            frequencies=freq_data,
            headings=heading_data,
            unit=""  # Auto-set by __post_init__
        )
        rao_components[dof_name] = component
        print(f"[OK] Created RAO component for {dof.name}")

    # Create RAOSet
    rao_set = RAOSet(
        vessel_name=vessel_name,
        analysis_tool="AQWA",
        water_depth=water_depth,
        surge=rao_components['surge'],
        sway=rao_components['sway'],
        heave=rao_components['heave'],
        roll=rao_components['roll'],
        pitch=rao_components['pitch'],
        yaw=rao_components['yaw'],
        created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        source_file="mock_aqwa_data.LIS",
        notes="Example conversion from mock AQWA data"
    )

    print("\n[OK] Created complete RAOSet")

    # Create AddedMassSet
    am_matrices = []
    for i, (freq, matrix_data) in enumerate(zip(mock_data['frequencies'], mock_data['added_mass_matrices'])):
        matrix = HydrodynamicMatrix(
            matrix=matrix_data,
            frequency=freq,
            matrix_type="added_mass",
            units={
                'linear-linear': 'kg',
                'linear-angular': 'kg.m',
                'angular-angular': 'kg.m^2'
            }
        )
        am_matrices.append(matrix)

    added_mass_set = AddedMassSet(
        vessel_name=vessel_name,
        analysis_tool="AQWA",
        water_depth=water_depth,
        matrices=am_matrices,
        frequencies=freq_data,
        created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        source_file="mock_aqwa_data.LIS"
    )

    print(f"[OK] Created AddedMassSet with {len(am_matrices)} matrices")

    # Create DampingSet
    damp_matrices = []
    for i, (freq, matrix_data) in enumerate(zip(mock_data['frequencies'], mock_data['damping_matrices'])):
        matrix = HydrodynamicMatrix(
            matrix=matrix_data,
            frequency=freq,
            matrix_type="damping",
            units={
                'linear-linear': 'N.s/m',
                'linear-angular': 'N.s or N.m.s/rad',
                'angular-angular': 'N.m.s/rad'
            }
        )
        damp_matrices.append(matrix)

    damping_set = DampingSet(
        vessel_name=vessel_name,
        analysis_tool="AQWA",
        water_depth=water_depth,
        matrices=damp_matrices,
        frequencies=freq_data,
        created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        source_file="mock_aqwa_data.LIS"
    )

    print(f"[OK] Created DampingSet with {len(damp_matrices)} matrices")

    # Create complete DiffractionResults
    results = DiffractionResults(
        vessel_name=vessel_name,
        analysis_tool="AQWA",
        water_depth=water_depth,
        raos=rao_set,
        added_mass=added_mass_set,
        damping=damping_set,
        created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        analysis_date="2026-01-03",
        source_files=["mock_aqwa_data.LIS"],
        notes="Complete example conversion demonstrating Phase 2 unified schema"
    )

    print("\n[OK] Created complete DiffractionResults object")
    print(f"  Vessel: {results.vessel_name}")
    print(f"  Tool: {results.analysis_tool}")
    print(f"  Water Depth: {results.water_depth}m")

    return results


def run_validation(results):
    """
    Run comprehensive validation on diffraction results

    Args:
        results: DiffractionResults object

    Returns:
        Validation report dictionary
    """
    print("\n" + "=" * 70)
    print("STEP 3: Running Validation")
    print("=" * 70)

    report = validate_results(results)

    return report


def export_to_orcaflex(results, output_dir):
    """
    Export results to all OrcaFlex formats

    Args:
        results: DiffractionResults object
        output_dir: Output directory path

    Returns:
        Dictionary of generated file paths
    """
    print("\n" + "=" * 70)
    print("STEP 4: Exporting to OrcaFlex Formats")
    print("=" * 70)

    exporter = OrcaFlexExporter(results, Path(output_dir))
    output_files = exporter.export_all()

    print("\n[OK] All exports complete!")
    print("\nGenerated Files:")
    for output_type, file_path in output_files.items():
        file_size = file_path.stat().st_size if file_path.exists() else 0
        print(f"  - {output_type:15s}: {file_path.name:40s} ({file_size:,} bytes)")

    return output_files


def main():
    """Main execution function"""

    print("\n" + "=" * 70)
    print("COMPLETE DIFFRACTION CONVERSION EXAMPLE")
    print("Phase 2: Output Standardization Demonstration")
    print("=" * 70)

    # Create output directory
    output_dir = Path("docs/modules/diffraction/examples/outputs")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Step 1: Create mock data
    mock_data = create_mock_aqwa_data()

    # Step 2: Convert to unified schema
    results = convert_to_unified_schema(mock_data)

    # Step 3: Validate
    validation_report = run_validation(results)

    # Export validation report
    import json
    validation_file = output_dir / "validation_report.json"
    with open(validation_file, 'w') as f:
        json.dump(validation_report, f, indent=2)
    print(f"\n[OK] Validation report saved: {validation_file}")

    # Step 4: Export to OrcaFlex
    output_files = export_to_orcaflex(results, output_dir)

    # Summary
    print("\n" + "=" * 70)
    print("EXAMPLE COMPLETE - SUMMARY")
    print("=" * 70)
    print(f"\nOutput Directory: {output_dir}")
    print(f"Total Files Generated: {len(output_files) + 1}")  # +1 for validation
    print(f"Validation Status: {validation_report['overall_status']}")

    print("\nNext Steps:")
    print("1. Open the Excel file to view formatted results")
    print("2. Review CSV files for OrcaFlex import")
    print("3. Check validation report for quality assurance")
    print("4. Use vessel_type.yml in OrcaFlex models")

    print("\n" + "=" * 70)

    return results, validation_report, output_files


if __name__ == '__main__':
    results, validation, outputs = main()
