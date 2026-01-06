#!/usr/bin/env python3
"""
OrcaWave Conversion Example

ABOUTME: Complete end-to-end example demonstrating OrcaWave converter workflow with mock data for testing without OrcFxAPI.

This example shows:
1. Creating mock vessel data using test utilities
2. Using OrcaWaveDataExtractor to extract diffraction data
3. Converting to unified DiffractionResults schema
4. Running comprehensive validation
5. Exporting to all 6 OrcaFlex formats

Can be run without OrcFxAPI by using mock data generators.

Version: 3.0.0 (Phase 3)
Date: 2026-01-04
"""

import sys
from pathlib import Path

# Add src to path for imports
repo_root = Path(__file__).parents[4]
sys.path.insert(0, str(repo_root / "src"))

from digitalmodel.modules.diffraction.orcawave_test_utilities import (
    MockDataGenerator,
    create_test_vessel
)
from digitalmodel.modules.diffraction.orcawave_data_extraction import (
    OrcaWaveDataExtractor,
    extract_all_rao_data,
    extract_all_added_mass,
    extract_all_damping
)
from digitalmodel.modules.diffraction.output_schemas import (
    DiffractionResults,
    RAOSet,
    AddedMassSet,
    DampingSet,
    RAOComponent,
    HydrodynamicMatrix,
    FrequencyData,
    HeadingData,
    DOF
)
from digitalmodel.modules.diffraction.orcaflex_exporter import OrcaFlexExporter
from digitalmodel.modules.diffraction.output_validator import validate_results

import numpy as np
from datetime import datetime


def main():
    """Run complete OrcaWave conversion example"""

    print("=" * 80)
    print("OrcaWave Conversion Example - Complete Workflow")
    print("=" * 80)
    print()

    # Configuration
    vessel_name = "TestFPSO"
    water_depth = 1200.0  # meters
    output_dir = Path(__file__).parent / "outputs" / "orcawave_test"
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"Vessel: {vessel_name}")
    print(f"Water Depth: {water_depth} m")
    print(f"Output Directory: {output_dir}")
    print()

    # ========================================================================
    # STEP 1: Create Mock Vessel Data
    # ========================================================================
    print("STEP 1: Creating Mock Vessel Data")
    print("-" * 80)

    # Create test vessel with realistic diffraction data
    mock_vessel = create_test_vessel(
        vessel_name=vessel_name,
        nfreq=17,  # 17 frequencies from 0.2 to 2.0 rad/s
        nhead=12   # 12 headings from 0 to 330 degrees
    )

    print(f"[OK] Created mock vessel: {mock_vessel.Name}")
    print(f"     RAO table size: {mock_vessel.VesselType.LoadRAOs.Size} entries")
    print(f"     Added mass/damping table size: {mock_vessel.VesselType.AddedMassAndDamping.Size} entries")
    print()

    # ========================================================================
    # STEP 2: Extract Data Using OrcaWaveDataExtractor
    # ========================================================================
    print("STEP 2: Extracting Data Using OrcaWaveDataExtractor")
    print("-" * 80)

    extractor = OrcaWaveDataExtractor(mock_vessel)

    # Extract RAO data
    print("Extracting RAO data...")
    rao_data = extract_all_rao_data(mock_vessel)
    print(f"[OK] Extracted RAO data:")
    print(f"     Frequencies: {len(rao_data['frequencies'])}")
    print(f"     Headings: {len(rao_data['headings'])}")
    print(f"     DOFs: {list(k for k in rao_data.keys() if k not in ['frequencies', 'headings'])}")

    # Extract added mass data
    print("\nExtracting added mass data...")
    added_mass_data = extract_all_added_mass(mock_vessel)
    print(f"[OK] Extracted {len(added_mass_data['matrices'])} added mass matrices")

    # Extract damping data
    print("Extracting damping data...")
    damping_data = extract_all_damping(mock_vessel)
    print(f"[OK] Extracted {len(damping_data['matrices'])} damping matrices")
    print()

    # ========================================================================
    # STEP 3: Build Unified Schema Objects
    # ========================================================================
    print("STEP 3: Building Unified Schema Objects")
    print("-" * 80)

    # Build FrequencyData
    frequencies = FrequencyData(
        values=rao_data['frequencies'],
        periods=2.0 * np.pi / rao_data['frequencies'],
        count=len(rao_data['frequencies']),
        min_freq=rao_data['frequencies'].min(),
        max_freq=rao_data['frequencies'].max()
    )

    # Build HeadingData
    headings = HeadingData(
        values=rao_data['headings'],
        count=len(rao_data['headings']),
        min_heading=rao_data['headings'].min(),
        max_heading=rao_data['headings'].max()
    )

    print(f"[OK] Frequency data: {frequencies.count} frequencies")
    print(f"     Range: {frequencies.min_freq:.3f} to {frequencies.max_freq:.3f} rad/s")
    print(f"[OK] Heading data: {headings.count} headings")
    print(f"     Range: {headings.min_heading:.1f} to {headings.max_heading:.1f} deg")

    # Build RAO components
    print("\nBuilding RAO components...")
    rao_components = {}
    for dof in DOF:
        dof_name = dof.name.lower()
        if dof_name in rao_data:
            component = RAOComponent(
                dof=dof,
                magnitude=rao_data[dof_name]['magnitude'],
                phase=rao_data[dof_name]['phase'],
                frequencies=frequencies,
                headings=headings,
                unit=""  # Will be set by __post_init__
            )
            rao_components[dof_name] = component
            print(f"     {dof.name}: magnitude range [{component.magnitude.min():.3f}, {component.magnitude.max():.3f}] {component.unit}")

    # Build RAOSet
    rao_set = RAOSet(
        vessel_name=vessel_name,
        analysis_tool="OrcaWave",
        water_depth=water_depth,
        surge=rao_components.get('surge'),
        sway=rao_components.get('sway'),
        heave=rao_components.get('heave'),
        roll=rao_components.get('roll'),
        pitch=rao_components.get('pitch'),
        yaw=rao_components.get('yaw'),
        created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        source_file="mock_test_data"
    )
    print(f"[OK] RAOSet created")

    # Build AddedMassSet
    print("\nBuilding added mass matrices...")
    am_matrices = []
    for freq, matrix_data in zip(added_mass_data['frequencies'], added_mass_data['matrices']):
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

    am_frequencies = FrequencyData(
        values=added_mass_data['frequencies'],
        periods=2.0 * np.pi / added_mass_data['frequencies'],
        count=len(added_mass_data['frequencies']),
        min_freq=added_mass_data['frequencies'].min(),
        max_freq=added_mass_data['frequencies'].max()
    )

    added_mass_set = AddedMassSet(
        vessel_name=vessel_name,
        analysis_tool="OrcaWave",
        water_depth=water_depth,
        matrices=am_matrices,
        frequencies=am_frequencies,
        created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        source_file="mock_test_data"
    )
    print(f"[OK] AddedMassSet created with {len(am_matrices)} matrices")

    # Build DampingSet
    print("Building damping matrices...")
    damp_matrices = []
    for freq, matrix_data in zip(damping_data['frequencies'], damping_data['matrices']):
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

    damp_frequencies = FrequencyData(
        values=damping_data['frequencies'],
        periods=2.0 * np.pi / damping_data['frequencies'],
        count=len(damping_data['frequencies']),
        min_freq=damping_data['frequencies'].min(),
        max_freq=damping_data['frequencies'].max()
    )

    damping_set = DampingSet(
        vessel_name=vessel_name,
        analysis_tool="OrcaWave",
        water_depth=water_depth,
        matrices=damp_matrices,
        frequencies=damp_frequencies,
        created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        source_file="mock_test_data"
    )
    print(f"[OK] DampingSet created with {len(damp_matrices)} matrices")

    # Build complete DiffractionResults
    print("\nBuilding complete DiffractionResults...")
    results = DiffractionResults(
        vessel_name=vessel_name,
        analysis_tool="OrcaWave",
        water_depth=water_depth,
        raos=rao_set,
        added_mass=added_mass_set,
        damping=damping_set,
        created_date=datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        analysis_date=None,
        source_files=["mock_test_data"],
        notes="Generated from OrcaWave test utilities for validation"
    )
    print(f"[OK] DiffractionResults created")
    print()

    # ========================================================================
    # STEP 4: Validate Results
    # ========================================================================
    print("STEP 4: Validating Results")
    print("-" * 80)

    validation_report = validate_results(
        results,
        output_file=output_dir / f"{vessel_name}_validation.json"
    )

    print(f"Overall Status: {validation_report['overall_status']}")
    print()

    # Print validation details
    categories = [
        'schema_validation',
        'physical_validity',
        'range_checks',
        'frequency_coverage',
        'heading_coverage',
        'symmetry_checks'
    ]

    for category in categories:
        if category in validation_report:
            cat_data = validation_report[category]
            if isinstance(cat_data, dict):
                total_issues = sum(len(v) if isinstance(v, list) else 0 for v in cat_data.values())
                status = "PASS" if total_issues == 0 else "WARNING/FAIL"
                print(f"  {category}: {status}")
                if total_issues > 0:
                    for key, issues in cat_data.items():
                        if isinstance(issues, list) and issues:
                            print(f"    - {key}: {len(issues)} issue(s)")

    print()
    print(f"[OK] Validation report saved to: {output_dir / f'{vessel_name}_validation.json'}")
    print()

    # ========================================================================
    # STEP 5: Export to OrcaFlex Formats
    # ========================================================================
    print("STEP 5: Exporting to OrcaFlex Formats")
    print("-" * 80)

    exporter = OrcaFlexExporter(results, output_dir)
    output_files = exporter.export_all()

    print("Generated output files:")
    for output_type, file_path in output_files.items():
        file_size = file_path.stat().st_size / 1024  # KB
        print(f"  - {output_type:20s}: {file_path.name:40s} ({file_size:.1f} KB)")

    print()

    # ========================================================================
    # STEP 6: Summary Statistics
    # ========================================================================
    print("STEP 6: Summary Statistics")
    print("-" * 80)

    # RAO statistics
    print("RAO Statistics:")
    for dof in DOF:
        dof_name = dof.name.lower()
        if dof_name in rao_components:
            comp = rao_components[dof_name]
            print(f"  {dof.name:8s}: mag=[{comp.magnitude.min():8.3f}, {comp.magnitude.max():8.3f}] {comp.unit}")

    # Matrix statistics
    print("\nAdded Mass Statistics (diagonal terms):")
    sample_matrix = am_matrices[len(am_matrices)//2].matrix  # Middle frequency
    for i, dof_name in enumerate(['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']):
        print(f"  {dof_name:8s}: {sample_matrix[i,i]:12.2e}")

    print("\nDamping Statistics (diagonal terms):")
    sample_matrix = damp_matrices[len(damp_matrices)//2].matrix  # Middle frequency
    for i, dof_name in enumerate(['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']):
        print(f"  {dof_name:8s}: {sample_matrix[i,i]:12.2e}")

    print()
    print("=" * 80)
    print("OrcaWave Conversion Example Complete!")
    print("=" * 80)
    print()
    print(f"All outputs saved to: {output_dir}")
    print()

    # Return results for testing
    return results, validation_report, output_files


if __name__ == "__main__":
    results, validation_report, output_files = main()
