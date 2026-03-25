#!/usr/bin/env python3
"""
ABOUTME: Example script demonstrating StandardReport framework usage
ABOUTME: Shows report creation, parametric studies, and export to multiple formats
"""

import sys
from pathlib import Path
import time

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.reporting import (
    StandardReport,
    ReportMetadata,
    ParametricStudy,
    export_all_formats,
    export_parametric_study_html,
)


def example_1_basic_report():
    """Example 1: Create a basic analysis report"""
    print("=" * 70)
    print("EXAMPLE 1: Basic Analysis Report")
    print("=" * 70)

    # Create report metadata
    metadata = ReportMetadata(
        module="structural_analysis",
        analysis_type="stress_analysis",
        execution_time_seconds=12.5,
        status="success"
    )

    # Create report
    report = StandardReport(metadata=metadata)

    # Add parameters
    report.add_parameter("safety_factor", 1.5, unit="-", description="Global safety factor")
    report.add_parameter("material", "S355", unit="-", description="Steel grade")
    report.add_parameter("thickness", 12.0, unit="mm", description="Plate thickness")

    # Add results
    report.add_result(
        metric_name="max_von_mises_stress",
        value=250.5,
        unit="MPa",
        description="Maximum von Mises stress",
        passed=True,
        threshold=300.0
    )

    report.add_result(
        metric_name="max_displacement",
        value=8.3,
        unit="mm",
        description="Maximum displacement"
    )

    report.add_result(
        metric_name="capacity_ratio",
        value=0.835,
        unit="-",
        description="Utilization ratio",
        passed=True,
        threshold=1.0
    )

    # Add validations
    report.add_validation(
        check_name="stress_check",
        passed=True,
        message="All stresses within allowable limits",
        severity="info"
    )

    report.add_validation(
        check_name="deflection_check",
        passed=True,
        message="Deflections acceptable",
        severity="info"
    )

    # Get summary
    summary = report.get_summary()
    print(f"\nReport Summary:")
    print(f"  Module: {summary['module']}")
    print(f"  Analysis Type: {summary['analysis_type']}")
    print(f"  Parameters: {summary['parameter_count']}")
    print(f"  Results: {summary['result_count']}")
    print(f"  Validations: {summary['validation_count']}")
    print(f"  Validations Passed: {summary['validations_passed']}/{summary['validation_count']}")

    # Export to all formats
    output_dir = Path(__file__).parent.parent / "reports" / "examples"
    files = export_all_formats(report, output_dir / "basic_report")

    print(f"\n✓ Exported to:")
    for format_name, filepath in files.items():
        print(f"  - {format_name.upper()}: {filepath}")

    print()
    return report


def example_2_fatigue_analysis_report():
    """Example 2: Fatigue analysis report with complex results"""
    print("=" * 70)
    print("EXAMPLE 2: Fatigue Analysis Report")
    print("=" * 70)

    # Create report
    metadata = ReportMetadata(
        module="fatigue_analysis",
        analysis_type="rainflow_counting",
        execution_time_seconds=45.8,
        status="success"
    )

    report = StandardReport(metadata=metadata)

    # Add parameters
    report.add_parameter("sn_curve", "DNV-RP-C203 Curve D", description="S-N curve standard")
    report.add_parameter("safety_factor_fatigue", 1.0, unit="-")
    report.add_parameter("reference_stress", 100.0, unit="MPa", description="Reference stress range")

    # Add results with complex values
    report.add_result(
        metric_name="total_cycles",
        value=15234,
        unit="cycles",
        description="Total number of stress cycles counted"
    )

    report.add_result(
        metric_name="fatigue_damage",
        value=0.756,
        unit="-",
        description="Palmgren-Miner cumulative damage",
        passed=True,
        threshold=1.0
    )

    report.add_result(
        metric_name="fatigue_life",
        value=28.5,
        unit="years",
        description="Estimated fatigue life"
    )

    # Complex result: stress range histogram
    report.add_result(
        metric_name="stress_range_bins",
        value=[50, 75, 100, 125, 150, 175, 200],
        unit="MPa",
        description="Stress range bin centers"
    )

    report.add_result(
        metric_name="cycle_counts",
        value=[5234, 3842, 2891, 1753, 981, 421, 112],
        unit="cycles",
        description="Cycle counts per bin"
    )

    # Add validations
    report.add_validation(
        check_name="damage_check",
        passed=True,
        message="Fatigue damage within acceptable limits (< 1.0)",
        severity="info",
        details={"damage": 0.756, "limit": 1.0}
    )

    report.add_validation(
        check_name="life_check",
        passed=True,
        message="Fatigue life exceeds design life requirement",
        severity="info",
        details={"calculated_life_years": 28.5, "required_life_years": 20.0}
    )

    # Export
    output_dir = Path(__file__).parent.parent / "reports" / "examples"
    files = export_all_formats(report, output_dir / "fatigue_report")

    print(f"\n✓ Fatigue Report Exported:")
    for format_name, filepath in files.items():
        print(f"  - {format_name.upper()}: {filepath}")

    print()
    return report


def example_3_parametric_study():
    """Example 3: Parametric study varying safety factor"""
    print("=" * 70)
    print("EXAMPLE 3: Parametric Study (Safety Factor)")
    print("=" * 70)

    # Create parametric study
    study = ParametricStudy(
        study_name="Safety Factor Parametric Study",
        description="Analyzing effect of safety factor on stress and capacity",
        parameter_name="safety_factor"
    )

    # Run analyses for different safety factors
    safety_factors = [1.2, 1.5, 1.8, 2.0, 2.5, 3.0]

    print(f"\nRunning {len(safety_factors)} analyses...")

    for sf in safety_factors:
        print(f"  - Safety factor: {sf}")

        # Simulate analysis (in real use, this would be actual analysis)
        metadata = ReportMetadata(
            module="structural_analysis",
            analysis_type="parametric_stress_check",
            execution_time_seconds=5.0,
            status="success"
        )

        report = StandardReport(metadata=metadata)

        # Add parameters
        report.add_parameter("safety_factor", sf, unit="-")
        report.add_parameter("load", 1000.0, unit="kN")

        # Calculate results (simplified example)
        max_stress = 300.0 / sf  # Stress inversely proportional to SF
        capacity_ratio = 1.0 / sf
        displacement = 10.0 * sf  # Displacement proportional to SF

        report.add_result("max_stress", max_stress, unit="MPa", passed=max_stress < 300)
        report.add_result("capacity_ratio", capacity_ratio, unit="-", passed=capacity_ratio < 1.0)
        report.add_result("displacement", displacement, unit="mm")

        # Add validation
        passed = max_stress < 300 and capacity_ratio < 1.0
        report.add_validation(
            check_name="design_check",
            passed=passed,
            message=f"Design check: {'PASS' if passed else 'FAIL'}",
            severity="info" if passed else "error"
        )

        study.add_report(report)

    # Get comparison table
    table = study.get_comparison_table()

    print(f"\n✓ Study Complete!")
    print(f"  Reports: {len(study.reports)}")
    print(f"  Parameter values: {study.get_parameter_values()}")

    # Display comparison table
    print(f"\nComparison Table:")
    print(f"{'Safety Factor':<15} {'Max Stress (MPa)':<20} {'Capacity Ratio':<20} {'Displacement (mm)':<20}")
    print("-" * 75)
    for i in range(len(table["safety_factor"])):
        sf = table["safety_factor"][i]
        stress = table["max_stress"][i]
        ratio = table["capacity_ratio"][i]
        disp = table["displacement"][i]
        print(f"{sf:<15.1f} {stress:<20.2f} {ratio:<20.3f} {disp:<20.2f}")

    # Export study
    output_dir = Path(__file__).parent.parent / "reports" / "examples"
    study.save_json(output_dir / "parametric_study.json")

    # Export comparison HTML
    export_parametric_study_html(study, output_dir / "parametric_study.html")

    print(f"\n✓ Parametric Study Exported:")
    print(f"  - JSON: {output_dir / 'parametric_study.json'}")
    print(f"  - HTML: {output_dir / 'parametric_study.html'}")

    print()
    return study


def example_4_workflow_integration():
    """Example 4: Integration with analysis workflow"""
    print("=" * 70)
    print("EXAMPLE 4: Workflow Integration")
    print("=" * 70)

    print("\nSimulating analysis workflow...\n")

    # Start timing
    start_time = time.time()

    # Step 1: Setup
    print("Step 1: Loading configuration...")
    time.sleep(0.5)
    print("  ✓ Configuration loaded")

    # Step 2: Analysis
    print("\nStep 2: Running analysis...")
    time.sleep(1.0)
    print("  ✓ Analysis complete")

    # Step 3: Create report
    print("\nStep 3: Generating report...")

    execution_time = time.time() - start_time

    metadata = ReportMetadata(
        module="orcaflex_integration",
        analysis_type="dynamic_analysis",
        execution_time_seconds=execution_time,
        status="success"
    )

    report = StandardReport(metadata=metadata)

    # Add workflow parameters
    report.add_parameter("model_file", "mooring_system.dat", description="OrcaFlex model")
    report.add_parameter("simulation_time", 3600.0, unit="s", description="Simulation duration")
    report.add_parameter("time_step", 0.1, unit="s", description="Integration time step")

    # Add results
    report.add_result("max_line_tension", 1250.5, unit="kN", description="Maximum line tension")
    report.add_result("mean_offset", 15.3, unit="m", description="Mean vessel offset")
    report.add_result("computation_time", execution_time, unit="s", description="Total computation time")

    # Validation
    report.add_validation(
        check_name="tension_check",
        passed=True,
        message="Line tensions within MBL limits"
    )

    # Export
    output_dir = Path(__file__).parent.parent / "reports" / "examples"
    files = export_all_formats(report, output_dir / "workflow_report")

    print(f"  ✓ Report generated and exported")

    print(f"\n✓ Workflow Complete!")
    print(f"  Execution time: {execution_time:.2f}s")
    print(f"  Reports saved to: {output_dir}")

    print()
    return report


def main():
    """Run all examples"""
    print()
    print("=" * 70)
    print("  digitalmodel - StandardReport Framework Examples")
    print("=" * 70)
    print()

    try:
        # Run examples
        report1 = example_1_basic_report()
        report2 = example_2_fatigue_analysis_report()
        study = example_3_parametric_study()
        report4 = example_4_workflow_integration()

        print("=" * 70)
        print("✅ ALL EXAMPLES COMPLETED SUCCESSFULLY")
        print("=" * 70)
        print()
        print("Generated Reports:")
        print("  1. Basic Report (HTML, JSON, CSV)")
        print("  2. Fatigue Analysis Report (HTML, JSON, CSV)")
        print("  3. Parametric Study (JSON, HTML comparison table)")
        print("  4. Workflow Integration Report (HTML, JSON, CSV)")
        print()
        print("Check reports/examples/ directory for generated files!")
        print()

    except Exception as e:
        print()
        print("=" * 70)
        print("❌ EXAMPLE FAILED")
        print("=" * 70)
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        print()
        sys.exit(1)


if __name__ == '__main__':
    main()
