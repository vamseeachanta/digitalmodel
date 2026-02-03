# ABOUTME: Example demonstrating unified validation pipeline with HTML report generation
# Shows how to use validators, compose pipelines, and generate interactive reports

import numpy as np
from pathlib import Path

from digitalmodel.infrastructure.validation.pipeline import (
    RangeValidator,
    MatrixValidator,
    PhysicalPlausibilityValidator,
    UnitConsistencyValidator,
    PolarDataValidator,
    TimeSeriesValidator,
    ValidationPipeline,
    generate_html_report,
)


def example_mooring_line_validation():
    """Example: Validate mooring line analysis data"""
    print("=" * 70)
    print("EXAMPLE 1: Mooring Line Validation")
    print("=" * 70)

    # Sample mooring line data
    mooring_data = {
        "tension": 850000.0,  # 850 kN
        "angle": 45.0,        # degrees
        "displacement": 3.2,  # meters
        "length": 1000.0,     # meters
    }

    # Create validators
    validators = [
        PhysicalPlausibilityValidator("tension", "force"),
        RangeValidator("angle", min_value=0, max_value=90),
        RangeValidator("displacement", min_value=0, max_value=50),
        RangeValidator("length", min_value=100, max_value=5000),
    ]

    # Create and execute pipeline
    pipeline = ValidationPipeline(validators=validators, parallel=True)
    results = pipeline.execute(mooring_data)

    # Print results
    for result in results:
        status = "PASS" if result.passed else "FAIL"
        print(f"\n{result.validator_name}: {status}")
        for issue in result.issues:
            print(f"  [{issue.severity.name}] {issue.message}")

    # Generate aggregated summary
    summary = pipeline.aggregate_results(results)
    print(f"\nSummary: {summary['passed_validators']}/{summary['total_validators']} validators passed")
    print(f"Issues: {summary['total_issues']} total ({summary['errors']} errors, {summary['warnings']} warnings)")


def example_rao_polar_data_validation():
    """Example: Validate RAO polar data"""
    print("\n" + "=" * 70)
    print("EXAMPLE 2: RAO Polar Data Validation")
    print("=" * 70)

    # Generate sample RAO data (Response Amplitude Operator)
    headings = np.arange(0, 360, 15)  # Every 15 degrees
    surge_rao = np.abs(np.sin(np.radians(headings))) * 1.5 + 0.5
    heave_rao = np.abs(np.cos(np.radians(headings))) * 1.2 + 0.3

    rao_data = {
        "heading": headings,
        "surge_rao": surge_rao,
        "heave_rao": heave_rao
    }

    # Create validators
    validators = [
        PolarDataValidator("heading", "surge_rao", max_gap_degrees=20),
        PolarDataValidator("heading", "heave_rao", max_gap_degrees=20),
        RangeValidator("surge_rao", min_value=0, max_value=5),
        RangeValidator("heave_rao", min_value=0, max_value=5),
    ]

    # Execute validation
    pipeline = ValidationPipeline(validators=validators, parallel=True)
    results = pipeline.execute(rao_data)

    # Print results
    for result in results:
        status = "PASS" if result.passed else "FAIL"
        print(f"\n{result.validator_name}: {status}")
        print(f"  Coverage: {result.summary.get('coverage', 'N/A')}deg")
        print(f"  Points: {result.summary.get('num_points', 'N/A')}")


def example_time_series_validation():
    """Example: Validate time series data"""
    print("\n" + "=" * 70)
    print("EXAMPLE 3: Time Series Validation")
    print("=" * 70)

    # Generate sample time series with some issues
    time = np.arange(0, 100, 0.1)
    displacement = np.sin(2 * np.pi * 0.1 * time) + 0.1 * np.random.randn(len(time))

    # Add an outlier
    displacement[500] = 10.0

    time_series_data = {
        "time": time,
        "displacement": displacement
    }

    # Create validators
    validators = [
        TimeSeriesValidator(
            "time",
            "displacement",
            detect_gaps=True,
            detect_outliers=True,
            outlier_std_threshold=3.0
        ),
        RangeValidator("displacement", min_value=-5, max_value=5),
    ]

    # Execute validation
    pipeline = ValidationPipeline(validators=validators, parallel=True)
    results = pipeline.execute(time_series_data)

    # Print results
    for result in results:
        status = "PASS" if result.passed else "FAIL"
        print(f"\n{result.validator_name}: {status}")
        if result.issues:
            print(f"  Found {len(result.issues)} issues:")
            for issue in result.issues[:3]:  # Show first 3
                # Avoid unicode encoding issues
                msg = str(issue.message).replace("\u03c3", "std").replace("\u00b0", "deg")
                print(f"    - {msg}")


def example_stiffness_matrix_validation():
    """Example: Validate 6x6 stiffness matrix"""
    print("\n" + "=" * 70)
    print("EXAMPLE 4: Stiffness Matrix Validation")
    print("=" * 70)

    # Create 6x6 stiffness matrix (symmetric positive-definite)
    stiffness = np.diag([1e6, 1e6, 1e6, 1e8, 1e8, 1e8])

    matrix_data = {
        "stiffness": stiffness
    }

    # Create validators
    validators = [
        MatrixValidator(
            "stiffness",
            expected_shape=(6, 6),
            check_symmetric=True,
            check_positive_definite=True
        )
    ]

    # Execute validation
    pipeline = ValidationPipeline(validators=validators)
    results = pipeline.execute(matrix_data)

    # Print results
    for result in results:
        status = "PASS" if result.passed else "FAIL"
        print(f"\n{result.validator_name}: {status}")
        print(f"  Shape: {result.summary.get('shape', 'N/A')}")
        print(f"  Symmetric: {result.summary.get('symmetric_check', 'N/A')}")
        print(f"  Positive Definite: {result.summary.get('positive_definite_check', 'N/A')}")


def example_comprehensive_validation_with_html_report():
    """Example: Comprehensive validation with HTML report generation"""
    print("\n" + "=" * 70)
    print("EXAMPLE 5: Comprehensive Validation with HTML Report")
    print("=" * 70)

    # Create comprehensive dataset
    data = {
        "tension": 950000.0,  # Slightly high
        "moment": 5.5e6,
        "displacement": 4.2,
        "heading": np.arange(0, 360, 15),
        "rao": np.abs(np.sin(np.radians(np.arange(0, 360, 15)))) + 0.5,
        "time": np.arange(0, 100, 0.1),
        "acceleration": np.random.randn(1000) * 2.0,
        "stiffness_matrix": np.diag([1e6, 1e6, 1e6, 1e8, 1e8, 1e8])
    }

    # Create comprehensive validator list
    validators = [
        PhysicalPlausibilityValidator("tension", "force"),
        PhysicalPlausibilityValidator("moment", "moment"),
        PhysicalPlausibilityValidator("displacement", "displacement"),
        PolarDataValidator("heading", "rao", max_gap_degrees=20),
        RangeValidator("rao", min_value=0, max_value=5),
        TimeSeriesValidator("time", "acceleration", detect_gaps=True, detect_outliers=True),
        MatrixValidator(
            "stiffness_matrix",
            expected_shape=(6, 6),
            check_symmetric=True,
            check_positive_definite=True
        ),
    ]

    # Execute pipeline with caching
    pipeline = ValidationPipeline(validators=validators, parallel=True, fail_fast=False)
    results = pipeline.execute(data)

    # Print summary
    summary = pipeline.aggregate_results(results)
    print(f"\nValidation Summary:")
    print(f"  Total validators: {summary['total_validators']}")
    print(f"  Passed: {summary['passed_validators']}")
    print(f"  Failed: {summary['failed_validators']}")
    print(f"  Total issues: {summary['total_issues']}")
    print(f"    - Critical: {summary['critical_issues']}")
    print(f"    - Errors: {summary['errors']}")
    print(f"    - Warnings: {summary['warnings']}")
    print(f"    - Info: {summary['info']}")

    # Generate HTML report
    output_dir = Path(__file__).parent.parent / "reports" / "validation"
    output_dir.mkdir(parents=True, exist_ok=True)

    html_file = output_dir / "validation_report_example.html"
    json_file = output_dir / "validation_results_example.json"

    generate_html_report(results, html_file, export_json=json_file)

    print(f"\nHTML Report generated: {html_file}")
    print(f"JSON Results exported: {json_file}")
    print(f"\nOpen the HTML file in your browser to view the interactive report!")


if __name__ == "__main__":
    # Run all examples
    example_mooring_line_validation()
    example_rao_polar_data_validation()
    example_time_series_validation()
    example_stiffness_matrix_validation()
    example_comprehensive_validation_with_html_report()

    print("\n" + "=" * 70)
    print("All examples completed successfully!")
    print("=" * 70)
