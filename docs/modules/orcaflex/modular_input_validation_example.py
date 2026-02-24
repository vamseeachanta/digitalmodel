"""
Example usage of OrcaFlex modular input validation system.

This script demonstrates how to use the validation framework to validate
CALM buoy YAML input files at all three levels.
"""

from pathlib import Path
from digitalmodel.orcaflex.modular_input_validation import (
    ModularInputValidator,
    ValidationConfig
)


def example_basic_validation():
    """Example: Basic validation of a single file."""
    print("=" * 80)
    print("Example 1: Basic Validation")
    print("=" * 80)

    # Create validator with default configuration
    validator = ModularInputValidator()

    # Validate a single file
    file_path = Path("specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml")

    if file_path.exists():
        result = validator.validate_file(file_path)

        print(f"File: {result.file_path.name}")
        print(f"Overall Status: {result.overall_status.value}")
        print(f"Validation Time: {result.validation_time_seconds:.2f}s")
        print(f"Issues Found: {len(result.issues)}")
        print()


def example_custom_configuration():
    """Example: Validation with custom configuration."""
    print("=" * 80)
    print("Example 2: Custom Configuration")
    print("=" * 80)

    # Create custom configuration
    config = ValidationConfig(
        tolerance_percent=15.0,  # Use 15% tolerance instead of default 10%
        enable_orcaflex=True,
        skip_levels=[],  # Run all levels
        generate_reports=True,
        report_formats=['console', 'csv', 'markdown'],  # Skip HTML
        reports_dir=Path('reports/validation/calm_buoy'),
        enable_color=True
    )

    # Create validator with custom config
    validator = ModularInputValidator(config)

    # Validate directory
    directory = Path("specs/modules/orcaflex/modular-input-file/output")

    if directory.exists():
        results = validator.validate_directory(directory, pattern="*.yml")

        # Get summary
        summary = validator.get_validation_summary(results)

        print(f"Validated {summary['total_files']} files")
        print(f"Passed: {summary['passed']}")
        print(f"Warnings: {summary['warnings']}")
        print(f"Failed: {summary['failed']}")
        print()


def example_programmatic_workflow():
    """Example: Programmatic workflow with report generation."""
    print("=" * 80)
    print("Example 3: Programmatic Workflow")
    print("=" * 80)

    # Create validator
    validator = ModularInputValidator()

    # Validate multiple files
    files = [
        Path("specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml"),
        Path("specs/modules/orcaflex/modular-input-file/output/discretised_calm_buoy_base.yml")
    ]

    # Filter existing files
    existing_files = [f for f in files if f.exists()]

    if existing_files:
        # Run validation
        results = validator.validate_files(existing_files)

        # Generate reports
        validator.generate_reports(results)

        # Process results programmatically
        for result in results:
            print(f"\nFile: {result.file_path.name}")

            # Level 1 results
            if result.level_1:
                print(f"  Level 1: {result.level_1.status.value}")
                print(f"    YAML Valid: {result.level_1.yaml_valid}")
                print(f"    Includes Resolved: {result.level_1.includes_resolved}")

            # Level 2 results
            if result.level_2:
                print(f"  Level 2: {result.level_2.status.value}")
                print(f"    OrcaFlex Available: {result.level_2.orcaflex_available.value}")
                print(f"    File Loadable: {result.level_2.orcaflex_loadable}")

            # Level 3 results
            if result.level_3:
                print(f"  Level 3: {result.level_3.status.value}")
                print(f"    Parameters Validated: {result.level_3.parameters_validated}")
                print(f"    Critical Issues: {result.level_3.critical_issues}")
                print(f"    Warnings: {result.level_3.warnings}")


def example_level_selective_validation():
    """Example: Running only specific validation levels."""
    print("=" * 80)
    print("Example 4: Selective Level Validation")
    print("=" * 80)

    # Run only Level 1 and Level 3 (skip OrcaFlex API check)
    config = ValidationConfig(
        skip_levels=[2],  # Skip Level 2
        generate_reports=False
    )

    validator = ModularInputValidator(config)

    file_path = Path("specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml")

    if file_path.exists():
        result = validator.validate_file(file_path)

        print(f"File: {result.file_path.name}")
        print(f"Level 1 Status: {result.level_1.status.value if result.level_1 else 'N/A'}")
        print(f"Level 2 Status: {result.level_2.status.value if result.level_2 else 'SKIPPED'}")
        print(f"Level 3 Status: {result.level_3.status.value if result.level_3 else 'N/A'}")
        print()


def example_cli_equivalent():
    """Example: Programmatic equivalent of CLI usage."""
    print("=" * 80)
    print("Example 5: CLI Equivalent Usage")
    print("=" * 80)

    # This is equivalent to:
    # python -m digitalmodel.orcaflex.modular_input_validation.cli \
    #   path/to/files/ --tolerance 12 --formats console csv --skip-level 2

    config = ValidationConfig(
        tolerance_percent=12.0,
        skip_levels=[2],
        report_formats=['console', 'csv'],
        generate_reports=True
    )

    validator = ModularInputValidator(config)

    # Run validation workflow
    directory = Path("specs/modules/orcaflex/modular-input-file/output")

    if directory.exists():
        results = validator.run_validation(directory, generate_reports=True)

        print(f"Validated {len(results)} files")
        print("Reports generated in:", config.reports_dir)
        print()


if __name__ == '__main__':
    # Run all examples
    example_basic_validation()
    example_custom_configuration()
    example_programmatic_workflow()
    example_level_selective_validation()
    example_cli_equivalent()

    print("=" * 80)
    print("Examples complete!")
    print("=" * 80)
