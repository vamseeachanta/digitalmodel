#!/usr/bin/env python
"""RAO Quality Check CLI.

ABOUTME: CLI tool for validating displacement RAO data quality.
ABOUTME: Checks phase angles at long periods and peak period detection.

Usage:
    python scripts/rao_quality_check.py <rao_file.yml> [options]

Examples:
    # Basic quality check with HTML report
    python scripts/rao_quality_check.py data/vessel_raos.yml

    # Specify vessel type
    python scripts/rao_quality_check.py data/vessel_raos.yml --vessel-type semi_submersible

    # Custom output directory
    python scripts/rao_quality_check.py data/vessel_raos.yml --output-dir reports/qa

    # Batch processing
    python scripts/rao_quality_check.py data/*.yml --batch
"""

import argparse
import sys
from pathlib import Path
import yaml
from typing import List, Optional

# Add package to path for development
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.modules.marine_analysis import (
    RAODataValidators,
    VesselType,
    DisplacementRAOQualityReport,
)
from digitalmodel.modules.marine_analysis.rao_quality_report import RAOQualityReportGenerator


def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Validate displacement RAO data quality",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Check single file
  python scripts/rao_quality_check.py data/vessel_raos.yml

  # Specify vessel type
  python scripts/rao_quality_check.py data/raos.yml --vessel-type ship

  # Custom tolerances
  python scripts/rao_quality_check.py data/raos.yml --amp-tol 0.10 --phase-tol 15

  # Generate CSV only
  python scripts/rao_quality_check.py data/raos.yml --csv-only
        """
    )

    parser.add_argument(
        "rao_files",
        nargs="+",
        help="RAO file(s) to validate (OrcaFlex YAML format)"
    )

    parser.add_argument(
        "--vessel-type",
        choices=["auto", "ship", "fpso", "semi_submersible", "spar", "barge"],
        default="auto",
        help="Vessel type for validation (default: auto-detect)"
    )

    parser.add_argument(
        "--amp-tol",
        type=float,
        default=0.05,
        help="Amplitude tolerance as fraction (default: 0.05 = 5%%)"
    )

    parser.add_argument(
        "--phase-tol",
        type=float,
        default=10.0,
        help="Phase tolerance in degrees (default: 10.0)"
    )

    parser.add_argument(
        "--output-dir",
        type=str,
        default="docs/reports/rao_qa",
        help="Output directory for reports (default: docs/reports/rao_qa)"
    )

    parser.add_argument(
        "--csv-only",
        action="store_true",
        help="Generate CSV summary only (no HTML)"
    )

    parser.add_argument(
        "--no-report",
        action="store_true",
        help="Run checks without generating reports"
    )

    parser.add_argument(
        "--quiet",
        "-q",
        action="store_true",
        help="Suppress console output"
    )

    parser.add_argument(
        "--fail-on-warning",
        action="store_true",
        help="Return non-zero exit code on warnings"
    )

    return parser.parse_args()


def get_vessel_type(vessel_type_str: str) -> Optional[VesselType]:
    """Convert string to VesselType enum."""
    if vessel_type_str == "auto":
        return None

    type_map = {
        "ship": VesselType.SHIP,
        "fpso": VesselType.FPSO,
        "semi_submersible": VesselType.SEMI_SUBMERSIBLE,
        "spar": VesselType.SPAR,
        "barge": VesselType.BARGE,
    }
    return type_map.get(vessel_type_str)


def print_report_summary(report: DisplacementRAOQualityReport, quiet: bool = False):
    """Print report summary to console."""
    if quiet:
        return

    print("\n" + "=" * 60)
    print("RAO QUALITY CHECK SUMMARY")
    print("=" * 60)
    print(f"Source File: {report.source_file}")
    print(f"Vessel Type: {report.vessel_type.value} (confidence: {report.vessel_type_confidence:.1%})")
    print(f"Long Period Threshold: {report.long_period_threshold:.1f}s")
    print("-" * 60)
    print(f"Total Checks: {report.total_checks}")
    print(f"  Passed:   {report.passed_checks}")
    print(f"  Warnings: {report.warning_checks}")
    print(f"  Failed:   {report.failed_checks}")
    print(f"Pass Rate: {report.pass_rate:.1f}%")
    print(f"Overall Status: {report.overall_status}")
    print("-" * 60)

    # Show failures
    if report.failed_checks > 0:
        print("\nFAILED CHECKS:")
        for check in report.phase_checks:
            if check.status == "FAIL":
                print(f"  - {check.message}")
        for check in report.peak_checks:
            if check.status == "FAIL":
                print(f"  - {check.message}")

    # Show warnings
    if report.warning_checks > 0:
        print("\nWARNINGS:")
        for check in report.phase_checks:
            if check.status == "WARNING":
                print(f"  - {check.message}")
        for check in report.peak_checks:
            if check.status == "WARNING":
                print(f"  - {check.message}")

    print("=" * 60)


def process_file(
    rao_file: Path,
    vessel_type: Optional[VesselType],
    amp_tol: float,
    phase_tol: float,
    output_dir: str,
    csv_only: bool,
    no_report: bool,
    quiet: bool,
) -> DisplacementRAOQualityReport:
    """Process a single RAO file."""
    # Load RAO data
    with open(rao_file, 'r', encoding='utf-8') as f:
        rao_data = yaml.safe_load(f)

    # Initialize validators
    validators = RAODataValidators()

    # Run quality check
    report = validators.validate_displacement_rao_quality(
        rao_data,
        source_file=str(rao_file),
        vessel_type=vessel_type,
        amplitude_tolerance=amp_tol,
        phase_tolerance=phase_tol,
    )

    # Print summary
    print_report_summary(report, quiet)

    # Generate reports
    if not no_report:
        generator = RAOQualityReportGenerator(output_dir=output_dir)
        report_name = rao_file.stem

        if not csv_only:
            html_path = generator.generate_html_report(report, report_name=report_name)
            if not quiet:
                print(f"\nHTML Report: {html_path}")

        csv_path = generator.export_csv_summary(report, report_name=report_name)
        if not quiet:
            print(f"CSV Summary: {csv_path}")

    return report


def main():
    """Main entry point."""
    args = parse_args()

    # Parse vessel type
    vessel_type = get_vessel_type(args.vessel_type)

    # Track overall status
    all_passed = True
    total_files = 0
    failed_files = 0

    # Process each file
    for rao_file_pattern in args.rao_files:
        rao_path = Path(rao_file_pattern)

        if rao_path.exists():
            files = [rao_path]
        else:
            # Handle glob patterns
            files = list(Path(".").glob(rao_file_pattern))

        for rao_file in files:
            total_files += 1

            if not args.quiet:
                print(f"\nProcessing: {rao_file}")

            try:
                report = process_file(
                    rao_file=rao_file,
                    vessel_type=vessel_type,
                    amp_tol=args.amp_tol,
                    phase_tol=args.phase_tol,
                    output_dir=args.output_dir,
                    csv_only=args.csv_only,
                    no_report=args.no_report,
                    quiet=args.quiet,
                )

                # Check status
                if report.overall_status == "FAIL":
                    all_passed = False
                    failed_files += 1
                elif report.overall_status == "WARNING" and args.fail_on_warning:
                    all_passed = False
                    failed_files += 1

            except Exception as e:
                if not args.quiet:
                    print(f"ERROR processing {rao_file}: {e}")
                all_passed = False
                failed_files += 1

    # Final summary
    if not args.quiet and total_files > 1:
        print("\n" + "=" * 60)
        print(f"BATCH SUMMARY: {total_files - failed_files}/{total_files} files passed")
        print("=" * 60)

    # Exit code
    sys.exit(0 if all_passed else 1)


if __name__ == "__main__":
    main()
