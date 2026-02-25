"""
ABOUTME: CSV reporter for validation logs
ABOUTME: Generates timestamped CSV validation logs with status and metrics
"""

from pathlib import Path
from typing import List
from datetime import datetime
import csv

from ..models import ValidationResult, ValidationStatus


class CSVReporter:
    """
    CSV reporter for validation logs.

    Generates CSV files with:
    - Timestamp
    - File path
    - Overall status
    - Level 1/2/3 status
    - Issue counts
    - Validation time
    """

    def __init__(self, output_path: Path):
        """
        Initialize CSV reporter.

        Args:
            output_path: Path to output CSV file
        """
        self.output_path = Path(output_path)
        self.output_path.parent.mkdir(parents=True, exist_ok=True)

    def report(self, results: List[ValidationResult]) -> None:
        """
        Generate CSV report.

        Args:
            results: List of validation results
        """
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        with open(self.output_path, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)

            # Header
            writer.writerow([
                'Timestamp',
                'File',
                'Overall Status',
                'Level 1 Status',
                'Level 2 Status',
                'Level 3 Status',
                'YAML Valid',
                'OrcaFlex Loadable',
                'Parameters Validated',
                'Total Issues',
                'Critical Issues',
                'Warnings',
                'Validation Time (s)',
                'Comments'
            ])

            # Data rows
            for result in results:
                writer.writerow([
                    timestamp,
                    str(result.file_path),
                    result.overall_status.value,
                    result.level_1.status.value if result.level_1 else 'N/A',
                    result.level_2.status.value if result.level_2 else 'N/A',
                    result.level_3.status.value if result.level_3 else 'N/A',
                    'Yes' if result.level_1 and result.level_1.yaml_valid else 'No',
                    'Yes' if result.level_2 and result.level_2.orcaflex_loadable else 'No',
                    result.level_3.parameters_validated if result.level_3 else 0,
                    len(result.issues),
                    result.level_3.critical_issues if result.level_3 else 0,
                    result.level_3.warnings if result.level_3 else 0,
                    f"{result.validation_time_seconds:.2f}",
                    self._get_summary_comment(result)
                ])

        print(f"CSV report saved to: {self.output_path}")

    def _get_summary_comment(self, result: ValidationResult) -> str:
        """
        Get summary comment for result.

        Args:
            result: Validation result

        Returns:
            Summary comment string
        """
        comments = []

        if result.level_1 and result.level_1.syntax_errors:
            comments.append(f"YAML errors: {len(result.level_1.syntax_errors)}")

        if result.level_2:
            if result.level_2.orcaflex_available.value == "no":
                comments.append("OrcaFlex not available")
            elif result.level_2.load_errors:
                comments.append(f"Load errors: {len(result.level_2.load_errors)}")
            elif result.level_2.warnings:
                comments.append(f"OrcaFlex warnings: {len(result.level_2.warnings)}")

        if result.level_3:
            if result.level_3.critical_issues > 0:
                comments.append(f"Critical issues: {result.level_3.critical_issues}")

        return "; ".join(comments) if comments else "OK"
