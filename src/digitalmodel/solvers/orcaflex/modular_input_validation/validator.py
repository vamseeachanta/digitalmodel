"""
ABOUTME: Main validator orchestrator for modular input files
ABOUTME: Coordinates Level 1/2/3 validation and report generation
"""

from pathlib import Path
from typing import List, Optional, Union
from datetime import datetime
import time

from .models import ValidationResult, ValidationStatus, ValidationIssue, Severity, ValidationLevel
from .config import ValidationConfig
from .level_1_yaml import Level1YAMLValidator
from .level_2_orcaflex import Level2OrcaFlexValidator
from .level_3_physical import Level3PhysicalValidator
from .reporters import ConsoleReporter, CSVReporter, MarkdownReporter, HTMLReporter


class ModularInputValidator:
    """
    Main orchestrator for modular input file validation.

    Coordinates:
    - Level 1: YAML syntax and structure
    - Level 2: OrcaFlex API validation
    - Level 3: Physical consistency checks
    - Multi-format reporting
    """

    def __init__(self, config: Optional[ValidationConfig] = None):
        """
        Initialize modular input validator.

        Args:
            config: Validation configuration (uses defaults if not provided)
        """
        self.config = config or ValidationConfig()
        self.config.ensure_output_directories()

        # Initialize validators
        self.level_1_validator = Level1YAMLValidator()
        self.level_2_validator = Level2OrcaFlexValidator()
        self.level_3_validator = Level3PhysicalValidator(self.config)

    def validate_file(self, file_path: Union[str, Path]) -> ValidationResult:
        """
        Validate a single YAML file.

        Args:
            file_path: Path to YAML file to validate

        Returns:
            ValidationResult with findings from all enabled levels
        """
        file_path = Path(file_path)
        start_time = time.time()

        result = ValidationResult(file_path=file_path)

        # Level 1: YAML Syntax and Structure
        if self.config.should_run_level(1):
            result.level_1 = self.level_1_validator.validate(file_path)
            self._collect_issues_from_level_1(result)

            # If Level 1 fails critically, stop validation
            if result.level_1.status == ValidationStatus.FAIL:
                result.overall_status = ValidationStatus.FAIL
                result.validation_time_seconds = time.time() - start_time
                return result

        # Level 2: OrcaFlex API Validation
        if self.config.should_run_level(2):
            result.level_2 = self.level_2_validator.validate(file_path)
            self._collect_issues_from_level_2(result)

        # Level 3: Physical Consistency
        if self.config.should_run_level(3):
            result.level_3 = self.level_3_validator.validate(file_path)
            self._collect_issues_from_level_3(result)

        # Determine overall status
        result.overall_status = self._determine_overall_status(result)
        result.validation_time_seconds = time.time() - start_time

        return result

    def validate_files(self, file_paths: List[Union[str, Path]]) -> List[ValidationResult]:
        """
        Validate multiple YAML files.

        Args:
            file_paths: List of paths to YAML files

        Returns:
            List of ValidationResult objects
        """
        results = []

        for file_path in file_paths:
            result = self.validate_file(file_path)
            results.append(result)

        return results

    def validate_directory(
        self,
        directory: Union[str, Path],
        pattern: str = "*.yml"
    ) -> List[ValidationResult]:
        """
        Validate all YAML files in a directory.

        Args:
            directory: Directory to search
            pattern: Glob pattern for file matching (default: *.yml)

        Returns:
            List of ValidationResult objects
        """
        directory = Path(directory)

        if not directory.exists() or not directory.is_dir():
            raise ValueError(f"Directory not found: {directory}")

        # Find all matching files
        file_paths = list(directory.glob(pattern))

        if not file_paths:
            print(f"Warning: No files matching '{pattern}' found in {directory}")
            return []

        print(f"Found {len(file_paths)} files to validate")

        return self.validate_files(file_paths)

    def generate_reports(
        self,
        results: List[ValidationResult],
        timestamp: Optional[str] = None
    ) -> None:
        """
        Generate validation reports in all configured formats.

        Args:
            results: List of validation results
            timestamp: Optional timestamp for report filenames
        """
        if not self.config.generate_reports:
            return

        if not results:
            print("No validation results to report")
            return

        # Generate timestamp for filenames
        ts = timestamp or datetime.now().strftime("%Y%m%d_%H%M%S")

        # Console report (always generated)
        if "console" in self.config.report_formats:
            console_reporter = ConsoleReporter(enable_color=self.config.enable_color)
            console_reporter.report(results)
            print()  # Add spacing

        # CSV report
        if "csv" in self.config.report_formats:
            csv_path = self.config.reports_dir / f"validation_{ts}.csv"
            csv_reporter = CSVReporter(csv_path)
            csv_reporter.report(results)

        # Markdown report
        if "markdown" in self.config.report_formats:
            md_path = self.config.reports_dir / f"validation_{ts}.md"
            md_reporter = MarkdownReporter(md_path)
            md_reporter.report(results)

        # HTML report
        if "html" in self.config.report_formats:
            html_path = self.config.reports_dir / f"validation_{ts}.html"
            html_reporter = HTMLReporter(html_path)
            html_reporter.report(results)

    def run_validation(
        self,
        paths: Union[str, Path, List[Union[str, Path]]],
        generate_reports: bool = True
    ) -> List[ValidationResult]:
        """
        High-level validation workflow.

        Args:
            paths: Single file, directory, or list of files to validate
            generate_reports: Whether to generate reports automatically

        Returns:
            List of ValidationResult objects
        """
        # Handle different input types
        if isinstance(paths, (str, Path)):
            path = Path(paths)

            if path.is_file():
                results = [self.validate_file(path)]
            elif path.is_dir():
                results = self.validate_directory(path)
            else:
                raise ValueError(f"Invalid path: {path}")

        elif isinstance(paths, list):
            results = self.validate_files(paths)
        else:
            raise TypeError("paths must be a file path, directory path, or list of paths")

        # Generate reports if requested
        if generate_reports:
            self.generate_reports(results)

        return results

    def _collect_issues_from_level_1(self, result: ValidationResult) -> None:
        """
        Collect issues from Level 1 results.

        Args:
            result: ValidationResult to update
        """
        if not result.level_1:
            return

        level_1 = result.level_1

        # Add syntax errors
        for error in level_1.syntax_errors:
            result.issues.append(ValidationIssue(
                level=ValidationLevel.LEVEL_1_YAML,
                severity=Severity.CRITICAL,
                message=error,
                parameter=None
            ))

        # Add missing includes
        for missing in level_1.missing_includes:
            result.issues.append(ValidationIssue(
                level=ValidationLevel.LEVEL_1_YAML,
                severity=Severity.WARNING,
                message=f"Missing include file: {missing}",
                parameter="includefile"
            ))

    def _collect_issues_from_level_2(self, result: ValidationResult) -> None:
        """
        Collect issues from Level 2 results.

        Args:
            result: ValidationResult to update
        """
        if not result.level_2:
            return

        level_2 = result.level_2

        # Add load errors
        for error in level_2.load_errors:
            result.issues.append(ValidationIssue(
                level=ValidationLevel.LEVEL_2_ORCAFLEX,
                severity=Severity.CRITICAL,
                message=error,
                parameter=None
            ))

        # Add warnings
        for warning in level_2.warnings:
            result.issues.append(ValidationIssue(
                level=ValidationLevel.LEVEL_2_ORCAFLEX,
                severity=Severity.WARNING,
                message=warning,
                parameter=None
            ))

    def _collect_issues_from_level_3(self, result: ValidationResult) -> None:
        """
        Collect issues from Level 3 results.

        Args:
            result: ValidationResult to update
        """
        if not result.level_3:
            return

        level_3 = result.level_3

        # Collect from all check types
        all_checks = (
            level_3.hull_geometry_checks +
            level_3.metocean_checks +
            level_3.mooring_capacity_checks +
            level_3.project_comparison_checks
        )

        # Convert checks to issues (only warnings and critical)
        for check in all_checks:
            if check.severity in [Severity.WARNING, Severity.CRITICAL]:
                result.issues.append(ValidationIssue(
                    level=ValidationLevel.LEVEL_3_PHYSICAL,
                    severity=check.severity,
                    message=check.message,
                    parameter=check.parameter,
                    actual_value=check.actual_value,
                    expected_value=check.expected_value or check.expected_range
                ))

    def _determine_overall_status(self, result: ValidationResult) -> ValidationStatus:
        """
        Determine overall validation status.

        Args:
            result: ValidationResult to evaluate

        Returns:
            Overall ValidationStatus
        """
        # If Level 1 failed, overall is FAIL
        if result.level_1 and result.level_1.status == ValidationStatus.FAIL:
            return ValidationStatus.FAIL

        # If Level 2 failed (and not skipped), overall is FAIL
        if result.level_2:
            if result.level_2.status == ValidationStatus.FAIL:
                return ValidationStatus.FAIL

        # If Level 3 has critical issues, overall is FAIL
        if result.level_3:
            if result.level_3.critical_issues > 0:
                return ValidationStatus.FAIL

        # Check for warnings across all levels
        has_warnings = False

        if result.level_1 and result.level_1.status == ValidationStatus.WARN:
            has_warnings = True

        if result.level_2 and result.level_2.status == ValidationStatus.WARN:
            has_warnings = True

        if result.level_3 and result.level_3.warnings > 0:
            has_warnings = True

        if has_warnings:
            return ValidationStatus.WARN

        # Check if any level passed
        has_passed = False

        if result.level_1 and result.level_1.status == ValidationStatus.PASS:
            has_passed = True

        if result.level_2 and result.level_2.status == ValidationStatus.PASS:
            has_passed = True

        if result.level_3 and result.level_3.status == ValidationStatus.PASS:
            has_passed = True

        return ValidationStatus.PASS if has_passed else ValidationStatus.UNKNOWN

    def get_validation_summary(self, results: List[ValidationResult]) -> dict:
        """
        Get summary statistics for validation results.

        Args:
            results: List of validation results

        Returns:
            Dictionary with summary statistics
        """
        total = len(results)
        passed = sum(1 for r in results if r.overall_status == ValidationStatus.PASS)
        warned = sum(1 for r in results if r.overall_status == ValidationStatus.WARN)
        failed = sum(1 for r in results if r.overall_status == ValidationStatus.FAIL)
        skipped = sum(1 for r in results if r.overall_status == ValidationStatus.SKIPPED)

        total_issues = sum(len(r.issues) for r in results)
        total_critical = sum(
            sum(1 for i in r.issues if i.severity == Severity.CRITICAL)
            for r in results
        )
        total_warnings = sum(
            sum(1 for i in r.issues if i.severity == Severity.WARNING)
            for r in results
        )

        return {
            'total_files': total,
            'passed': passed,
            'warnings': warned,
            'failed': failed,
            'skipped': skipped,
            'total_issues': total_issues,
            'critical_issues': total_critical,
            'warning_issues': total_warnings,
            'pass_rate': (passed / total * 100) if total > 0 else 0.0
        }
