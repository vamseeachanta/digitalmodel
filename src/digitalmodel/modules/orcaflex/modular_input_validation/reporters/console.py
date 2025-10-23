"""
ABOUTME: Console reporter with color-coded output
ABOUTME: Uses loguru for INFO/WARNING/CRITICAL logging with emoji indicators
"""

from pathlib import Path
from typing import List
from loguru import logger
import sys

from ..models import ValidationResult, ValidationStatus, Severity


class ConsoleReporter:
    """
    Console reporter with color-coded output.

    Uses loguru for structured logging with:
    - 🟢 INFO (green) - passed checks
    - 🟡 WARNING (yellow) - warnings
    - 🔴 CRITICAL (red) - failures
    """

    def __init__(self, enable_color: bool = True):
        """
        Initialize console reporter.

        Args:
            enable_color: Enable colored output
        """
        self.enable_color = enable_color
        self._setup_logger()

    def _setup_logger(self) -> None:
        """Configure loguru logger."""
        # Remove default handler
        logger.remove()

        # Add custom handler with colors
        logger.add(
            sys.stdout,
            colorize=self.enable_color,
            format="<green>{time:HH:mm:ss}</green> | <level>{level: <8}</level> | <level>{message}</level>",
            level="INFO"
        )

    def report(self, results: List[ValidationResult]) -> None:
        """
        Generate console report.

        Args:
            results: List of validation results
        """
        logger.info("=" * 80)
        logger.info("CALM BUOY MODULAR INPUT VALIDATION REPORT")
        logger.info("=" * 80)
        logger.info("")

        for idx, result in enumerate(results, 1):
            self._report_file_result(idx, result)
            logger.info("")

        # Summary
        self._report_summary(results)

    def _report_file_result(self, idx: int, result: ValidationResult) -> None:
        """
        Report results for a single file.

        Args:
            idx: File index
            result: Validation result
        """
        status_emoji = self._get_status_emoji(result.overall_status)
        logger.info(f"{status_emoji} File {idx}: {result.file_path.name}")
        logger.info(f"   Status: {result.overall_status.value.upper()}")
        logger.info(f"   Validation time: {result.validation_time_seconds:.2f}s")

        # Level 1: YAML
        if result.level_1:
            self._report_level_1(result.level_1)

        # Level 2: OrcaFlex
        if result.level_2:
            self._report_level_2(result.level_2)

        # Level 3: Physical
        if result.level_3:
            self._report_level_3(result.level_3)

        # Issues summary
        if result.issues:
            logger.warning(f"   Issues found: {len(result.issues)}")
            for issue in result.issues[:5]:  # Show first 5
                severity_emoji = self._get_severity_emoji(issue.severity)
                logger.log(
                    self._severity_to_level(issue.severity),
                    f"      {severity_emoji} {issue.message}"
                )
            if len(result.issues) > 5:
                logger.info(f"      ... and {len(result.issues) - 5} more")

    def _report_level_1(self, level_1) -> None:
        """Report Level 1 results."""
        status = "✓" if level_1.yaml_valid else "✗"
        logger.info(f"   Level 1 (YAML): {status}")

        if level_1.syntax_errors:
            for error in level_1.syntax_errors:
                logger.error(f"      🔴 {error}")

        if level_1.total_modules > 0:
            logger.info(f"      Modules: {level_1.total_modules}")
            if level_1.missing_includes:
                logger.warning(f"      Missing includes: {len(level_1.missing_includes)}")

    def _report_level_2(self, level_2) -> None:
        """Report Level 2 results."""
        logger.info(f"   Level 2 (OrcaFlex): {level_2.status.value}")

        logger.info(f"      Software available: {level_2.orcaflex_available.value}")
        if level_2.orcaflex_version:
            logger.info(f"      OrcaFlex version: {level_2.orcaflex_version}")

        if level_2.orcaflex_loadable:
            logger.success(f"      ✓ File loaded successfully")
            logger.info(f"      Object count: {level_2.object_count}")

            if level_2.warnings:
                logger.warning(f"      Warnings: {len(level_2.warnings)}")
                for warning in level_2.warnings[:3]:
                    logger.warning(f"         - {warning}")
        elif level_2.load_errors:
            for error in level_2.load_errors:
                logger.error(f"      🔴 {error}")

    def _report_level_3(self, level_3) -> None:
        """Report Level 3 results."""
        logger.info(f"   Level 3 (Physical): {level_3.status.value}")
        logger.info(f"      Parameters validated: {level_3.parameters_validated}")

        if level_3.critical_issues > 0:
            logger.critical(f"      🔴 Critical issues: {level_3.critical_issues}")
        if level_3.warnings > 0:
            logger.warning(f"      🟡 Warnings: {level_3.warnings}")

        # Show critical checks
        all_checks = (
            level_3.hull_geometry_checks +
            level_3.metocean_checks +
            level_3.mooring_capacity_checks +
            level_3.project_comparison_checks
        )

        critical_checks = [c for c in all_checks if c.severity == Severity.CRITICAL]
        if critical_checks:
            logger.critical("      Critical parameter issues:")
            for check in critical_checks[:5]:
                logger.critical(f"         - {check.parameter}: {check.message}")

    def _report_summary(self, results: List[ValidationResult]) -> None:
        """
        Report overall summary.

        Args:
            results: List of validation results
        """
        logger.info("=" * 80)
        logger.info("SUMMARY")
        logger.info("=" * 80)

        total = len(results)
        passed = sum(1 for r in results if r.overall_status == ValidationStatus.PASS)
        warned = sum(1 for r in results if r.overall_status == ValidationStatus.WARN)
        failed = sum(1 for r in results if r.overall_status == ValidationStatus.FAIL)
        skipped = sum(1 for r in results if r.overall_status == ValidationStatus.SKIPPED)

        logger.info(f"Total files: {total}")
        logger.success(f"Passed: {passed}")
        logger.warning(f"Warnings: {warned}")
        logger.error(f"Failed: {failed}")
        if skipped > 0:
            logger.info(f"Skipped: {skipped}")

        # Overall result
        if failed > 0:
            logger.error("\n🔴 VALIDATION FAILED")
        elif warned > 0:
            logger.warning("\n🟡 VALIDATION PASSED WITH WARNINGS")
        else:
            logger.success("\n🟢 VALIDATION PASSED")

    def _get_status_emoji(self, status: ValidationStatus) -> str:
        """Get emoji for status."""
        return {
            ValidationStatus.PASS: "🟢",
            ValidationStatus.WARN: "🟡",
            ValidationStatus.FAIL: "🔴",
            ValidationStatus.UNKNOWN: "⚪",
            ValidationStatus.SKIPPED: "⏭️"
        }.get(status, "⚪")

    def _get_severity_emoji(self, severity: Severity) -> str:
        """Get emoji for severity."""
        return {
            Severity.INFO: "🟢",
            Severity.WARNING: "🟡",
            Severity.CRITICAL: "🔴"
        }.get(severity, "⚪")

    def _severity_to_level(self, severity: Severity) -> str:
        """Convert severity to loguru level."""
        return {
            Severity.INFO: "INFO",
            Severity.WARNING: "WARNING",
            Severity.CRITICAL: "CRITICAL"
        }.get(severity, "INFO")
