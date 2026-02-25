"""
ABOUTME: Markdown reporter for validation results
ABOUTME: Generates human-readable Markdown reports with tables and summaries
"""

from pathlib import Path
from typing import List
from datetime import datetime

from ..models import ValidationResult, ValidationStatus, Severity


class MarkdownReporter:
    """
    Markdown reporter for validation results.

    Generates Markdown files with:
    - Executive summary
    - Detailed results per file
    - Issue tables
    - Physical consistency checks
    """

    def __init__(self, output_path: Path):
        """
        Initialize Markdown reporter.

        Args:
            output_path: Path to output Markdown file
        """
        self.output_path = Path(output_path)
        self.output_path.parent.mkdir(parents=True, exist_ok=True)

    def report(self, results: List[ValidationResult]) -> None:
        """
        Generate Markdown report.

        Args:
            results: List of validation results
        """
        with open(self.output_path, 'w', encoding='utf-8') as f:
            f.write(self._generate_header())
            f.write(self._generate_summary(results))
            f.write(self._generate_detailed_results(results))

        print(f"Markdown report saved to: {self.output_path}")

    def _generate_header(self) -> str:
        """Generate report header."""
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        return f"""# CALM Buoy Modular Input Validation Report

**Generated:** {timestamp}

---

"""

    def _generate_summary(self, results: List[ValidationResult]) -> str:
        """Generate summary section."""
        total = len(results)
        passed = sum(1 for r in results if r.overall_status == ValidationStatus.PASS)
        warned = sum(1 for r in results if r.overall_status == ValidationStatus.WARN)
        failed = sum(1 for r in results if r.overall_status == ValidationStatus.FAIL)

        total_issues = sum(len(r.issues) for r in results)
        total_critical = sum(r.level_3.critical_issues if r.level_3 else 0 for r in results)
        total_warnings = sum(r.level_3.warnings if r.level_3 else 0 for r in results)

        return f"""## Executive Summary

| Metric | Value |
|--------|-------|
| Total Files | {total} |
| ✅ Passed | {passed} |
| ⚠️ Warnings | {warned} |
| ❌ Failed | {failed} |
| Total Issues | {total_issues} |
| Critical Issues | {total_critical} |
| Warnings | {total_warnings} |

---

"""

    def _generate_detailed_results(self, results: List[ValidationResult]) -> str:
        """Generate detailed results section."""
        output = "## Detailed Results\n\n"

        for idx, result in enumerate(results, 1):
            output += self._generate_file_section(idx, result)

        return output

    def _generate_file_section(self, idx: int, result: ValidationResult) -> str:
        """Generate section for a single file."""
        status_badge = self._get_status_badge(result.overall_status)

        output = f"""### {idx}. {result.file_path.name} {status_badge}

**File:** `{result.file_path}`
**Status:** {result.overall_status.value.upper()}
**Validation Time:** {result.validation_time_seconds:.2f}s

"""

        # Level 1: YAML
        if result.level_1:
            output += self._generate_level_1_section(result.level_1)

        # Level 2: OrcaFlex
        if result.level_2:
            output += self._generate_level_2_section(result.level_2)

        # Level 3: Physical
        if result.level_3:
            output += self._generate_level_3_section(result.level_3)

        # Issues
        if result.issues:
            output += self._generate_issues_section(result.issues)

        output += "---\n\n"

        return output

    def _generate_level_1_section(self, level_1) -> str:
        """Generate Level 1 section."""
        status = "✅" if level_1.yaml_valid else "❌"

        output = f"""#### Level 1: YAML Syntax {status}

| Check | Result |
|-------|--------|
| YAML Valid | {"Yes" if level_1.yaml_valid else "No"} |
| File Exists | {"Yes" if level_1.file_exists else "No"} |
| Includes Resolved | {"Yes" if level_1.includes_resolved else "No"} |
| Total Modules | {level_1.total_modules} |
| Sections | {len(level_1.sections)} |

"""

        if level_1.syntax_errors:
            output += "**Syntax Errors:**\n\n"
            for error in level_1.syntax_errors:
                output += f"- 🔴 {error}\n"
            output += "\n"

        return output

    def _generate_level_2_section(self, level_2) -> str:
        """Generate Level 2 section."""
        output = f"""#### Level 2: OrcaFlex API

| Check | Result |
|-------|--------|
| Software Available | {level_2.orcaflex_available.value.upper()} |
| Version | {level_2.orcaflex_version or 'N/A'} |
| File Loadable | {"Yes" if level_2.orcaflex_loadable else "No"} |
| Object Count | {level_2.object_count} |
| Static Analysis | {"Passed" if level_2.static_analysis_passed else "Failed"} |

"""

        if level_2.warnings:
            output += f"**Warnings ({len(level_2.warnings)}):**\n\n"
            for warning in level_2.warnings[:10]:
                output += f"- ⚠️ {warning}\n"
            if len(level_2.warnings) > 10:
                output += f"- ... and {len(level_2.warnings) - 10} more\n"
            output += "\n"

        if level_2.load_errors:
            output += "**Load Errors:**\n\n"
            for error in level_2.load_errors:
                output += f"- ❌ {error}\n"
            output += "\n"

        return output

    def _generate_level_3_section(self, level_3) -> str:
        """Generate Level 3 section."""
        output = f"""#### Level 3: Physical Consistency

| Check | Result |
|-------|--------|
| Parameters Validated | {level_3.parameters_validated} |
| Critical Issues | {level_3.critical_issues} |
| Warnings | {level_3.warnings} |

"""

        # Hull geometry checks
        if level_3.hull_geometry_checks:
            output += "**Hull Geometry:**\n\n"
            output += self._generate_checks_table(level_3.hull_geometry_checks)

        # Metocean checks
        if level_3.metocean_checks:
            output += "**Metocean Parameters:**\n\n"
            output += self._generate_checks_table(level_3.metocean_checks)

        # Mooring capacity checks
        if level_3.mooring_capacity_checks:
            output += "**Mooring Capacity:**\n\n"
            output += self._generate_checks_table(level_3.mooring_capacity_checks)

        return output

    def _generate_checks_table(self, checks) -> str:
        """Generate table for physical checks."""
        output = "| Parameter | Actual | Expected | Status | Message |\n"
        output += "|-----------|--------|----------|--------|----------|\n"

        for check in checks:
            emoji = {"info": "✅", "warning": "⚠️", "critical": "❌"}.get(check.severity.value, "⚪")
            expected = check.expected_value or check.expected_range or "-"
            output += f"| {check.parameter} | {check.actual_value} | {expected} | {emoji} | {check.message} |\n"

        output += "\n"
        return output

    def _generate_issues_section(self, issues) -> str:
        """Generate issues section."""
        output = f"**Issues ({len(issues)}):**\n\n"

        for issue in issues:
            severity_emoji = {"info": "ℹ️", "warning": "⚠️", "critical": "🔴"}.get(issue.severity.value, "⚪")
            output += f"- {severity_emoji} **{issue.level.value}**: {issue.message}\n"

        output += "\n"
        return output

    def _get_status_badge(self, status: ValidationStatus) -> str:
        """Get status badge."""
        return {
            ValidationStatus.PASS: "✅",
            ValidationStatus.WARN: "⚠️",
            ValidationStatus.FAIL: "❌",
            ValidationStatus.SKIPPED: "⏭️",
            ValidationStatus.UNKNOWN: "❔"
        }.get(status, "❔")
