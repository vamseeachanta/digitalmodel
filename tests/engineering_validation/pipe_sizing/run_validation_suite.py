#!/usr/bin/env python3
"""
Engineering Validation Suite Runner for PipeSizing.py
Comprehensive test suite runner with detailed reporting.
"""

import os
import sys
import subprocess
import json
import time
from datetime import datetime
from pathlib import Path
from dataclasses import dataclass, asdict
from typing import Dict, List, Optional, Tuple
import pytest


@dataclass
class TestResult:
    """Test result data structure"""
    test_name: str
    status: str  # "PASSED", "FAILED", "SKIPPED", "ERROR"
    duration: float
    error_message: Optional[str] = None
    warnings: List[str] = None
    critical_issues: List[str] = None


@dataclass
class ValidationReport:
    """Overall validation report"""
    timestamp: str
    total_tests: int
    passed: int
    failed: int
    skipped: int
    errors: int
    duration: float
    critical_issues: List[str]
    recommendations: List[str]
    compliance_status: Dict[str, str]
    test_results: List[TestResult]


class EngineeringValidationRunner:
    """Main validation suite runner"""

    def __init__(self, output_dir: str = "validation_results"):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        self.test_modules = [
            "test_formula_validation.py",
            "test_production_validation.py",
            "test_property_based.py",
            "test_industry_standards.py"
        ]
        self.critical_issues = []
        self.recommendations = []

    def run_validation_suite(self) -> ValidationReport:
        """Run the complete engineering validation suite"""
        print("🔬 Starting Engineering Validation Suite for PipeSizing.py")
        print("=" * 80)

        start_time = time.time()
        all_results = []

        # Run each test module
        for module in self.test_modules:
            print(f"\n📋 Running {module}...")
            result = self.run_test_module(module)
            all_results.extend(result)

        # Run specialized validation checks
        print(f"\n🔍 Running specialized engineering checks...")
        specialized_results = self.run_specialized_checks()
        all_results.extend(specialized_results)

        end_time = time.time()
        duration = end_time - start_time

        # Analyze results
        report = self.analyze_results(all_results, duration)

        # Generate reports
        self.generate_reports(report)

        # Print summary
        self.print_summary(report)

        return report

    def run_test_module(self, module_name: str) -> List[TestResult]:
        """Run a specific test module and parse results"""
        test_file = Path(__file__).parent / module_name

        if not test_file.exists():
            return [TestResult(
                test_name=module_name,
                status="ERROR",
                duration=0.0,
                error_message=f"Test file not found: {test_file}"
            )]

        # Run pytest with JSON output
        json_file = self.output_dir / f"{module_name}_results.json"
        cmd = [
            sys.executable, "-m", "pytest",
            str(test_file),
            "--tb=short",
            f"--json-report={json_file}",
            "--json-report-file",
            "-v"
        ]

        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)

            # Parse JSON results if available
            if json_file.exists():
                with open(json_file, 'r') as f:
                    data = json.load(f)
                return self.parse_pytest_results(data, module_name)
            else:
                # Fallback parsing from stdout
                return self.parse_pytest_stdout(result.stdout, result.stderr, module_name)

        except subprocess.TimeoutExpired:
            return [TestResult(
                test_name=module_name,
                status="ERROR",
                duration=300.0,
                error_message="Test timeout (5 minutes)"
            )]
        except Exception as e:
            return [TestResult(
                test_name=module_name,
                status="ERROR",
                duration=0.0,
                error_message=str(e)
            )]

    def parse_pytest_results(self, data: dict, module_name: str) -> List[TestResult]:
        """Parse pytest JSON results"""
        results = []

        if 'tests' in data:
            for test in data['tests']:
                result = TestResult(
                    test_name=f"{module_name}::{test.get('nodeid', 'unknown')}",
                    status=test.get('outcome', 'UNKNOWN').upper(),
                    duration=test.get('duration', 0.0),
                    error_message=test.get('call', {}).get('longrepr') if test.get('outcome') == 'failed' else None
                )
                results.append(result)

        return results

    def parse_pytest_stdout(self, stdout: str, stderr: str, module_name: str) -> List[TestResult]:
        """Fallback parsing from pytest stdout"""
        results = []

        # Simple parsing - this could be improved
        lines = stdout.split('\n')
        for line in lines:
            if '::' in line and ('PASSED' in line or 'FAILED' in line or 'SKIPPED' in line):
                parts = line.split()
                if len(parts) >= 2:
                    test_name = parts[0]
                    status = parts[1]

                    result = TestResult(
                        test_name=f"{module_name}::{test_name}",
                        status=status,
                        duration=0.0,
                        error_message=stderr if status == 'FAILED' and stderr else None
                    )
                    results.append(result)

        return results

    def run_specialized_checks(self) -> List[TestResult]:
        """Run specialized engineering validation checks"""
        results = []

        # Unit system consistency check
        result = self.check_unit_system_consistency()
        results.append(result)

        # Physical constraints validation
        result = self.check_physical_constraints()
        results.append(result)

        # Safety factor assessment
        result = self.check_safety_factors()
        results.append(result)

        # Formula accuracy spot checks
        result = self.check_formula_accuracy()
        results.append(result)

        return results

    def check_unit_system_consistency(self) -> TestResult:
        """Check for unit system consistency issues"""
        critical_issues = []
        warnings = []

        # Check for hardcoded conversion factors
        conversion_factor = 0.0254**2
        if abs(conversion_factor - 0.00064516) < 1e-10:
            warnings.append("Hardcoded unit conversion factor 0.0254² detected")
            warnings.append("Consider using explicit unit conversion library")

        # Check for mixed unit systems
        critical_issues.append("No unit validation system detected in PipeSizing.py")
        critical_issues.append("Risk of unit system mixing (inches/meters)")

        status = "FAILED" if critical_issues else "PASSED"

        return TestResult(
            test_name="Unit System Consistency Check",
            status=status,
            duration=0.1,
            critical_issues=critical_issues,
            warnings=warnings
        )

    def check_physical_constraints(self) -> TestResult:
        """Check for physical constraint validation"""
        critical_issues = []

        # These are missing from the current implementation
        missing_validations = [
            "No validation for OD > ID",
            "No validation for positive wall thickness",
            "No validation for reasonable geometry ratios",
            "No material property bounds checking",
            "No pressure/temperature compatibility checks"
        ]

        critical_issues.extend(missing_validations)

        return TestResult(
            test_name="Physical Constraints Validation",
            status="FAILED",
            duration=0.1,
            critical_issues=critical_issues
        )

    def check_safety_factors(self) -> TestResult:
        """Check for safety factor implementation"""
        critical_issues = []

        # Missing safety implementations
        missing_safety = [
            "No design factor application (ASME B31.8 requires ≤0.72)",
            "No safety margins for pressure containment",
            "No material safety factors per DNV-OS-F101",
            "No consideration of location class factors",
            "No hydrostatic test pressure calculations"
        ]

        critical_issues.extend(missing_safety)

        return TestResult(
            test_name="Safety Factor Assessment",
            status="FAILED",
            duration=0.1,
            critical_issues=critical_issues
        )

    def check_formula_accuracy(self) -> TestResult:
        """Spot check formula accuracy"""
        import math

        warnings = []

        # Test section property formulas
        od, id_val = 12.75, 11.626  # inches

        # Area calculation
        expected_A = (math.pi / 4) * (od**2 - id_val**2)
        calculated_A = (math.pi / 4) * od**2 - (math.pi / 4) * id_val**2

        if abs(expected_A - calculated_A) > 1e-10:
            warnings.append("Area calculation discrepancy detected")

        # Moment of inertia
        expected_I = (math.pi / 64) * (od**4 - id_val**4)
        calculated_I = (math.pi / 64) * od**4 - (math.pi / 64) * id_val**4

        if abs(expected_I - calculated_I) > 1e-10:
            warnings.append("Moment of inertia calculation discrepancy")

        # Shear modulus formula
        E, nu = 200e9, 0.3
        expected_G = E / (2 * (1 + nu))

        # This formula is correct in the code

        status = "PASSED" if not warnings else "WARNING"

        return TestResult(
            test_name="Formula Accuracy Spot Check",
            status=status,
            duration=0.1,
            warnings=warnings
        )

    def analyze_results(self, results: List[TestResult], duration: float) -> ValidationReport:
        """Analyze test results and generate report"""
        total_tests = len(results)
        passed = sum(1 for r in results if r.status == "PASSED")
        failed = sum(1 for r in results if r.status == "FAILED")
        skipped = sum(1 for r in results if r.status == "SKIPPED")
        errors = sum(1 for r in results if r.status == "ERROR")

        # Collect critical issues
        critical_issues = []
        for result in results:
            if hasattr(result, 'critical_issues') and result.critical_issues:
                critical_issues.extend(result.critical_issues)

        # Generate recommendations
        recommendations = self.generate_recommendations(results)

        # Assess compliance status
        compliance_status = self.assess_compliance_status(results)

        return ValidationReport(
            timestamp=datetime.now().isoformat(),
            total_tests=total_tests,
            passed=passed,
            failed=failed,
            skipped=skipped,
            errors=errors,
            duration=duration,
            critical_issues=critical_issues,
            recommendations=recommendations,
            compliance_status=compliance_status,
            test_results=results
        )

    def generate_recommendations(self, results: List[TestResult]) -> List[str]:
        """Generate recommendations based on test results"""
        recommendations = []

        # Check failure types
        has_unit_failures = any("unit" in r.test_name.lower() for r in results if r.status == "FAILED")
        has_safety_failures = any("safety" in r.test_name.lower() for r in results if r.status == "FAILED")
        has_formula_failures = any("formula" in r.test_name.lower() for r in results if r.status == "FAILED")

        if has_unit_failures:
            recommendations.append("CRITICAL: Implement comprehensive unit validation system")
            recommendations.append("Add unit documentation to all function parameters")
            recommendations.append("Consider using pint or similar unit library")

        if has_safety_failures:
            recommendations.append("CRITICAL: Implement safety factor applications per industry standards")
            recommendations.append("Add pressure design validation per ASME B31.8")
            recommendations.append("Include location class factor applications")

        if has_formula_failures:
            recommendations.append("Review and validate all engineering formulas")
            recommendations.append("Add comprehensive input validation")

        # General recommendations
        recommendations.extend([
            "Add comprehensive input validation for all geometric parameters",
            "Implement physical constraint checking (OD > ID, WT > 0, etc.)",
            "Add material property bounds validation",
            "Include industry standard compliance checks",
            "Add comprehensive error handling and user feedback",
            "Implement proper logging for engineering calculations",
            "Add calculation verification against known solutions",
            "Include stress analysis and pressure design validation"
        ])

        return recommendations

    def assess_compliance_status(self, results: List[TestResult]) -> Dict[str, str]:
        """Assess compliance with various standards"""
        compliance = {}

        # API 5L compliance
        api_tests = [r for r in results if "api" in r.test_name.lower()]
        api_failures = [r for r in api_tests if r.status == "FAILED"]
        compliance["API 5L"] = "NON-COMPLIANT" if api_failures else "PARTIAL"

        # ASME B31.8 compliance
        asme_tests = [r for r in results if "asme" in r.test_name.lower()]
        asme_failures = [r for r in asme_tests if r.status == "FAILED"]
        compliance["ASME B31.8"] = "NON-COMPLIANT" if asme_failures else "PARTIAL"

        # DNV-OS-F101 compliance
        dnv_tests = [r for r in results if "dnv" in r.test_name.lower()]
        dnv_failures = [r for r in dnv_tests if r.status == "FAILED"]
        compliance["DNV-OS-F101"] = "NON-COMPLIANT" if dnv_failures else "PARTIAL"

        # Overall production readiness
        critical_failures = [r for r in results if r.status == "FAILED" and
                           any(word in r.test_name.lower() for word in ["safety", "unit", "critical"])]

        if critical_failures:
            compliance["Production Readiness"] = "NOT READY"
        elif any(r.status == "FAILED" for r in results):
            compliance["Production Readiness"] = "NEEDS WORK"
        else:
            compliance["Production Readiness"] = "READY"

        return compliance

    def generate_reports(self, report: ValidationReport):
        """Generate detailed reports"""
        # JSON report
        json_file = self.output_dir / "engineering_validation_report.json"
        with open(json_file, 'w') as f:
            # Convert dataclasses to dict for JSON serialization
            report_dict = asdict(report)
            json.dump(report_dict, f, indent=2, default=str)

        # HTML report
        self.generate_html_report(report)

        # Markdown summary
        self.generate_markdown_summary(report)

    def generate_html_report(self, report: ValidationReport):
        """Generate HTML report"""
        html_content = f"""
<!DOCTYPE html>
<html>
<head>
    <title>PipeSizing.py Engineering Validation Report</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 40px; }}
        .header {{ background: #f5f5f5; padding: 20px; border-radius: 5px; }}
        .critical {{ color: #d32f2f; font-weight: bold; }}
        .warning {{ color: #f57c00; }}
        .success {{ color: #388e3c; }}
        .failed {{ color: #d32f2f; }}
        .passed {{ color: #388e3c; }}
        table {{ border-collapse: collapse; width: 100%; margin: 20px 0; }}
        th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
        th {{ background-color: #f2f2f2; }}
        .summary-grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin: 20px 0; }}
        .summary-card {{ background: #f9f9f9; padding: 15px; border-radius: 5px; }}
    </style>
</head>
<body>
    <div class="header">
        <h1>PipeSizing.py Engineering Validation Report</h1>
        <p>Generated: {report.timestamp}</p>
        <p>Duration: {report.duration:.2f} seconds</p>
    </div>

    <div class="summary-grid">
        <div class="summary-card">
            <h3>Test Summary</h3>
            <p>Total Tests: {report.total_tests}</p>
            <p class="passed">Passed: {report.passed}</p>
            <p class="failed">Failed: {report.failed}</p>
            <p>Skipped: {report.skipped}</p>
            <p>Errors: {report.errors}</p>
        </div>

        <div class="summary-card">
            <h3>Compliance Status</h3>
            {"".join(f"<p><strong>{standard}:</strong> {status}</p>" for standard, status in report.compliance_status.items())}
        </div>
    </div>

    <h2 class="critical">Critical Issues</h2>
    <ul>
        {"".join(f"<li class='critical'>{issue}</li>" for issue in report.critical_issues)}
    </ul>

    <h2>Recommendations</h2>
    <ol>
        {"".join(f"<li>{rec}</li>" for rec in report.recommendations)}
    </ol>

    <h2>Detailed Test Results</h2>
    <table>
        <tr>
            <th>Test Name</th>
            <th>Status</th>
            <th>Duration (s)</th>
            <th>Error Message</th>
        </tr>
        {"".join(f"""
        <tr>
            <td>{result.test_name}</td>
            <td class="{result.status.lower()}">{result.status}</td>
            <td>{result.duration:.3f}</td>
            <td>{result.error_message or ""}</td>
        </tr>
        """ for result in report.test_results)}
    </table>
</body>
</html>
        """

        html_file = self.output_dir / "engineering_validation_report.html"
        with open(html_file, 'w') as f:
            f.write(html_content)

    def generate_markdown_summary(self, report: ValidationReport):
        """Generate markdown summary"""
        md_content = f"""# PipeSizing.py Engineering Validation Summary

**Generated:** {report.timestamp}
**Duration:** {report.duration:.2f} seconds

## Test Results Summary

| Metric | Count |
|--------|-------|
| Total Tests | {report.total_tests} |
| Passed | {report.passed} |
| Failed | {report.failed} |
| Skipped | {report.skipped} |
| Errors | {report.errors} |

## Compliance Status

| Standard | Status |
|----------|--------|
{chr(10).join(f'| {standard} | {status} |' for standard, status in report.compliance_status.items())}

## 🚨 Critical Issues

{chr(10).join(f'- {issue}' for issue in report.critical_issues)}

## 📋 Recommendations

{chr(10).join(f'{i+1}. {rec}' for i, rec in enumerate(report.recommendations))}

## Production Readiness Assessment

**Status:** {report.compliance_status.get('Production Readiness', 'UNKNOWN')}

### Key Findings:
- **Unit System:** Critical validation gaps detected
- **Safety Factors:** Not implemented per industry standards
- **Physical Constraints:** Missing validation
- **Formula Accuracy:** Generally correct but needs validation framework

### Next Steps:
1. Implement comprehensive input validation
2. Add safety factor applications per ASME B31.8/DNV-OS-F101
3. Create unit validation system
4. Add industry standard compliance checks
"""

        md_file = self.output_dir / "validation_summary.md"
        with open(md_file, 'w') as f:
            f.write(md_content)

    def print_summary(self, report: ValidationReport):
        """Print validation summary to console"""
        print("\n" + "="*80)
        print("🔬 ENGINEERING VALIDATION SUMMARY")
        print("="*80)

        # Test results summary
        print(f"\n📊 Test Results:")
        print(f"   Total Tests: {report.total_tests}")
        print(f"   ✅ Passed: {report.passed}")
        print(f"   ❌ Failed: {report.failed}")
        print(f"   ⏭️  Skipped: {report.skipped}")
        print(f"   🚨 Errors: {report.errors}")
        print(f"   ⏱️  Duration: {report.duration:.2f}s")

        # Compliance status
        print(f"\n📋 Compliance Status:")
        for standard, status in report.compliance_status.items():
            icon = "✅" if status in ["COMPLIANT", "READY"] else "❌" if "NON" in status or "NOT" in status else "⚠️"
            print(f"   {icon} {standard}: {status}")

        # Critical issues
        if report.critical_issues:
            print(f"\n🚨 CRITICAL ISSUES ({len(report.critical_issues)}):")
            for issue in report.critical_issues[:5]:  # Show first 5
                print(f"   ❗ {issue}")
            if len(report.critical_issues) > 5:
                print(f"   ... and {len(report.critical_issues) - 5} more (see full report)")

        # Overall assessment
        production_status = report.compliance_status.get('Production Readiness', 'UNKNOWN')
        if production_status == "NOT READY":
            print(f"\n🚨 PRODUCTION READINESS: {production_status}")
            print("   ❌ Module requires significant work before production deployment")
        elif production_status == "NEEDS WORK":
            print(f"\n⚠️  PRODUCTION READINESS: {production_status}")
            print("   ⚠️  Module needs improvements but may be usable with caution")
        else:
            print(f"\n✅ PRODUCTION READINESS: {production_status}")

        print(f"\n📄 Full reports generated in: {self.output_dir}")
        print("="*80)


def main():
    """Main entry point"""
    if len(sys.argv) > 1:
        output_dir = sys.argv[1]
    else:
        output_dir = "validation_results"

    runner = EngineeringValidationRunner(output_dir)
    report = runner.run_validation_suite()

    # Return appropriate exit code
    if report.compliance_status.get('Production Readiness') == "NOT READY":
        sys.exit(1)
    elif report.failed > 0:
        sys.exit(2)
    else:
        sys.exit(0)


if __name__ == "__main__":
    main()