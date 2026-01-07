#!/usr/bin/env python3
# ABOUTME: Validation script to verify cross-repository testing infrastructure
# Checks all dependencies and configuration before running tests

"""
Cross-Repository Testing Infrastructure Validator

Validates that all components are properly installed and configured:
- Required Python packages (pandas, plotly, pyyaml)
- Configuration files
- Test modules
- Output directories
- Repository discovery
"""

import sys
from pathlib import Path
from typing import List, Tuple


class ValidationResult:
    """Container for validation results."""

    def __init__(self):
        self.passed: List[str] = []
        self.failed: List[Tuple[str, str]] = []
        self.warnings: List[str] = []

    def add_pass(self, check: str):
        """Add a passing check."""
        self.passed.append(check)

    def add_fail(self, check: str, reason: str):
        """Add a failing check."""
        self.failed.append((check, reason))

    def add_warning(self, message: str):
        """Add a warning."""
        self.warnings.append(message)

    def is_valid(self) -> bool:
        """Return True if all checks passed."""
        return len(self.failed) == 0

    def print_summary(self):
        """Print validation summary."""
        print("=" * 70)
        print("VALIDATION SUMMARY")
        print("=" * 70)

        if self.passed:
            print(f"\n[PASS] Passed ({len(self.passed)}):")
            for check in self.passed:
                print(f"   - {check}")

        if self.warnings:
            print(f"\n[WARN] Warnings ({len(self.warnings)}):")
            for warning in self.warnings:
                print(f"   - {warning}")

        if self.failed:
            print(f"\n[FAIL] Failed ({len(self.failed)}):")
            for check, reason in self.failed:
                print(f"   - {check}")
                print(f"     Reason: {reason}")

        print("\n" + "=" * 70)

        if self.is_valid():
            print("[SUCCESS] All validation checks passed!")
            print("You can now run: scripts/run_cross_repo_tests.bat")
        else:
            print("[ERROR] Validation failed. Please fix the issues above.")

        print("=" * 70)


def validate_dependencies(result: ValidationResult):
    """Validate required Python dependencies."""
    required_packages = ["pandas", "plotly", "yaml"]

    for package in required_packages:
        try:
            __import__(package)
            result.add_pass(f"Python package: {package}")
        except ImportError:
            result.add_fail(
                f"Python package: {package}",
                f"Install with: uv add {package if package != 'yaml' else 'pyyaml'}"
            )


def validate_files(result: ValidationResult):
    """Validate required files exist."""
    base_path = Path(__file__).parent.parent

    required_files = {
        "config/cross-repo-test-config.yaml": "Configuration file",
        "scripts/cross_repo_test_runner.py": "Main test runner",
        "tests/cross_repo/test_standards_compliance.py": "Compliance tests",
        "tests/cross_repo/__init__.py": "Test module init",
    }

    for file_path, description in required_files.items():
        full_path = base_path / file_path
        if full_path.exists():
            result.add_pass(f"File exists: {description}")
        else:
            result.add_fail(
                f"File missing: {description}",
                f"Expected at: {full_path}"
            )


def validate_directories(result: ValidationResult):
    """Validate required directories exist."""
    base_path = Path(__file__).parent.parent

    required_dirs = {
        "reports/cross_repo_tests": "Output directory for test results",
        "tests/cross_repo": "Cross-repository test module",
        "config": "Configuration directory",
    }

    for dir_path, description in required_dirs.items():
        full_path = base_path / dir_path
        if full_path.exists() and full_path.is_dir():
            result.add_pass(f"Directory exists: {description}")
        else:
            result.add_fail(
                f"Directory missing: {description}",
                f"Create with: mkdir -p {full_path}"
            )


def validate_configuration(result: ValidationResult):
    """Validate configuration file structure."""
    base_path = Path(__file__).parent.parent
    config_path = base_path / "config" / "cross-repo-test-config.yaml"

    if not config_path.exists():
        result.add_fail(
            "Configuration validation",
            f"Config file not found: {config_path}"
        )
        return

    try:
        import yaml

        with open(config_path, "r") as f:
            config = yaml.safe_load(f)

        # Check required sections
        required_sections = ["repositories", "execution", "standards", "reporting"]
        for section in required_sections:
            if section in config:
                result.add_pass(f"Config section: {section}")
            else:
                result.add_fail(
                    f"Config section: {section}",
                    f"Missing section in {config_path}"
                )

        # Check scan path
        scan_path = Path(config.get("repositories", {}).get("scan_path", ""))
        if scan_path.exists():
            result.add_pass(f"Scan path exists: {scan_path}")
        else:
            result.add_warning(
                f"Scan path does not exist: {scan_path}"
            )

    except Exception as e:
        result.add_fail(
            "Configuration parsing",
            f"Error reading config: {str(e)}"
        )


def validate_repository_discovery(result: ValidationResult):
    """Validate repository discovery."""
    try:
        import yaml

        base_path = Path(__file__).parent.parent
        config_path = base_path / "config" / "cross-repo-test-config.yaml"

        with open(config_path, "r") as f:
            config = yaml.safe_load(f)

        scan_path = Path(config["repositories"]["scan_path"])

        if not scan_path.exists():
            result.add_warning(
                f"Cannot discover repositories: {scan_path} does not exist"
            )
            return

        # Count repositories
        repos = []
        for item in scan_path.iterdir():
            if (
                item.is_dir()
                and (item / "pyproject.toml").exists()
                and (item / "tests").exists()
            ):
                repos.append(item.name)

        if repos:
            result.add_pass(
                f"Repository discovery: Found {len(repos)} repositories"
            )
            if len(repos) < 5:
                for repo in repos:
                    result.add_pass(f"  - {repo}")
        else:
            result.add_warning(
                "No repositories found with pyproject.toml and tests/ directory"
            )

    except Exception as e:
        result.add_fail(
            "Repository discovery",
            f"Error during discovery: {str(e)}"
        )


def validate_executable_permissions(result: ValidationResult):
    """Validate shell scripts are executable."""
    base_path = Path(__file__).parent.parent

    scripts = [
        "scripts/run_cross_repo_tests.sh",
    ]

    for script_path in scripts:
        full_path = base_path / script_path
        if full_path.exists():
            import os
            import stat

            is_executable = os.access(full_path, os.X_OK)
            if is_executable:
                result.add_pass(f"Executable: {script_path}")
            else:
                result.add_warning(
                    f"{script_path} is not executable. Run: chmod +x {full_path}"
                )


def main():
    """Run all validations."""
    print("=" * 70)
    print("Cross-Repository Testing Infrastructure Validator")
    print("=" * 70)
    print()

    result = ValidationResult()

    print("Checking dependencies...")
    validate_dependencies(result)

    print("Checking files...")
    validate_files(result)

    print("Checking directories...")
    validate_directories(result)

    print("Checking configuration...")
    validate_configuration(result)

    print("Checking repository discovery...")
    validate_repository_discovery(result)

    print("Checking permissions...")
    validate_executable_permissions(result)

    print()
    result.print_summary()

    return 0 if result.is_valid() else 1


if __name__ == "__main__":
    sys.exit(main())
