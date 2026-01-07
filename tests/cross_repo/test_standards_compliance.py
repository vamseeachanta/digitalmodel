# ABOUTME: Cross-repository standards compliance test suite
# Tests module structure, file organization, CLAUDE.md, and agent registry compliance

"""
Cross-Repository Standards Compliance Tests

Tests all repositories in workspace-hub for compliance with:
- Module structure standards
- File organization standards
- CLAUDE.md presence and structure
- Agent registry consistency
"""

import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import yaml


class ComplianceChecker:
    """Check repository compliance with workspace standards."""

    def __init__(self, repo_path: Path, config: Dict):
        """
        Initialize compliance checker.

        Args:
            repo_path: Path to repository root
            config: Configuration dictionary from cross-repo-test-config.yaml
        """
        self.repo_path = repo_path
        self.config = config
        self.standards = config.get("standards", {})

    def check_all(self) -> Dict[str, any]:
        """
        Run all compliance checks.

        Returns:
            Dictionary with check results and suggestions
        """
        results = {
            "repo_name": self.repo_path.name,
            "repo_path": str(self.repo_path),
            "checks": {},
            "overall_score": 0.0,
            "suggestions": []
        }

        # Run individual checks
        checks = [
            self.check_module_structure,
            self.check_file_organization,
            self.check_claude_md,
            self.check_agent_registry
        ]

        total_checks = 0
        passed_checks = 0

        for check_func in checks:
            check_name, passed, details, suggestions = check_func()
            results["checks"][check_name] = {
                "passed": passed,
                "details": details
            }
            if suggestions:
                results["suggestions"].extend(suggestions)

            total_checks += 1
            if passed:
                passed_checks += 1

        # Calculate overall compliance score
        results["overall_score"] = (passed_checks / total_checks * 100) if total_checks > 0 else 0.0

        return results

    def check_module_structure(self) -> Tuple[str, bool, str, List[str]]:
        """
        Check for standard module structure pattern.

        Returns:
            Tuple of (check_name, passed, details, suggestions)
        """
        if not self.standards.get("module_structure", {}).get("enabled", False):
            return "module_structure", True, "Check disabled", []

        pattern = self.standards["module_structure"]["pattern"]

        # Look for src/*/modules/ pattern
        src_path = self.repo_path / "src"
        if not src_path.exists():
            return (
                "module_structure",
                False,
                "No src/ directory found",
                ["Create src/ directory with package structure: src/<package>/modules/"]
            )

        # Find any package directories
        packages = [p for p in src_path.iterdir() if p.is_dir() and not p.name.startswith(".")]

        if not packages:
            return (
                "module_structure",
                False,
                "No packages found in src/",
                ["Create package directory: src/<package_name>/"]
            )

        # Check if any package has modules/ subdirectory
        has_modules = any((pkg / "modules").exists() for pkg in packages)

        if has_modules:
            details = f"Found modules/ in {len([p for p in packages if (p / 'modules').exists()])} package(s)"
            return "module_structure", True, details, []
        else:
            return (
                "module_structure",
                False,
                "No modules/ subdirectory found in packages",
                [f"Create modules/ directory in: {pkg.name}/" for pkg in packages]
            )

    def check_file_organization(self) -> Tuple[str, bool, str, List[str]]:
        """
        Check that no work files are in repository root.

        Returns:
            Tuple of (check_name, passed, details, suggestions)
        """
        if not self.standards.get("file_organization", {}).get("enabled", False):
            return "file_organization", True, "Check disabled", []

        allowed_root_files = set(self.standards["file_organization"].get("allowed_root_files", []))

        # Get all files in root (not directories)
        root_files = [f for f in self.repo_path.iterdir() if f.is_file()]

        # Check for disallowed files
        violations = []
        for file in root_files:
            if file.name not in allowed_root_files and not file.name.startswith("."):
                violations.append(file.name)

        if violations:
            details = f"Found {len(violations)} disallowed file(s) in root: {', '.join(violations[:5])}"
            suggestions = [
                f"Move {file} to appropriate directory (src/, tests/, docs/, scripts/, etc.)"
                for file in violations[:5]
            ]
            return "file_organization", False, details, suggestions
        else:
            details = f"All {len(root_files)} root files are allowed"
            return "file_organization", True, details, []

    def check_claude_md(self) -> Tuple[str, bool, str, List[str]]:
        """
        Check for CLAUDE.md presence and required sections.

        Returns:
            Tuple of (check_name, passed, details, suggestions)
        """
        if not self.standards.get("claude_md", {}).get("enabled", False):
            return "claude_md", True, "Check disabled", []

        claude_md_path = self.repo_path / "CLAUDE.md"

        if not claude_md_path.exists():
            if self.standards["claude_md"].get("required", False):
                return (
                    "claude_md",
                    False,
                    "CLAUDE.md not found",
                    ["Create CLAUDE.md with project configuration and standards"]
                )
            else:
                return "claude_md", True, "CLAUDE.md not required", []

        # Check for required sections
        required_sections = self.standards["claude_md"].get("required_sections", [])

        try:
            content = claude_md_path.read_text(encoding="utf-8")

            missing_sections = []
            for section in required_sections:
                if section not in content:
                    missing_sections.append(section)

            if missing_sections:
                details = f"Missing section(s): {', '.join(missing_sections)}"
                suggestions = [
                    f"Add '## {section}' section to CLAUDE.md"
                    for section in missing_sections
                ]
                return "claude_md", False, details, suggestions
            else:
                details = f"CLAUDE.md present with all {len(required_sections)} required section(s)"
                return "claude_md", True, details, []

        except Exception as e:
            return (
                "claude_md",
                False,
                f"Error reading CLAUDE.md: {str(e)}",
                ["Verify CLAUDE.md is valid UTF-8 encoded text"]
            )

    def check_agent_registry(self) -> Tuple[str, bool, str, List[str]]:
        """
        Check for agent registry in standard locations.

        Returns:
            Tuple of (check_name, passed, details, suggestions)
        """
        if not self.standards.get("agent_registry", {}).get("enabled", False):
            return "agent_registry", True, "Check disabled", []

        possible_paths = self.standards["agent_registry"].get("possible_paths", [])

        found_registries = []
        for path_str in possible_paths:
            registry_path = self.repo_path / path_str
            if registry_path.exists():
                found_registries.append(path_str)

                # Validate JSON structure
                try:
                    with open(registry_path, "r", encoding="utf-8") as f:
                        registry_data = json.load(f)

                    # Check if it has agents
                    if isinstance(registry_data, dict) and "agents" in registry_data:
                        agent_count = len(registry_data["agents"])
                        details = f"Found valid agent registry at {path_str} with {agent_count} agent(s)"
                        return "agent_registry", True, details, []
                    else:
                        details = f"Registry at {path_str} has invalid structure"
                        return (
                            "agent_registry",
                            False,
                            details,
                            ["Agent registry should have 'agents' key with array of agent definitions"]
                        )

                except json.JSONDecodeError as e:
                    details = f"Invalid JSON in {path_str}: {str(e)}"
                    return (
                        "agent_registry",
                        False,
                        details,
                        [f"Fix JSON syntax errors in {path_str}"]
                    )

        if not found_registries:
            suggestions = [
                f"Create agent registry at one of: {', '.join(possible_paths)}",
                "Registry should be JSON with structure: {'agents': [...]}"
            ]
            return (
                "agent_registry",
                False,
                "No agent registry found",
                suggestions
            )

        return "agent_registry", True, f"Found registry at {found_registries[0]}", []


def test_repository_compliance(repo_path: Path, config: Dict) -> Dict:
    """
    Test a single repository for standards compliance.

    Args:
        repo_path: Path to repository
        config: Configuration dictionary

    Returns:
        Compliance test results
    """
    checker = ComplianceChecker(repo_path, config)
    return checker.check_all()


def load_config(config_path: Path) -> Dict:
    """Load configuration from YAML file."""
    with open(config_path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


if __name__ == "__main__":
    # Manual test execution
    import sys

    if len(sys.argv) < 2:
        print("Usage: python test_standards_compliance.py <repo_path>")
        sys.exit(1)

    repo = Path(sys.argv[1])
    config_file = Path(__file__).parent.parent.parent / "config" / "cross-repo-test-config.yaml"

    config = load_config(config_file)
    results = test_repository_compliance(repo, config)

    print(json.dumps(results, indent=2))
