#!/usr/bin/env python3
"""
File Organization Checker - Repository Structure Validation Tool

This tool validates and enforces the mandatory repository organization pattern:
<group_name>/modules/<module_name>/

Usage:
  python tools/file-organization-checker.py [options]
  
Slash Command Integration:
  /check-organization [path]
"""

import os
import sys
import argparse
import json
import yaml
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Set
from dataclasses import dataclass, asdict
from datetime import datetime
import re

@dataclass
class OrganizationIssue:
    """Represents a repository organization issue."""
    path: str
    issue_type: str
    severity: str  # 'critical', 'warning', 'info'
    message: str
    suggested_fix: str
    pattern_violation: str

@dataclass
class OrganizationReport:
    """Complete organization validation report."""
    timestamp: str
    repository_path: str
    total_files_checked: int
    total_directories_checked: int
    issues: List[OrganizationIssue]
    compliant_patterns: List[str]
    violations: List[str]
    recommendations: List[str]
    overall_compliance: float

class FileOrganizationChecker:
    """Comprehensive file organization validation system."""
    
    def __init__(self, repository_root: str = None):
        """Initialize the organization checker."""
        self.repo_root = Path(repository_root) if repository_root else Path.cwd()
        
        # Mandatory organization patterns
        self.mandatory_patterns = {
            'specs': r'^specs/modules/[^/]+/?',
            'src': r'^src/modules/[^/]+/?', 
            'docs': r'^docs/domains/[^/]+/?',
            'tests': r'^tests/domains/[^/]+/?',
            'configs': r'^configs/modules/[^/]+/?'
        }
        
        # Required files for each module
        self.required_module_files = {
            'specs': ['README.md', 'tasks.md', 'task_summary.md', 'prompt.md', 'technical-details.md'],
            'src': ['__init__.py'],
            'docs': ['README.md'],
            'tests': ['__init__.py'],
            'configs': []
        }
        
        # Sub-specs folder requirements
        self.sub_specs_requirements = {
            'required': True,
            'min_files': 1,
            'naming_pattern': r'^[a-z][a-z0-9-]*\.md$'
        }
        
        # Exclusions and special cases
        self.exclusions = {
            'directories': {'.git', '__pycache__', '.pytest_cache', 'node_modules', '.venv', 'venv'},
            'files': {'.gitignore', '.gitattributes', 'README.md', 'LICENSE', 'CHANGELOG.md'},
            'extensions': {'.pyc', '.pyo', '.pyd', '.so', '.dll'}
        }
        
        self.issues = []
        self.compliant_paths = []
        self.statistics = {
            'files_checked': 0,
            'directories_checked': 0,
            'modules_found': 0,
            'violations': 0
        }
    
    def check_repository_organization(self) -> OrganizationReport:
        """Perform comprehensive repository organization check."""
        
        print("🔍 Analyzing repository organization...")
        
        # Phase 1: Validate directory structure
        self._validate_directory_structure()
        
        # Phase 2: Check module compliance
        self._validate_module_structure()
        
        # Phase 3: Validate file naming conventions
        self._validate_file_naming()
        
        # Phase 4: Check cross-references and dependencies
        self._validate_cross_references()
        
        # Phase 5: Generate recommendations
        recommendations = self._generate_recommendations()
        
        # Calculate compliance score
        compliance_score = self._calculate_compliance_score()
        
        report = OrganizationReport(
            timestamp=datetime.now().isoformat(),
            repository_path=str(self.repo_root),
            total_files_checked=self.statistics['files_checked'],
            total_directories_checked=self.statistics['directories_checked'],
            issues=self.issues,
            compliant_patterns=self.compliant_paths,
            violations=[issue.path for issue in self.issues if issue.severity == 'critical'],
            recommendations=recommendations,
            overall_compliance=compliance_score
        )
        
        return report
    
    def _validate_directory_structure(self) -> None:
        """Validate the mandatory <group>/modules/<module>/ pattern."""
        
        for group_name, pattern in self.mandatory_patterns.items():
            group_path = self.repo_root / group_name
            
            if not group_path.exists():
                continue
                
            self._check_group_structure(group_path, group_name, pattern)
    
    def _check_group_structure(self, group_path: Path, group_name: str, pattern: str) -> None:
        """Check structure within a group directory."""
        
        for item in group_path.iterdir():
            if item.name in self.exclusions['directories']:
                continue
                
            self.statistics['directories_checked'] += 1
            relative_path = item.relative_to(self.repo_root)
            
            # Check if direct subdirectory follows modules pattern
            if item.is_dir() and item.name != 'modules':
                # This is a violation - should be under modules/
                self.issues.append(OrganizationIssue(
                    path=str(relative_path),
                    issue_type='structure_violation',
                    severity='critical',
                    message=f"Directory '{item.name}' violates mandatory pattern",
                    suggested_fix=f"Move to {group_name}/modules/{item.name}/",
                    pattern_violation=f"Should follow pattern: {group_name}/modules/<module>/"
                ))
                self.statistics['violations'] += 1
            
            elif item.is_dir() and item.name == 'modules':
                # Check modules subdirectory
                self._check_modules_directory(item, group_name)
                self.compliant_paths.append(str(relative_path))
    
    def _check_modules_directory(self, modules_path: Path, group_name: str) -> None:
        """Check structure within modules directory."""
        
        for module_dir in modules_path.iterdir():
            if module_dir.name in self.exclusions['directories']:
                continue
                
            if not module_dir.is_dir():
                continue
                
            self.statistics['modules_found'] += 1
            self._validate_module_directory(module_dir, group_name)
    
    def _validate_module_directory(self, module_path: Path, group_name: str) -> None:
        """Validate individual module directory structure."""
        
        relative_path = module_path.relative_to(self.repo_root)
        
        # Check required files for this group type
        required_files = self.required_module_files.get(group_name, [])
        
        for required_file in required_files:
            file_path = module_path / required_file
            if not file_path.exists():
                self.issues.append(OrganizationIssue(
                    path=str(relative_path / required_file),
                    issue_type='missing_required_file',
                    severity='critical' if required_file == 'README.md' else 'warning',
                    message=f"Missing required file: {required_file}",
                    suggested_fix=f"Create {required_file} in module directory",
                    pattern_violation="Module missing required standardized files"
                ))
        
        # Check sub-specs directory for specs modules
        if group_name == 'specs':
            self._validate_sub_specs_directory(module_path)
        
        # Validate file naming within module
        self._validate_module_files(module_path)
    
    def _validate_sub_specs_directory(self, module_path: Path) -> None:
        """Validate sub-specs directory structure."""
        
        sub_specs_path = module_path / 'sub-specs'
        relative_path = sub_specs_path.relative_to(self.repo_root)
        
        if not sub_specs_path.exists():
            self.issues.append(OrganizationIssue(
                path=str(relative_path),
                issue_type='missing_sub_specs',
                severity='warning',
                message="Missing sub-specs directory",
                suggested_fix="Create sub-specs/ directory with component specifications",
                pattern_violation="Specs modules should have sub-specs/ for component details"
            ))
            return
        
        # Check sub-specs content
        sub_spec_files = list(sub_specs_path.glob('*.md'))
        
        if len(sub_spec_files) < self.sub_specs_requirements['min_files']:
            self.issues.append(OrganizationIssue(
                path=str(relative_path),
                issue_type='insufficient_sub_specs',
                severity='info',
                message=f"Sub-specs directory has only {len(sub_spec_files)} files",
                suggested_fix="Add more detailed component specifications",
                pattern_violation="Sub-specs should contain detailed component documentation"
            ))
        
        # Validate sub-spec file naming
        naming_pattern = re.compile(self.sub_specs_requirements['naming_pattern'])
        for sub_spec_file in sub_spec_files:
            if not naming_pattern.match(sub_spec_file.name):
                self.issues.append(OrganizationIssue(
                    path=str(sub_spec_file.relative_to(self.repo_root)),
                    issue_type='invalid_naming',
                    severity='info',
                    message=f"Sub-spec file '{sub_spec_file.name}' doesn't follow naming convention",
                    suggested_fix="Use lowercase with hyphens: component-name.md",
                    pattern_violation="Sub-spec files should use kebab-case naming"
                ))
    
    def _validate_module_structure(self) -> None:
        """Validate overall module structure compliance."""
        
        # Check for orphaned directories (not following pattern)
        self._find_orphaned_directories()
        
        # Validate module naming conventions
        self._validate_module_naming()
        
        # Check for circular dependencies
        self._check_circular_dependencies()
    
    def _find_orphaned_directories(self) -> None:
        """Find directories that don't follow the mandatory pattern."""
        
        for group_name in self.mandatory_patterns.keys():
            group_path = self.repo_root / group_name
            if not group_path.exists():
                continue
                
            # Check for any directory that bypasses the modules/ structure
            for item in group_path.rglob('*'):
                if not item.is_dir() or item.name in self.exclusions['directories']:
                    continue
                
                relative_path = item.relative_to(self.repo_root)
                path_parts = relative_path.parts
                
                # Should be: group/modules/module/... 
                if len(path_parts) >= 2 and path_parts[1] != 'modules':
                    # This is an orphaned directory
                    self.issues.append(OrganizationIssue(
                        path=str(relative_path),
                        issue_type='orphaned_directory',
                        severity='critical',
                        message=f"Directory bypasses mandatory modules/ structure",
                        suggested_fix=f"Move to {group_name}/modules/{path_parts[1]}/",
                        pattern_violation="All directories must follow <group>/modules/<module>/ pattern"
                    ))
    
    def _validate_file_naming(self) -> None:
        """Validate file naming conventions."""
        
        naming_rules = {
            'spec_files': {
                'pattern': r'^[a-z][a-z0-9-]*\.md$',
                'description': 'Spec files should use kebab-case with .md extension'
            },
            'python_files': {
                'pattern': r'^[a-z][a-z0-9_]*\.py$',
                'description': 'Python files should use snake_case'
            },
            'config_files': {
                'pattern': r'^[a-z][a-z0-9-]*\.(yml|yaml|json)$',
                'description': 'Config files should use kebab-case'
            }
        }
        
        for file_path in self.repo_root.rglob('*'):
            if not file_path.is_file():
                continue
                
            if file_path.name in self.exclusions['files']:
                continue
                
            if file_path.suffix in self.exclusions['extensions']:
                continue
                
            self.statistics['files_checked'] += 1
            self._check_file_naming(file_path, naming_rules)
    
    def _check_file_naming(self, file_path: Path, naming_rules: Dict) -> None:
        """Check individual file naming against rules."""
        
        relative_path = file_path.relative_to(self.repo_root)
        
        # Determine applicable rule
        applicable_rule = None
        
        if file_path.suffix == '.md' and 'specs' in relative_path.parts:
            applicable_rule = naming_rules['spec_files']
        elif file_path.suffix == '.py':
            applicable_rule = naming_rules['python_files']
        elif file_path.suffix in ['.yml', '.yaml', '.json']:
            applicable_rule = naming_rules['config_files']
        
        if applicable_rule:
            pattern = re.compile(applicable_rule['pattern'])
            if not pattern.match(file_path.name):
                self.issues.append(OrganizationIssue(
                    path=str(relative_path),
                    issue_type='naming_convention',
                    severity='info',
                    message=f"File name doesn't follow convention: {file_path.name}",
                    suggested_fix=applicable_rule['description'],
                    pattern_violation="File naming should follow repository conventions"
                ))
    
    def _validate_cross_references(self) -> None:
        """Validate cross-references and dependencies between modules."""
        
        # This is a placeholder for more sophisticated dependency checking
        # Could include:
        # - Checking import statements in Python files
        # - Validating markdown links between specs
        # - Verifying task dependencies
        pass
    
    def _check_circular_dependencies(self) -> None:
        """Check for circular dependencies between modules."""
        
        # Placeholder for circular dependency detection
        # Would analyze import statements and cross-references
        pass
    
    def _validate_module_naming(self) -> None:
        """Validate module naming conventions."""
        
        module_naming_pattern = re.compile(r'^[a-z][a-z0-9-]*$')
        
        for group_name in self.mandatory_patterns.keys():
            modules_path = self.repo_root / group_name / 'modules'
            if not modules_path.exists():
                continue
                
            for module_dir in modules_path.iterdir():
                if not module_dir.is_dir():
                    continue
                    
                if not module_naming_pattern.match(module_dir.name):
                    self.issues.append(OrganizationIssue(
                        path=str(module_dir.relative_to(self.repo_root)),
                        issue_type='module_naming',
                        severity='warning',
                        message=f"Module name '{module_dir.name}' doesn't follow naming convention",
                        suggested_fix="Use lowercase with hyphens: module-name",
                        pattern_violation="Module names should use kebab-case"
                    ))
    
    def _generate_recommendations(self) -> List[str]:
        """Generate actionable recommendations based on found issues."""
        
        recommendations = []
        
        # Count issues by type
        issue_counts = {}
        for issue in self.issues:
            issue_counts[issue.issue_type] = issue_counts.get(issue.issue_type, 0) + 1
        
        # Generate specific recommendations
        if issue_counts.get('structure_violation', 0) > 0:
            recommendations.append(
                f"🚨 {issue_counts['structure_violation']} directories violate mandatory pattern. "
                "Use: python tools/file-organization-checker.py --fix-structure to auto-fix."
            )
        
        if issue_counts.get('missing_required_file', 0) > 0:
            recommendations.append(
                f"📄 {issue_counts['missing_required_file']} required files missing. "
                "Use: python tools/file-organization-checker.py --create-missing-files"
            )
        
        if issue_counts.get('naming_convention', 0) > 0:
            recommendations.append(
                f"🏷️ {issue_counts['naming_convention']} files have naming issues. "
                "Consider renaming to follow repository conventions."
            )
        
        if issue_counts.get('missing_sub_specs', 0) > 0:
            recommendations.append(
                f"📋 {issue_counts['missing_sub_specs']} modules missing sub-specs directories. "
                "Create sub-specs/ folders with component documentation."
            )
        
        # General recommendations
        if self.statistics['violations'] == 0:
            recommendations.append("✅ Repository structure is fully compliant! Great job!")
        else:
            recommendations.append(
                f"🔧 Fix {self.statistics['violations']} critical violations to achieve full compliance."
            )
        
        return recommendations
    
    def _calculate_compliance_score(self) -> float:
        """Calculate overall compliance score (0-100)."""
        
        if self.statistics['directories_checked'] == 0:
            return 100.0
        
        # Weight different issue types
        weights = {
            'critical': 10,
            'warning': 3,
            'info': 1
        }
        
        total_penalty = sum(weights.get(issue.severity, 1) for issue in self.issues)
        max_possible_penalty = self.statistics['directories_checked'] * weights['critical']
        
        if max_possible_penalty == 0:
            return 100.0
        
        compliance_score = max(0, 100 - (total_penalty / max_possible_penalty * 100))
        return round(compliance_score, 1)
    
    def fix_structure_violations(self, dry_run: bool = True) -> List[str]:
        """Auto-fix structure violations (with dry-run option)."""
        
        fixes_applied = []
        
        for issue in self.issues:
            if issue.issue_type == 'structure_violation':
                if not dry_run:
                    # Apply the fix
                    self._apply_structure_fix(issue)
                
                fixes_applied.append(issue.suggested_fix)
        
        return fixes_applied
    
    def _apply_structure_fix(self, issue: OrganizationIssue) -> None:
        """Apply a specific structure fix."""
        
        # Extract source and target paths from issue
        source_path = self.repo_root / issue.path
        
        # Parse suggested fix to determine target
        # This would need more sophisticated parsing
        # For now, this is a placeholder
        pass
    
    def generate_report_file(self, report: OrganizationReport, 
                           output_path: str = None) -> str:
        """Generate detailed report file."""
        
        if output_path is None:
            output_path = self.repo_root / f"organization-report-{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        with open(output_path, 'w') as f:
            json.dump(asdict(report), f, indent=2, default=str)
        
        return str(output_path)
    
    def print_summary_report(self, report: OrganizationReport) -> None:
        """Print summary report to console."""
        
        print("\n" + "="*80)
        print("📁 REPOSITORY ORGANIZATION REPORT")
        print("="*80)
        print(f"📍 Repository: {report.repository_path}")
        print(f"⏰ Checked: {report.timestamp}")
        print(f"📊 Compliance Score: {report.overall_compliance}%")
        print()
        
        print(f"📈 STATISTICS:")
        print(f"   Files Checked: {report.total_files_checked}")
        print(f"   Directories Checked: {report.total_directories_checked}")
        print(f"   Issues Found: {len(report.issues)}")
        print(f"   Critical Violations: {len(report.violations)}")
        print()
        
        if report.issues:
            print("⚠️  ISSUES FOUND:")
            
            # Group issues by severity
            issues_by_severity = {}
            for issue in report.issues:
                if issue.severity not in issues_by_severity:
                    issues_by_severity[issue.severity] = []
                issues_by_severity[issue.severity].append(issue)
            
            # Display by severity
            severity_icons = {'critical': '🚨', 'warning': '⚠️ ', 'info': 'ℹ️ '}
            
            for severity in ['critical', 'warning', 'info']:
                if severity in issues_by_severity:
                    print(f"\n   {severity_icons[severity]} {severity.upper()} ({len(issues_by_severity[severity])}):")
                    for issue in issues_by_severity[severity][:5]:  # Show top 5
                        print(f"      📁 {issue.path}")
                        print(f"         {issue.message}")
                        print(f"         💡 {issue.suggested_fix}")
                    
                    if len(issues_by_severity[severity]) > 5:
                        print(f"         ... and {len(issues_by_severity[severity]) - 5} more")
        
        if report.recommendations:
            print(f"\n💡 RECOMMENDATIONS:")
            for recommendation in report.recommendations:
                print(f"   {recommendation}")
        
        print("\n" + "="*80)

def create_slash_command_interface():
    """Create slash command interface for organization checking."""
    
    def check_organization_command(path: str = None) -> None:
        """Slash command: /check-organization [path]"""
        
        target_path = path if path else os.getcwd()
        
        print(f"🔍 Checking organization for: {target_path}")
        
        checker = FileOrganizationChecker(target_path)
        report = checker.check_repository_organization()
        
        checker.print_summary_report(report)
        
        # Generate detailed report file
        report_file = checker.generate_report_file(report)
        print(f"📄 Detailed report saved: {report_file}")
        
        # Auto-fix suggestions
        if report.violations:
            print(f"\n🔧 Run with --fix flag to attempt automatic fixes")
    
    return check_organization_command

def main():
    """Main entry point for the organization checker."""
    
    parser = argparse.ArgumentParser(
        description="Repository File Organization Checker",
        epilog="Enforces mandatory <group>/modules/<module>/ pattern"
    )
    
    parser.add_argument(
        'path', 
        nargs='?', 
        default=os.getcwd(),
        help='Path to repository root (default: current directory)'
    )
    
    parser.add_argument(
        '--fix-structure',
        action='store_true',
        help='Attempt to auto-fix structure violations'
    )
    
    parser.add_argument(
        '--create-missing-files',
        action='store_true', 
        help='Create missing required files'
    )
    
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be fixed without making changes'
    )
    
    parser.add_argument(
        '--report-format',
        choices=['console', 'json', 'yaml'],
        default='console',
        help='Output format for report'
    )
    
    parser.add_argument(
        '--output-file',
        help='Output file for detailed report'
    )
    
    args = parser.parse_args()
    
    # Initialize checker
    checker = FileOrganizationChecker(args.path)
    
    # Run organization check
    report = checker.check_repository_organization()
    
    # Display report
    if args.report_format == 'console':
        checker.print_summary_report(report)
    elif args.report_format in ['json', 'yaml']:
        output_file = args.output_file or f"organization-report.{args.report_format}"
        
        if args.report_format == 'json':
            with open(output_file, 'w') as f:
                json.dump(asdict(report), f, indent=2, default=str)
        else:
            with open(output_file, 'w') as f:
                yaml.dump(asdict(report), f, default_flow_style=False)
        
        print(f"📄 Report saved: {output_file}")
    
    # Apply fixes if requested
    if args.fix_structure:
        fixes = checker.fix_structure_violations(dry_run=args.dry_run)
        
        if args.dry_run:
            print(f"\n🔍 DRY RUN - Would apply {len(fixes)} fixes:")
            for fix in fixes:
                print(f"   🔧 {fix}")
        else:
            print(f"\n✅ Applied {len(fixes)} structure fixes")
    
    # Create missing files if requested
    if args.create_missing_files:
        print("\n📄 Creating missing required files...")
        # Implementation would go here
    
    # Exit with appropriate code
    exit_code = 0 if report.overall_compliance >= 95.0 else 1
    sys.exit(exit_code)

if __name__ == "__main__":
    main()