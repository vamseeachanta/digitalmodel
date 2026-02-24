#!/usr/bin/env python3
"""
ABOUTME: OrcaFlex modular YAML validation suite for syntax and reference checking
ABOUTME: Validates YAML files, includefile references, and generates status reports

This script validates OrcaFlex modular YAML files by:
1. Checking YAML syntax validity
2. Verifying all includefile references exist
3. Analyzing module dependencies
4. Validating file structure and content
5. Generating validation status reports

Usage:
    python validate_modules.py [--config CONFIG_FILE] [--report]
"""

import os
import sys
import yaml
import argparse
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple, Set
from collections import defaultdict


class ModuleValidator:
    """Validates OrcaFlex modular YAML structure"""

    def __init__(self, base_dir: str):
        self.base_dir = Path(base_dir)
        self.output_dir = self.base_dir / 'output'
        self.validation_results = {
            'timestamp': datetime.now().isoformat(),
            'base_files': {},
            'modules': {},
            'dependencies': {},
            'errors': [],
            'warnings': [],
            'summary': {}
        }

    def validate_yaml_syntax(self, file_path: Path) -> Tuple[bool, str, dict]:
        """Validate YAML syntax and return parsed content"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = yaml.safe_load(f)
            return True, "Valid YAML syntax", content
        except yaml.YAMLError as e:
            return False, f"YAML syntax error: {str(e)}", None
        except Exception as e:
            return False, f"Error reading file: {str(e)}", None

    def extract_includefiles(self, content: dict) -> List[str]:
        """Extract all includefile references from YAML content"""
        includefiles = []

        def traverse(obj):
            if isinstance(obj, dict):
                for key, value in obj.items():
                    if key == 'includefile' and isinstance(value, str):
                        includefiles.append(value)
                    else:
                        traverse(value)
            elif isinstance(obj, list):
                for item in obj:
                    traverse(item)

        traverse(content)
        return includefiles

    def validate_base_file(self, base_file: str) -> dict:
        """Validate a base configuration file"""
        file_path = self.output_dir / base_file
        result = {
            'file': base_file,
            'exists': file_path.exists(),
            'valid_syntax': False,
            'includefiles': [],
            'missing_modules': [],
            'sections': [],
            'size_bytes': 0,
            'status': 'FAIL'
        }

        if not result['exists']:
            self.validation_results['errors'].append(f"Base file not found: {base_file}")
            return result

        result['size_bytes'] = file_path.stat().st_size

        # Validate YAML syntax
        valid, msg, content = self.validate_yaml_syntax(file_path)
        result['valid_syntax'] = valid

        if not valid:
            self.validation_results['errors'].append(f"{base_file}: {msg}")
            return result

        # Extract sections and includefiles
        if content:
            result['sections'] = list(content.keys())
            result['includefiles'] = self.extract_includefiles(content)

        # Check if all referenced modules exist
        for module in result['includefiles']:
            module_path = self.output_dir / module
            if not module_path.exists():
                result['missing_modules'].append(module)
                self.validation_results['errors'].append(
                    f"{base_file}: Missing module '{module}'"
                )

        # Determine status
        if valid and len(result['missing_modules']) == 0:
            result['status'] = 'PASS'
        elif valid and len(result['missing_modules']) > 0:
            result['status'] = 'WARN'

        return result

    def validate_module(self, module_file: str) -> dict:
        """Validate an individual module file"""
        file_path = self.output_dir / module_file
        result = {
            'file': module_file,
            'exists': file_path.exists(),
            'valid_syntax': False,
            'size_bytes': 0,
            'line_count': 0,
            'has_content': False,
            'nested_includes': [],
            'status': 'FAIL'
        }

        if not result['exists']:
            return result

        result['size_bytes'] = file_path.stat().st_size

        # Count lines
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()
                result['line_count'] = len(lines)
                result['has_content'] = any(line.strip() for line in lines)
        except Exception as e:
            self.validation_results['warnings'].append(
                f"{module_file}: Could not read file: {str(e)}"
            )

        # Validate YAML syntax
        valid, msg, content = self.validate_yaml_syntax(file_path)
        result['valid_syntax'] = valid

        if not valid:
            self.validation_results['errors'].append(f"{module_file}: {msg}")
            return result

        # Check for nested includefiles (modules referencing other modules)
        if content:
            result['nested_includes'] = self.extract_includefiles(content)

        # Determine status
        if valid and result['has_content']:
            result['status'] = 'PASS'
        elif valid and not result['has_content']:
            result['status'] = 'WARN'
            self.validation_results['warnings'].append(
                f"{module_file}: File is empty or has no content"
            )

        return result

    def build_dependency_graph(self) -> Dict[str, Set[str]]:
        """Build dependency graph showing which modules each base file uses"""
        graph = defaultdict(set)

        for base_file, data in self.validation_results['base_files'].items():
            graph[base_file] = set(data['includefiles'])

        return dict(graph)

    def analyze_module_usage(self) -> Dict[str, List[str]]:
        """Analyze which base files use each module"""
        usage = defaultdict(list)

        for base_file, data in self.validation_results['base_files'].items():
            for module in data['includefiles']:
                usage[module].append(base_file)

        return dict(usage)

    def validate_all(self, base_files: List[str]) -> dict:
        """Run complete validation on all files"""
        print("=" * 70)
        print("OrcaFlex Modular YML Validation Suite")
        print("=" * 70)
        print()

        # Validate base files
        print("Validating Base Configuration Files...")
        for base_file in base_files:
            print(f"  - {base_file}...", end=" ")
            result = self.validate_base_file(base_file)
            self.validation_results['base_files'][base_file] = result
            print(f"[{result['status']}]")
        print()

        # Collect all unique modules
        all_modules = set()
        for base_file, data in self.validation_results['base_files'].items():
            all_modules.update(data['includefiles'])

        # Validate modules
        print(f"Validating {len(all_modules)} Module Files...")
        for module in sorted(all_modules):
            result = self.validate_module(module)
            self.validation_results['modules'][module] = result
            if result['status'] != 'PASS':
                print(f"  - {module}... [{result['status']}]")
        print()

        # Build dependency analysis
        self.validation_results['dependencies'] = self.build_dependency_graph()

        # Generate summary
        self.generate_summary()

        return self.validation_results

    def generate_summary(self):
        """Generate validation summary statistics"""
        summary = {
            'total_base_files': len(self.validation_results['base_files']),
            'total_modules': len(self.validation_results['modules']),
            'base_files_passed': 0,
            'modules_passed': 0,
            'total_errors': len(self.validation_results['errors']),
            'total_warnings': len(self.validation_results['warnings']),
            'overall_status': 'FAIL'
        }

        # Count passes
        for data in self.validation_results['base_files'].values():
            if data['status'] == 'PASS':
                summary['base_files_passed'] += 1

        for data in self.validation_results['modules'].values():
            if data['status'] == 'PASS':
                summary['modules_passed'] += 1

        # Determine overall status
        if summary['total_errors'] == 0:
            if summary['total_warnings'] == 0:
                summary['overall_status'] = 'PASS'
            else:
                summary['overall_status'] = 'WARN'

        self.validation_results['summary'] = summary

    def print_summary(self):
        """Print validation summary to console"""
        summary = self.validation_results['summary']

        print("=" * 70)
        print("VALIDATION SUMMARY")
        print("=" * 70)
        print(f"Overall Status: {summary['overall_status']}")
        print()
        print(f"Base Files: {summary['base_files_passed']}/{summary['total_base_files']} passed")
        print(f"Modules:    {summary['modules_passed']}/{summary['total_modules']} passed")
        print(f"Errors:     {summary['total_errors']}")
        print(f"Warnings:   {summary['total_warnings']}")
        print()

        if self.validation_results['errors']:
            print("ERRORS:")
            for error in self.validation_results['errors']:
                print(f"  [X] {error}")
            print()

        if self.validation_results['warnings']:
            print("WARNINGS:")
            for warning in self.validation_results['warnings']:
                print(f"  [!] {warning}")
            print()

        print("=" * 70)

    def generate_markdown_report(self, output_file: str = None):
        """Generate detailed markdown validation report"""
        if output_file is None:
            output_file = self.output_dir / 'VALIDATION_STATUS.md'

        with open(output_file, 'w', encoding='utf-8') as f:
            # Header
            f.write("# OrcaFlex Modular YML Validation Status\n\n")
            f.write(f"**Generated:** {self.validation_results['timestamp']}\n\n")
            f.write(f"**Overall Status:** ")

            status = self.validation_results['summary']['overall_status']
            if status == 'PASS':
                f.write("✅ **PASS**\n\n")
            elif status == 'WARN':
                f.write("⚠️ **PASS WITH WARNINGS**\n\n")
            else:
                f.write("❌ **FAIL**\n\n")

            # Summary Table
            f.write("## Summary Statistics\n\n")
            f.write("| Metric | Count | Status |\n")
            f.write("|--------|-------|--------|\n")

            summary = self.validation_results['summary']
            f.write(f"| Base Configuration Files | {summary['base_files_passed']}/{summary['total_base_files']} | ")
            f.write("✅ Pass\n" if summary['base_files_passed'] == summary['total_base_files'] else "❌ Fail\n")

            f.write(f"| Module Files | {summary['modules_passed']}/{summary['total_modules']} | ")
            f.write("✅ Pass\n" if summary['modules_passed'] == summary['total_modules'] else "❌ Fail\n")

            f.write(f"| Errors | {summary['total_errors']} | ")
            f.write("✅ None\n" if summary['total_errors'] == 0 else f"❌ {summary['total_errors']}\n")

            f.write(f"| Warnings | {summary['total_warnings']} | ")
            f.write("✅ None\n" if summary['total_warnings'] == 0 else f"⚠️ {summary['total_warnings']}\n")

            f.write("\n")

            # Base Files Detail
            f.write("## Base Configuration Files\n\n")
            for base_file, data in self.validation_results['base_files'].items():
                status_icon = "✅" if data['status'] == 'PASS' else ("⚠️" if data['status'] == 'WARN' else "❌")
                f.write(f"### {status_icon} {base_file}\n\n")
                f.write(f"- **Status:** {data['status']}\n")
                f.write(f"- **File Size:** {data['size_bytes']:,} bytes\n")
                f.write(f"- **Valid YAML:** {'Yes' if data['valid_syntax'] else 'No'}\n")
                f.write(f"- **Sections:** {len(data['sections'])}\n")
                f.write(f"- **Module References:** {len(data['includefiles'])}\n")

                if data['missing_modules']:
                    f.write(f"- **Missing Modules:** {len(data['missing_modules'])}\n")
                    for module in data['missing_modules']:
                        f.write(f"  - ❌ `{module}`\n")

                f.write("\n**Sections:**\n")
                for section in data['sections']:
                    f.write(f"- {section}\n")

                f.write("\n**Module Dependencies:**\n")
                for module in sorted(data['includefiles']):
                    module_status = self.validation_results['modules'].get(module, {}).get('status', 'UNKNOWN')
                    status_icon = "✅" if module_status == 'PASS' else ("⚠️" if module_status == 'WARN' else "❌")
                    f.write(f"- {status_icon} `{module}`\n")

                f.write("\n")

            # Module Usage Analysis
            f.write("## Module Usage Analysis\n\n")
            usage = self.analyze_module_usage()

            # Categorize modules
            shared = {m: u for m, u in usage.items() if len(u) > 1}
            specific = {m: u for m, u in usage.items() if len(u) == 1}

            f.write(f"### Shared Modules ({len(shared)})\n\n")
            f.write("These modules are used by multiple base configurations:\n\n")
            f.write("| Module | Used By | Status |\n")
            f.write("|--------|---------|--------|\n")
            for module in sorted(shared.keys()):
                status = self.validation_results['modules'].get(module, {}).get('status', 'UNKNOWN')
                status_icon = "✅" if status == 'PASS' else ("⚠️" if status == 'WARN' else "❌")
                users = ', '.join(shared[module])
                f.write(f"| `{module}` | {users} | {status_icon} {status} |\n")
            f.write("\n")

            f.write(f"### Configuration-Specific Modules ({len(specific)})\n\n")
            f.write("These modules are used by only one configuration:\n\n")
            for config in self.validation_results['base_files'].keys():
                config_modules = [m for m, u in specific.items() if config in u]
                if config_modules:
                    f.write(f"**{config}:** ({len(config_modules)} modules)\n")
                    for module in sorted(config_modules):
                        status = self.validation_results['modules'].get(module, {}).get('status', 'UNKNOWN')
                        status_icon = "✅" if status == 'PASS' else ("⚠️" if status == 'WARN' else "❌")
                        f.write(f"- {status_icon} `{module}`\n")
                    f.write("\n")

            # Module Details
            f.write("## Module File Details\n\n")
            f.write("| Module | Size | Lines | Status | Notes |\n")
            f.write("|--------|------|-------|--------|-------|\n")

            for module in sorted(self.validation_results['modules'].keys()):
                data = self.validation_results['modules'][module]
                status_icon = "✅" if data['status'] == 'PASS' else ("⚠️" if data['status'] == 'WARN' else "❌")
                size = f"{data['size_bytes']:,}" if data['exists'] else "N/A"
                lines = str(data['line_count']) if data['exists'] else "N/A"

                notes = []
                if not data['exists']:
                    notes.append("Missing")
                elif not data['valid_syntax']:
                    notes.append("Invalid YAML")
                elif not data['has_content']:
                    notes.append("Empty")

                notes_str = ", ".join(notes) if notes else "-"
                f.write(f"| `{module}` | {size} | {lines} | {status_icon} {data['status']} | {notes_str} |\n")

            f.write("\n")

            # Errors Section
            if self.validation_results['errors']:
                f.write("## Errors\n\n")
                for i, error in enumerate(self.validation_results['errors'], 1):
                    f.write(f"{i}. ❌ {error}\n")
                f.write("\n")

            # Warnings Section
            if self.validation_results['warnings']:
                f.write("## Warnings\n\n")
                for i, warning in enumerate(self.validation_results['warnings'], 1):
                    f.write(f"{i}. ⚠️ {warning}\n")
                f.write("\n")

            # Recommendations
            f.write("## Recommendations\n\n")
            if summary['overall_status'] == 'PASS':
                f.write("✅ All validations passed successfully!\n\n")
                f.write("The modular file structure is ready for use in OrcaFlex.\n")
            else:
                f.write("### Action Items\n\n")
                if self.validation_results['errors']:
                    f.write("1. **Fix Errors:** Address all error messages listed above\n")
                if self.validation_results['warnings']:
                    f.write("2. **Review Warnings:** Investigate warning messages for potential issues\n")
                f.write("3. **Re-run Validation:** Run this script again after making changes\n")

            f.write("\n---\n\n")
            f.write("*This report was automatically generated by the OrcaFlex Modular YML Validation Suite.*\n")

        return output_file


def main():
    parser = argparse.ArgumentParser(
        description='Validate OrcaFlex modular YAML files'
    )
    parser.add_argument(
        '--base-dir',
        default='.',
        help='Base directory containing input/output folders (default: current directory)'
    )
    parser.add_argument(
        '--config',
        nargs='+',
        default=['calm_buoy_base.yml', 'discretised_calm_buoy_base.yml'],
        help='Base configuration files to validate'
    )
    parser.add_argument(
        '--report',
        action='store_true',
        help='Generate markdown validation report'
    )
    parser.add_argument(
        '--report-file',
        help='Output file for markdown report'
    )

    args = parser.parse_args()

    # Initialize validator
    validator = ModuleValidator(args.base_dir)

    # Run validation
    results = validator.validate_all(args.config)

    # Print summary
    validator.print_summary()

    # Generate report if requested
    if args.report or args.report_file:
        report_file = validator.generate_markdown_report(args.report_file)
        print(f"Validation report generated: {report_file}")
        print()

    # Exit with appropriate code
    if results['summary']['overall_status'] == 'FAIL':
        sys.exit(1)
    elif results['summary']['overall_status'] == 'WARN':
        sys.exit(0)  # Warnings don't cause failure
    else:
        sys.exit(0)


if __name__ == '__main__':
    main()
