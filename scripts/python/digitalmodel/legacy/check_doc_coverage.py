#!/usr/bin/env python3
"""
Documentation coverage checker for digitalmodel CI/CD pipeline.

This script analyzes Python source code to determine documentation coverage
and generates reports for CI/CD integration.
"""

import ast
import sys
import argparse
import json
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, asdict
from datetime import datetime


@dataclass
class DocstringInfo:
    """Information about a docstring."""
    name: str
    type: str  # 'function', 'class', 'method', 'module'
    has_docstring: bool
    docstring_length: int
    line_number: int
    file_path: str
    is_public: bool
    complexity_score: int = 0  # For functions, based on number of arguments, etc.


@dataclass
class FileAnalysis:
    """Analysis results for a single file."""
    file_path: str
    total_items: int
    documented_items: int
    coverage_percentage: float
    items: List[DocstringInfo]
    module_docstring: Optional[str] = None


@dataclass
class DocumentationReport:
    """Overall documentation coverage report."""
    total_files: int
    total_items: int
    documented_items: int
    overall_coverage: float
    file_analyses: List[FileAnalysis]
    timestamp: str
    threshold_met: bool
    threshold: float


class DocumentationAnalyzer:
    """Analyzes Python code for documentation coverage."""

    def __init__(self, threshold: float = 80.0, include_private: bool = False):
        """
        Initialize the documentation analyzer.

        Args:
            threshold: Minimum documentation coverage percentage
            include_private: Whether to include private methods/functions
        """
        self.threshold = threshold
        self.include_private = include_private

    def analyze_file(self, file_path: Path) -> Optional[FileAnalysis]:
        """
        Analyze a single Python file for documentation coverage.

        Args:
            file_path: Path to the Python file

        Returns:
            FileAnalysis object or None if file couldn't be parsed
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            tree = ast.parse(content)
            items = []
            module_docstring = None

            # Check for module docstring
            if (tree.body and isinstance(tree.body[0], ast.Expr) and
                isinstance(tree.body[0].value, ast.Str)):
                module_docstring = tree.body[0].value.s

            # Walk the AST to find functions and classes
            for node in ast.walk(tree):
                if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                    item = self._analyze_function(node, file_path)
                    if item and (self.include_private or item.is_public):
                        items.append(item)

                elif isinstance(node, ast.ClassDef):
                    item = self._analyze_class(node, file_path)
                    if item and (self.include_private or item.is_public):
                        items.append(item)

                        # Analyze methods within the class
                        for class_node in node.body:
                            if isinstance(class_node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                                method_item = self._analyze_function(
                                    class_node, file_path, is_method=True
                                )
                                if method_item and (self.include_private or method_item.is_public):
                                    items.append(method_item)

            # Calculate coverage
            documented_count = sum(1 for item in items if item.has_docstring)
            total_count = len(items)
            coverage = (documented_count / total_count * 100) if total_count > 0 else 100

            return FileAnalysis(
                file_path=str(file_path),
                total_items=total_count,
                documented_items=documented_count,
                coverage_percentage=coverage,
                items=items,
                module_docstring=module_docstring
            )

        except (SyntaxError, UnicodeDecodeError, Exception) as e:
            print(f"Error analyzing {file_path}: {e}")
            return None

    def _analyze_function(
        self,
        node: ast.FunctionDef,
        file_path: Path,
        is_method: bool = False
    ) -> DocstringInfo:
        """Analyze a function or method node."""
        name = node.name
        is_public = not name.startswith('_') or name.startswith('__') and name.endswith('__')

        # Check for docstring
        has_docstring = False
        docstring_length = 0

        if (node.body and isinstance(node.body[0], ast.Expr) and
            isinstance(node.body[0].value, ast.Str)):
            has_docstring = True
            docstring_length = len(node.body[0].value.s)

        # Calculate complexity score based on arguments and decorators
        complexity_score = len(node.args.args) + len(node.decorator_list)

        item_type = 'method' if is_method else 'function'

        return DocstringInfo(
            name=name,
            type=item_type,
            has_docstring=has_docstring,
            docstring_length=docstring_length,
            line_number=node.lineno,
            file_path=str(file_path),
            is_public=is_public,
            complexity_score=complexity_score
        )

    def _analyze_class(self, node: ast.ClassDef, file_path: Path) -> DocstringInfo:
        """Analyze a class node."""
        name = node.name
        is_public = not name.startswith('_')

        # Check for docstring
        has_docstring = False
        docstring_length = 0

        if (node.body and isinstance(node.body[0], ast.Expr) and
            isinstance(node.body[0].value, ast.Str)):
            has_docstring = True
            docstring_length = len(node.body[0].value.s)

        return DocstringInfo(
            name=name,
            type='class',
            has_docstring=has_docstring,
            docstring_length=docstring_length,
            line_number=node.lineno,
            file_path=str(file_path),
            is_public=is_public
        )

    def analyze_directory(self, directory: Path) -> DocumentationReport:
        """
        Analyze all Python files in a directory recursively.

        Args:
            directory: Path to the directory to analyze

        Returns:
            DocumentationReport with overall results
        """
        file_analyses = []
        total_items = 0
        documented_items = 0

        # Find all Python files
        python_files = list(directory.rglob('*.py'))
        python_files = [f for f in python_files if not self._should_ignore_file(f)]

        for file_path in python_files:
            analysis = self.analyze_file(file_path)
            if analysis:
                file_analyses.append(analysis)
                total_items += analysis.total_items
                documented_items += analysis.documented_items

        # Calculate overall coverage
        overall_coverage = (documented_items / total_items * 100) if total_items > 0 else 100
        threshold_met = overall_coverage >= self.threshold

        return DocumentationReport(
            total_files=len(file_analyses),
            total_items=total_items,
            documented_items=documented_items,
            overall_coverage=overall_coverage,
            file_analyses=file_analyses,
            timestamp=datetime.now().isoformat(),
            threshold_met=threshold_met,
            threshold=self.threshold
        )

    def _should_ignore_file(self, file_path: Path) -> bool:
        """Check if a file should be ignored."""
        ignore_patterns = [
            '__pycache__',
            '.git',
            'test_',
            '_test.py',
            'conftest.py',
            'setup.py',
            'migrations',
            '.venv',
            'venv',
            'env',
            'build',
            'dist',
            '.pytest_cache',
            '.mypy_cache'
        ]

        path_str = str(file_path)
        return any(pattern in path_str for pattern in ignore_patterns)

    def generate_report(self, report: DocumentationReport) -> str:
        """Generate a human-readable documentation coverage report."""
        lines = [
            "# 📚 Documentation Coverage Report",
            "",
            f"**Analysis Date:** {report.timestamp}",
            f"**Coverage Threshold:** {report.threshold}%",
            f"**Threshold Met:** {'✅ Yes' if report.threshold_met else '❌ No'}",
            "",
            "## 📊 Summary",
            "",
            f"- **Total Files Analyzed:** {report.total_files}",
            f"- **Total Items:** {report.total_items}",
            f"- **Documented Items:** {report.documented_items}",
            f"- **Overall Coverage:** {report.overall_coverage:.1f}%",
            ""
        ]

        # Coverage status
        if report.overall_coverage >= 90:
            lines.append("🟢 **Excellent** - Documentation coverage is very high")
        elif report.overall_coverage >= 80:
            lines.append("🟡 **Good** - Documentation coverage is acceptable")
        elif report.overall_coverage >= 60:
            lines.append("🟠 **Fair** - Documentation coverage needs improvement")
        else:
            lines.append("🔴 **Poor** - Documentation coverage is critically low")

        lines.append("")

        # File-by-file breakdown
        if report.file_analyses:
            lines.extend([
                "## 📋 File Analysis",
                "",
                "| File | Items | Documented | Coverage |",
                "|------|-------|------------|----------|"
            ])

            # Sort files by coverage (lowest first to highlight problems)
            sorted_files = sorted(report.file_analyses, key=lambda x: x.coverage_percentage)

            for analysis in sorted_files:
                coverage_icon = "✅" if analysis.coverage_percentage >= report.threshold else "❌"
                rel_path = Path(analysis.file_path).relative_to(Path.cwd())
                lines.append(
                    f"| {rel_path} | {analysis.total_items} | "
                    f"{analysis.documented_items} | {analysis.coverage_percentage:.1f}% {coverage_icon} |"
                )
            lines.append("")

        # Detailed issues for files below threshold
        problem_files = [f for f in report.file_analyses if f.coverage_percentage < report.threshold]
        if problem_files:
            lines.extend([
                "## ⚠️ Files Needing Attention",
                ""
            ])

            for analysis in problem_files:
                lines.extend([
                    f"### {Path(analysis.file_path).relative_to(Path.cwd())}",
                    f"**Coverage:** {analysis.coverage_percentage:.1f}% "
                    f"({analysis.documented_items}/{analysis.total_items})",
                    ""
                ])

                # List undocumented items
                undocumented = [item for item in analysis.items if not item.has_docstring]
                if undocumented:
                    lines.append("**Undocumented items:**")
                    for item in undocumented:
                        lines.append(f"- `{item.name}` ({item.type}, line {item.line_number})")
                    lines.append("")

        # Recommendations
        lines.extend([
            "## 💡 Recommendations",
            ""
        ])

        if not report.threshold_met:
            lines.extend([
                f"- 📝 **Add docstrings** to bring coverage above {report.threshold}%",
                "- 🎯 **Focus on public APIs** - prioritize public functions and classes",
                "- 📖 **Follow docstring conventions** - use Google, NumPy, or Sphinx style",
                "- 🔄 **Set up pre-commit hooks** to check documentation on new code"
            ])
        else:
            lines.extend([
                "- ✅ **Great job!** Documentation coverage meets the threshold",
                "- 🚀 **Keep it up** - maintain good documentation practices",
                "- 📈 **Consider raising the threshold** for even better coverage"
            ])

        return "\n".join(lines)

    def save_json_report(self, report: DocumentationReport, output_file: Path):
        """Save the report as JSON for programmatic use."""
        # Convert to dict, handling dataclasses
        report_dict = {
            'total_files': report.total_files,
            'total_items': report.total_items,
            'documented_items': report.documented_items,
            'overall_coverage': report.overall_coverage,
            'timestamp': report.timestamp,
            'threshold_met': report.threshold_met,
            'threshold': report.threshold,
            'file_analyses': [
                {
                    'file_path': analysis.file_path,
                    'total_items': analysis.total_items,
                    'documented_items': analysis.documented_items,
                    'coverage_percentage': analysis.coverage_percentage,
                    'module_docstring': analysis.module_docstring,
                    'items': [asdict(item) for item in analysis.items]
                }
                for analysis in report.file_analyses
            ]
        }

        with open(output_file, 'w') as f:
            json.dump(report_dict, f, indent=2)


def main():
    """Main entry point for the documentation coverage checker."""
    parser = argparse.ArgumentParser(
        description="Check documentation coverage in Python code"
    )
    parser.add_argument(
        'directory',
        type=Path,
        help='Directory to analyze'
    )
    parser.add_argument(
        '--threshold',
        type=float,
        default=80.0,
        help='Minimum coverage percentage (default: 80.0)'
    )
    parser.add_argument(
        '--include-private',
        action='store_true',
        help='Include private methods and functions'
    )
    parser.add_argument(
        '--output',
        type=Path,
        help='Output file for the report (markdown format)'
    )
    parser.add_argument(
        '--json-output',
        type=Path,
        help='Output file for JSON report'
    )
    parser.add_argument(
        '--fail-under',
        type=float,
        help='Fail if coverage is under this threshold (overrides --threshold)'
    )

    args = parser.parse_args()

    if not args.directory.exists() or not args.directory.is_dir():
        print(f"Error: Directory {args.directory} does not exist or is not a directory")
        sys.exit(1)

    # Use fail_under threshold if provided
    threshold = args.fail_under if args.fail_under is not None else args.threshold

    analyzer = DocumentationAnalyzer(
        threshold=threshold,
        include_private=args.include_private
    )

    # Analyze the directory
    print(f"Analyzing documentation coverage in {args.directory}...")
    report = analyzer.analyze_directory(args.directory)

    # Generate and output report
    report_text = analyzer.generate_report(report)

    if args.output:
        with open(args.output, 'w') as f:
            f.write(report_text)
        print(f"Report saved to {args.output}")
    else:
        print(report_text)

    # Save JSON report if requested
    if args.json_output:
        analyzer.save_json_report(report, args.json_output)
        print(f"JSON report saved to {args.json_output}")

    # Print summary
    print(f"\n📊 Summary: {report.documented_items}/{report.total_items} items documented "
          f"({report.overall_coverage:.1f}%)")

    # Exit with error code if threshold not met
    if not report.threshold_met:
        print(f"❌ FAILED: Documentation coverage {report.overall_coverage:.1f}% "
              f"is below threshold {threshold}%")
        sys.exit(1)
    else:
        print(f"✅ PASSED: Documentation coverage {report.overall_coverage:.1f}% "
              f"meets threshold {threshold}%")
        sys.exit(0)


if __name__ == '__main__':
    main()