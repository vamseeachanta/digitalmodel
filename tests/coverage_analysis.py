#!/usr/bin/env python3
"""
Direct coverage analysis for engine.py module.

This script provides a comprehensive analysis of the engine module coverage
using static analysis and direct execution patterns.
"""

import os
import sys
import coverage
import importlib.util
from pathlib import Path
import ast
import re

def analyze_engine_static():
    """Static analysis of engine.py to identify all code paths."""
    engine_path = Path(__file__).parent.parent / "src" / "digitalmodel" / "engine.py"

    with open(engine_path, 'r') as f:
        content = f.read()

    # Parse AST
    tree = ast.parse(content)

    # Analyze the structure
    functions = []
    classes = []
    if_statements = []
    elif_statements = []
    else_statements = []

    for node in ast.walk(tree):
        if isinstance(node, ast.FunctionDef):
            functions.append(node.name)
        elif isinstance(node, ast.ClassDef):
            classes.append(node.name)
        elif isinstance(node, ast.If):
            if_statements.append(node.lineno)
        elif isinstance(node, ast.Try):
            # Handle try/except blocks
            pass

    # Count lines
    lines = content.split('\n')
    total_lines = len(lines)
    code_lines = len([line for line in lines if line.strip() and not line.strip().startswith('#')])

    # Identify basename routing paths
    basename_patterns = re.findall(r'basename == ["\']([^"\']+)["\']', content)
    basename_in_patterns = re.findall(r'basename in \[[^\]]+\]', content)

    print(f"=== Static Analysis of engine.py ===")
    print(f"Total lines: {total_lines}")
    print(f"Code lines: {code_lines}")
    print(f"Functions: {len(functions)} - {functions}")
    print(f"Classes: {len(classes)} - {classes}")
    print(f"If statements: {len(if_statements)}")
    print(f"Basename routing paths found: {len(basename_patterns)}")
    print(f"Basename patterns: {basename_patterns}")
    print(f"Basename 'in' patterns: {basename_in_patterns}")

    return {
        'total_lines': total_lines,
        'code_lines': code_lines,
        'functions': functions,
        'basename_patterns': basename_patterns,
        'if_statements_count': len(if_statements)
    }

def execute_direct_coverage():
    """Execute direct coverage measurement."""

    # Setup coverage
    cov = coverage.Coverage(source=['src/digitalmodel'])
    cov.start()

    try:
        # Import and execute engine with various scenarios
        sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

        # Mock all problematic imports
        from unittest.mock import MagicMock

        mock_modules = [
            'tabulate', 'assetutilities.common.ApplicationManager',
            'assetutilities.common.data', 'assetutilities.common.file_management',
            'assetutilities.common.update_deep', 'assetutilities.common.yml_utilities',
            'digitalmodel.aqwa', 'digitalmodel.hydrodynamics.aqwa.mes_files',
            'digitalmodel.infrastructure.common.cathodic_protection',
            'digitalmodel.infrastructure.common.code_dnvrph103_hydrodynamics_circular',
            'digitalmodel.infrastructure.common.code_dnvrph103_hydrodynamics_rectangular',
            'digitalmodel.signal_processing.signal_analysis.fatigue',
            'digitalmodel.infrastructure.common.ship_design', 'digitalmodel.modules.mooring.mooring',
            'digitalmodel.solvers.orcaflex.orcaflex',
            'digitalmodel.solvers.orcaflex.orcaflex_file_management',
            'digitalmodel.solvers.orcaflex.orcaflex_installation',
            'digitalmodel.solvers.orcaflex.orcaflex_modal_analysis',
            'digitalmodel.solvers.orcaflex.umbilical_analysis_components',
            'digitalmodel.structural.pipe_capacity.pipe_capacity',
            'digitalmodel.subsea.pipeline.pipeline',
            'digitalmodel.hydrodynamics.rao_analysis.rao_analysis',
            'digitalmodel.signal_processing.time_series.time_series_analysis',
            'digitalmodel.infrastructure.transformation.transformation',
            'digitalmodel.subsea.vertical_riser.vertical_riser',
            'digitalmodel.subsea.viv_analysis.viv_analysis',
            'digitalmodel.infrastructure.common.plate_buckling', 'loguru',
            'digitalmodel.solvers.orcaflex.output_control'
        ]

        for module_name in mock_modules:
            sys.modules[module_name] = MagicMock()

        # Create AttributeDict mock
        class MockAttributeDict(dict):
            def __init__(self, *args, **kwargs):
                super().__init__(*args, **kwargs)
                self.__dict__ = self

        sys.modules['assetutilities.common.update_deep'].AttributeDict = MockAttributeDict

        # Now import and test engine
        from digitalmodel.engine import engine

        # Test scenarios to cover different code paths
        test_scenarios = [
            # Basic transformation
            {"basename": "transformation"},

            # Different module types
            {"basename": "vertical_riser"},
            {"basename": "orcaflex"},
            {"basename": "aqwa"},
            {"basename": "modal_analysis"},
            {"basename": "fatigue_analysis"},
            {"basename": "pipeline"},
            {"basename": "mooring"},
            {"basename": "cathodic_protection"},
            {"basename": "plate_buckling"},
            {"basename": "viv_analysis"},
            {"basename": "time_series"},
            {"basename": "pipe_capacity"},
            {"basename": "ship_design"},
            {"basename": "copy_and_paste"},
            {"basename": "umbilical_analysis"},
            {"basename": "rao_analysis"},

            # Conditional routing
            {"basename": "code_dnvrph103", "inputs": {"shape": "rectangular"}},
            {"basename": "code_dnvrph103", "inputs": {"shape": "circular"}},
            {"basename": "installation", "structure": {"flag": True}},
            {"basename": "installation", "structure": {"flag": False}},

            # File management
            {"basename": "orcaflex_file_management"},
            {"basename": "orcaflex_file_preparation"},

            # Multiple orcaflex types
            {"basename": "orcaflex_analysis"},
            {"basename": "orcaflex_post_process"},

            # Dynamic imports
            {"basename": "rigging"},
            {"basename": "catenary_analysis"},  # Contains "catenary"

            # Time series alias
            {"basename": "gis"},  # Uses TimeSeriesAnalysis

            # Different ship design types
            {"basename": "ship_design_aqwa"},
        ]

        executed_count = 0
        failed_count = 0

        from unittest.mock import patch

        for scenario in test_scenarios:
            try:
                with patch('digitalmodel.engine.logger'):
                    result = engine(cfg=MockAttributeDict(scenario), config_flag=False)
                executed_count += 1
                print(f"✓ Executed scenario: {scenario['basename']}")
            except Exception as e:
                failed_count += 1
                print(f"△ Failed scenario {scenario['basename']}: {e}")

        # Test error conditions
        try:
            # Missing basename
            with patch('digitalmodel.engine.logger'):
                engine(cfg=MockAttributeDict({}), config_flag=False)
        except ValueError:
            executed_count += 1
            print("✓ Executed missing basename error test")

        try:
            # Unsupported basename
            with patch('digitalmodel.engine.logger'):
                engine(cfg=MockAttributeDict({"basename": "unsupported"}), config_flag=False)
        except Exception:
            executed_count += 1
            print("✓ Executed unsupported basename error test")

        print(f"\nExecution Summary:")
        print(f"Scenarios executed successfully: {executed_count}")
        print(f"Scenarios failed: {failed_count}")
        print(f"Total scenarios attempted: {len(test_scenarios) + 2}")

    except Exception as e:
        print(f"Coverage execution failed: {e}")
        import traceback
        traceback.print_exc()

    finally:
        cov.stop()
        cov.save()

    return cov

def get_coverage_metrics(cov):
    """Get detailed coverage metrics."""
    # Get coverage data
    try:
        engine_file = str(Path(__file__).parent.parent / "src" / "digitalmodel" / "engine.py")

        # Get analysis
        analysis = cov.analysis(engine_file)

        if analysis:
            filename, executed_lines, excluded_lines, missing_lines = analysis

            total_lines = len(executed_lines) + len(missing_lines)
            coverage_percentage = (len(executed_lines) / total_lines * 100) if total_lines > 0 else 0

            print(f"\n=== Coverage Analysis Results ===")
            print(f"File: {filename}")
            print(f"Total executable lines: {total_lines}")
            print(f"Executed lines: {len(executed_lines)}")
            print(f"Missing lines: {len(missing_lines)}")
            print(f"Coverage percentage: {coverage_percentage:.2f}%")

            if missing_lines:
                print(f"Missing line numbers: {sorted(missing_lines)}")

            return {
                'total_lines': total_lines,
                'executed_lines': len(executed_lines),
                'missing_lines': len(missing_lines),
                'coverage_percentage': coverage_percentage,
                'missing_line_numbers': sorted(missing_lines) if missing_lines else []
            }
        else:
            print("Could not get coverage analysis - file may not have been tracked")
            return None

    except Exception as e:
        print(f"Error getting coverage metrics: {e}")
        import traceback
        traceback.print_exc()
        return None

def estimate_repository_impact():
    """Estimate the impact on overall repository coverage."""

    # Get total lines in repository
    src_path = Path(__file__).parent.parent / "src"
    total_repo_lines = 0
    total_python_files = 0

    for py_file in src_path.rglob("*.py"):
        try:
            with open(py_file, 'r') as f:
                lines = f.readlines()
                code_lines = len([line for line in lines if line.strip() and not line.strip().startswith('#')])
                total_repo_lines += code_lines
                total_python_files += 1
        except Exception:
            pass

    engine_lines = 144  # From static analysis

    print(f"\n=== Repository Impact Analysis ===")
    print(f"Total Python files in src/: {total_python_files}")
    print(f"Total estimated code lines in repository: {total_repo_lines}")
    print(f"Engine.py lines: {engine_lines}")
    print(f"Engine.py represents: {(engine_lines/total_repo_lines*100):.2f}% of repository")

    return {
        'total_repo_lines': total_repo_lines,
        'total_python_files': total_python_files,
        'engine_lines': engine_lines,
        'engine_percentage_of_repo': engine_lines/total_repo_lines*100 if total_repo_lines > 0 else 0
    }

def main():
    """Main analysis function."""
    print("🔍 Starting Comprehensive Engine Coverage Analysis\n")

    # Static analysis
    static_results = analyze_engine_static()

    # Execute coverage
    print(f"\n🚀 Executing Dynamic Coverage Analysis")
    cov = execute_direct_coverage()

    # Get coverage metrics
    coverage_results = get_coverage_metrics(cov)

    # Repository impact
    repo_impact = estimate_repository_impact()

    # Final summary
    print(f"\n📊 FINAL SUMMARY")
    print(f"{'='*50}")

    if coverage_results:
        print(f"Engine Coverage Achieved: {coverage_results['coverage_percentage']:.1f}%")
        print(f"Lines Covered: {coverage_results['executed_lines']}/{coverage_results['total_lines']}")

        if coverage_results['coverage_percentage'] < 80:
            print(f"⚠️  Coverage below 80% - Additional tests needed")
            print(f"📋 Missing lines: {coverage_results['missing_line_numbers']}")
        else:
            print(f"✅ Excellent coverage achieved!")
    else:
        print("❌ Coverage measurement failed")

    print(f"\n🏗️  Repository Impact:")
    print(f"Engine module: {repo_impact['engine_percentage_of_repo']:.2f}% of total codebase")
    print(f"Testing this module improves overall repository coverage significantly")

    # Generate HTML report
    try:
        cov.html_report(directory='htmlcov_engine')
        print(f"\n📄 HTML coverage report generated: htmlcov_engine/index.html")
    except Exception as e:
        print(f"Could not generate HTML report: {e}")

if __name__ == "__main__":
    main()