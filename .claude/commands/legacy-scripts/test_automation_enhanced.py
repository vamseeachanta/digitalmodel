#!/usr/bin/env python3
"""
Enhanced Cross-Repository Test Automation Agent

This is a comprehensive test automation agent designed for cross-repository deployment.
It provides unified test discovery, execution, failure analysis, auto-fixing,
coverage tracking, and intelligent reporting capabilities.

Key Features:
- Automatic test discovery across different repository structures
- Parallel test execution with resource management
- AI-powered failure analysis and auto-fixing
- Comprehensive coverage tracking and reporting
- Cross-repository reusability with adaptive configuration
- Integration with CI/CD pipelines
- Before/after implementation tracking
- Enforces Arrange-Act-Assert pattern for all unit tests
- Generates unit tests with clear section comments

Usage:
    /test-automation-enhanced run-all --parallel --coverage --fix-auto --generate-summary
    /test-automation-enhanced run-module MODULE_NAME --verbose
    /test-automation-enhanced analyze-failures --auto-fix
    /test-automation-enhanced coverage-report --format html
    /test-automation-enhanced health-check
    /test-automation-enhanced generate-test --file FILE_PATH --pattern aaa
    /test-automation-enhanced validate-pattern --check-aaa
    /test-automation-enhanced generate-summary  # Generate module test summaries for refactoring
"""

import sys
import os
import argparse
import json
import yaml
import time
import subprocess
import shutil
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime
from dataclasses import dataclass, asdict
from concurrent.futures import ThreadPoolExecutor, as_completed
import hashlib
import sqlite3
import re
import ast
import textwrap

# Arrange-Act-Assert Pattern Enforcer
class AAAPatternEnforcer:
    """Enforces Arrange-Act-Assert pattern in test files."""
    
    def __init__(self):
        self.aaa_template = '''
def test_{test_name}():
    """Test {description}."""
    
    # Arrange: Set up test data and expectations
    {arrange_code}
    
    # Act: Execute the functionality being tested
    {act_code}
    
    # Assert: Verify the results match expectations
    {assert_code}
'''
        self.validation_patterns = {
            'arrange': r'#\s*Arrange[:\s]',
            'act': r'#\s*Act[:\s]',
            'assert': r'#\s*Assert[:\s]'
        }
    
    def validate_test_file(self, file_path: Path) -> Dict[str, Any]:
        """Validate that a test file follows AAA pattern."""
        if not file_path.exists():
            return {'valid': False, 'error': 'File not found'}
        
        content = file_path.read_text()
        test_functions = self._extract_test_functions(content)
        
        results = {
            'file': str(file_path),
            'total_tests': len(test_functions),
            'aaa_compliant': 0,
            'non_compliant': [],
            'suggestions': []
        }
        
        for test_name, test_code in test_functions.items():
            if self._is_aaa_compliant(test_code):
                results['aaa_compliant'] += 1
            else:
                results['non_compliant'].append(test_name)
                results['suggestions'].append(
                    self._generate_aaa_suggestion(test_name, test_code)
                )
        
        results['compliance_rate'] = (
            (results['aaa_compliant'] / results['total_tests'] * 100)
            if results['total_tests'] > 0 else 0
        )
        
        return results
    
    def _extract_test_functions(self, content: str) -> Dict[str, str]:
        """Extract test functions from Python file content."""
        test_functions = {}
        
        try:
            tree = ast.parse(content)
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    if node.name.startswith('test_'):
                        # Get the source code for this function
                        start_line = node.lineno - 1
                        end_line = node.end_lineno
                        lines = content.split('\n')[start_line:end_line]
                        test_functions[node.name] = '\n'.join(lines)
        except SyntaxError:
            # If parsing fails, fall back to regex
            pattern = r'def\s+(test_\w+)\s*\([^)]*\):(.*?)(?=\ndef|\nclass|\Z)'
            matches = re.findall(pattern, content, re.DOTALL)
            for name, body in matches:
                test_functions[name] = f"def {name}():{body}"
        
        return test_functions
    
    def _is_aaa_compliant(self, test_code: str) -> bool:
        """Check if test code follows AAA pattern."""
        has_arrange = bool(re.search(self.validation_patterns['arrange'], test_code))
        has_act = bool(re.search(self.validation_patterns['act'], test_code))
        has_assert = bool(re.search(self.validation_patterns['assert'], test_code))
        
        return has_arrange and has_act and has_assert
    
    def _generate_aaa_suggestion(self, test_name: str, test_code: str) -> Dict[str, str]:
        """Generate suggestion to make test AAA compliant."""
        lines = test_code.split('\n')
        
        # Try to identify sections based on common patterns
        setup_lines = []
        action_lines = []
        assertion_lines = []
        
        for line in lines:
            stripped = line.strip()
            if not stripped or stripped.startswith('#'):
                continue
            
            if 'assert' in stripped.lower() or 'expect' in stripped.lower():
                assertion_lines.append(line)
            elif any(keyword in stripped.lower() for keyword in ['create', 'mock', 'fixture', '=']):
                setup_lines.append(line)
            else:
                action_lines.append(line)
        
        return {
            'test_name': test_name,
            'suggested_structure': self.aaa_template.format(
                test_name=test_name.replace('test_', ''),
                description=test_name.replace('test_', '').replace('_', ' '),
                arrange_code='\n    '.join(setup_lines) or '# TODO: Add test setup',
                act_code='\n    '.join(action_lines) or '# TODO: Add action to test',
                assert_code='\n    '.join(assertion_lines) or '# TODO: Add assertions'
            )
        }
    
    def generate_aaa_test(self, function_name: str, function_code: str,
                         produces_output: bool = False) -> str:
        """Generate AAA-compliant test for a function."""
        # Parse function to understand parameters and return type
        params = self._extract_function_params(function_code)
        
        test_name = f"test_{function_name}"
        
        arrange_code = self._generate_arrange_section(function_name, params, produces_output)
        act_code = self._generate_act_section(function_name, params, produces_output)
        assert_code = self._generate_assert_section(function_name, produces_output)
        
        return self.aaa_template.format(
            test_name=function_name,
            description=f"{function_name} functionality",
            arrange_code=arrange_code,
            act_code=act_code,
            assert_code=assert_code
        )
    
    def _extract_function_params(self, function_code: str) -> List[str]:
        """Extract parameter names from function definition."""
        match = re.search(r'def\s+\w+\s*\(([^)]*)\)', function_code)
        if match:
            params_str = match.group(1)
            params = [p.strip().split(':')[0].split('=')[0].strip() 
                     for p in params_str.split(',') if p.strip()]
            return [p for p in params if p != 'self']
        return []
    
    def _generate_arrange_section(self, function_name: str, params: List[str],
                                 produces_output: bool) -> str:
        """Generate Arrange section for test."""
        lines = []
        
        # Generate test data for parameters
        for param in params:
            if 'file' in param.lower() or 'path' in param.lower():
                lines.append(f"{param} = Path('test_data/test_file.txt')")
                lines.append(f"{param}.write_text('test content')")
            elif 'data' in param.lower() or 'dict' in param.lower():
                lines.append(f"{param} = {{'key': 'value', 'test': True}}")
            elif 'list' in param.lower() or 'array' in param.lower():
                lines.append(f"{param} = [1, 2, 3, 4, 5]")
            elif 'num' in param.lower() or 'count' in param.lower():
                lines.append(f"{param} = 42")
            else:
                lines.append(f"{param} = 'test_{param}'")
        
        # Add expected result
        lines.append("expected_result = None  # TODO: Define expected result")
        
        if produces_output:
            lines.append("")
            lines.append("# Set up output capture if function produces files/artifacts")
            lines.append("output_dir = Path('test_output')")
            lines.append("output_dir.mkdir(exist_ok=True)")
        
        return '\n    '.join(lines) if lines else "# TODO: Set up test data"
    
    def _generate_act_section(self, function_name: str, params: List[str],
                             produces_output: bool) -> str:
        """Generate Act section for test."""
        lines = []
        
        # Generate function call
        param_list = ', '.join(params) if params else ''
        lines.append(f"result = {function_name}({param_list})")
        
        if produces_output:
            lines.append("")
            lines.append("# Check if output was produced")
            lines.append("output_files = list(output_dir.glob('*'))")
        
        return '\n    '.join(lines)
    
    def _generate_assert_section(self, function_name: str, produces_output: bool) -> str:
        """Generate Assert section for test."""
        lines = []
        
        # Basic assertions
        lines.append("assert result is not None, 'Function should return a value'")
        lines.append("assert result == expected_result, f'Expected {expected_result}, got {result}'")
        
        if produces_output:
            lines.append("")
            lines.append("# Verify output artifacts were created")
            lines.append("assert len(output_files) > 0, 'Function should produce output files'")
            lines.append("")
            lines.append("# Verify output content (example for specific file types)")
            lines.append("for output_file in output_files:")
            lines.append("    if output_file.suffix == '.json':")
            lines.append("        with open(output_file) as f:")
            lines.append("            data = json.load(f)")
            lines.append("            assert 'required_key' in data, 'Output should contain required data'")
            lines.append("    elif output_file.suffix == '.csv':")
            lines.append("        import pandas as pd")
            lines.append("        df = pd.read_csv(output_file)")
            lines.append("        assert not df.empty, 'CSV output should not be empty'")
            lines.append("")
            lines.append("# Clean up test artifacts")
            lines.append("shutil.rmtree(output_dir, ignore_errors=True)")
        
        return '\n    '.join(lines)

# Test Generator with AAA Pattern
class AAATestGenerator:
    """Generates tests following AAA pattern for functions that produce output."""
    
    def __init__(self, enforcer: AAAPatternEnforcer):
        self.enforcer = enforcer
        
    def generate_test_suite(self, source_file: Path, output_dir: Path = None) -> Dict[str, str]:
        """Generate complete test suite for a source file."""
        if not source_file.exists():
            return {'error': 'Source file not found'}
        
        content = source_file.read_text()
        functions = self._extract_functions(content)
        
        test_suite = {
            'source_file': str(source_file),
            'test_file': str(source_file).replace('.py', '_test.py'),
            'tests': []
        }
        
        # Generate imports
        imports = self._generate_imports(source_file, functions)
        
        # Generate tests for each function
        for func_name, func_code in functions.items():
            produces_output = self._check_produces_output(func_code)
            test_code = self.enforcer.generate_aaa_test(
                func_name, func_code, produces_output
            )
            test_suite['tests'].append({
                'function': func_name,
                'produces_output': produces_output,
                'test_code': test_code
            })
        
        # Combine into complete test file
        test_file_content = self._combine_test_file(imports, test_suite['tests'])
        
        # Save if output directory specified
        if output_dir:
            output_dir.mkdir(parents=True, exist_ok=True)
            test_file_path = output_dir / Path(test_suite['test_file']).name
            test_file_path.write_text(test_file_content)
            test_suite['saved_to'] = str(test_file_path)
        
        test_suite['content'] = test_file_content
        return test_suite
    
    def _extract_functions(self, content: str) -> Dict[str, str]:
        """Extract all functions from source code."""
        functions = {}
        
        try:
            tree = ast.parse(content)
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    # Skip private and test functions
                    if not node.name.startswith('_') and not node.name.startswith('test_'):
                        start_line = node.lineno - 1
                        end_line = node.end_lineno
                        lines = content.split('\n')[start_line:end_line]
                        functions[node.name] = '\n'.join(lines)
        except SyntaxError:
            pass
        
        return functions
    
    def _check_produces_output(self, func_code: str) -> bool:
        """Check if function produces files or artifacts."""
        output_indicators = [
            r'\bwrite\b', r'\bsave\b', r'\bdump\b', r'\bexport\b', r'\bcreate\b',
            r'to_csv', r'to_json', r'to_excel', r'to_file',
            r'matplotlib', r'plt\.save', r'fig\.save',
            r'open\(.*["\']w["\']', r'with\s+open'
        ]
        
        for indicator in output_indicators:
            if re.search(indicator, func_code, re.IGNORECASE):
                return True
        
        return False
    
    def _generate_imports(self, source_file: Path, functions: Dict) -> str:
        """Generate necessary imports for test file."""
        imports = [
            "import pytest",
            "import json",
            "import shutil",
            "from pathlib import Path",
            "from unittest.mock import Mock, patch, MagicMock",
            "",
            f"# Import functions to test",
            f"from {source_file.stem} import {', '.join(functions.keys())}"
        ]
        
        return '\n'.join(imports)
    
    def _combine_test_file(self, imports: str, tests: List[Dict]) -> str:
        """Combine imports and tests into complete test file."""
        sections = [
            '"""',
            'Unit tests following Arrange-Act-Assert pattern.',
            'Auto-generated with clear section comments.',
            '"""',
            '',
            imports,
            '',
            ''
        ]
        
        # Add fixture for output directory if any test produces output
        if any(test['produces_output'] for test in tests):
            sections.extend([
                '@pytest.fixture',
                'def test_output_dir():',
                '    """Fixture to provide temporary output directory."""',
                '    output_dir = Path("test_output_temp")',
                '    output_dir.mkdir(exist_ok=True)',
                '    yield output_dir',
                '    # Cleanup after test',
                '    shutil.rmtree(output_dir, ignore_errors=True)',
                '',
                ''
            ])
        
        # Add all test functions
        for test in tests:
            sections.append(test['test_code'])
            sections.append('')
        
        return '\n'.join(sections)

# Cross-repository compatibility layer
class RepositoryAdapter:
    """Adapts test automation to different repository structures."""
    
    def __init__(self):
        self.repo_root = self._find_repo_root()
        self.repo_type = self._detect_repo_type()
        self.config = self._load_config()
        
    def _find_repo_root(self) -> Path:
        """Find the repository root directory."""
        current = Path.cwd()
        while current != current.parent:
            if (current / '.git').exists():
                return current
            current = current.parent
        return Path.cwd()
    
    def _detect_repo_type(self) -> str:
        """Detect repository type based on structure."""
        indicators = {
            'python': ['setup.py', 'pyproject.toml', 'requirements.txt'],
            'node': ['package.json', 'node_modules'],
            'java': ['pom.xml', 'build.gradle'],
            'dotnet': ['*.csproj', '*.sln'],
            'ruby': ['Gemfile', 'Rakefile'],
            'go': ['go.mod', 'go.sum']
        }
        
        for lang, files in indicators.items():
            for pattern in files:
                if list(self.repo_root.glob(pattern)):
                    return lang
        return 'generic'
    
    def _load_config(self) -> Dict:
        """Load repository-specific configuration."""
        config_paths = [
            self.repo_root / '.agent-os' / 'test-config.yml',
            self.repo_root / 'test_automation_settings.yml',
            self.repo_root / '.test-automation.yml'
        ]
        
        for path in config_paths:
            if path.exists():
                with open(path) as f:
                    return yaml.safe_load(f)
        
        # Return default configuration
        return self._get_default_config()
    
    def _get_default_config(self) -> Dict:
        """Get default configuration based on repo type."""
        defaults = {
            'python': {
                'test_paths': ['tests/', 'test/'],
                'test_pattern': 'test_*.py',
                'runner': 'pytest',
                'coverage_tool': 'pytest-cov'
            },
            'node': {
                'test_paths': ['test/', 'tests/', 'spec/'],
                'test_pattern': '*.test.js',
                'runner': 'npm test',
                'coverage_tool': 'jest --coverage'
            },
            'java': {
                'test_paths': ['src/test/'],
                'test_pattern': '*Test.java',
                'runner': 'mvn test',
                'coverage_tool': 'jacoco'
            },
            'generic': {
                'test_paths': ['tests/', 'test/', 'spec/'],
                'test_pattern': '*test*',
                'runner': 'auto-detect',
                'coverage_tool': 'auto-detect'
            }
        }
        return defaults.get(self.repo_type, defaults['generic'])

@dataclass
class TestResult:
    """Represents a single test execution result."""
    module: str
    test_file: str
    test_name: Optional[str]
    status: str  # 'passed', 'failed', 'skipped', 'error'
    duration: float
    error_message: Optional[str] = None
    stdout: Optional[str] = None
    stderr: Optional[str] = None
    coverage_data: Optional[Dict] = None

@dataclass
class FailureAnalysis:
    """Represents analysis of a test failure."""
    failure_type: str
    confidence: float
    fixable: bool
    fix_suggestion: Optional[str] = None
    manual_review_reason: Optional[str] = None
    coverage_impact: Optional[float] = None
    priority_score: float = 0.0

class EnhancedTestDiscovery:
    """Enhanced test discovery with cross-repository support."""
    
    def __init__(self, adapter: RepositoryAdapter):
        self.adapter = adapter
        self.discovered_tests = {}
        self.module_map = {}
        
    def discover_all(self) -> Dict[str, List[Path]]:
        """Discover all tests in the repository."""
        test_paths = self.adapter.config.get('test_paths', ['tests/'])
        test_pattern = self.adapter.config.get('test_pattern', '*test*')
        
        for test_path in test_paths:
            path = self.adapter.repo_root / test_path
            if path.exists():
                self._scan_directory(path, test_pattern)
        
        return self.discovered_tests
    
    def _scan_directory(self, path: Path, pattern: str):
        """Recursively scan directory for tests."""
        for file_path in path.rglob(pattern):
            if file_path.is_file():
                module = self._extract_module_name(file_path)
                if module not in self.discovered_tests:
                    self.discovered_tests[module] = []
                self.discovered_tests[module].append(file_path)
    
    def _extract_module_name(self, file_path: Path) -> str:
        """Extract module name from file path."""
        relative_path = file_path.relative_to(self.adapter.repo_root)
        parts = relative_path.parts
        
        # Try to extract meaningful module name
        if 'modules' in parts:
            idx = parts.index('modules')
            if idx + 1 < len(parts):
                return parts[idx + 1]
        
        # Fall back to parent directory name
        return file_path.parent.name

class IntelligentTestRunner:
    """Intelligent test runner with parallel execution and resource management."""
    
    def __init__(self, adapter: RepositoryAdapter, max_workers: int = 4):
        self.adapter = adapter
        self.max_workers = max_workers
        self.results = []
        
    def run_all(self, tests: Dict[str, List[Path]], 
                parallel: bool = True,
                coverage: bool = False) -> List[TestResult]:
        """Run all discovered tests."""
        if parallel:
            return self._run_parallel(tests, coverage)
        else:
            return self._run_sequential(tests, coverage)
    
    def _run_parallel(self, tests: Dict[str, List[Path]], 
                     coverage: bool) -> List[TestResult]:
        """Run tests in parallel."""
        results = []
        
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            futures = []
            for module, test_files in tests.items():
                for test_file in test_files:
                    future = executor.submit(
                        self._run_single_test, 
                        module, 
                        test_file, 
                        coverage
                    )
                    futures.append(future)
            
            for future in as_completed(futures):
                try:
                    result = future.result(timeout=300)
                    results.append(result)
                except Exception as e:
                    print(f"Test execution error: {e}")
        
        return results
    
    def _run_sequential(self, tests: Dict[str, List[Path]], 
                       coverage: bool) -> List[TestResult]:
        """Run tests sequentially."""
        results = []
        
        for module, test_files in tests.items():
            for test_file in test_files:
                result = self._run_single_test(module, test_file, coverage)
                results.append(result)
        
        return results
    
    def _run_single_test(self, module: str, test_file: Path, 
                        coverage: bool) -> TestResult:
        """Run a single test file."""
        start_time = time.time()
        
        # Build test command based on repository type
        cmd = self._build_test_command(test_file, coverage)
        
        try:
            process = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=300,
                cwd=self.adapter.repo_root
            )
            
            duration = time.time() - start_time
            
            return TestResult(
                module=module,
                test_file=test_file.name,
                test_name=None,
                status='passed' if process.returncode == 0 else 'failed',
                duration=duration,
                error_message=process.stderr if process.returncode != 0 else None,
                stdout=process.stdout,
                stderr=process.stderr,
                coverage_data=self._extract_coverage_data(process.stdout) if coverage else None
            )
            
        except subprocess.TimeoutExpired:
            return TestResult(
                module=module,
                test_file=test_file.name,
                test_name=None,
                status='error',
                duration=300.0,
                error_message="Test timed out after 5 minutes"
            )
        except Exception as e:
            return TestResult(
                module=module,
                test_file=test_file.name,
                test_name=None,
                status='error',
                duration=time.time() - start_time,
                error_message=f"Execution error: {str(e)}"
            )
    
    def _build_test_command(self, test_file: Path, coverage: bool) -> List[str]:
        """Build test command based on repository configuration."""
        runner = self.adapter.config.get('runner', 'pytest')
        
        if self.adapter.repo_type == 'python':
            if coverage:
                return ['python', '-m', 'pytest', str(test_file), 
                       '--cov', '--cov-report=json', '-v']
            else:
                return ['python', '-m', 'pytest', str(test_file), '-v']
        elif self.adapter.repo_type == 'node':
            return ['npm', 'test', str(test_file)]
        else:
            # Generic fallback
            return ['python', '-m', 'pytest', str(test_file), '-v']
    
    def _extract_coverage_data(self, output: str) -> Optional[Dict]:
        """Extract coverage data from test output."""
        # Simple coverage extraction - can be enhanced
        coverage_match = re.search(r'TOTAL\s+\d+\s+\d+\s+(\d+)%', output)
        if coverage_match:
            return {'coverage_percentage': int(coverage_match.group(1))}
        return None

class AIFailureAnalyzer:
    """AI-powered failure analysis with pattern recognition and learning."""
    
    def __init__(self, adapter: RepositoryAdapter):
        self.adapter = adapter
        self.pattern_db = self._init_pattern_database()
        self.learned_patterns = self._load_learned_patterns()
        
    def _init_pattern_database(self) -> Dict:
        """Initialize pattern database for failure analysis."""
        return {
            'import_error': {
                'patterns': [
                    r"ModuleNotFoundError: No module named '(.+)'",
                    r"ImportError: cannot import name '(.+)'",
                ],
                'confidence_base': 0.9,
                'fixable': True
            },
            'file_not_found': {
                'patterns': [
                    r"FileNotFoundError: \[Errno 2\] No such file or directory: '(.+)'",
                    r"IOError: \[Errno 2\] No such file or directory: '(.+)'"
                ],
                'confidence_base': 0.85,
                'fixable': True
            },
            'assertion_error': {
                'patterns': [
                    r"AssertionError: (.+)",
                    r"assert (.+) == (.+)"
                ],
                'confidence_base': 0.7,
                'fixable': False
            },
            'config_error': {
                'patterns': [
                    r"yaml\.scanner\.ScannerError",
                    r"KeyError: '(.+)'"
                ],
                'confidence_base': 0.8,
                'fixable': True
            },
            'dependency_error': {
                'patterns': [
                    r"(.+) license not found",
                    r"Licensed software required"
                ],
                'confidence_base': 0.75,
                'fixable': True
            }
        }
    
    def _load_learned_patterns(self) -> Dict:
        """Load learned patterns from database."""
        db_path = self.adapter.repo_root / '.test_automation' / 'learned_patterns.db'
        if not db_path.exists():
            return {}
        
        patterns = {}
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()
        
        try:
            cursor.execute('''
                SELECT pattern_type, pattern, confidence, fix_applied, success_rate
                FROM learned_patterns
                WHERE success_rate > 0.7
            ''')
            
            for row in cursor.fetchall():
                pattern_type, pattern, confidence, fix, success = row
                if pattern_type not in patterns:
                    patterns[pattern_type] = []
                patterns[pattern_type].append({
                    'pattern': pattern,
                    'confidence': confidence,
                    'fix': fix,
                    'success_rate': success
                })
        except:
            pass
        finally:
            conn.close()
        
        return patterns
    
    def analyze(self, result: TestResult) -> FailureAnalysis:
        """Analyze test failure and provide recommendations."""
        if result.status == 'passed':
            return FailureAnalysis(
                failure_type='none',
                confidence=1.0,
                fixable=False
            )
        
        error_text = f"{result.error_message or ''}\n{result.stderr or ''}"
        
        # Check learned patterns first
        for pattern_type, patterns in self.learned_patterns.items():
            for pattern_data in patterns:
                if re.search(pattern_data['pattern'], error_text, re.IGNORECASE):
                    return FailureAnalysis(
                        failure_type=pattern_type,
                        confidence=pattern_data['confidence'],
                        fixable=True,
                        fix_suggestion=pattern_data['fix'],
                        priority_score=pattern_data['success_rate']
                    )
        
        # Check built-in patterns
        for failure_type, pattern_info in self.pattern_db.items():
            for pattern in pattern_info['patterns']:
                if re.search(pattern, error_text, re.IGNORECASE):
                    return FailureAnalysis(
                        failure_type=failure_type,
                        confidence=pattern_info['confidence_base'],
                        fixable=pattern_info['fixable'],
                        fix_suggestion=self._generate_fix_suggestion(failure_type, error_text),
                        priority_score=self._calculate_priority(failure_type, result)
                    )
        
        return FailureAnalysis(
            failure_type='unknown',
            confidence=0.3,
            fixable=False,
            manual_review_reason="Unrecognized failure pattern"
        )
    
    def _generate_fix_suggestion(self, failure_type: str, error_text: str) -> str:
        """Generate fix suggestion based on failure type."""
        suggestions = {
            'import_error': "Add missing module to requirements or create mock",
            'file_not_found': "Create missing file or update file path",
            'config_error': "Fix configuration syntax or add missing keys",
            'dependency_error': "Mock licensed software or install dependency"
        }
        return suggestions.get(failure_type, "Manual review required")
    
    def _calculate_priority(self, failure_type: str, result: TestResult) -> float:
        """Calculate priority score for fixing."""
        base_scores = {
            'import_error': 0.9,
            'file_not_found': 0.8,
            'config_error': 0.7,
            'dependency_error': 0.6,
            'assertion_error': 0.4,
            'unknown': 0.2
        }
        
        score = base_scores.get(failure_type, 0.1)
        
        # Adjust based on module importance
        if 'critical' in result.module.lower() or 'core' in result.module.lower():
            score *= 1.5
        
        return min(score, 1.0)

class AutoFixEngine:
    """Automatic fix application with rollback capability."""
    
    def __init__(self, adapter: RepositoryAdapter):
        self.adapter = adapter
        self.backup_dir = adapter.repo_root / '.test_automation' / 'backups'
        self.backup_dir.mkdir(parents=True, exist_ok=True)
        self.applied_fixes = []
        
    def apply_fixes(self, failures: List[Tuple[TestResult, FailureAnalysis]]) -> Dict:
        """Apply automatic fixes for fixable failures."""
        fix_results = {}
        
        for result, analysis in failures:
            if not analysis.fixable:
                continue
            
            fix_key = f"{result.module}::{result.test_file}"
            
            try:
                if analysis.failure_type == 'import_error':
                    success = self._fix_import_error(result, analysis)
                elif analysis.failure_type == 'file_not_found':
                    success = self._fix_file_not_found(result, analysis)
                elif analysis.failure_type == 'config_error':
                    success = self._fix_config_error(result, analysis)
                elif analysis.failure_type == 'dependency_error':
                    success = self._fix_dependency_error(result, analysis)
                else:
                    success = False
                
                fix_results[fix_key] = {
                    'status': 'applied' if success else 'failed',
                    'type': analysis.failure_type,
                    'confidence': analysis.confidence
                }
                
                if success:
                    self.applied_fixes.append({
                        'test': fix_key,
                        'type': analysis.failure_type,
                        'timestamp': datetime.now().isoformat()
                    })
                    
            except Exception as e:
                fix_results[fix_key] = {
                    'status': 'error',
                    'error': str(e)
                }
        
        return fix_results
    
    def _fix_import_error(self, result: TestResult, analysis: FailureAnalysis) -> bool:
        """Fix import errors by adding mocks or updating imports."""
        # Find the test file
        test_file = self._find_test_file(result)
        if not test_file:
            return False
        
        # Backup the file
        backup_path = self._backup_file(test_file)
        
        try:
            content = test_file.read_text()
            
            # Extract missing module name
            import_match = re.search(r"No module named '(.+)'", result.error_message or "")
            if not import_match:
                return False
            
            missing_module = import_match.group(1)
            
            # Generate mock code
            mock_code = f'''
# Auto-generated mock for {missing_module}
import sys
from unittest.mock import MagicMock

sys.modules['{missing_module}'] = MagicMock()
'''
            
            # Insert mock at the beginning of the file
            new_content = mock_code + "\n" + content
            test_file.write_text(new_content)
            
            return True
            
        except Exception as e:
            # Restore backup on error
            self._restore_backup(test_file, backup_path)
            raise e
    
    def _fix_file_not_found(self, result: TestResult, analysis: FailureAnalysis) -> bool:
        """Fix file not found errors by creating placeholder files."""
        error_text = result.error_message or ""
        file_match = re.search(r"No such file or directory: '(.+)'", error_text)
        
        if not file_match:
            return False
        
        missing_file = Path(file_match.group(1))
        
        # Make path relative to repo root if absolute
        if missing_file.is_absolute():
            try:
                missing_file = missing_file.relative_to(self.adapter.repo_root)
            except:
                pass
        
        full_path = self.adapter.repo_root / missing_file
        
        try:
            # Create directory structure
            full_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Create appropriate placeholder content
            if full_path.suffix in ['.yml', '.yaml']:
                content = "# Auto-generated placeholder\nplaceholder: true\n"
            elif full_path.suffix == '.json':
                content = '{"placeholder": true}'
            elif full_path.suffix == '.csv':
                content = "column1,column2\nplaceholder,data\n"
            else:
                content = "# Auto-generated placeholder file\n"
            
            full_path.write_text(content)
            return True
            
        except Exception:
            return False
    
    def _fix_config_error(self, result: TestResult, analysis: FailureAnalysis) -> bool:
        """Fix configuration errors."""
        # This would need more sophisticated logic based on specific config errors
        return False
    
    def _fix_dependency_error(self, result: TestResult, analysis: FailureAnalysis) -> bool:
        """Fix dependency errors by adding mocks for licensed software."""
        test_file = self._find_test_file(result)
        if not test_file:
            return False
        
        backup_path = self._backup_file(test_file)
        
        try:
            content = test_file.read_text()
            
            # Add comprehensive mocks for common licensed software
            mock_code = '''
# Auto-generated mocks for licensed software
try:
    import OrcaFlexAPI
except ImportError:
    from unittest.mock import MagicMock
    OrcaFlexAPI = MagicMock()
    
try:
    import ansys
except ImportError:
    from unittest.mock import MagicMock
    ansys = MagicMock()
'''
            
            # Insert after imports
            import_end = self._find_import_section_end(content)
            if import_end >= 0:
                new_content = content[:import_end] + "\n" + mock_code + "\n" + content[import_end:]
                test_file.write_text(new_content)
                return True
            
            return False
            
        except Exception as e:
            self._restore_backup(test_file, backup_path)
            raise e
    
    def _find_test_file(self, result: TestResult) -> Optional[Path]:
        """Find the actual test file path."""
        # Search for the test file in common locations
        search_paths = [
            self.adapter.repo_root / 'tests' / 'modules' / result.module,
            self.adapter.repo_root / 'tests' / result.module,
            self.adapter.repo_root / 'test' / result.module,
        ]
        
        for search_path in search_paths:
            if search_path.exists():
                for file_path in search_path.rglob(result.test_file):
                    return file_path
        
        return None
    
    def _find_import_section_end(self, content: str) -> int:
        """Find the end of import section in Python file."""
        lines = content.split('\n')
        last_import = -1
        
        for i, line in enumerate(lines):
            if line.strip().startswith(('import ', 'from ')):
                last_import = i
            elif last_import >= 0 and line.strip() and not line.strip().startswith('#'):
                # Found first non-import, non-comment line
                break
        
        if last_import >= 0:
            # Return position after last import
            return sum(len(line) + 1 for line in lines[:last_import + 1])
        
        return 0
    
    def _backup_file(self, file_path: Path) -> Path:
        """Create backup of file before modification."""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        backup_name = f"{file_path.name}.{timestamp}.backup"
        backup_path = self.backup_dir / backup_name
        shutil.copy2(file_path, backup_path)
        return backup_path
    
    def _restore_backup(self, file_path: Path, backup_path: Path):
        """Restore file from backup."""
        shutil.copy2(backup_path, file_path)

class ModuleTestSummary:
    """Generate test summaries for each module to help with module-based refactoring."""
    
    def __init__(self, adapter: RepositoryAdapter):
        self.adapter = adapter
        self.summary_dir = adapter.repo_root / 'test_summaries'
        self.summary_dir.mkdir(exist_ok=True)
    
    def generate_module_summaries(self, results: List[TestResult], 
                                 analyses: List[FailureAnalysis]) -> Dict[str, Dict]:
        """Generate comprehensive test summaries for each module."""
        
        # Group results by module
        module_data = {}
        for i, result in enumerate(results):
            if result.module not in module_data:
                module_data[result.module] = {
                    'tests': [],
                    'analyses': [],
                    'total': 0,
                    'passed': 0,
                    'failed': 0,
                    'skipped': 0,
                    'error': 0,
                    'duration': 0.0,
                    'coverage': None,
                    'failure_patterns': {},
                    'test_files': set(),
                    'refactor_recommendations': []
                }
            
            module_data[result.module]['tests'].append(result)
            module_data[result.module]['total'] += 1
            module_data[result.module]['duration'] += result.duration
            module_data[result.module]['test_files'].add(result.test_file)
            
            # Count status
            if result.status == 'passed':
                module_data[result.module]['passed'] += 1
            elif result.status == 'failed':
                module_data[result.module]['failed'] += 1
                if i < len(analyses):
                    module_data[result.module]['analyses'].append(analyses[i])
                    # Track failure patterns
                    failure_type = analyses[i].failure_type
                    if failure_type not in module_data[result.module]['failure_patterns']:
                        module_data[result.module]['failure_patterns'][failure_type] = 0
                    module_data[result.module]['failure_patterns'][failure_type] += 1
            elif result.status == 'skipped':
                module_data[result.module]['skipped'] += 1
            elif result.status == 'error':
                module_data[result.module]['error'] += 1
            
            # Aggregate coverage data
            if result.coverage_data:
                if module_data[result.module]['coverage'] is None:
                    module_data[result.module]['coverage'] = []
                module_data[result.module]['coverage'].append(result.coverage_data)
        
        # Generate summaries and recommendations for each module
        module_summaries = {}
        for module_name, data in module_data.items():
            summary = self._create_module_summary(module_name, data)
            module_summaries[module_name] = summary
            
            # Save individual module summary
            self._save_module_summary(module_name, summary)
        
        # Save overall module summary
        self._save_overall_summary(module_summaries)
        
        return module_summaries
    
    def _create_module_summary(self, module_name: str, data: Dict) -> Dict:
        """Create detailed summary for a specific module."""
        
        # Calculate metrics
        pass_rate = (data['passed'] / data['total'] * 100) if data['total'] > 0 else 0
        avg_duration = data['duration'] / data['total'] if data['total'] > 0 else 0
        
        # Calculate average coverage if available
        avg_coverage = None
        if data['coverage']:
            coverage_values = [c.get('coverage_percentage', 0) for c in data['coverage'] if c]
            if coverage_values:
                avg_coverage = sum(coverage_values) / len(coverage_values)
        
        # Generate refactoring recommendations
        recommendations = self._generate_refactor_recommendations(module_name, data, pass_rate)
        
        # Convert set to list for JSON serialization
        data['test_files'] = list(data['test_files'])
        
        summary = {
            'module_name': module_name,
            'metrics': {
                'total_tests': data['total'],
                'passed': data['passed'],
                'failed': data['failed'],
                'skipped': data['skipped'],
                'errors': data['error'],
                'pass_rate': pass_rate,
                'total_duration': data['duration'],
                'avg_duration': avg_duration,
                'coverage': avg_coverage,
                'test_files': data['test_files']
            },
            'failure_analysis': {
                'patterns': data['failure_patterns'],
                'most_common': max(data['failure_patterns'].items(), key=lambda x: x[1])[0] 
                              if data['failure_patterns'] else None,
                'fixable_count': sum(1 for a in data['analyses'] if a.fixable)
            },
            'refactor_recommendations': recommendations,
            'risk_assessment': self._assess_module_risk(data, pass_rate),
            'generated_at': datetime.now().isoformat()
        }
        
        return summary
    
    def _generate_refactor_recommendations(self, module_name: str, data: Dict, 
                                          pass_rate: float) -> List[Dict]:
        """Generate specific refactoring recommendations for the module."""
        
        recommendations = []
        
        # Check pass rate
        if pass_rate < 50:
            recommendations.append({
                'priority': 'HIGH',
                'type': 'test_coverage',
                'recommendation': f"Module '{module_name}' has critical test failures ({pass_rate:.1f}% pass rate). "
                                "Consider splitting into smaller, more testable components.",
                'impact': 'Breaking changes likely required'
            })
        elif pass_rate < 80:
            recommendations.append({
                'priority': 'MEDIUM',
                'type': 'test_improvement',
                'recommendation': f"Module '{module_name}' needs test improvements ({pass_rate:.1f}% pass rate). "
                                "Focus on fixing failing tests before refactoring.",
                'impact': 'Minor refactoring recommended'
            })
        
        # Check for import errors
        import_errors = data['failure_patterns'].get('import_error', 0)
        if import_errors > 2:
            recommendations.append({
                'priority': 'HIGH',
                'type': 'dependency_management',
                'recommendation': f"Module has {import_errors} import errors. "
                                "Consider creating a dependency injection pattern or mock factory.",
                'impact': 'Architectural change recommended'
            })
        
        # Check for slow tests
        if data['duration'] > 60:  # More than 1 minute total
            slow_tests = [t for t in data['tests'] if t.duration > 10]
            if slow_tests:
                recommendations.append({
                    'priority': 'MEDIUM',
                    'type': 'performance',
                    'recommendation': f"Module has {len(slow_tests)} slow tests (>10s each). "
                                    "Consider extracting I/O operations or using test fixtures.",
                    'impact': 'Performance optimization needed'
                })
        
        # Check for file organization
        if len(data['test_files']) > 5:
            recommendations.append({
                'priority': 'LOW',
                'type': 'organization',
                'recommendation': f"Module has {len(data['test_files'])} test files. "
                                "Consider consolidating related tests or splitting the module.",
                'impact': 'Code organization improvement'
            })
        
        # Check for assertion errors
        assertion_errors = data['failure_patterns'].get('assertion_error', 0)
        if assertion_errors > 5:
            recommendations.append({
                'priority': 'MEDIUM',
                'type': 'logic_review',
                'recommendation': f"Module has {assertion_errors} assertion failures. "
                                "Business logic may need review and refactoring.",
                'impact': 'Logic review required'
            })
        
        return recommendations
    
    def _assess_module_risk(self, data: Dict, pass_rate: float) -> Dict:
        """Assess the risk level for refactoring this module."""
        
        risk_score = 0
        risk_factors = []
        
        # Factor 1: Test pass rate
        if pass_rate < 50:
            risk_score += 40
            risk_factors.append("Very low test pass rate")
        elif pass_rate < 80:
            risk_score += 20
            risk_factors.append("Low test pass rate")
        
        # Factor 2: Number of test files (complexity indicator)
        if len(data['test_files']) > 10:
            risk_score += 20
            risk_factors.append("High complexity (many test files)")
        elif len(data['test_files']) > 5:
            risk_score += 10
            risk_factors.append("Moderate complexity")
        
        # Factor 3: Error rate
        error_rate = (data['error'] / data['total'] * 100) if data['total'] > 0 else 0
        if error_rate > 10:
            risk_score += 30
            risk_factors.append("High error rate in tests")
        elif error_rate > 5:
            risk_score += 15
            risk_factors.append("Moderate error rate")
        
        # Factor 4: Unfixable failures
        unfixable = sum(1 for a in data['analyses'] if not a.fixable)
        if unfixable > 5:
            risk_score += 20
            risk_factors.append(f"{unfixable} unfixable test failures")
        elif unfixable > 0:
            risk_score += 10
            risk_factors.append(f"{unfixable} unfixable test failures")
        
        # Determine risk level
        if risk_score >= 60:
            risk_level = "HIGH"
            recommendation = "Refactor with extreme caution. Consider incremental approach."
        elif risk_score >= 30:
            risk_level = "MEDIUM"
            recommendation = "Refactor carefully with comprehensive testing."
        else:
            risk_level = "LOW"
            recommendation = "Safe to refactor with standard precautions."
        
        return {
            'risk_level': risk_level,
            'risk_score': risk_score,
            'risk_factors': risk_factors,
            'recommendation': recommendation
        }
    
    def _save_module_summary(self, module_name: str, summary: Dict):
        """Save individual module summary to file."""
        
        # Create module-specific directory
        module_dir = self.summary_dir / 'modules' / module_name
        module_dir.mkdir(parents=True, exist_ok=True)
        
        # Save as JSON
        json_path = module_dir / 'test_summary.json'
        with open(json_path, 'w') as f:
            json.dump(summary, f, indent=2, default=str)
        
        # Save as Markdown for readability
        md_path = module_dir / 'test_summary.md'
        md_content = self._format_summary_as_markdown(summary)
        md_path.write_text(md_content)
    
    def _save_overall_summary(self, module_summaries: Dict[str, Dict]):
        """Save overall summary of all modules."""
        
        overall_path = self.summary_dir / 'overall_module_summary.json'
        with open(overall_path, 'w') as f:
            json.dump(module_summaries, f, indent=2, default=str)
        
        # Create overview markdown
        overview_md = self._create_overview_markdown(module_summaries)
        overview_path = self.summary_dir / 'module_refactor_guide.md'
        overview_path.write_text(overview_md)
    
    def _format_summary_as_markdown(self, summary: Dict) -> str:
        """Format module summary as readable Markdown."""
        
        md = f"""# Test Summary: {summary['module_name']}

Generated: {summary['generated_at']}

## Test Metrics

- **Total Tests:** {summary['metrics']['total_tests']}
- **Passed:** {summary['metrics']['passed']} ✅
- **Failed:** {summary['metrics']['failed']} ❌
- **Skipped:** {summary['metrics']['skipped']} ⏭️
- **Errors:** {summary['metrics']['errors']} 🔥
- **Pass Rate:** {summary['metrics']['pass_rate']:.1f}%
- **Total Duration:** {summary['metrics']['total_duration']:.2f}s
- **Average Duration:** {summary['metrics']['avg_duration']:.2f}s
"""
        
        if summary['metrics']['coverage'] is not None:
            md += f"- **Coverage:** {summary['metrics']['coverage']:.1f}%\n"
        
        md += f"\n### Test Files ({len(summary['metrics']['test_files'])})\n"
        for test_file in summary['metrics']['test_files']:
            md += f"- {test_file}\n"
        
        md += "\n## Failure Analysis\n\n"
        if summary['failure_analysis']['patterns']:
            md += "### Failure Patterns\n"
            for pattern, count in sorted(summary['failure_analysis']['patterns'].items(), 
                                        key=lambda x: x[1], reverse=True):
                md += f"- **{pattern}:** {count} occurrences\n"
            
            if summary['failure_analysis']['most_common']:
                md += f"\n**Most Common:** {summary['failure_analysis']['most_common']}\n"
            
            md += f"**Fixable Failures:** {summary['failure_analysis']['fixable_count']}\n"
        else:
            md += "No failures detected! 🎉\n"
        
        md += f"\n## Risk Assessment\n\n"
        risk = summary['risk_assessment']
        risk_emoji = {'HIGH': '🔴', 'MEDIUM': '🟡', 'LOW': '🟢'}.get(risk['risk_level'], '⚪')
        md += f"**Risk Level:** {risk['risk_level']} {risk_emoji}\n"
        md += f"**Risk Score:** {risk['risk_score']}/100\n"
        md += f"**Recommendation:** {risk['recommendation']}\n"
        
        if risk['risk_factors']:
            md += "\n### Risk Factors\n"
            for factor in risk['risk_factors']:
                md += f"- {factor}\n"
        
        md += "\n## Refactoring Recommendations\n\n"
        if summary['refactor_recommendations']:
            for rec in sorted(summary['refactor_recommendations'], 
                            key=lambda x: {'HIGH': 0, 'MEDIUM': 1, 'LOW': 2}.get(x['priority'], 3)):
                priority_emoji = {'HIGH': '🔴', 'MEDIUM': '🟡', 'LOW': '🔵'}.get(rec['priority'], '⚪')
                md += f"### {priority_emoji} {rec['type'].replace('_', ' ').title()}\n"
                md += f"**Priority:** {rec['priority']}\n"
                md += f"**Recommendation:** {rec['recommendation']}\n"
                md += f"**Impact:** {rec['impact']}\n\n"
        else:
            md += "No specific refactoring recommendations at this time.\n"
        
        return md
    
    def _create_overview_markdown(self, module_summaries: Dict[str, Dict]) -> str:
        """Create overview markdown for all modules."""
        
        md = """# Module Refactoring Guide

This guide provides an overview of all modules' test health and refactoring recommendations.

## Quick Overview

| Module | Pass Rate | Risk Level | Tests | Duration | Priority Actions |
|--------|-----------|------------|-------|----------|-----------------|
"""
        
        # Sort modules by risk level and pass rate
        sorted_modules = sorted(module_summaries.items(), 
                              key=lambda x: (
                                  {'HIGH': 0, 'MEDIUM': 1, 'LOW': 2}.get(x[1]['risk_assessment']['risk_level'], 3),
                                  x[1]['metrics']['pass_rate']
                              ))
        
        for module_name, summary in sorted_modules:
            risk_level = summary['risk_assessment']['risk_level']
            risk_emoji = {'HIGH': '🔴', 'MEDIUM': '🟡', 'LOW': '🟢'}.get(risk_level, '⚪')
            
            # Get highest priority recommendation
            recs = summary['refactor_recommendations']
            priority_action = "None"
            if recs:
                high_priority = [r for r in recs if r['priority'] == 'HIGH']
                if high_priority:
                    priority_action = high_priority[0]['type'].replace('_', ' ').title()
                elif recs:
                    priority_action = recs[0]['type'].replace('_', ' ').title()
            
            md += f"| {module_name} | {summary['metrics']['pass_rate']:.1f}% | "
            md += f"{risk_emoji} {risk_level} | {summary['metrics']['total_tests']} | "
            md += f"{summary['metrics']['total_duration']:.1f}s | {priority_action} |\n"
        
        md += "\n## Refactoring Priority Order\n\n"
        
        # Group by risk level
        high_risk = [m for m, s in module_summaries.items() if s['risk_assessment']['risk_level'] == 'HIGH']
        medium_risk = [m for m, s in module_summaries.items() if s['risk_assessment']['risk_level'] == 'MEDIUM']
        low_risk = [m for m, s in module_summaries.items() if s['risk_assessment']['risk_level'] == 'LOW']
        
        if high_risk:
            md += "### 🔴 High Risk Modules (Refactor with Caution)\n"
            for module in high_risk:
                md += f"1. **{module}** - {module_summaries[module]['risk_assessment']['recommendation']}\n"
            md += "\n"
        
        if medium_risk:
            md += "### 🟡 Medium Risk Modules (Standard Refactoring)\n"
            for module in medium_risk:
                md += f"1. **{module}** - {module_summaries[module]['risk_assessment']['recommendation']}\n"
            md += "\n"
        
        if low_risk:
            md += "### 🟢 Low Risk Modules (Safe to Refactor)\n"
            for module in low_risk:
                md += f"1. **{module}** - {module_summaries[module]['risk_assessment']['recommendation']}\n"
            md += "\n"
        
        md += """## How to Use This Guide

1. **Start with Low Risk Modules**: These are safe to refactor and can serve as practice
2. **Address High Priority Issues First**: Focus on the "Priority Actions" column
3. **Check Individual Summaries**: Review `test_summaries/modules/[module_name]/test_summary.md`
4. **Run Tests After Each Change**: Use `/test-automation-enhanced run-module [module_name]`
5. **Monitor Risk Factors**: Address risk factors before major refactoring

## Commands

```bash
# Run tests for specific module
/test-automation-enhanced run-module MODULE_NAME

# Generate updated summary
/test-automation-enhanced generate-summary

# Fix auto-fixable issues
/test-automation-enhanced fix --module MODULE_NAME
```
"""
        
        return md

class ComprehensiveReporter:
    """Generate comprehensive test reports with insights."""
    
    def __init__(self, adapter: RepositoryAdapter):
        self.adapter = adapter
        self.report_dir = adapter.repo_root / 'test_reports'
        self.report_dir.mkdir(exist_ok=True)
        self.module_summary = ModuleTestSummary(adapter)
        
    def generate_report(self, 
                       results: List[TestResult],
                       analyses: List[FailureAnalysis],
                       fix_results: Dict,
                       format: str = 'html') -> Path:
        """Generate comprehensive test report."""
        
        report_data = self._compile_report_data(results, analyses, fix_results)
        
        if format == 'html':
            return self._generate_html_report(report_data)
        elif format == 'json':
            return self._generate_json_report(report_data)
        elif format == 'markdown':
            return self._generate_markdown_report(report_data)
        else:
            raise ValueError(f"Unsupported report format: {format}")
    
    def _compile_report_data(self, 
                            results: List[TestResult],
                            analyses: List[FailureAnalysis],
                            fix_results: Dict) -> Dict:
        """Compile report data from results."""
        
        # Calculate statistics
        total_tests = len(results)
        passed = sum(1 for r in results if r.status == 'passed')
        failed = sum(1 for r in results if r.status == 'failed')
        skipped = sum(1 for r in results if r.status == 'skipped')
        errors = sum(1 for r in results if r.status == 'error')
        
        # Calculate fix statistics
        total_fixes_attempted = len(fix_results)
        fixes_applied = sum(1 for v in fix_results.values() if v.get('status') == 'applied')
        
        # Group results by module
        module_results = {}
        for result in results:
            if result.module not in module_results:
                module_results[result.module] = {
                    'total': 0,
                    'passed': 0,
                    'failed': 0,
                    'duration': 0.0,
                    'failures': []
                }
            
            module_results[result.module]['total'] += 1
            module_results[result.module]['duration'] += result.duration
            
            if result.status == 'passed':
                module_results[result.module]['passed'] += 1
            elif result.status == 'failed':
                module_results[result.module]['failed'] += 1
                module_results[result.module]['failures'].append(result.test_file)
        
        return {
            'metadata': {
                'timestamp': datetime.now().isoformat(),
                'repository': self.adapter.repo_root.name,
                'repo_type': self.adapter.repo_type
            },
            'summary': {
                'total_tests': total_tests,
                'passed': passed,
                'failed': failed,
                'skipped': skipped,
                'errors': errors,
                'pass_rate': (passed / total_tests * 100) if total_tests > 0 else 0,
                'fixes_attempted': total_fixes_attempted,
                'fixes_applied': fixes_applied
            },
            'modules': module_results,
            'failure_types': self._categorize_failures(results, analyses),
            'recommendations': self._generate_recommendations(results, analyses)
        }
    
    def _categorize_failures(self, results: List[TestResult], 
                            analyses: List[FailureAnalysis]) -> Dict:
        """Categorize failures by type."""
        categories = {}
        
        for i, result in enumerate(results):
            if result.status == 'failed' and i < len(analyses):
                analysis = analyses[i]
                if analysis.failure_type not in categories:
                    categories[analysis.failure_type] = {
                        'count': 0,
                        'fixable': 0,
                        'examples': []
                    }
                
                categories[analysis.failure_type]['count'] += 1
                if analysis.fixable:
                    categories[analysis.failure_type]['fixable'] += 1
                
                if len(categories[analysis.failure_type]['examples']) < 3:
                    categories[analysis.failure_type]['examples'].append(
                        f"{result.module}::{result.test_file}"
                    )
        
        return categories
    
    def _generate_recommendations(self, results: List[TestResult], 
                                 analyses: List[FailureAnalysis]) -> List[str]:
        """Generate actionable recommendations."""
        recommendations = []
        
        # Check pass rate
        pass_rate = sum(1 for r in results if r.status == 'passed') / len(results) * 100
        if pass_rate < 50:
            recommendations.append(
                f"Critical: Pass rate is {pass_rate:.1f}%. Focus on fixing high-priority failures."
            )
        elif pass_rate < 80:
            recommendations.append(
                f"Pass rate is {pass_rate:.1f}%. Consider automated fixing for common patterns."
            )
        
        # Check for common failure patterns
        import_errors = sum(1 for a in analyses if a.failure_type == 'import_error')
        if import_errors > 5:
            recommendations.append(
                f"Found {import_errors} import errors. Consider adding comprehensive mocks."
            )
        
        # Check for slow tests
        slow_tests = [r for r in results if r.duration > 30]
        if slow_tests:
            recommendations.append(
                f"Found {len(slow_tests)} slow tests (>30s). Consider optimization or parallelization."
            )
        
        return recommendations
    
    def _generate_html_report(self, data: Dict) -> Path:
        """Generate HTML report."""
        html_template = '''
<!DOCTYPE html>
<html>
<head>
    <title>Test Automation Report - {repo_name}</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; 
                margin: 0; padding: 20px; background: #f5f5f5; }}
        .header {{ background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   color: white; padding: 30px; border-radius: 10px; margin-bottom: 30px; }}
        .summary {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                    gap: 20px; margin-bottom: 30px; }}
        .card {{ background: white; padding: 20px; border-radius: 10px; 
                 box-shadow: 0 2px 10px rgba(0,0,0,0.1); }}
        .metric {{ font-size: 2em; font-weight: bold; margin: 10px 0; }}
        .passed {{ color: #10b981; }}
        .failed {{ color: #ef4444; }}
        .warning {{ color: #f59e0b; }}
        .module {{ margin-bottom: 20px; }}
        .module-header {{ background: #f3f4f6; padding: 15px; border-radius: 5px;
                          margin-bottom: 10px; display: flex; justify-content: space-between; }}
        .progress-bar {{ background: #e5e7eb; height: 20px; border-radius: 10px; 
                         overflow: hidden; margin: 10px 0; }}
        .progress-fill {{ height: 100%; transition: width 0.3s; }}
        .recommendations {{ background: #fef3c7; padding: 20px; border-radius: 10px;
                           border-left: 4px solid #f59e0b; }}
        .recommendation {{ margin: 10px 0; }}
        table {{ width: 100%; border-collapse: collapse; margin: 20px 0; }}
        th, td {{ padding: 12px; text-align: left; border-bottom: 1px solid #e5e7eb; }}
        th {{ background: #f9fafb; font-weight: 600; }}
    </style>
</head>
<body>
    <div class="header">
        <h1>Test Automation Report</h1>
        <p>Repository: {repo_name} | Type: {repo_type} | Generated: {timestamp}</p>
    </div>
    
    <div class="summary">
        <div class="card">
            <h3>Total Tests</h3>
            <div class="metric">{total_tests}</div>
        </div>
        <div class="card">
            <h3>Passed</h3>
            <div class="metric passed">{passed}</div>
        </div>
        <div class="card">
            <h3>Failed</h3>
            <div class="metric failed">{failed}</div>
        </div>
        <div class="card">
            <h3>Pass Rate</h3>
            <div class="metric">{pass_rate:.1f}%</div>
            <div class="progress-bar">
                <div class="progress-fill passed" style="width: {pass_rate}%"></div>
            </div>
        </div>
        <div class="card">
            <h3>Auto-Fixes Applied</h3>
            <div class="metric">{fixes_applied}/{fixes_attempted}</div>
        </div>
    </div>
    
    <div class="card">
        <h2>Module Results</h2>
        {module_results}
    </div>
    
    <div class="card">
        <h2>Failure Analysis</h2>
        <table>
            <tr>
                <th>Failure Type</th>
                <th>Count</th>
                <th>Fixable</th>
                <th>Examples</th>
            </tr>
            {failure_analysis}
        </table>
    </div>
    
    <div class="recommendations">
        <h2>Recommendations</h2>
        {recommendations}
    </div>
</body>
</html>
'''
        
        # Build module results HTML
        module_html = ""
        for module, stats in data['modules'].items():
            pass_rate = (stats['passed'] / stats['total'] * 100) if stats['total'] > 0 else 0
            module_html += f'''
            <div class="module">
                <div class="module-header">
                    <div>
                        <strong>{module}</strong>
                        <span style="margin-left: 20px;">
                            Tests: {stats['total']} | 
                            Passed: <span class="passed">{stats['passed']}</span> | 
                            Failed: <span class="failed">{stats['failed']}</span>
                        </span>
                    </div>
                    <div>Duration: {stats['duration']:.2f}s</div>
                </div>
                <div class="progress-bar">
                    <div class="progress-fill passed" style="width: {pass_rate}%"></div>
                </div>
            </div>
            '''
        
        # Build failure analysis HTML
        failure_html = ""
        for failure_type, stats in data['failure_types'].items():
            examples = ", ".join(stats['examples'][:3])
            failure_html += f'''
            <tr>
                <td>{failure_type}</td>
                <td>{stats['count']}</td>
                <td>{stats['fixable']}</td>
                <td>{examples}</td>
            </tr>
            '''
        
        # Build recommendations HTML
        rec_html = ""
        for rec in data['recommendations']:
            rec_html += f'<div class="recommendation">• {rec}</div>'
        
        # Fill template
        html_content = html_template.format(
            repo_name=data['metadata']['repository'],
            repo_type=data['metadata']['repo_type'],
            timestamp=data['metadata']['timestamp'],
            total_tests=data['summary']['total_tests'],
            passed=data['summary']['passed'],
            failed=data['summary']['failed'],
            pass_rate=data['summary']['pass_rate'],
            fixes_attempted=data['summary']['fixes_attempted'],
            fixes_applied=data['summary']['fixes_applied'],
            module_results=module_html,
            failure_analysis=failure_html,
            recommendations=rec_html
        )
        
        # Save report
        report_path = self.report_dir / f"report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.html"
        report_path.write_text(html_content)
        
        return report_path
    
    def _generate_json_report(self, data: Dict) -> Path:
        """Generate JSON report."""
        report_path = self.report_dir / f"report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        with open(report_path, 'w') as f:
            json.dump(data, f, indent=2, default=str)
        
        return report_path
    
    def _generate_markdown_report(self, data: Dict) -> Path:
        """Generate Markdown report."""
        md_content = f'''# Test Automation Report

**Repository:** {data['metadata']['repository']}  
**Type:** {data['metadata']['repo_type']}  
**Generated:** {data['metadata']['timestamp']}

## Summary

- **Total Tests:** {data['summary']['total_tests']}
- **Passed:** {data['summary']['passed']} ✅
- **Failed:** {data['summary']['failed']} ❌
- **Pass Rate:** {data['summary']['pass_rate']:.1f}%
- **Auto-Fixes:** {data['summary']['fixes_applied']}/{data['summary']['fixes_attempted']} applied

## Module Results

| Module | Total | Passed | Failed | Duration |
|--------|-------|--------|--------|----------|
'''
        
        for module, stats in data['modules'].items():
            md_content += f"| {module} | {stats['total']} | {stats['passed']} | {stats['failed']} | {stats['duration']:.2f}s |\n"
        
        md_content += "\n## Failure Analysis\n\n"
        md_content += "| Failure Type | Count | Fixable |\n"
        md_content += "|--------------|-------|--------|\n"
        
        for failure_type, stats in data['failure_types'].items():
            md_content += f"| {failure_type} | {stats['count']} | {stats['fixable']} |\n"
        
        md_content += "\n## Recommendations\n\n"
        for rec in data['recommendations']:
            md_content += f"- {rec}\n"
        
        report_path = self.report_dir / f"report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md"
        report_path.write_text(md_content)
        
        return report_path

class TestAutomationEnhanced:
    """Main enhanced test automation orchestrator."""
    
    def __init__(self):
        self.adapter = RepositoryAdapter()
        self.discovery = EnhancedTestDiscovery(self.adapter)
        self.runner = IntelligentTestRunner(self.adapter)
        self.analyzer = AIFailureAnalyzer(self.adapter)
        self.fixer = AutoFixEngine(self.adapter)
        self.reporter = ComprehensiveReporter(self.adapter)
        self.aaa_enforcer = AAAPatternEnforcer()
        self.test_generator = AAATestGenerator(self.aaa_enforcer)
        
    def run_all(self, parallel: bool = True, coverage: bool = False,
                auto_fix: bool = False, report_format: str = 'html',
                generate_module_summary: bool = True) -> Dict:
        """Run complete test automation workflow."""
        
        print(f"🔍 Discovering tests in {self.adapter.repo_root.name}...")
        tests = self.discovery.discover_all()
        print(f"   Found {sum(len(v) for v in tests.values())} tests across {len(tests)} modules")
        
        print("\n🚀 Running tests...")
        results = self.runner.run_all(tests, parallel=parallel, coverage=coverage)
        
        print("\n🔬 Analyzing failures...")
        analyses = []
        for result in results:
            if result.status == 'failed':
                analysis = self.analyzer.analyze(result)
                analyses.append(analysis)
        
        fix_results = {}
        if auto_fix and analyses:
            print("\n🔧 Applying automatic fixes...")
            failures_to_fix = [
                (r, a) for r, a in zip(
                    [r for r in results if r.status == 'failed'],
                    analyses
                ) if a.fixable
            ]
            fix_results = self.fixer.apply_fixes(failures_to_fix)
            print(f"   Applied {sum(1 for v in fix_results.values() if v.get('status') == 'applied')} fixes")
        
        # Generate module summaries for refactoring
        module_summaries = {}
        if generate_module_summary:
            print("\n📋 Generating module test summaries...")
            module_summary_gen = ModuleTestSummary(self.adapter)
            module_summaries = module_summary_gen.generate_module_summaries(results, analyses)
            print(f"   Generated summaries for {len(module_summaries)} modules")
            print(f"   Module refactor guide saved to: test_summaries/module_refactor_guide.md")
        
        print("\n📊 Generating report...")
        report_path = self.reporter.generate_report(
            results, analyses, fix_results, format=report_format
        )
        print(f"   Report saved to: {report_path}")
        
        # Return summary
        total = len(results)
        passed = sum(1 for r in results if r.status == 'passed')
        
        return {
            'success': passed == total,
            'summary': {
                'total': total,
                'passed': passed,
                'failed': total - passed,
                'pass_rate': (passed / total * 100) if total > 0 else 0
            },
            'report': str(report_path),
            'module_summaries': module_summaries if generate_module_summary else None
        }
    
    def health_check(self) -> Dict:
        """Perform health check of test infrastructure."""
        
        checks = {
            'repository_detected': self.adapter.repo_type != 'generic',
            'config_loaded': bool(self.adapter.config),
            'test_paths_exist': False,
            'runner_available': False,
            'coverage_available': False
        }
        
        # Check test paths
        for test_path in self.adapter.config.get('test_paths', []):
            if (self.adapter.repo_root / test_path).exists():
                checks['test_paths_exist'] = True
                break
        
        # Check runner
        runner = self.adapter.config.get('runner', '')
        if runner:
            try:
                subprocess.run(
                    ['which', runner.split()[0]], 
                    capture_output=True, 
                    check=True
                )
                checks['runner_available'] = True
            except:
                pass
        
        # Check coverage tool
        if self.adapter.repo_type == 'python':
            try:
                import pytest_cov
                checks['coverage_available'] = True
            except:
                pass
        
        return checks
    
    def validate_aaa_pattern(self, test_path: str = None) -> Dict:
        """Validate tests follow AAA pattern."""
        
        if test_path:
            # Validate specific file
            path = Path(test_path)
            if path.is_file():
                results = self.aaa_enforcer.validate_test_file(path)
                return {'files': [results]}
            elif path.is_dir():
                # Validate all test files in directory
                test_files = list(path.rglob('test_*.py'))
            else:
                return {'error': f'Path not found: {test_path}'}
        else:
            # Validate all test files in repository
            test_files = []
            for test_path in self.adapter.config.get('test_paths', ['tests/']):
                path = self.adapter.repo_root / test_path
                if path.exists():
                    test_files.extend(path.rglob('test_*.py'))
        
        results = {'files': []}
        total_compliant = 0
        total_tests = 0
        
        for file_path in test_files:
            validation = self.aaa_enforcer.validate_test_file(file_path)
            results['files'].append(validation)
            total_compliant += validation.get('aaa_compliant', 0)
            total_tests += validation.get('total_tests', 0)
        
        results['summary'] = {
            'total_files': len(test_files),
            'total_tests': total_tests,
            'aaa_compliant': total_compliant,
            'compliance_rate': (total_compliant / total_tests * 100) if total_tests > 0 else 0
        }
        
        return results
    
    def generate_aaa_tests(self, source_file: str, output_dir: str = None) -> Dict:
        """Generate AAA-compliant tests for a source file."""
        
        source_path = Path(source_file)
        if not source_path.exists():
            return {'error': f'Source file not found: {source_file}'}
        
        output_path = Path(output_dir) if output_dir else self.adapter.repo_root / 'tests' / 'generated'
        
        result = self.test_generator.generate_test_suite(source_path, output_path)
        
        if 'error' not in result:
            print(f"✅ Generated {len(result['tests'])} AAA-compliant tests")
            if 'saved_to' in result:
                print(f"   Saved to: {result['saved_to']}")
            
            # Show preview of generated tests
            print("\n📝 Test Preview:")
            for test_info in result['tests'][:2]:  # Show first 2 tests
                print(f"\n   Function: {test_info['function']}")
                print(f"   Produces Output: {test_info['produces_output']}")
                if test_info['produces_output']:
                    print("   ⚠️  Test includes output artifact verification")
        
        return result
    
    def generate_module_summary(self) -> Dict:
        """Generate module test summaries without running tests (uses existing results)."""
        
        print("\n📋 Generating Module Test Summaries...")
        
        # Try to find recent test results
        test_results_path = self.adapter.repo_root / 'test_reports'
        if not test_results_path.exists():
            return {'error': 'No test reports found. Run tests first with /test-automation-enhanced run-all'}
        
        # Find most recent JSON report
        json_reports = sorted(test_results_path.glob('report_*.json'), reverse=True)
        if not json_reports:
            return {'error': 'No test report data found. Run tests first to generate reports.'}
        
        latest_report = json_reports[0]
        print(f"   Using test data from: {latest_report.name}")
        
        # For this simplified version, we'll just indicate where summaries would be saved
        # In a full implementation, we'd parse the JSON and regenerate summaries
        
        module_summary_gen = ModuleTestSummary(self.adapter)
        
        # Since we don't have the raw TestResult objects, we'll need to run tests
        # or save them during test execution for later use
        print("   Note: Full summary generation requires running tests with --generate-summary flag")
        
        return {
            'status': 'info',
            'message': 'To generate module summaries, run: /test-automation-enhanced run-all --generate-summary',
            'summary_location': 'test_summaries/module_refactor_guide.md'
        }
    
    def fix_test_patterns(self, test_path: str = None, dry_run: bool = False) -> Dict:
        """Fix tests to comply with AAA pattern."""
        
        # First validate to find non-compliant tests
        validation_results = self.validate_aaa_pattern(test_path)
        
        if 'error' in validation_results:
            return validation_results
        
        fixed_files = []
        
        for file_result in validation_results['files']:
            if file_result.get('non_compliant'):
                file_path = Path(file_result['file'])
                
                if dry_run:
                    print(f"Would fix {len(file_result['non_compliant'])} tests in {file_path}")
                    for suggestion in file_result['suggestions']:
                        print(f"  - {suggestion['test_name']}")
                else:
                    # Backup original file
                    backup_path = file_path.with_suffix('.py.backup')
                    shutil.copy2(file_path, backup_path)
                    
                    # Read current content
                    content = file_path.read_text()
                    
                    # Apply suggestions
                    for suggestion in file_result['suggestions']:
                        # Replace non-compliant test with suggested structure
                        test_name = suggestion['test_name']
                        suggested_code = suggestion['suggested_structure']
                        
                        # Find and replace the test function
                        pattern = rf'def {test_name}\s*\([^)]*\):.*?(?=\ndef|\nclass|\Z)'
                        content = re.sub(pattern, suggested_code, content, flags=re.DOTALL)
                    
                    # Write fixed content
                    file_path.write_text(content)
                    
                    fixed_files.append({
                        'file': str(file_path),
                        'backup': str(backup_path),
                        'tests_fixed': len(file_result['non_compliant'])
                    })
                    
                    print(f"✅ Fixed {len(file_result['non_compliant'])} tests in {file_path}")
        
        return {
            'fixed_files': fixed_files,
            'dry_run': dry_run,
            'total_fixed': sum(f['tests_fixed'] for f in fixed_files)
        }

def main():
    """Main entry point for the enhanced test automation agent."""
    
    parser = argparse.ArgumentParser(
        prog='test-automation-enhanced',
        description='Enhanced cross-repository test automation agent with AAA pattern enforcement'
    )
    
    parser.add_argument('command', choices=[
        'run-all', 'run-module', 'analyze', 'fix', 'report', 'health-check',
        'validate-pattern', 'generate-test', 'fix-patterns', 'generate-summary'
    ])
    parser.add_argument('--parallel', action='store_true', default=True)
    parser.add_argument('--coverage', action='store_true')
    parser.add_argument('--auto-fix', action='store_true')
    parser.add_argument('--format', choices=['html', 'json', 'markdown'], default='html')
    parser.add_argument('--module', help='Module name for run-module command')
    parser.add_argument('--file', help='File path for generate-test or validate-pattern commands')
    parser.add_argument('--output', help='Output directory for generated tests')
    parser.add_argument('--check-aaa', action='store_true', help='Check for AAA pattern compliance')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be changed without making changes')
    parser.add_argument('--pattern', choices=['aaa'], default='aaa', help='Test pattern to enforce (default: aaa)')
    parser.add_argument('--generate-summary', action='store_true', default=True, 
                       help='Generate module test summaries for refactoring guidance (default: True)')
    
    args = parser.parse_args()
    
    automation = TestAutomationEnhanced()
    
    if args.command == 'run-all':
        # When running tests, also validate AAA pattern if requested
        if args.check_aaa:
            print("\n🔍 Validating AAA Pattern Compliance...")
            validation = automation.validate_aaa_pattern()
            print(f"   Compliance Rate: {validation['summary']['compliance_rate']:.1f}%")
            if validation['summary']['compliance_rate'] < 100:
                print("   Run 'test-automation-enhanced fix-patterns' to auto-fix non-compliant tests")
        
        result = automation.run_all(
            parallel=args.parallel,
            coverage=args.coverage,
            auto_fix=args.auto_fix,
            report_format=args.format,
            generate_module_summary=args.generate_summary
        )
        
        if result['success']:
            print("\n✅ All tests passed!")
        else:
            print(f"\n❌ Tests failed. Pass rate: {result['summary']['pass_rate']:.1f}%")
            sys.exit(1)
    
    elif args.command == 'health-check':
        checks = automation.health_check()
        print("\n🏥 Test Infrastructure Health Check:")
        for check, status in checks.items():
            icon = "✅" if status else "❌"
            print(f"  {icon} {check.replace('_', ' ').title()}")
    
    elif args.command == 'validate-pattern':
        print("\n🔍 Validating Arrange-Act-Assert Pattern...")
        result = automation.validate_aaa_pattern(args.file)
        
        if 'error' in result:
            print(f"❌ Error: {result['error']}")
            sys.exit(1)
        
        summary = result.get('summary', {})
        print(f"\n📊 AAA Pattern Validation Results:")
        print(f"   Total Files: {summary.get('total_files', 0)}")
        print(f"   Total Tests: {summary.get('total_tests', 0)}")
        print(f"   AAA Compliant: {summary.get('aaa_compliant', 0)}")
        print(f"   Compliance Rate: {summary.get('compliance_rate', 0):.1f}%")
        
        # Show non-compliant tests
        for file_result in result['files']:
            if file_result.get('non_compliant'):
                print(f"\n   ⚠️  Non-compliant tests in {file_result['file']}:")
                for test_name in file_result['non_compliant'][:5]:
                    print(f"      - {test_name}")
    
    elif args.command == 'generate-test':
        if not args.file:
            print("❌ Error: --file argument required for generate-test command")
            sys.exit(1)
        
        print(f"\n🔨 Generating AAA-compliant tests for {args.file}...")
        print("   ✨ Tests for functions producing output will include artifact verification")
        
        result = automation.generate_aaa_tests(args.file, args.output)
        
        if 'error' in result:
            print(f"❌ Error: {result['error']}")
            sys.exit(1)
        
        print(f"\n✅ Successfully generated {len(result['tests'])} tests")
        if 'saved_to' in result:
            print(f"   Saved to: {result['saved_to']}")
    
    elif args.command == 'fix-patterns':
        print("\n🔧 Fixing tests to comply with AAA pattern...")
        
        result = automation.fix_test_patterns(args.file, args.dry_run)
        
        if 'error' in result:
            print(f"❌ Error: {result['error']}")
            sys.exit(1)
        
        if args.dry_run:
            print("\n🔍 Dry run mode - no changes made")
        else:
            print(f"\n✅ Fixed {result['total_fixed']} tests across {len(result['fixed_files'])} files")
            for fixed in result['fixed_files']:
                print(f"   - {fixed['file']} ({fixed['tests_fixed']} tests)")
                print(f"     Backup: {fixed['backup']}")
    
    elif args.command == 'generate-summary':
        print("\n📋 Generating Module Test Summaries for Refactoring...")
        print("   This feature helps identify which modules are safe to refactor")
        
        # Try to generate from existing data first
        result = automation.generate_module_summary()
        
        if 'error' in result:
            print(f"\n❌ {result['error']}")
            print("\n💡 Tip: Run tests first to generate module summaries:")
            print("   /test-automation-enhanced run-all --generate-summary")
            sys.exit(1)
        else:
            print(f"\n✅ {result['message']}")
            print(f"   Summary location: {result['summary_location']}")
            print("\n📊 Module summaries include:")
            print("   - Risk assessment for refactoring each module")
            print("   - Test pass rates and failure patterns")
            print("   - Specific refactoring recommendations")
            print("   - Priority ordering for safe refactoring")
    
    else:
        print(f"Command '{args.command}' not yet implemented")

if __name__ == '__main__':
    main()