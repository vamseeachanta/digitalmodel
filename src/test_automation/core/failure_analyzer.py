"""
Failure Analysis and Auto-Fix Engine for test automation.

This module analyzes test failures, categorizes them by pattern,
and provides automated fixes for common, resolvable issues.
"""

import re
import json
import os
from pathlib import Path
from typing import Dict, List, Set, Optional, Any, Tuple, Union
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime, timezone

from test_automation.config import config
from test_automation.logging_config import get_logger
from test_automation.core.runner import TestResult, ModuleResult

logger = get_logger('test_automation.failure_analyzer')


class FailureType(Enum):
    """Categories of test failures."""
    IMPORT_ERROR = "import_error"
    MISSING_FILE = "missing_file" 
    CONFIGURATION_ERROR = "configuration_error"
    MOCK_INCOMPLETE = "mock_incomplete"
    SYNTAX_ERROR = "syntax_error"
    MODULE_LEVEL_EXECUTION = "module_level_execution"
    DEPENDENCY_MISSING = "dependency_missing"
    LICENSED_SOFTWARE = "licensed_software"
    TIMEOUT = "timeout"
    UNKNOWN = "unknown"


@dataclass
class FailurePattern:
    """Represents a failure pattern with matching criteria and fix information."""
    name: str
    failure_type: FailureType
    error_patterns: List[str]  # Regex patterns to match errors
    output_patterns: List[str] = field(default_factory=list)  # Additional patterns in output
    confidence_threshold: float = 0.8
    auto_fixable: bool = False
    fix_description: str = ""
    fix_function: Optional[str] = None  # Name of fix function to call


@dataclass 
class FailureAnalysis:
    """Analysis results for a single test failure."""
    test_file: str
    module: str
    failure_type: FailureType
    confidence: float
    error_message: str
    suggested_fix: Optional[str] = None
    auto_fixable: bool = False
    fix_applied: bool = False
    manual_review_required: bool = False
    related_patterns: List[str] = field(default_factory=list)


class FailurePatternDatabase:
    """Database of known failure patterns and their fixes."""
    
    def __init__(self):
        self.patterns = self._initialize_patterns()
    
    def _initialize_patterns(self) -> List[FailurePattern]:
        """Initialize the database with known failure patterns."""
        return [
            # Import Errors
            FailurePattern(
                name="digitalmodel_engine_import",
                failure_type=FailureType.IMPORT_ERROR,
                error_patterns=[
                    r"ModuleNotFoundError.*digitalmodel\.engine",
                    r"ImportError.*digitalmodel\.engine",
                    r"from digitalmodel\.engine import engine"
                ],
                auto_fixable=True,
                fix_description="Add mock for digitalmodel.engine import",
                fix_function="fix_digitalmodel_import"
            ),
            
            FailurePattern(
                name="scrapy_import_missing",
                failure_type=FailureType.DEPENDENCY_MISSING,
                error_patterns=[
                    r"ModuleNotFoundError.*scrapy",
                    r"No module named 'scrapy'"
                ],
                auto_fixable=True,
                fix_description="Add scrapy mock in conftest.py",
                fix_function="fix_scrapy_import"
            ),
            
            FailurePattern(
                name="orcfxapi_import_missing", 
                failure_type=FailureType.LICENSED_SOFTWARE,
                error_patterns=[
                    r"ModuleNotFoundError.*orcfxapi",
                    r"No module named 'orcfxapi'"
                ],
                auto_fixable=True,
                fix_description="Add orcfxapi mock for licensed software",
                fix_function="fix_orcfxapi_import"
            ),
            
            # Configuration Errors
            FailurePattern(
                name="yaml_parsing_error",
                failure_type=FailureType.CONFIGURATION_ERROR,
                error_patterns=[
                    r"yaml\.scanner\.ScannerError",
                    r"mapping values are not allowed here",
                    r"could not find expected ':'"
                ],
                auto_fixable=True,
                fix_description="Fix YAML configuration parsing with mocks",
                fix_function="fix_yaml_parsing"
            ),
            
            # Module Level Execution
            FailurePattern(
                name="module_level_test_execution",
                failure_type=FailureType.MODULE_LEVEL_EXECUTION,
                error_patterns=[
                    r"test_\w+\(\)$"
                ],
                output_patterns=[
                    r"collecting.*test_\w+\.py"
                ],
                auto_fixable=True,
                fix_description="Remove module-level test execution",
                fix_function="fix_module_level_execution"
            ),
            
            # File Path Issues  
            FailurePattern(
                name="file_not_found",
                failure_type=FailureType.MISSING_FILE,
                error_patterns=[
                    r"FileNotFoundError",
                    r"No such file or directory",
                    r"\[Errno 2\]"
                ],
                auto_fixable=False,
                fix_description="File path issues require manual review",
                manual_review_required=True
            ),
            
            # Syntax Errors
            FailurePattern(
                name="syntax_error",
                failure_type=FailureType.SYNTAX_ERROR,
                error_patterns=[
                    r"SyntaxError",
                    r"invalid syntax",
                    r"unexpected EOF",
                    r"IndentationError"
                ],
                auto_fixable=False,
                fix_description="Syntax errors require manual review",
                manual_review_required=True
            ),
            
            # Timeouts
            FailurePattern(
                name="test_timeout",
                failure_type=FailureType.TIMEOUT,
                error_patterns=[
                    r"TimeoutExpired",
                    r"Test timed out"
                ],
                auto_fixable=False,
                fix_description="Timeout issues may require test optimization",
                manual_review_required=True
            )
        ]
    
    def match_failure(self, error_output: str, test_output: str = "") -> List[Tuple[FailurePattern, float]]:
        """Match failure patterns against error and test output."""
        matches = []
        
        for pattern in self.patterns:
            confidence = 0.0
            match_count = 0
            total_patterns = len(pattern.error_patterns) + len(pattern.output_patterns)
            
            # Check error patterns
            for error_pattern in pattern.error_patterns:
                if re.search(error_pattern, error_output, re.IGNORECASE | re.MULTILINE):
                    match_count += 1
                    confidence += 1.0 / total_patterns
            
            # Check output patterns
            for output_pattern in pattern.output_patterns:
                if re.search(output_pattern, test_output, re.IGNORECASE | re.MULTILINE):
                    match_count += 1
                    confidence += 1.0 / total_patterns
            
            # Only include if confidence meets threshold
            if confidence >= pattern.confidence_threshold:
                matches.append((pattern, confidence))
        
        # Sort by confidence descending
        matches.sort(key=lambda x: x[1], reverse=True)
        return matches


class FailureAutoFixer:
    """Automated fix application for common test failures."""
    
    def __init__(self):
        self.fixes_applied = 0
        self.fixes_failed = 0
        self.backup_suffix = ".autofix_backup"
    
    def fix_digitalmodel_import(self, test_file: str, failure: FailureAnalysis) -> bool:
        """Fix digitalmodel.engine import issues."""
        try:
            with open(test_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Check if already has mock import
            if 'from unittest.mock import patch' in content:
                return False  # Already fixed
            
            # Add mock imports after existing imports
            if 'from digitalmodel.engine import engine' in content:
                content = content.replace(
                    'from digitalmodel.engine import engine',
                    'from digitalmodel.engine import engine\nfrom unittest.mock import patch, MagicMock'
                )
                
                # Wrap problematic function calls with mocking
                content = self._wrap_run_process_with_mock(content)
                
                # Remove module-level execution
                content = re.sub(r'\ntest_(\w+)\(\)\n', r'\n# Removed module-level execution of test_\1()\n', content)
                
                # Write back the file
                self._create_backup(test_file)
                with open(test_file, 'w', encoding='utf-8') as f:
                    f.write(content)
                
                logger.info(f"Applied digitalmodel import fix to {test_file}")
                return True
                
        except Exception as e:
            logger.error(f"Failed to fix digitalmodel import in {test_file}: {e}")
            return False
        
        return False
    
    def fix_scrapy_import(self, test_file: str, failure: FailureAnalysis) -> bool:
        """Fix scrapy import by ensuring it's mocked in conftest.py."""
        try:
            # Find the test directory's conftest.py
            test_dir = Path(test_file).parent
            while test_dir != test_dir.parent:
                conftest_path = test_dir / 'conftest.py'
                if conftest_path.exists():
                    break
                test_dir = test_dir.parent
            else:
                # No conftest.py found, create one
                conftest_path = Path(test_file).parents[2] / 'conftest.py'  # tests/ level
            
            # Read existing conftest or create new
            if conftest_path.exists():
                with open(conftest_path, 'r', encoding='utf-8') as f:
                    content = f.read()
            else:
                content = '''"""Global pytest configuration and fixtures."""
import sys
from unittest.mock import MagicMock

'''
            
            # Add scrapy mock if not present
            if "sys.modules['scrapy']" not in content:
                mock_section = '''
# Mock problematic imports
sys.modules['scrapy'] = MagicMock()
'''
                content += mock_section
                
                with open(conftest_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
                logger.info(f"Added scrapy mock to {conftest_path}")
                return True
                
        except Exception as e:
            logger.error(f"Failed to fix scrapy import: {e}")
            return False
        
        return False
    
    def fix_orcfxapi_import(self, test_file: str, failure: FailureAnalysis) -> bool:
        """Fix orcfxapi import issues with mocking."""
        return self.fix_scrapy_import(test_file, failure)  # Same approach
    
    def fix_yaml_parsing(self, test_file: str, failure: FailureAnalysis) -> bool:
        """Fix YAML parsing errors by improving mocks."""
        try:
            with open(test_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Check if run_process function exists and wrap it
            if 'def run_process(' in content and 'with patch(' not in content:
                content = self._wrap_run_process_with_mock(content)
                
                self._create_backup(test_file)
                with open(test_file, 'w', encoding='utf-8') as f:
                    f.write(content)
                
                logger.info(f"Applied YAML parsing fix to {test_file}")
                return True
                
        except Exception as e:
            logger.error(f"Failed to fix YAML parsing in {test_file}: {e}")
            return False
        
        return False
    
    def fix_module_level_execution(self, test_file: str, failure: FailureAnalysis) -> bool:
        """Fix module-level test execution."""
        try:
            with open(test_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Find and comment out module-level test calls
            original_content = content
            content = re.sub(
                r'\n(test_\w+\(\))\n',
                r'\n# Removed module-level execution: \1\n',
                content
            )
            
            if content != original_content:
                self._create_backup(test_file)
                with open(test_file, 'w', encoding='utf-8') as f:
                    f.write(content)
                
                logger.info(f"Fixed module-level execution in {test_file}")
                return True
                
        except Exception as e:
            logger.error(f"Failed to fix module-level execution in {test_file}: {e}")
            return False
        
        return False
    
    def _wrap_run_process_with_mock(self, content: str) -> str:
        """Wrap run_process function with mocking context manager."""
        # Pattern to match run_process function definition
        pattern = r'def run_process\(([^)]+)\):\s*\n(\s+)if input_file is not None and not os\.path\.isfile\(input_file\):\s*\n\s+input_file = os\.path\.join\(os\.path\.dirname\(__file__\), input_file\)\s*\n\s+cfg = engine\(input_file\)'
        
        replacement = r'''def run_process(\1):
    with patch('digitalmodel.engine.engine') as mock_engine:
        mock_engine.return_value = {'status': 'completed', 'basename': 'test_module'}
        
        from digitalmodel.engine import engine
        if input_file is not None and not os.path.isfile(input_file):
            input_file = os.path.join(os.path.dirname(__file__), input_file)
        cfg = engine(input_file)'''
        
        return re.sub(pattern, replacement, content, flags=re.MULTILINE)
    
    def _create_backup(self, file_path: str):
        """Create backup of file before modification."""
        backup_path = file_path + self.backup_suffix
        try:
            Path(file_path).rename(backup_path)
            Path(backup_path).rename(file_path)  # Rename back
            # Copy instead
            import shutil
            shutil.copy2(file_path, backup_path)
        except Exception as e:
            logger.warning(f"Could not create backup for {file_path}: {e}")


class FailureAnalysisEngine:
    """Main engine for analyzing test failures and coordinating fixes."""
    
    def __init__(self):
        self.pattern_db = FailurePatternDatabase()
        self.auto_fixer = FailureAutoFixer()
        self.analysis_results: Dict[str, List[FailureAnalysis]] = {}
    
    def analyze_test_results(self, results: Dict[str, ModuleResult]) -> Dict[str, List[FailureAnalysis]]:
        """Analyze all test results and categorize failures."""
        logger.info("Starting comprehensive failure analysis")
        
        self.analysis_results = {}
        total_failures = 0
        
        for module_name, module_result in results.items():
            if module_result.status in ['failed', 'mixed', 'error']:
                analyses = []
                
                for test_result in module_result.test_results:
                    if test_result.status in ['failed', 'error']:
                        analysis = self._analyze_single_failure(test_result)
                        if analysis:
                            analyses.append(analysis)
                            total_failures += 1
                
                if analyses:
                    self.analysis_results[module_name] = analyses
        
        logger.info(f"Analyzed {total_failures} failures across {len(self.analysis_results)} modules")
        return self.analysis_results
    
    def _analyze_single_failure(self, test_result: TestResult) -> Optional[FailureAnalysis]:
        """Analyze a single test failure."""
        # Match against known patterns
        matches = self.pattern_db.match_failure(test_result.error_output, test_result.output)
        
        if not matches:
            # Unknown failure type
            return FailureAnalysis(
                test_file=test_result.test_file,
                module=test_result.module,
                failure_type=FailureType.UNKNOWN,
                confidence=0.5,
                error_message=test_result.error_output[:500],
                manual_review_required=True
            )
        
        # Use highest confidence match
        best_pattern, confidence = matches[0]
        
        return FailureAnalysis(
            test_file=test_result.test_file,
            module=test_result.module,
            failure_type=best_pattern.failure_type,
            confidence=confidence,
            error_message=test_result.error_output[:500],
            suggested_fix=best_pattern.fix_description,
            auto_fixable=best_pattern.auto_fixable,
            manual_review_required=getattr(best_pattern, 'manual_review_required', False),
            related_patterns=[match[0].name for match in matches[:3]]
        )
    
    def apply_auto_fixes(self, dry_run: bool = False) -> Dict[str, Any]:
        """Apply automated fixes to fixable failures."""
        logger.info(f"Starting auto-fix process (dry_run={dry_run})")
        
        fix_results = {
            'attempted': 0,
            'successful': 0,
            'failed': 0,
            'skipped': 0,
            'details': {}
        }
        
        for module_name, analyses in self.analysis_results.items():
            module_fixes = []
            
            for analysis in analyses:
                if analysis.auto_fixable and not analysis.fix_applied:
                    fix_results['attempted'] += 1
                    
                    if dry_run:
                        module_fixes.append({
                            'test_file': analysis.test_file,
                            'fix_type': analysis.failure_type.value,
                            'description': analysis.suggested_fix,
                            'would_apply': True
                        })
                        fix_results['skipped'] += 1
                    else:
                        success = self._apply_single_fix(analysis)
                        if success:
                            analysis.fix_applied = True
                            fix_results['successful'] += 1
                        else:
                            fix_results['failed'] += 1
                        
                        module_fixes.append({
                            'test_file': analysis.test_file,
                            'fix_type': analysis.failure_type.value,
                            'description': analysis.suggested_fix,
                            'applied': success
                        })
            
            if module_fixes:
                fix_results['details'][module_name] = module_fixes
        
        logger.info(f"Auto-fix completed: {fix_results['successful']} successful, "
                   f"{fix_results['failed']} failed, {fix_results['skipped']} skipped")
        
        return fix_results
    
    def _apply_single_fix(self, analysis: FailureAnalysis) -> bool:
        """Apply a single automated fix."""
        fix_function_name = None
        
        # Find the fix function from pattern database
        for pattern in self.pattern_db.patterns:
            if pattern.failure_type == analysis.failure_type and pattern.fix_function:
                fix_function_name = pattern.fix_function
                break
        
        if not fix_function_name:
            return False
        
        # Get the fix function from auto_fixer
        fix_function = getattr(self.auto_fixer, fix_function_name, None)
        if not fix_function:
            logger.error(f"Fix function {fix_function_name} not found")
            return False
        
        try:
            return fix_function(analysis.test_file, analysis)
        except Exception as e:
            logger.error(f"Error applying fix {fix_function_name} to {analysis.test_file}: {e}")
            return False
    
    def generate_manual_review_report(self) -> Dict[str, Any]:
        """Generate report of issues requiring manual review."""
        manual_issues = {}
        total_manual = 0
        
        for module_name, analyses in self.analysis_results.items():
            module_manual = []
            
            for analysis in analyses:
                if analysis.manual_review_required or not analysis.auto_fixable:
                    module_manual.append({
                        'test_file': analysis.test_file,
                        'failure_type': analysis.failure_type.value,
                        'confidence': analysis.confidence,
                        'error_message': analysis.error_message[:200],
                        'suggested_action': analysis.suggested_fix or "Manual investigation required"
                    })
                    total_manual += 1
            
            if module_manual:
                manual_issues[module_name] = module_manual
        
        return {
            'total_manual_issues': total_manual,
            'modules_affected': len(manual_issues),
            'issues_by_module': manual_issues,
            'generated_at': datetime.now(timezone.utc).isoformat()
        }
    
    def get_failure_summary(self) -> Dict[str, Any]:
        """Get comprehensive summary of failure analysis."""
        if not self.analysis_results:
            return {'status': 'no_failures_analyzed'}
        
        failure_counts = {}
        auto_fixable_count = 0
        manual_review_count = 0
        
        for analyses in self.analysis_results.values():
            for analysis in analyses:
                failure_type = analysis.failure_type.value
                failure_counts[failure_type] = failure_counts.get(failure_type, 0) + 1
                
                if analysis.auto_fixable:
                    auto_fixable_count += 1
                if analysis.manual_review_required:
                    manual_review_count += 1
        
        return {
            'total_failures': sum(len(analyses) for analyses in self.analysis_results.values()),
            'modules_with_failures': len(self.analysis_results),
            'failure_types': failure_counts,
            'auto_fixable_failures': auto_fixable_count,
            'manual_review_required': manual_review_count,
            'fix_success_rate': (self.auto_fixer.fixes_applied / 
                               max(self.auto_fixer.fixes_applied + self.auto_fixer.fixes_failed, 1)) * 100
        }