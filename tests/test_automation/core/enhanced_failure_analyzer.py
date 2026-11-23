"""
Enhanced Failure Analysis Engine with Coverage-Aware Analysis and Auto-Fix Capabilities

This module extends the basic failure analyzer with advanced features:
- Coverage-aware pattern recognition  
- Confidence scoring for fix suggestions
- Fix pattern learning from successful remediation
- Coverage impact analysis for proposed fixes
- Rollback capability for failed auto-fixes
- Comprehensive audit trail
"""

import json
import os
import sqlite3
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field
import subprocess
import shutil

from .failure_analyzer import (
    FailureAnalysisEngine, FailureAnalysis, FailureType, 
    FailurePattern, FailurePatternDatabase, FailureAutoFixer
)
from test_automation.logging_config import get_logger

logger = get_logger('test_automation.enhanced_failure_analyzer')


@dataclass
class CoverageImpactAnalysis:
    """Analysis of coverage impact for a proposed fix."""
    test_file: str
    current_coverage: float
    estimated_coverage_change: float
    lines_potentially_covered: int
    confidence_in_estimate: float
    coverage_priority: str  # 'high', 'medium', 'low'


@dataclass
class FixAuditEntry:
    """Audit trail entry for applied fixes."""
    timestamp: datetime
    test_file: str
    fix_type: str
    fix_description: str
    success: bool
    coverage_before: Optional[float]
    coverage_after: Optional[float]
    rollback_available: bool
    confidence_score: float
    learning_applied: bool


@dataclass
class EnhancedFailureAnalysis(FailureAnalysis):
    """Extended failure analysis with coverage and confidence data."""
    coverage_impact: Optional[CoverageImpactAnalysis] = None
    fix_confidence_score: float = 0.0
    learning_pattern_matches: List[str] = field(default_factory=list)
    estimated_fix_time: float = 0.0  # minutes
    priority_score: float = 0.0
    similar_fixes_applied: int = 0


class CoverageAwarePatternDatabase(FailurePatternDatabase):
    """Enhanced pattern database with coverage awareness."""
    
    def __init__(self, coverage_data: Optional[Dict] = None):
        super().__init__()
        self.coverage_data = coverage_data or {}
        self.learning_db_path = Path("test_automation_learning.db")
        self._init_learning_database()
    
    def _init_learning_database(self):
        """Initialize SQLite database for pattern learning."""
        conn = sqlite3.connect(self.learning_db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS fix_patterns (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                pattern_name TEXT,
                fix_type TEXT,
                test_file_pattern TEXT,
                error_pattern TEXT,
                fix_success_rate REAL,
                applications_count INTEGER,
                last_applied DATETIME,
                coverage_improvement REAL,
                confidence_score REAL
            )
        """)
        
        cursor.execute("""
            CREATE TABLE IF NOT EXISTS fix_history (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp DATETIME,
                test_file TEXT,
                fix_type TEXT,
                success INTEGER,
                coverage_before REAL,
                coverage_after REAL,
                confidence_score REAL,
                pattern_id INTEGER,
                FOREIGN KEY (pattern_id) REFERENCES fix_patterns (id)
            )
        """)
        
        conn.commit()
        conn.close()
    
    def learn_from_successful_fix(self, fix_entry: FixAuditEntry):
        """Learn from successful fix applications to improve patterns."""
        conn = sqlite3.connect(self.learning_db_path)
        cursor = conn.cursor()
        
        # Find or create pattern
        cursor.execute("""
            SELECT id, applications_count, fix_success_rate, coverage_improvement
            FROM fix_patterns 
            WHERE pattern_name = ? AND fix_type = ?
        """, (fix_entry.fix_description, fix_entry.fix_type))
        
        result = cursor.fetchone()
        
        coverage_change = 0.0
        if fix_entry.coverage_after and fix_entry.coverage_before:
            coverage_change = fix_entry.coverage_after - fix_entry.coverage_before
        
        if result:
            # Update existing pattern
            pattern_id, count, success_rate, avg_coverage = result
            new_count = count + 1
            new_success_rate = ((success_rate * count) + (1 if fix_entry.success else 0)) / new_count
            new_avg_coverage = ((avg_coverage * count) + coverage_change) / new_count
            
            cursor.execute("""
                UPDATE fix_patterns 
                SET applications_count = ?, fix_success_rate = ?, 
                    coverage_improvement = ?, last_applied = ?, confidence_score = ?
                WHERE id = ?
            """, (new_count, new_success_rate, new_avg_coverage, 
                  fix_entry.timestamp, fix_entry.confidence_score, pattern_id))
        else:
            # Create new pattern
            cursor.execute("""
                INSERT INTO fix_patterns 
                (pattern_name, fix_type, test_file_pattern, fix_success_rate, 
                 applications_count, last_applied, coverage_improvement, confidence_score)
                VALUES (?, ?, ?, ?, 1, ?, ?, ?)
            """, (fix_entry.fix_description, fix_entry.fix_type, 
                  Path(fix_entry.test_file).name, 1 if fix_entry.success else 0,
                  fix_entry.timestamp, coverage_change, fix_entry.confidence_score))
            
            pattern_id = cursor.lastrowid
        
        # Add to fix history
        cursor.execute("""
            INSERT INTO fix_history 
            (timestamp, test_file, fix_type, success, coverage_before, 
             coverage_after, confidence_score, pattern_id)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        """, (fix_entry.timestamp, fix_entry.test_file, fix_entry.fix_type,
              1 if fix_entry.success else 0, fix_entry.coverage_before,
              fix_entry.coverage_after, fix_entry.confidence_score, pattern_id))
        
        conn.commit()
        conn.close()
        
        logger.info(f"Learned from fix: {fix_entry.fix_type} on {fix_entry.test_file}")
    
    def get_learned_patterns(self, failure_type: str) -> List[Dict]:
        """Get learned patterns for a specific failure type."""
        conn = sqlite3.connect(self.learning_db_path)
        cursor = conn.cursor()
        
        cursor.execute("""
            SELECT pattern_name, fix_success_rate, applications_count, 
                   coverage_improvement, confidence_score
            FROM fix_patterns 
            WHERE fix_type = ? AND applications_count > 0
            ORDER BY fix_success_rate DESC, applications_count DESC
        """, (failure_type,))
        
        results = cursor.fetchall()
        conn.close()
        
        return [
            {
                'name': result[0],
                'success_rate': result[1],
                'applications': result[2],
                'avg_coverage_improvement': result[3],
                'confidence': result[4]
            }
            for result in results
        ]
    
    def match_failure_with_learning(self, error_output: str, test_output: str = "") -> List[Tuple[FailurePattern, float, Dict]]:
        """Enhanced pattern matching that includes learned patterns."""
        base_matches = self.match_failure(error_output, test_output)
        enhanced_matches = []
        
        for pattern, confidence in base_matches:
            # Get learning data for this pattern type
            learned_data = self.get_learned_patterns(pattern.failure_type.value)
            
            # Adjust confidence based on learning
            if learned_data:
                best_learned = learned_data[0]
                learning_boost = best_learned['success_rate'] * 0.2  # Max 20% boost
                enhanced_confidence = min(1.0, confidence + learning_boost)
            else:
                enhanced_confidence = confidence
            
            enhanced_matches.append((pattern, enhanced_confidence, {
                'learned_patterns': learned_data,
                'confidence_boost': enhanced_confidence - confidence if learned_data else 0.0
            }))
        
        return enhanced_matches


class CoverageAwareAutoFixer(FailureAutoFixer):
    """Enhanced auto-fixer with coverage analysis and rollback capability."""
    
    def __init__(self, coverage_tracker=None):
        super().__init__()
        self.coverage_tracker = coverage_tracker
        self.audit_trail: List[FixAuditEntry] = []
        self.rollback_data: Dict[str, Dict] = {}
    
    def analyze_coverage_impact(self, test_file: str, fix_type: str) -> CoverageImpactAnalysis:
        """Analyze the potential coverage impact of applying a fix."""
        current_coverage = 0.0
        
        if self.coverage_tracker:
            # Get current module coverage
            module_path = self._get_module_from_test(test_file)
            coverage_data = self.coverage_tracker.get_coverage_trends(days=1)
            if coverage_data:
                current_coverage = coverage_data[0].get('overall_coverage', 0)
        
        # Estimate coverage improvement based on fix type
        coverage_estimates = {
            'import_error': 5.0,  # Import fixes often unlock many tests
            'mock_incomplete': 8.0,  # Mock fixes can dramatically improve coverage
            'configuration_error': 3.0,  # Config fixes have moderate impact
            'module_level_execution': 2.0,  # Execution fixes have smaller impact
            'dependency_missing': 6.0,  # Dependency fixes unlock functionality
            'licensed_software': 7.0,  # Licensed software fixes unlock modules
        }
        
        estimated_change = coverage_estimates.get(fix_type, 1.0)
        
        # Adjust estimate based on current coverage (lower coverage = higher potential)
        if current_coverage < 30:
            estimated_change *= 1.5
        elif current_coverage > 70:
            estimated_change *= 0.7
        
        priority = 'high' if estimated_change > 6 else 'medium' if estimated_change > 3 else 'low'
        
        return CoverageImpactAnalysis(
            test_file=test_file,
            current_coverage=current_coverage,
            estimated_coverage_change=estimated_change,
            lines_potentially_covered=int(estimated_change * 10),  # Rough estimate
            confidence_in_estimate=0.7,  # Default confidence
            coverage_priority=priority
        )
    
    def apply_fix_with_rollback(self, test_file: str, failure: EnhancedFailureAnalysis, 
                               fix_function_name: str) -> bool:
        """Apply fix with rollback capability and audit trail."""
        # Create rollback point
        self._create_rollback_point(test_file)
        
        # Measure coverage before fix
        coverage_before = self._measure_current_coverage(test_file)
        
        # Apply the fix
        fix_function = getattr(self, fix_function_name, None)
        if not fix_function:
            logger.error(f"Fix function {fix_function_name} not found")
            return False
        
        try:
            success = fix_function(test_file, failure)
            
            if success:
                # Measure coverage after fix
                coverage_after = self._measure_coverage_after_fix(test_file)
                
                # Create audit entry
                audit_entry = FixAuditEntry(
                    timestamp=datetime.now(timezone.utc),
                    test_file=test_file,
                    fix_type=failure.failure_type.value,
                    fix_description=failure.suggested_fix or "Auto-fix applied",
                    success=success,
                    coverage_before=coverage_before,
                    coverage_after=coverage_after,
                    rollback_available=True,
                    confidence_score=failure.fix_confidence_score,
                    learning_applied=len(failure.learning_pattern_matches) > 0
                )
                
                self.audit_trail.append(audit_entry)
                logger.info(f"Successfully applied fix to {test_file}, coverage change: "
                           f"{coverage_before:.1f}% -> {coverage_after:.1f}%")
                
                return True
            else:
                # Fix failed, rollback
                self._rollback_changes(test_file)
                return False
                
        except Exception as e:
            logger.error(f"Error applying fix to {test_file}: {e}")
            self._rollback_changes(test_file)
            return False
    
    def _create_rollback_point(self, test_file: str):
        """Create a rollback point for the test file."""
        rollback_path = test_file + ".rollback"
        try:
            shutil.copy2(test_file, rollback_path)
            self.rollback_data[test_file] = {
                'rollback_path': rollback_path,
                'original_mtime': os.path.getmtime(test_file)
            }
        except Exception as e:
            logger.warning(f"Could not create rollback point for {test_file}: {e}")
    
    def _rollback_changes(self, test_file: str) -> bool:
        """Rollback changes to a test file."""
        if test_file not in self.rollback_data:
            return False
        
        rollback_path = self.rollback_data[test_file]['rollback_path']
        
        try:
            if os.path.exists(rollback_path):
                shutil.copy2(rollback_path, test_file)
                os.remove(rollback_path)
                del self.rollback_data[test_file]
                logger.info(f"Rolled back changes to {test_file}")
                return True
        except Exception as e:
            logger.error(f"Failed to rollback {test_file}: {e}")
        
        return False
    
    def _measure_current_coverage(self, test_file: str) -> float:
        """Measure current coverage for the module containing the test."""
        if not self.coverage_tracker:
            return 0.0
        
        try:
            # Run coverage for just this test file
            result = subprocess.run([
                "python", "-m", "pytest", test_file,
                "--cov=src/digitalmodel", "--cov-report=json:temp_coverage.json", "-q"
            ], capture_output=True, text=True, timeout=60)
            
            if os.path.exists("temp_coverage.json"):
                with open("temp_coverage.json", 'r') as f:
                    coverage_data = json.load(f)
                os.remove("temp_coverage.json")
                return coverage_data.get("totals", {}).get("percent_covered", 0.0)
        except Exception as e:
            logger.debug(f"Could not measure coverage for {test_file}: {e}")
        
        return 0.0
    
    def _measure_coverage_after_fix(self, test_file: str) -> float:
        """Measure coverage after applying a fix."""
        # Same as _measure_current_coverage, but can be extended
        # to do more sophisticated analysis
        return self._measure_current_coverage(test_file)
    
    def _get_module_from_test(self, test_file: str) -> str:
        """Extract module name from test file path."""
        path = Path(test_file)
        if 'modules' in path.parts:
            modules_idx = path.parts.index('modules')
            if modules_idx + 1 < len(path.parts):
                return path.parts[modules_idx + 1]
        return 'core'
    
    def get_audit_trail(self) -> List[Dict]:
        """Get the complete audit trail of fixes."""
        return [
            {
                'timestamp': entry.timestamp.isoformat(),
                'test_file': entry.test_file,
                'fix_type': entry.fix_type,
                'fix_description': entry.fix_description,
                'success': entry.success,
                'coverage_before': entry.coverage_before,
                'coverage_after': entry.coverage_after,
                'coverage_improvement': (entry.coverage_after - entry.coverage_before) 
                                      if entry.coverage_after and entry.coverage_before else 0,
                'confidence_score': entry.confidence_score,
                'learning_applied': entry.learning_applied
            }
            for entry in self.audit_trail
        ]
    
    def cleanup_rollback_data(self):
        """Clean up rollback files after successful operations."""
        for test_file, rollback_info in list(self.rollback_data.items()):
            rollback_path = rollback_info['rollback_path']
            try:
                if os.path.exists(rollback_path):
                    os.remove(rollback_path)
                del self.rollback_data[test_file]
            except Exception as e:
                logger.warning(f"Could not clean up rollback file {rollback_path}: {e}")


class EnhancedFailureAnalysisEngine:
    """Enhanced failure analysis engine with coverage awareness and learning."""
    
    def __init__(self, coverage_tracker=None):
        self.coverage_tracker = coverage_tracker
        self.pattern_db = CoverageAwarePatternDatabase()
        self.auto_fixer = CoverageAwareAutoFixer(coverage_tracker)
        self.analysis_results: Dict[str, List[EnhancedFailureAnalysis]] = {}
    
    def analyze_test_results_enhanced(self, results: Dict[str, Any]) -> Dict[str, List[EnhancedFailureAnalysis]]:
        """Enhanced analysis with coverage awareness and confidence scoring."""
        logger.info("Starting enhanced failure analysis with coverage awareness")
        
        self.analysis_results = {}
        
        for module_name, module_result in results.items():
            if hasattr(module_result, 'status') and module_result.status in ['failed', 'mixed', 'error']:
                analyses = []
                
                for test_result in module_result.test_results:
                    if test_result.status in ['failed', 'error']:
                        analysis = self._analyze_single_failure_enhanced(test_result)
                        if analysis:
                            analyses.append(analysis)
                
                if analyses:
                    self.analysis_results[module_name] = analyses
        
        logger.info(f"Enhanced analysis completed for {len(self.analysis_results)} modules")
        return self.analysis_results
    
    def _analyze_single_failure_enhanced(self, test_result) -> Optional[EnhancedFailureAnalysis]:
        """Enhanced single failure analysis with coverage and confidence."""
        # Get enhanced pattern matches
        matches = self.pattern_db.match_failure_with_learning(
            test_result.error_output, 
            getattr(test_result, 'output', '')
        )
        
        if not matches:
            return EnhancedFailureAnalysis(
                test_file=test_result.test_file,
                module=getattr(test_result, 'module', 'unknown'),
                failure_type=FailureType.UNKNOWN,
                confidence=0.5,
                error_message=test_result.error_output[:500],
                manual_review_required=True,
                fix_confidence_score=0.3,
                priority_score=0.5
            )
        
        # Use best match
        best_pattern, confidence, learning_data = matches[0]
        
        # Calculate coverage impact
        coverage_impact = self.auto_fixer.analyze_coverage_impact(
            test_result.test_file, 
            best_pattern.failure_type.value
        )
        
        # Calculate priority score
        priority_score = self._calculate_priority_score(
            confidence, 
            coverage_impact, 
            learning_data
        )
        
        # Estimate fix time based on pattern complexity
        fix_time_estimates = {
            FailureType.MODULE_LEVEL_EXECUTION: 2.0,  # 2 minutes
            FailureType.IMPORT_ERROR: 5.0,  # 5 minutes
            FailureType.MOCK_INCOMPLETE: 8.0,  # 8 minutes
            FailureType.CONFIGURATION_ERROR: 10.0,  # 10 minutes
            FailureType.DEPENDENCY_MISSING: 15.0,  # 15 minutes
            FailureType.LICENSED_SOFTWARE: 12.0,  # 12 minutes
        }
        
        estimated_time = fix_time_estimates.get(best_pattern.failure_type, 20.0)
        
        return EnhancedFailureAnalysis(
            test_file=test_result.test_file,
            module=getattr(test_result, 'module', 'unknown'),
            failure_type=best_pattern.failure_type,
            confidence=confidence,
            error_message=test_result.error_output[:500],
            suggested_fix=best_pattern.fix_description,
            auto_fixable=best_pattern.auto_fixable,
            manual_review_required=getattr(best_pattern, 'manual_review_required', False),
            related_patterns=[match[0].name for match in matches[:3]],
            coverage_impact=coverage_impact,
            fix_confidence_score=confidence * (1 + learning_data.get('confidence_boost', 0)),
            learning_pattern_matches=[p['name'] for p in learning_data.get('learned_patterns', [])],
            estimated_fix_time=estimated_time,
            priority_score=priority_score,
            similar_fixes_applied=sum(p['applications'] for p in learning_data.get('learned_patterns', []))
        )
    
    def _calculate_priority_score(self, confidence: float, coverage_impact: CoverageImpactAnalysis, 
                                 learning_data: Dict) -> float:
        """Calculate priority score for fixing this failure."""
        # Base score from confidence
        score = confidence * 0.4
        
        # Coverage impact component (40% weight)
        coverage_weight = {
            'high': 0.4,
            'medium': 0.25,
            'low': 0.1
        }.get(coverage_impact.coverage_priority, 0.1)
        score += coverage_weight
        
        # Learning data component (20% weight)
        if learning_data.get('learned_patterns'):
            best_pattern = learning_data['learned_patterns'][0]
            learning_score = best_pattern['success_rate'] * 0.2
            score += learning_score
        
        return min(1.0, score)
    
    def apply_prioritized_fixes(self, max_fixes: int = 10, min_priority: float = 0.6) -> Dict[str, Any]:
        """Apply fixes in priority order with coverage awareness."""
        # Collect all analyses and sort by priority
        all_analyses = []
        for module_analyses in self.analysis_results.values():
            all_analyses.extend(module_analyses)
        
        # Sort by priority score descending
        prioritized_analyses = sorted(
            [a for a in all_analyses if a.auto_fixable and a.priority_score >= min_priority],
            key=lambda x: x.priority_score,
            reverse=True
        )
        
        # Apply fixes up to max_fixes
        fix_results = {
            'attempted': 0,
            'successful': 0,
            'failed': 0,
            'total_coverage_improvement': 0.0,
            'fixes_applied': []
        }
        
        for i, analysis in enumerate(prioritized_analyses[:max_fixes]):
            logger.info(f"Applying prioritized fix {i+1}/{min(max_fixes, len(prioritized_analyses))}: "
                       f"{analysis.test_file} (priority: {analysis.priority_score:.2f})")
            
            # Find fix function
            fix_function_name = self._get_fix_function_name(analysis.failure_type)
            if not fix_function_name:
                continue
            
            fix_results['attempted'] += 1
            
            success = self.auto_fixer.apply_fix_with_rollback(
                analysis.test_file, 
                analysis, 
                fix_function_name
            )
            
            if success:
                fix_results['successful'] += 1
                analysis.fix_applied = True
                
                # Learn from successful fix
                if self.auto_fixer.audit_trail:
                    latest_fix = self.auto_fixer.audit_trail[-1]
                    self.pattern_db.learn_from_successful_fix(latest_fix)
                    if latest_fix.coverage_after and latest_fix.coverage_before:
                        improvement = latest_fix.coverage_after - latest_fix.coverage_before
                        fix_results['total_coverage_improvement'] += improvement
                
                fix_results['fixes_applied'].append({
                    'test_file': analysis.test_file,
                    'priority_score': analysis.priority_score,
                    'estimated_coverage_improvement': analysis.coverage_impact.estimated_coverage_change
                })
            else:
                fix_results['failed'] += 1
        
        logger.info(f"Prioritized fix application completed: {fix_results['successful']} successful, "
                   f"total coverage improvement: {fix_results['total_coverage_improvement']:.2f}%")
        
        return fix_results
    
    def _get_fix_function_name(self, failure_type: FailureType) -> Optional[str]:
        """Get the fix function name for a failure type."""
        fix_function_mapping = {
            FailureType.IMPORT_ERROR: "fix_digitalmodel_import",
            FailureType.DEPENDENCY_MISSING: "fix_scrapy_import",
            FailureType.LICENSED_SOFTWARE: "fix_orcfxapi_import",
            FailureType.CONFIGURATION_ERROR: "fix_yaml_parsing",
            FailureType.MODULE_LEVEL_EXECUTION: "fix_module_level_execution",
        }
        return fix_function_mapping.get(failure_type)
    
    def generate_comprehensive_report(self) -> Dict[str, Any]:
        """Generate comprehensive analysis report with coverage insights."""
        if not self.analysis_results:
            return {'status': 'no_failures_analyzed'}
        
        # Basic statistics
        total_failures = sum(len(analyses) for analyses in self.analysis_results.values())
        auto_fixable = sum(1 for analyses in self.analysis_results.values() 
                          for a in analyses if a.auto_fixable)
        high_priority = sum(1 for analyses in self.analysis_results.values() 
                           for a in analyses if a.priority_score > 0.7)
        
        # Coverage impact summary
        total_potential_improvement = sum(
            a.coverage_impact.estimated_coverage_change
            for analyses in self.analysis_results.values()
            for a in analyses if a.coverage_impact
        )
        
        # Learning insights
        learned_patterns_available = any(
            a.learning_pattern_matches 
            for analyses in self.analysis_results.values()
            for a in analyses
        )
        
        return {
            'analysis_summary': {
                'total_failures': total_failures,
                'modules_affected': len(self.analysis_results),
                'auto_fixable_failures': auto_fixable,
                'high_priority_failures': high_priority,
                'fix_success_rate_potential': (auto_fixable / total_failures * 100) if total_failures > 0 else 0
            },
            'coverage_impact': {
                'total_potential_improvement': total_potential_improvement,
                'average_improvement_per_fix': total_potential_improvement / auto_fixable if auto_fixable > 0 else 0,
                'high_impact_fixes': sum(1 for analyses in self.analysis_results.values() 
                                       for a in analyses 
                                       if a.coverage_impact and a.coverage_impact.coverage_priority == 'high')
            },
            'learning_insights': {
                'learned_patterns_available': learned_patterns_available,
                'total_historical_applications': sum(
                    a.similar_fixes_applied
                    for analyses in self.analysis_results.values()
                    for a in analyses
                )
            },
            'audit_trail': self.auto_fixer.get_audit_trail(),
            'generated_at': datetime.now(timezone.utc).isoformat()
        }