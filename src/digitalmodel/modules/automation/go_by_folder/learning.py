"""
Learning module for Create Go-By Folder Tool
Captures insights and patterns from each execution to improve future runs
"""

import json
from pathlib import Path
from typing import Dict, List, Any, Optional
from datetime import datetime
import logging
from collections import defaultdict

logger = logging.getLogger(__name__)


class GoByLearningSystem:
    """Continuous learning system for go-by folder creation."""
    
    def __init__(self, learning_file: Optional[Path] = None):
        """
        Initialize learning system.
        
        Args:
            learning_file: Path to store learning data
        """
        self.learning_file = learning_file or Path.home() / '.goby_learning.json'
        self.learning_data = self.load_learning_data()
        self.current_session = {
            'start_time': datetime.now().isoformat(),
            'patterns_found': {},
            'file_types_processed': {},
            'errors_encountered': [],
            'successful_strategies': [],
            'performance_metrics': {}
        }
    
    def load_learning_data(self) -> Dict:
        """Load existing learning data."""
        if self.learning_file.exists():
            try:
                with open(self.learning_file, 'r', encoding='utf-8') as f:
                    return json.load(f)
            except Exception as e:
                logger.warning(f"Could not load learning data: {e}")
        
        return {
            'version': '1.0',
            'created': datetime.now().isoformat(),
            'sessions': [],
            'patterns': {},
            'file_type_strategies': {},
            'common_errors': {},
            'optimization_hints': {},
            'naming_patterns': defaultdict(int)
        }
    
    def save_learning_data(self) -> None:
        """Save learning data to file."""
        try:
            self.learning_file.parent.mkdir(parents=True, exist_ok=True)
            with open(self.learning_file, 'w', encoding='utf-8') as f:
                json.dump(self.learning_data, f, indent=2, default=str)
            logger.info(f"Learning data saved to {self.learning_file}")
        except Exception as e:
            logger.error(f"Could not save learning data: {e}")
    
    def learn_from_patterns(self, patterns: Dict) -> None:
        """
        Learn from detected patterns.
        
        Args:
            patterns: Detected patterns from analyzer
        """
        self.current_session['patterns_found'] = patterns
        
        # Update global pattern knowledge
        for pattern_type, pattern_data in patterns.items():
            if pattern_type not in self.learning_data['patterns']:
                self.learning_data['patterns'][pattern_type] = {
                    'count': 0,
                    'examples': []
                }
            
            self.learning_data['patterns'][pattern_type]['count'] += 1
            
            # Store unique examples (limit to 10)
            if isinstance(pattern_data, dict) and 'examples' in pattern_data:
                examples = self.learning_data['patterns'][pattern_type]['examples']
                for example in pattern_data['examples'][:3]:
                    if example not in examples:
                        examples.append(example)
                # Keep only last 10 examples
                self.learning_data['patterns'][pattern_type]['examples'] = examples[-10:]
    
    def learn_from_file_types(self, file_types: Dict) -> None:
        """
        Learn from file type processing.
        
        Args:
            file_types: File types and their processing stats
        """
        self.current_session['file_types_processed'] = file_types
        
        # Update file type strategies
        for ext, stats in file_types.items():
            if ext not in self.learning_data['file_type_strategies']:
                self.learning_data['file_type_strategies'][ext] = {
                    'total_processed': 0,
                    'avg_size': 0,
                    'best_minimization': None,
                    'typical_count': []
                }
            
            strategy = self.learning_data['file_type_strategies'][ext]
            strategy['total_processed'] += stats.get('count', 0)
            
            # Track typical counts for this file type
            strategy['typical_count'].append(stats.get('count', 0))
            if len(strategy['typical_count']) > 20:
                strategy['typical_count'] = strategy['typical_count'][-20:]
    
    def learn_from_errors(self, error: str, context: Dict) -> None:
        """
        Learn from errors encountered.
        
        Args:
            error: Error message
            context: Context when error occurred
        """
        error_entry = {
            'error': error,
            'context': context,
            'timestamp': datetime.now().isoformat()
        }
        self.current_session['errors_encountered'].append(error_entry)
        
        # Track common errors
        error_key = self.extract_error_key(error)
        if error_key not in self.learning_data['common_errors']:
            self.learning_data['common_errors'][error_key] = {
                'count': 0,
                'solutions': [],
                'contexts': []
            }
        
        self.learning_data['common_errors'][error_key]['count'] += 1
        self.learning_data['common_errors'][error_key]['contexts'].append(context)
    
    def learn_from_success(self, strategy: str, metrics: Dict) -> None:
        """
        Learn from successful strategies.
        
        Args:
            strategy: Strategy description
            metrics: Success metrics
        """
        success_entry = {
            'strategy': strategy,
            'metrics': metrics,
            'timestamp': datetime.now().isoformat()
        }
        self.current_session['successful_strategies'].append(success_entry)
    
    def suggest_optimizations(self, source_path: Path) -> List[str]:
        """
        Suggest optimizations based on learning.
        
        Args:
            source_path: Source folder path
            
        Returns:
            List of optimization suggestions
        """
        suggestions = []
        
        # Analyze source folder characteristics
        source_size = sum(f.stat().st_size for f in source_path.rglob('*') if f.is_file())
        file_count = len(list(source_path.rglob('*')))
        
        # Size-based suggestions
        if source_size > 10 * 1024 * 1024 * 1024:  # >10GB
            suggestions.append("Consider using --parallel 8 for large folder processing")
            suggestions.append("Enable checkpoint mode for recovery: --checkpoint-interval 500")
        
        # File count suggestions
        if file_count > 10000:
            suggestions.append("Use --exclude-patterns to skip unnecessary file types")
            suggestions.append("Consider --analysis-mode for pattern detection only first")
        
        # Pattern-based suggestions from learning
        if self.learning_data['patterns']:
            common_patterns = sorted(
                self.learning_data['patterns'].items(),
                key=lambda x: x[1].get('count', 0),
                reverse=True
            )[:3]
            
            if common_patterns:
                suggestions.append(f"Common patterns detected previously: {', '.join([p[0] for p in common_patterns])}")
        
        # Error-based suggestions
        if self.learning_data['common_errors']:
            for error_key, error_data in self.learning_data['common_errors'].items():
                if error_data['count'] > 3 and error_data.get('solutions'):
                    suggestions.append(f"Known issue '{error_key}': {error_data['solutions'][0]}")
        
        return suggestions
    
    def get_naming_suggestion(self, patterns: Dict, file_types: Dict) -> str:
        """
        Suggest a brief descriptive name based on patterns.
        
        Args:
            patterns: Detected patterns
            file_types: File types found
            
        Returns:
            Suggested brief name
        """
        name_parts = []
        
        # Check for common project types in patterns
        pattern_str = str(patterns).lower()
        if 'fsts' in pattern_str or 'floating' in pattern_str:
            name_parts.append('fsts')
        if 'lngc' in pattern_str or 'lng' in pattern_str:
            name_parts.append('lngc')
        if 'mooring' in pattern_str:
            name_parts.append('mooring')
        if 'aqwa' in pattern_str:
            name_parts.append('aqwa')
        if 'orcaflex' in pattern_str:
            name_parts.append('orcaflex')
        
        # Check for configuration variations
        if 'hwl' in pattern_str and 'lwl' in pattern_str:
            name_parts.append('multiwater')
        elif 'parameter' in str(patterns) or 'sweep' in str(patterns):
            name_parts.append('sweep')
        
        # Check for multiple configs
        if len(patterns.get('file_patterns', {})) > 10:
            name_parts.append('multiconfig')
        elif len(patterns.get('file_patterns', {})) > 5:
            name_parts.append('variants')
        
        # Default if no patterns found
        if not name_parts:
            # Use file type as hint
            if '.yml' in file_types or '.yaml' in file_types:
                name_parts.append('config')
            if '.sim' in file_types:
                name_parts.append('simulation')
            name_parts.append('template')
        
        # Track this naming pattern
        suggested_name = '-'.join(name_parts[:4])  # Limit to 4 parts
        self.learning_data['naming_patterns'][suggested_name] += 1
        
        return suggested_name
    
    def complete_session(self, success: bool, stats: Dict) -> None:
        """
        Complete learning session and save insights.
        
        Args:
            success: Whether session was successful
            stats: Final statistics
        """
        self.current_session['end_time'] = datetime.now().isoformat()
        self.current_session['success'] = success
        self.current_session['final_stats'] = stats
        
        # Calculate and store performance metrics
        if 'start_time' in self.current_session:
            start = datetime.fromisoformat(self.current_session['start_time'])
            duration = (datetime.now() - start).total_seconds()
            
            files_processed = stats.get('total_processed', 0)
            if files_processed > 0 and duration > 0:
                self.current_session['performance_metrics'] = {
                    'files_per_second': files_processed / duration,
                    'duration_seconds': duration,
                    'size_reduction': stats.get('size_reduction', 0)
                }
        
        # Add session to history
        self.learning_data['sessions'].append(self.current_session)
        
        # Keep only last 100 sessions
        if len(self.learning_data['sessions']) > 100:
            self.learning_data['sessions'] = self.learning_data['sessions'][-100:]
        
        # Update optimization hints based on performance
        self.update_optimization_hints()
        
        # Save learning data
        self.save_learning_data()
    
    def update_optimization_hints(self) -> None:
        """Update optimization hints based on accumulated learning."""
        hints = self.learning_data['optimization_hints']
        
        # Analyze recent sessions for performance patterns
        recent_sessions = self.learning_data['sessions'][-10:]
        if recent_sessions:
            avg_fps = sum(
                s.get('performance_metrics', {}).get('files_per_second', 0)
                for s in recent_sessions
            ) / len(recent_sessions)
            
            if avg_fps > 0:
                if avg_fps < 10:
                    hints['performance'] = "Consider using parallel processing for better performance"
                elif avg_fps > 100:
                    hints['performance'] = "Current settings provide excellent performance"
            
            # Check for recurring errors
            all_errors = []
            for session in recent_sessions:
                all_errors.extend(session.get('errors_encountered', []))
            
            if len(all_errors) > 5:
                hints['stability'] = "Multiple errors detected. Consider more conservative settings."
    
    def extract_error_key(self, error: str) -> str:
        """
        Extract key from error for categorization.
        
        Args:
            error: Error message
            
        Returns:
            Error key for grouping
        """
        # Common error patterns
        if 'codec' in error or 'charmap' in error:
            return 'encoding_error'
        elif 'permission' in error.lower():
            return 'permission_error'
        elif 'not found' in error.lower():
            return 'file_not_found'
        elif 'memory' in error.lower():
            return 'memory_error'
        elif 'timeout' in error.lower():
            return 'timeout_error'
        else:
            # Use first few words as key
            words = error.split()[:3]
            return '_'.join(words).lower()
    
    def get_summary_report(self) -> str:
        """
        Get a summary report of learning insights.
        
        Returns:
            Summary report string
        """
        report = ["Go-By Folder Learning System Report", "=" * 40]
        
        # Sessions summary
        total_sessions = len(self.learning_data['sessions'])
        successful = sum(1 for s in self.learning_data['sessions'] if s.get('success'))
        report.append(f"\nTotal Sessions: {total_sessions}")
        report.append(f"Successful: {successful}")
        if total_sessions > 0:
            report.append(f"Success Rate: {successful/total_sessions*100:.1f}%")
        
        # Common patterns
        if self.learning_data['patterns']:
            report.append("\nCommon Patterns Detected:")
            for pattern, data in sorted(
                self.learning_data['patterns'].items(),
                key=lambda x: x[1].get('count', 0),
                reverse=True
            )[:5]:
                report.append(f"  - {pattern}: {data.get('count', 0)} times")
        
        # File type statistics
        if self.learning_data['file_type_strategies']:
            report.append("\nFile Types Processed:")
            for ext, strategy in sorted(
                self.learning_data['file_type_strategies'].items(),
                key=lambda x: x[1].get('total_processed', 0),
                reverse=True
            )[:10]:
                report.append(f"  - {ext}: {strategy.get('total_processed', 0)} files")
        
        # Common errors
        if self.learning_data['common_errors']:
            report.append("\nCommon Issues:")
            for error_key, data in sorted(
                self.learning_data['common_errors'].items(),
                key=lambda x: x[1].get('count', 0),
                reverse=True
            )[:5]:
                report.append(f"  - {error_key}: {data.get('count', 0)} occurrences")
        
        # Optimization hints
        if self.learning_data['optimization_hints']:
            report.append("\nOptimization Hints:")
            for hint_type, hint in self.learning_data['optimization_hints'].items():
                report.append(f"  - {hint_type}: {hint}")
        
        # Popular naming patterns
        if self.learning_data['naming_patterns']:
            report.append("\nPopular Naming Patterns:")
            for name, count in sorted(
                self.learning_data['naming_patterns'].items(),
                key=lambda x: x[1],
                reverse=True
            )[:5]:
                report.append(f"  - {name}: used {count} times")
        
        return "\n".join(report)


def create_learning_system(config: Dict[str, Any]) -> GoByLearningSystem:
    """
    Create a learning system instance.
    
    Args:
        config: Configuration dictionary
        
    Returns:
        Configured GoByLearningSystem instance
    """
    learning_file = config.get('learning_file')
    if learning_file:
        learning_file = Path(learning_file)
    
    return GoByLearningSystem(learning_file)