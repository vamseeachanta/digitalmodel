"""
Real-time Pattern Modification Engine for OrcaFlex Browser Interface
Handles dynamic pattern generation and file matching based on parameter changes
"""

import asyncio
import re
from typing import Dict, List, Optional, Set, Callable, Any, Tuple
from pathlib import Path
from dataclasses import dataclass
import logging
from concurrent.futures import ThreadPoolExecutor
import time

logger = logging.getLogger(__name__)


@dataclass
class PatternResult:
    """Result of pattern matching operation"""
    pattern: str
    matched_files: List[Path]
    component_files: Dict[str, List[Path]]
    match_time: float
    error: Optional[str] = None


class PatternEngine:
    """Real-time pattern generation and file matching engine"""
    
    # Component patterns for different file types
    COMPONENT_PATTERNS = {
        'strut': ['Strut1', 'Strut2', 'Strut3', 'Strut4', 'Strut5', 'Strut6', 'Strut7', 'Strut8'],
        'jacket': ['Jacket1', 'Jacket2', 'Jacket3', 'Jacket4'],
        'mooring': ['Mooring1', 'Mooring2', 'Line1', 'Line2', 'Line3', 'Line4'],
        'fst': ['FST1_6dof_dyn', 'FST2_6dof_dyn'],
        'summary': ['strut_dyn', 'jacket_dyn', 'mooring_dyn']
    }
    
    def __init__(self, base_path: Optional[Path] = None, cache_ttl: int = 60):
        """
        Initialize pattern engine
        
        Args:
            base_path: Base directory path for OrcaFlex files
            cache_ttl: Cache time-to-live in seconds
        """
        self.base_path = base_path or Path("D:/1522/ctr7/orcaflex/rev_a08")
        self.cache_ttl = cache_ttl
        self.pattern_cache: Dict[str, PatternResult] = {}
        self.cache_timestamps: Dict[str, float] = {}
        self.executor = ThreadPoolExecutor(max_workers=4)
        self.pattern_listeners: List[Callable[[PatternResult], None]] = []
        
    def add_pattern_listener(self, callback: Callable[[PatternResult], None]):
        """Add a callback to be notified when patterns change"""
        self.pattern_listeners.append(callback)
        
    def remove_pattern_listener(self, callback: Callable[[PatternResult], None]):
        """Remove a pattern listener"""
        if callback in self.pattern_listeners:
            self.pattern_listeners.remove(callback)
    
    def _notify_listeners(self, result: PatternResult):
        """Notify all listeners of pattern result"""
        for listener in self.pattern_listeners:
            try:
                listener(result)
            except Exception as e:
                logger.error(f"Error notifying pattern listener: {e}")
    
    def modify_pattern(self, base_pattern: str, modifications: Dict[str, Any]) -> str:
        """
        Modify pattern based on real-time parameter changes
        
        Args:
            base_pattern: Base file pattern
            modifications: Dictionary of modifications to apply
            
        Returns:
            Modified pattern string
        """
        pattern = base_pattern
        
        # Apply modifications
        if 'vessel_type' in modifications:
            pattern = re.sub(r'^(fsts|flng|lngc)', modifications['vessel_type'], pattern)
        
        if 'loading_condition' in modifications:
            pattern = re.sub(r'l\d{3}', modifications['loading_condition'], pattern)
        
        if 'tide_level' in modifications:
            pattern = re.sub(r'(hwl|mwl|lwl)', modifications['tide_level'], pattern)
        
        if 'return_period' in modifications:
            pattern = re.sub(r'\d{4}yr', modifications['return_period'], pattern)
        
        if 'wave_direction' in modifications:
            pattern = re.sub(r'\d{3}deg', modifications['wave_direction'], pattern)
        
        if 'analysis_type' in modifications:
            pattern = re.sub(r'\d{2}[a-z]', modifications['analysis_type'], pattern)
        
        return pattern
    
    def generate_component_patterns(self, base_pattern: str) -> Dict[str, List[str]]:
        """
        Generate patterns for all component types
        
        Args:
            base_pattern: Base file pattern
            
        Returns:
            Dictionary mapping component types to pattern lists
        """
        component_patterns = {}
        
        for comp_type, components in self.COMPONENT_PATTERNS.items():
            patterns = []
            for component in components:
                patterns.append(f"{base_pattern}_{component}")
            component_patterns[comp_type] = patterns
        
        return component_patterns
    
    def _match_files_in_folder(self, pattern: str, folder: Path) -> List[Path]:
        """
        Match files in a specific folder
        
        Args:
            pattern: Pattern to match
            folder: Folder to search in
            
        Returns:
            List of matching file paths
        """
        if not folder.exists():
            return []
        
        matched_files = []
        
        # Convert pattern to regex
        regex_pattern = pattern.replace('*', '.*')
        regex_pattern = f"^{regex_pattern}"
        
        try:
            for file_path in folder.glob("*.csv"):
                if re.match(regex_pattern, file_path.stem, re.IGNORECASE):
                    matched_files.append(file_path)
        except Exception as e:
            logger.error(f"Error matching files in {folder}: {e}")
        
        return matched_files
    
    async def match_pattern_async(self, pattern: str, folders: Optional[List[Path]] = None) -> PatternResult:
        """
        Asynchronously match pattern against files
        
        Args:
            pattern: Pattern to match
            folders: List of folders to search (if None, searches default locations)
            
        Returns:
            PatternResult with matched files
        """
        start_time = time.time()
        
        # Check cache
        cache_key = f"{pattern}:{folders}"
        if cache_key in self.pattern_cache:
            cached_time = self.cache_timestamps.get(cache_key, 0)
            if time.time() - cached_time < self.cache_ttl:
                logger.debug(f"Using cached result for pattern: {pattern}")
                return self.pattern_cache[cache_key]
        
        # Default folders if not specified
        if folders is None:
            folders = [
                self.base_path / "output" / "csv",
                self.base_path / "output" / "csv" / "combined",
                self.base_path / "output" / "csv" / "individual"
            ]
        
        # Match base pattern
        all_matched_files = []
        component_files = {}
        
        try:
            # Use thread pool for parallel file matching
            loop = asyncio.get_event_loop()
            
            # Match base pattern
            tasks = []
            for folder in folders:
                task = loop.run_in_executor(
                    self.executor,
                    self._match_files_in_folder,
                    pattern,
                    folder
                )
                tasks.append(task)
            
            results = await asyncio.gather(*tasks)
            for matched in results:
                all_matched_files.extend(matched)
            
            # Generate and match component patterns
            component_patterns = self.generate_component_patterns(pattern)
            
            for comp_type, patterns in component_patterns.items():
                component_files[comp_type] = []
                comp_tasks = []
                
                for comp_pattern in patterns:
                    for folder in folders:
                        task = loop.run_in_executor(
                            self.executor,
                            self._match_files_in_folder,
                            comp_pattern,
                            folder
                        )
                        comp_tasks.append(task)
                
                comp_results = await asyncio.gather(*comp_tasks)
                for matched in comp_results:
                    component_files[comp_type].extend(matched)
            
            match_time = time.time() - start_time
            
            result = PatternResult(
                pattern=pattern,
                matched_files=all_matched_files,
                component_files=component_files,
                match_time=match_time
            )
            
            # Update cache
            self.pattern_cache[cache_key] = result
            self.cache_timestamps[cache_key] = time.time()
            
            # Notify listeners
            self._notify_listeners(result)
            
            logger.info(f"Pattern '{pattern}' matched {len(all_matched_files)} files in {match_time:.2f}s")
            
            return result
            
        except Exception as e:
            logger.error(f"Error matching pattern: {e}")
            return PatternResult(
                pattern=pattern,
                matched_files=[],
                component_files={},
                match_time=time.time() - start_time,
                error=str(e)
            )
    
    def match_pattern(self, pattern: str, folders: Optional[List[Path]] = None) -> PatternResult:
        """
        Synchronously match pattern against files
        
        Args:
            pattern: Pattern to match
            folders: List of folders to search
            
        Returns:
            PatternResult with matched files
        """
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            return loop.run_until_complete(self.match_pattern_async(pattern, folders))
        finally:
            loop.close()
    
    def validate_pattern(self, pattern: str) -> Tuple[bool, Optional[str]]:
        """
        Validate pattern syntax
        
        Args:
            pattern: Pattern to validate
            
        Returns:
            Tuple of (is_valid, error_message)
        """
        if not pattern:
            return False, "Pattern cannot be empty"
        
        # Check for invalid characters
        invalid_chars = ['/', '\\', ':', '?', '"', '<', '>', '|']
        for char in invalid_chars:
            if char in pattern:
                return False, f"Pattern contains invalid character: {char}"
        
        # Check pattern structure
        parts = pattern.split('_')
        if len(parts) < 2:
            return False, "Pattern must contain at least vessel type and one parameter"
        
        return True, None
    
    def get_pattern_statistics(self, pattern: str) -> Dict[str, Any]:
        """
        Get statistics for a pattern
        
        Args:
            pattern: Pattern to analyze
            
        Returns:
            Dictionary with pattern statistics
        """
        result = self.match_pattern(pattern)
        
        stats = {
            'pattern': pattern,
            'total_files': len(result.matched_files),
            'component_breakdown': {},
            'file_sizes': [],
            'match_time': result.match_time,
            'cached': pattern in self.pattern_cache
        }
        
        # Component breakdown
        for comp_type, files in result.component_files.items():
            stats['component_breakdown'][comp_type] = len(files)
        
        # File sizes
        for file_path in result.matched_files[:10]:  # Sample first 10 files
            if file_path.exists():
                stats['file_sizes'].append(file_path.stat().st_size)
        
        return stats
    
    def clear_cache(self):
        """Clear pattern cache"""
        self.pattern_cache.clear()
        self.cache_timestamps.clear()
        logger.info("Pattern cache cleared")
    
    def shutdown(self):
        """Shutdown pattern engine"""
        self.executor.shutdown(wait=True)
        self.clear_cache()


class PatternOptimizer:
    """Optimize patterns for better performance"""
    
    @staticmethod
    def optimize_pattern(pattern: str) -> str:
        """
        Optimize pattern for faster matching
        
        Args:
            pattern: Pattern to optimize
            
        Returns:
            Optimized pattern
        """
        # Remove redundant wildcards
        pattern = re.sub(r'\*+', '*', pattern)
        
        # Remove trailing wildcards if not needed
        if pattern.endswith('*') and '_' in pattern:
            pattern = pattern.rstrip('*')
        
        return pattern
    
    @staticmethod
    def suggest_patterns(base_pattern: str, available_files: List[str]) -> List[str]:
        """
        Suggest optimized patterns based on available files
        
        Args:
            base_pattern: Base pattern
            available_files: List of available filenames
            
        Returns:
            List of suggested patterns
        """
        suggestions = [base_pattern]
        
        # Analyze common prefixes
        if available_files:
            # Find common patterns
            common_parts = set()
            for filename in available_files[:100]:  # Sample first 100
                parts = filename.split('_')
                if len(parts) >= 3:
                    common_parts.add('_'.join(parts[:3]))
            
            # Create suggestions based on common patterns
            for common in list(common_parts)[:5]:
                if common.startswith(base_pattern[:10]):
                    suggestions.append(f"{common}*")
        
        return suggestions


# Example usage
if __name__ == "__main__":
    import asyncio
    
    # Configure logging
    logging.basicConfig(level=logging.INFO)
    
    # Create engine instance
    engine = PatternEngine()
    
    # Test pattern modification
    base_pattern = "fsts_03c_0100yr_l015_hwl"
    modifications = {
        'loading_condition': 'l095',
        'tide_level': 'mwl'
    }
    
    modified = engine.modify_pattern(base_pattern, modifications)
    print(f"Modified pattern: {modified}")
    
    # Test pattern matching
    result = engine.match_pattern(modified)
    print(f"Matched {len(result.matched_files)} files")
    print(f"Component files: {len(result.component_files)} types")
    
    # Get statistics
    stats = engine.get_pattern_statistics(modified)
    print(f"Pattern statistics: {stats}")
    
    # Cleanup
    engine.shutdown()