"""
Real-time File Search Engine for OrcaFlex Browser Interface
Provides instant file pattern updates and intelligent file categorization
"""

import asyncio
import re
from typing import Dict, List, Optional, Set, Tuple, Any, Callable
from pathlib import Path
from dataclasses import dataclass, field
from enum import Enum
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
from collections import defaultdict
import hashlib

logger = logging.getLogger(__name__)


class FileCategory(Enum):
    """Categories for file classification"""
    SUMMARY = "summary"
    TIME_SERIES = "time_series"
    STRUT = "strut"
    JACKET = "jacket"
    MOORING = "mooring"
    FST_MOTION = "fst_motion"
    CONFIGURATION = "configuration"
    UNKNOWN = "unknown"


@dataclass
class FileInfo:
    """Information about a matched file"""
    path: Path
    category: FileCategory
    component: Optional[str]
    size: int
    modified_time: float
    parameters: Dict[str, str]
    match_score: float = 1.0


@dataclass
class SearchResult:
    """Result of file search operation"""
    pattern: str
    total_files: int
    files_by_category: Dict[FileCategory, List[FileInfo]]
    search_time: float
    available: bool
    error: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class FileSearchEngine:
    """Real-time file search engine with intelligent categorization"""
    
    # File category patterns
    CATEGORY_PATTERNS = {
        FileCategory.SUMMARY: [
            r'^dm.*\.csv$',
            r'.*_summary\.csv$',
            r'.*_overview\.csv$'
        ],
        FileCategory.STRUT: [
            r'.*Strut\d+.*\.csv$',
            r'.*strut.*\.csv$'
        ],
        FileCategory.JACKET: [
            r'.*Jacket\d+.*\.csv$',
            r'.*jacket.*\.csv$'
        ],
        FileCategory.MOORING: [
            r'.*Mooring\d+.*\.csv$',
            r'.*Line\d+.*\.csv$',
            r'.*mooring.*\.csv$'
        ],
        FileCategory.FST_MOTION: [
            r'.*FST\d+.*6dof.*\.csv$',
            r'.*motion.*\.csv$'
        ],
        FileCategory.CONFIGURATION: [
            r'.*inputs\.csv$',
            r'.*config.*\.csv$',
            r'.*settings.*\.csv$'
        ]
    }
    
    def __init__(
        self,
        base_path: Optional[Path] = None,
        cache_ttl: int = 30,
        max_workers: int = 4
    ):
        """
        Initialize file search engine
        
        Args:
            base_path: Base directory path for searching
            cache_ttl: Cache time-to-live in seconds
            max_workers: Maximum number of worker threads
        """
        self.base_path = base_path or Path("D:/1522/ctr7/orcaflex/rev_a08")
        self.cache_ttl = cache_ttl
        self.max_workers = max_workers
        self.executor = ThreadPoolExecutor(max_workers=max_workers)
        
        # Caching
        self.search_cache: Dict[str, SearchResult] = {}
        self.cache_timestamps: Dict[str, float] = {}
        self.file_index: Dict[str, FileInfo] = {}
        
        # Real-time update listeners
        self.update_listeners: List[Callable[[SearchResult], None]] = []
        
        # Initialize file index
        self._initialize_file_index()
    
    def _initialize_file_index(self):
        """Initialize file index for faster searching"""
        try:
            search_paths = [
                self.base_path / "output" / "csv",
                self.base_path / "output" / "csv" / "combined",
                self.base_path / "output" / "csv" / "individual"
            ]
            
            for path in search_paths:
                if path.exists():
                    self._index_directory(path)
            
            logger.info(f"Indexed {len(self.file_index)} files")
        except Exception as e:
            logger.error(f"Error initializing file index: {e}")
    
    def _index_directory(self, directory: Path):
        """Index files in a directory"""
        try:
            for file_path in directory.glob("**/*.csv"):
                file_info = self._create_file_info(file_path)
                if file_info:
                    file_key = str(file_path)
                    self.file_index[file_key] = file_info
        except Exception as e:
            logger.error(f"Error indexing directory {directory}: {e}")
    
    def _create_file_info(self, file_path: Path) -> Optional[FileInfo]:
        """Create FileInfo object for a file"""
        try:
            stat = file_path.stat()
            category = self._categorize_file(file_path.name)
            component = self._extract_component(file_path.name)
            parameters = self._extract_parameters(file_path.stem)
            
            return FileInfo(
                path=file_path,
                category=category,
                component=component,
                size=stat.st_size,
                modified_time=stat.st_mtime,
                parameters=parameters
            )
        except Exception as e:
            logger.debug(f"Error creating FileInfo for {file_path}: {e}")
            return None
    
    def _categorize_file(self, filename: str) -> FileCategory:
        """Categorize file based on filename patterns"""
        for category, patterns in self.CATEGORY_PATTERNS.items():
            for pattern in patterns:
                if re.match(pattern, filename, re.IGNORECASE):
                    return category
        return FileCategory.UNKNOWN
    
    def _extract_component(self, filename: str) -> Optional[str]:
        """Extract component name from filename"""
        # Extract component patterns
        patterns = [
            r'(Strut\d+)',
            r'(Jacket\d+)',
            r'(Mooring\d+)',
            r'(Line\d+)',
            r'(FST\d+)',
        ]
        
        for pattern in patterns:
            match = re.search(pattern, filename, re.IGNORECASE)
            if match:
                return match.group(1)
        
        return None
    
    def _extract_parameters(self, basename: str) -> Dict[str, str]:
        """Extract parameters from filename"""
        parameters = {}
        
        # Vessel type
        for vessel in ['fsts', 'flng', 'lngc']:
            if vessel in basename.lower():
                parameters['vessel_type'] = vessel
                break
        
        # Loading condition
        match = re.search(r'(l\d{3})', basename.lower())
        if match:
            parameters['loading_condition'] = match.group(1)
        
        # Tide level
        for tide in ['hwl', 'mwl', 'lwl']:
            if tide in basename.lower():
                parameters['tide_level'] = tide
                break
        
        # Return period
        match = re.search(r'(\d{4}yr)', basename.lower())
        if match:
            parameters['return_period'] = match.group(1)
        
        # Wave direction
        match = re.search(r'(\d{3}deg)', basename.lower())
        if match:
            parameters['wave_direction'] = match.group(1)
        
        # Analysis type
        match = re.search(r'(\d{2}[a-z])', basename.lower())
        if match:
            parameters['analysis_type'] = match.group(1)
        
        return parameters
    
    def add_update_listener(self, callback: Callable[[SearchResult], None]):
        """Add listener for search updates"""
        self.update_listeners.append(callback)
    
    def remove_update_listener(self, callback: Callable[[SearchResult], None]):
        """Remove update listener"""
        if callback in self.update_listeners:
            self.update_listeners.remove(callback)
    
    def _notify_listeners(self, result: SearchResult):
        """Notify all listeners of search result"""
        for listener in self.update_listeners:
            try:
                listener(result)
            except Exception as e:
                logger.error(f"Error notifying listener: {e}")
    
    async def search_files_async(
        self,
        pattern: str,
        categories: Optional[List[FileCategory]] = None,
        real_time: bool = True
    ) -> SearchResult:
        """
        Asynchronously search for files matching pattern
        
        Args:
            pattern: Search pattern
            categories: Optional list of categories to filter
            real_time: Whether to notify listeners in real-time
            
        Returns:
            SearchResult with matched files
        """
        start_time = time.time()
        
        # Check cache
        cache_key = f"{pattern}:{categories}"
        if cache_key in self.search_cache:
            cached_time = self.cache_timestamps.get(cache_key, 0)
            if time.time() - cached_time < self.cache_ttl:
                cached_result = self.search_cache[cache_key]
                if real_time:
                    self._notify_listeners(cached_result)
                return cached_result
        
        try:
            # Perform search
            matched_files = await self._perform_search(pattern, categories)
            
            # Categorize results
            files_by_category = defaultdict(list)
            for file_info in matched_files:
                files_by_category[file_info.category].append(file_info)
            
            # Create result
            result = SearchResult(
                pattern=pattern,
                total_files=len(matched_files),
                files_by_category=dict(files_by_category),
                search_time=time.time() - start_time,
                available=len(matched_files) > 0,
                metadata={
                    'categories_searched': [c.value for c in (categories or [])]
                }
            )
            
            # Update cache
            self.search_cache[cache_key] = result
            self.cache_timestamps[cache_key] = time.time()
            
            # Notify listeners if real-time
            if real_time:
                self._notify_listeners(result)
            
            logger.info(f"Search '{pattern}' found {len(matched_files)} files in {result.search_time:.2f}s")
            
            return result
            
        except Exception as e:
            logger.error(f"Error searching files: {e}")
            return SearchResult(
                pattern=pattern,
                total_files=0,
                files_by_category={},
                search_time=time.time() - start_time,
                available=False,
                error=str(e)
            )
    
    async def _perform_search(
        self,
        pattern: str,
        categories: Optional[List[FileCategory]] = None
    ) -> List[FileInfo]:
        """Perform the actual file search"""
        matched_files = []
        
        # Convert pattern to regex
        regex_pattern = self._pattern_to_regex(pattern)
        
        # Search in file index
        for file_key, file_info in self.file_index.items():
            # Check category filter
            if categories and file_info.category not in categories:
                continue
            
            # Check pattern match
            if re.match(regex_pattern, file_info.path.stem, re.IGNORECASE):
                # Calculate match score
                file_info.match_score = self._calculate_match_score(
                    pattern,
                    file_info.path.stem
                )
                matched_files.append(file_info)
        
        # Sort by match score and modified time
        matched_files.sort(
            key=lambda f: (f.match_score, f.modified_time),
            reverse=True
        )
        
        return matched_files
    
    def _pattern_to_regex(self, pattern: str) -> str:
        """Convert search pattern to regex"""
        # Escape special regex characters except wildcards
        pattern = re.escape(pattern)
        # Replace escaped wildcards with regex wildcards
        pattern = pattern.replace(r'\*', '.*')
        pattern = pattern.replace(r'\?', '.')
        return f"^{pattern}"
    
    def _calculate_match_score(self, pattern: str, filename: str) -> float:
        """Calculate match score for ranking results"""
        score = 1.0
        
        # Exact match gets highest score
        if pattern.lower() == filename.lower():
            return 2.0
        
        # Partial matches get lower scores based on position
        pattern_lower = pattern.lower()
        filename_lower = filename.lower()
        
        if pattern_lower in filename_lower:
            # Earlier matches get higher scores
            position = filename_lower.index(pattern_lower)
            score = 1.5 - (position / len(filename_lower)) * 0.5
        
        return score
    
    def search_files(
        self,
        pattern: str,
        categories: Optional[List[FileCategory]] = None
    ) -> SearchResult:
        """
        Synchronously search for files
        
        Args:
            pattern: Search pattern
            categories: Optional category filter
            
        Returns:
            SearchResult
        """
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        try:
            return loop.run_until_complete(
                self.search_files_async(pattern, categories, real_time=False)
            )
        finally:
            loop.close()
    
    def get_file_count(self, pattern: str) -> int:
        """Get quick count of files matching pattern"""
        regex_pattern = self._pattern_to_regex(pattern)
        count = 0
        
        for file_info in self.file_index.values():
            if re.match(regex_pattern, file_info.path.stem, re.IGNORECASE):
                count += 1
        
        return count
    
    def get_availability_indicator(self, pattern: str) -> Dict[str, Any]:
        """Get file availability indicator"""
        count = self.get_file_count(pattern)
        
        return {
            'available': count > 0,
            'count': count,
            'status': 'available' if count > 0 else 'not_found',
            'color': 'green' if count > 0 else 'red',
            'message': f"{count} files available" if count > 0 else "No files found"
        }
    
    def refresh_index(self):
        """Refresh the file index"""
        logger.info("Refreshing file index...")
        self.file_index.clear()
        self._initialize_file_index()
        self.clear_cache()
    
    def clear_cache(self):
        """Clear search cache"""
        self.search_cache.clear()
        self.cache_timestamps.clear()
        logger.info("Search cache cleared")
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get search engine statistics"""
        category_counts = defaultdict(int)
        for file_info in self.file_index.values():
            category_counts[file_info.category.value] += 1
        
        return {
            'total_indexed_files': len(self.file_index),
            'categories': dict(category_counts),
            'cache_size': len(self.search_cache),
            'index_size_mb': sum(f.size for f in self.file_index.values()) / (1024 * 1024)
        }
    
    def shutdown(self):
        """Shutdown search engine"""
        self.executor.shutdown(wait=True)
        self.clear_cache()
        logger.info("Search engine shutdown")


# Example usage
if __name__ == "__main__":
    import asyncio
    
    # Configure logging
    logging.basicConfig(level=logging.INFO)
    
    # Create search engine
    engine = FileSearchEngine()
    
    # Test search
    pattern = "fsts_*_l015_hwl"
    result = engine.search_files(pattern)
    
    print(f"Found {result.total_files} files matching '{pattern}'")
    print(f"Search time: {result.search_time:.3f}s")
    
    # Print results by category
    for category, files in result.files_by_category.items():
        print(f"\n{category.value}: {len(files)} files")
        for file_info in files[:3]:  # Show first 3
            print(f"  - {file_info.path.name} (score: {file_info.match_score:.2f})")
    
    # Get availability indicator
    availability = engine.get_availability_indicator(pattern)
    print(f"\nAvailability: {availability}")
    
    # Get statistics
    stats = engine.get_statistics()
    print(f"\nEngine statistics: {stats}")
    
    # Cleanup
    engine.shutdown()