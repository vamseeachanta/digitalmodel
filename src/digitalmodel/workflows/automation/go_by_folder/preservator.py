"""
File preservation logic for Create Go-By Folder Tool
"""

from pathlib import Path
from typing import Dict, List, Set
import logging

logger = logging.getLogger(__name__)


class FilePreservator:
    """Select and preserve original files."""
    
    def __init__(self, selection_strategy: str = 'chronological'):
        """
        Initialize file preservator.
        
        Args:
            selection_strategy: Strategy for selecting originals
                - 'chronological': Oldest file first
                - 'smallest': Smallest file first
                - 'alphabetical': Alphabetically first
        """
        self.selection_strategy = selection_strategy
        self.preserved_files = set()
        self.preservation_map = {}
        
    def select_originals(self, file_groups: Dict[str, List]) -> Set[Path]:
        """
        Select one original file per pattern to preserve unchanged.
        
        Args:
            file_groups: Dictionary mapping patterns to file lists
            
        Returns:
            Set of paths to preserve as originals
        """
        for pattern, files in file_groups.items():
            if not files:
                continue
            
            # Select based on strategy
            if self.selection_strategy == 'chronological':
                # Sort by creation time, select oldest
                files.sort(key=lambda f: f.get('created', float('inf')))
                original = files[0]
                
            elif self.selection_strategy == 'smallest':
                # Select smallest file
                files.sort(key=lambda f: f.get('size', float('inf')))
                original = files[0]
                
            else:  # alphabetical
                # Sort by path
                files.sort(key=lambda f: str(f.get('path', '')))
                original = files[0]
            
            if original and 'path' in original:
                original_path = original['path']
                self.preserved_files.add(original_path)
                self.preservation_map[pattern] = original_path
                
                logger.debug(f"Selected original for pattern '{pattern}': {original_path}")
        
        logger.info(f"Selected {len(self.preserved_files)} original files to preserve")
        return self.preserved_files
    
    def select_by_type(self, files: List[Dict]) -> Set[Path]:
        """
        Select one original per file type/extension.
        
        Args:
            files: List of file information dictionaries
            
        Returns:
            Set of paths to preserve as originals
        """
        # Group by extension
        by_extension = {}
        for file_info in files:
            ext = file_info.get('extension', 'no_extension')
            if ext not in by_extension:
                by_extension[ext] = []
            by_extension[ext].append(file_info)
        
        # Select one from each extension
        return self.select_originals(by_extension)
    
    def should_preserve(self, file_path: Path) -> bool:
        """
        Check if file should be preserved as original.
        
        Args:
            file_path: Path to check
            
        Returns:
            True if file should be preserved unchanged
        """
        return file_path in self.preserved_files
    
    def get_preservation_stats(self) -> Dict:
        """
        Get preservation statistics.
        
        Returns:
            Dictionary with preservation statistics
        """
        return {
            'total_preserved': len(self.preserved_files),
            'patterns_covered': len(self.preservation_map),
            'selection_strategy': self.selection_strategy
        }