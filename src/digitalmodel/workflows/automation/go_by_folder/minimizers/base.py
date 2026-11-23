"""
Base minimizer class for Create Go-By Folder Tool
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional, Union
import logging

logger = logging.getLogger(__name__)


class BaseMinimizer(ABC):
    """Abstract base class for file minimizers."""
    
    def __init__(self, max_size: int = 10240):
        """
        Initialize minimizer.
        
        Args:
            max_size: Maximum size for minimized files in bytes
        """
        self.max_size = max_size
    
    @abstractmethod
    def minimize(
        self, 
        file_path: Path, 
        preserve: bool = False
    ) -> Optional[Union[bytes, str]]:
        """
        Minimize file content.
        
        Args:
            file_path: Path to file to minimize
            preserve: If True, return None to indicate file should be preserved
            
        Returns:
            Minimized content as bytes/string, or None if preserving original
        """
        pass
    
    @abstractmethod
    def can_handle(self, file_path: Path) -> bool:
        """
        Check if this minimizer can handle the file.
        
        Args:
            file_path: Path to file
            
        Returns:
            True if this minimizer can handle the file type
        """
        pass
    
    def create_stub_metadata(self, file_path: Path) -> str:
        """
        Create stub file metadata.
        
        Args:
            file_path: Original file path
            
        Returns:
            Metadata string for stub file
        """
        stat = file_path.stat()
        return f"""# Stub file for: {file_path.name}
# Original size: {stat.st_size} bytes
# File type: {file_path.suffix}
# This is a placeholder for the original file
"""