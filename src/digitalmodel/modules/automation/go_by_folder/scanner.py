"""
File system scanner for Create Go-By Folder Tool
"""

import os
import hashlib
import fnmatch
from pathlib import Path
from typing import Dict, List, Generator, Optional, Set
from datetime import datetime
import logging

from .exceptions import ScanError

logger = logging.getLogger(__name__)


class FileScanner:
    """Scan and analyze files in source folder."""
    
    def __init__(
        self,
        source_path: Path,
        exclude_patterns: Optional[List[str]] = None,
        include_patterns: Optional[List[str]] = None,
        follow_symlinks: bool = False
    ):
        """
        Initialize file scanner.

        Args:
            source_path: Path to source folder
            exclude_patterns: Glob patterns to exclude
            include_patterns: Glob patterns to include (if specified, only these are included)
            follow_symlinks: Whether to follow symbolic links
        """
        self.source_path = Path(source_path)
        self.root_path = self.source_path  # Alias for tests
        self.exclude_patterns = exclude_patterns or []
        self.include_patterns = include_patterns or []
        self.follow_symlinks = follow_symlinks

        # Registries
        self.file_registry = {}
        self.symlink_registry = {}
        self.symlinks = self.symlink_registry  # Alias for tests
        self.directory_structure = set()

        # Statistics
        self.total_files = 0
        self.total_size = 0
        self.file_types = {}
        self.scan_errors = []
        
    def scan(self, progress_callback=None) -> Generator[Dict, None, None]:
        """
        Scan source folder and yield file information.
        
        Args:
            progress_callback: Optional callback for progress updates
            
        Yields:
            Dictionary with file metadata
        """
        logger.info(f"Starting scan of {self.source_path}")
        
        file_count = 0
        
        for root, dirs, files in os.walk(self.source_path, followlinks=False):
            root_path = Path(root)
            relative_root = root_path.relative_to(self.source_path)
            
            # Track directory structure
            self.directory_structure.add(relative_root)
            
            # Check for symlinks in directories
            dirs_to_remove = []
            for dirname in dirs[:]:  # Use slice copy to allow modification
                dirpath = root_path / dirname
                if dirpath.is_symlink():
                    self.register_symlink(dirpath, 'directory')
                    dirs_to_remove.append(dirname)
            
            # Remove symlinked directories from traversal
            for dirname in dirs_to_remove:
                dirs.remove(dirname)
            
            # Process files
            for filename in files:
                filepath = Path(root) / filename
                relative_path = filepath.relative_to(self.source_path)

                # Check if file should be processed
                if not self.should_process(relative_path):
                    logger.debug(f"Skipping excluded file: {relative_path}")
                    continue

                # Check for symlinks - wrapped in try/except for permission errors
                try:
                    if filepath.is_symlink():
                        self.register_symlink(filepath, 'file')
                        continue
                except (PermissionError, OSError) as e:
                    logger.warning(f"Permission error checking symlink {filepath}: {e}")
                    self.scan_errors.append({
                        'path': str(filepath),
                        'error': str(e)
                    })
                    continue

                try:
                    file_info = self.analyze_file(filepath)
                    self.register_file(file_info)

                    file_count += 1
                    if progress_callback and file_count % 100 == 0:
                        progress_callback(file_count, str(relative_path))

                    yield file_info

                except (PermissionError, OSError) as e:
                    logger.warning(f"Permission error scanning {filepath}: {e}")
                    self.scan_errors.append({
                        'path': str(filepath),
                        'error': str(e)
                    })
                except Exception as e:
                    logger.warning(f"Error scanning {filepath}: {e}")
                    self.scan_errors.append({
                        'path': str(filepath),
                        'error': str(e)
                    })
        
        logger.info(
            f"Scan complete: {self.total_files} files, "
            f"{self.total_size / (1024*1024):.2f}MB, "
            f"{len(self.symlink_registry)} symlinks"
        )
    
    def should_process(self, relative_path: Path) -> bool:
        """
        Check if file should be processed based on patterns.
        
        Args:
            relative_path: Relative path to file
            
        Returns:
            True if file should be processed
        """
        path_str = str(relative_path).replace('\\', '/')
        
        # Check exclude patterns
        for pattern in self.exclude_patterns:
            if fnmatch.fnmatch(path_str, pattern):
                return False
        
        # If include patterns specified, file must match one
        if self.include_patterns:
            for pattern in self.include_patterns:
                if fnmatch.fnmatch(path_str, pattern):
                    return True
            return False
        
        return True
    
    def analyze_file(self, filepath: Path) -> Dict:
        """
        Analyze single file and return metadata.

        Args:
            filepath: Path to file

        Returns:
            Dictionary with file metadata
        """
        stat = filepath.stat()

        file_info = {
            'path': filepath,
            'relative_path': filepath.relative_to(self.source_path),
            'name': filepath.name,  # Added for test compatibility
            'size': stat.st_size,
            'modified': datetime.fromtimestamp(stat.st_mtime),
            'created': datetime.fromtimestamp(stat.st_ctime),
            'extension': filepath.suffix.lower(),
            'stem': filepath.stem,
            'parent': filepath.parent.relative_to(self.source_path),
            'is_binary': None,  # Will be determined if needed
            'hash': None  # Will be calculated if needed
        }

        # Calculate hash for small files (< 1MB)
        if stat.st_size < 1_000_000:
            try:
                file_info['hash'] = self.calculate_hash(filepath)
            except Exception as e:
                logger.debug(f"Could not hash {filepath}: {e}")

        # Detect if binary
        file_info['is_binary'] = self.is_binary_file(filepath)

        return file_info
    
    def register_file(self, file_info: Dict) -> None:
        """
        Register file in internal registries.
        
        Args:
            file_info: File metadata dictionary
        """
        relative_path = file_info['relative_path']
        
        # Add to main registry
        self.file_registry[relative_path] = file_info
        
        # Update statistics
        self.total_files += 1
        self.total_size += file_info['size']
        
        # Track file types
        ext = file_info['extension']
        if ext not in self.file_types:
            self.file_types[ext] = {'count': 0, 'total_size': 0}
        self.file_types[ext]['count'] += 1
        self.file_types[ext]['total_size'] += file_info['size']
    
    def register_symlink(self, link_path: Path, link_type: str) -> None:
        """
        Register symlink in registry.
        
        Args:
            link_path: Path to symlink
            link_type: 'file' or 'directory'
        """
        try:
            target = link_path.resolve()
            relative_path = link_path.relative_to(self.source_path)
            
            self.symlink_registry[relative_path] = {
                'path': link_path,
                'target': target,
                'type': link_type,
                'valid': target.exists()
            }
            
            logger.debug(f"Found {link_type} symlink: {relative_path} -> {target}")
            
        except Exception as e:
            logger.warning(f"Error processing symlink {link_path}: {e}")
    
    def calculate_hash(self, filepath: Path, algorithm='md5') -> str:
        """
        Calculate file hash.

        Args:
            filepath: Path to file
            algorithm: Hash algorithm to use

        Returns:
            Hex digest of file hash
        """
        hash_func = hashlib.new(algorithm)

        with open(filepath, 'rb') as f:
            # Read in chunks to handle large files
            for chunk in iter(lambda: f.read(8192), b''):
                hash_func.update(chunk)

        return hash_func.hexdigest()

    def calculate_file_hash(self, filepath: Path, algorithm='md5') -> str:
        """
        Calculate file hash (alias for calculate_hash).

        Args:
            filepath: Path to file
            algorithm: Hash algorithm to use

        Returns:
            Hex digest of file hash
        """
        return self.calculate_hash(filepath, algorithm)
    
    def is_binary_file(self, filepath: Path) -> bool:
        """
        Detect if file is binary.
        
        Args:
            filepath: Path to file
            
        Returns:
            True if file appears to be binary
        """
        # Check by extension first
        text_extensions = {
            '.txt', '.md', '.py', '.js', '.html', '.css', '.json', '.yml', '.yaml',
            '.xml', '.csv', '.log', '.ini', '.cfg', '.conf', '.sh', '.bat', '.ps1',
            '.java', '.c', '.cpp', '.h', '.hpp', '.cs', '.rb', '.go', '.rs', '.ts'
        }
        
        if filepath.suffix.lower() in text_extensions:
            return False
        
        # Check by reading first bytes
        try:
            with open(filepath, 'rb') as f:
                chunk = f.read(1024)
                if not chunk:
                    return False
                
                # Look for null bytes (common in binary files)
                if b'\0' in chunk:
                    return True
                
                # Check if mostly printable ASCII
                printable = sum(1 for byte in chunk if 32 <= byte <= 126 or byte in (9, 10, 13))
                return (printable / len(chunk)) < 0.7
                
        except Exception:
            # If we can't read it, assume binary
            return True
    
    def get_statistics(self) -> Dict:
        """
        Get scan statistics.
        
        Returns:
            Dictionary with scan statistics
        """
        return {
            'total_files': self.total_files,
            'total_size': self.total_size,
            'total_size_mb': self.total_size / (1024 * 1024),
            'directory_count': len(self.directory_structure),
            'symlink_count': len(self.symlink_registry),
            'file_types': self.file_types,
            'unique_extensions': len(self.file_types),
            'scan_errors': len(self.scan_errors)
        }