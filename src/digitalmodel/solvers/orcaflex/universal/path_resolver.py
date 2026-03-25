"""
Path Resolution System for Universal Access
===========================================

Provides path resolution capabilities to enable running from any directory.
"""

import os
import sys
from pathlib import Path
from typing import Optional, Union, List
import logging

logger = logging.getLogger(__name__)


class PathResolver:
    """
    Resolve paths for universal access from any directory.
    
    This class provides strategies to:
    - Find the digitalmodel repository from any location
    - Resolve relative and absolute paths
    - Handle cross-platform path differences
    - Validate and normalize paths
    """
    
    def __init__(self):
        """Initialize the path resolver."""
        self.repo_root = self._find_digitalmodel_root()
        self.current_dir = Path.cwd()
        
    def _find_digitalmodel_root(self) -> Optional[Path]:
        """
        Find digitalmodel repository from any location.
        
        Tries multiple strategies:
        1. Current directory if it's the repo
        2. Search upward for .git and src/digitalmodel
        3. Common installation locations
        4. Environment variable DIGITALMODEL_ROOT
        5. Python path locations
        
        Returns:
            Path to digitalmodel root or None if not found
        """
        # Strategy 1: Check if current directory is in the repo
        current = Path.cwd()
        while current != current.parent:
            if self._is_digitalmodel_root(current):
                logger.debug(f"Found digitalmodel root via upward search: {current}")
                return current
            current = current.parent
        
        # Strategy 2: Check common locations
        common_paths = self._get_common_paths()
        for path in common_paths:
            if path.exists() and self._is_digitalmodel_root(path):
                logger.debug(f"Found digitalmodel root at common location: {path}")
                return path
        
        # Strategy 3: Environment variable
        env_path = os.environ.get("DIGITALMODEL_ROOT")
        if env_path:
            path = Path(env_path)
            if path.exists() and self._is_digitalmodel_root(path):
                logger.debug(f"Found digitalmodel root via environment: {path}")
                return path
        
        # Strategy 4: Check Python path
        for path_str in sys.path:
            path = Path(path_str)
            if path.name == "digitalmodel" and self._is_digitalmodel_root(path):
                logger.debug(f"Found digitalmodel root via Python path: {path}")
                return path
            # Check parent directories
            if path.parent.name == "digitalmodel" and self._is_digitalmodel_root(path.parent):
                logger.debug(f"Found digitalmodel root via Python path parent: {path.parent}")
                return path.parent
        
        # Strategy 5: Try to import and find from module location
        try:
            import digitalmodel
            if hasattr(digitalmodel, "__file__"):
                module_path = Path(digitalmodel.__file__).parent
                # Go up to find root
                while module_path != module_path.parent:
                    if self._is_digitalmodel_root(module_path):
                        logger.debug(f"Found digitalmodel root via module import: {module_path}")
                        return module_path
                    module_path = module_path.parent
        except ImportError:
            pass
        
        logger.warning("Could not find digitalmodel repository root")
        return None
    
    def _is_digitalmodel_root(self, path: Path) -> bool:
        """Check if a path is the digitalmodel repository root."""
        required_indicators = [
            path / "src" / "digitalmodel",
            path / ".git",
        ]
        return all(indicator.exists() for indicator in required_indicators[:1])  # At least src/digitalmodel
    
    def _get_common_paths(self) -> List[Path]:
        """Get list of common installation paths to check."""
        paths = []
        
        # Windows paths
        if sys.platform == "win32":
            paths.extend([
                Path("D:/github/digitalmodel"),
                Path("C:/github/digitalmodel"),
                Path("D:/git/digitalmodel"),
                Path("C:/git/digitalmodel"),
            ])
        
        # Unix/Linux/Mac paths
        paths.extend([
            Path.home() / "github" / "digitalmodel",
            Path.home() / "git" / "digitalmodel",
            Path.home() / "projects" / "digitalmodel",
            Path.home() / "work" / "digitalmodel",
            Path("/opt/digitalmodel"),
            Path("/usr/local/digitalmodel"),
        ])
        
        return paths
    
    def resolve_path(self, path_input: Union[str, Path, None]) -> Path:
        """
        Resolve a path input to an absolute Path object.
        
        Args:
            path_input: String path, Path object, or None
        
        Returns:
            Resolved absolute Path
        """
        if path_input is None:
            return self.current_dir
        
        path = Path(path_input)
        
        # If already absolute, return it
        if path.is_absolute():
            return path.resolve()
        
        # Try relative to current directory first
        relative_to_cwd = self.current_dir / path
        if relative_to_cwd.exists():
            return relative_to_cwd.resolve()
        
        # Try relative to repo root if available
        if self.repo_root:
            relative_to_repo = self.repo_root / path
            if relative_to_repo.exists():
                return relative_to_repo.resolve()
        
        # Return the path resolved from current directory as fallback
        return relative_to_cwd.resolve()
    
    def resolve_model_path(self, model_ref: str) -> Path:
        """
        Resolve a model reference to an absolute path.
        
        Handles various reference formats:
        - Absolute paths: /path/to/model.yml
        - Relative paths: ./models/model.yml
        - Repo paths: @repo/path/to/model.yml
        - Name only: model.yml (searches common locations)
        
        Args:
            model_ref: Model reference string
        
        Returns:
            Resolved Path to model file
        """
        # Handle repo-relative paths
        if model_ref.startswith("@repo/"):
            if self.repo_root:
                return self.repo_root / model_ref[6:]
            else:
                raise ValueError("Cannot resolve @repo path: repository root not found")
        
        # Handle absolute paths
        if os.path.isabs(model_ref):
            return Path(model_ref).resolve()
        
        path = Path(model_ref)
        
        # Try relative to current directory
        if (self.current_dir / path).exists():
            return (self.current_dir / path).resolve()
        
        # Try relative to repo root
        if self.repo_root and (self.repo_root / path).exists():
            return (self.repo_root / path).resolve()
        
        # Search common model directories
        search_dirs = self._get_model_search_dirs()
        for search_dir in search_dirs:
            potential_path = search_dir / path
            if potential_path.exists():
                return potential_path.resolve()
        
        # If not found, return path relative to current directory
        return (self.current_dir / path).resolve()
    
    def _get_model_search_dirs(self) -> List[Path]:
        """Get directories to search for models."""
        dirs = []
        
        # Current directory subdirectories
        dirs.extend([
            self.current_dir / "models",
            self.current_dir / "tests",
            self.current_dir / "examples",
        ])
        
        # Repo directories if available
        if self.repo_root:
            dirs.extend([
                self.repo_root / "tests" / "modules" / "orcaflex",
                self.repo_root / "examples",
                self.repo_root / "models",
            ])
        
        return [d for d in dirs if d.exists()]
    
    def validate_path(self, path: Path, must_exist: bool = False) -> bool:
        """
        Validate a path for security and existence.
        
        Args:
            path: Path to validate
            must_exist: If True, path must exist
        
        Returns:
            True if path is valid
        """
        try:
            # Resolve to absolute path to check for directory traversal
            resolved = path.resolve()
            
            # Check if path exists if required
            if must_exist and not resolved.exists():
                logger.warning(f"Path does not exist: {resolved}")
                return False
            
            # Basic security check - ensure path doesn't try to escape to system directories
            if sys.platform == "win32":
                # Windows system directories
                system_dirs = [
                    Path("C:/Windows"),
                    Path("C:/Program Files"),
                    Path("C:/Program Files (x86)"),
                ]
            else:
                # Unix system directories
                system_dirs = [
                    Path("/etc"),
                    Path("/usr/bin"),
                    Path("/bin"),
                    Path("/sbin"),
                ]
            
            for sys_dir in system_dirs:
                if sys_dir.exists() and resolved.is_relative_to(sys_dir):
                    logger.warning(f"Path points to system directory: {resolved}")
                    return False
            
            return True
            
        except Exception as e:
            logger.error(f"Path validation error: {e}")
            return False
    
    def ensure_directory(self, path: Path) -> Path:
        """
        Ensure a directory exists, creating it if necessary.
        
        Args:
            path: Directory path
        
        Returns:
            Path object for the directory
        """
        path = self.resolve_path(path)
        path.mkdir(parents=True, exist_ok=True)
        return path
    
    def get_relative_path(self, path: Path, base: Optional[Path] = None) -> Path:
        """
        Get relative path from base directory.
        
        Args:
            path: Path to make relative
            base: Base directory (default: current directory)
        
        Returns:
            Relative path
        """
        if base is None:
            base = self.current_dir
        
        try:
            return path.relative_to(base)
        except ValueError:
            # Paths don't share a common base
            return path