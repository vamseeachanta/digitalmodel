"""
Model Discovery System
======================

Discover and filter OrcaFlex models based on patterns and criteria.
"""

import re
import logging
from pathlib import Path
from typing import List, Optional, Union, Set
from fnmatch import fnmatch

logger = logging.getLogger(__name__)


class ModelDiscovery:
    """
    Discover and filter OrcaFlex models based on patterns.
    
    Supports:
    - Glob patterns (*.yml, fsts_*.dat)
    - Regex patterns for advanced matching
    - Recursive directory searching
    - Exclusion patterns
    - Model validation
    """
    
    # Known OrcaFlex file extensions
    ORCAFLEX_EXTENSIONS = {'.yml', '.yaml', '.dat', '.sim'}
    
    # Patterns to exclude by default
    DEFAULT_EXCLUDE_PATTERNS = [
        '*includefile*',
        '*backup*',
        '*_old*',
        '*_temp*',
        '*.tmp',
        '*.bak',
    ]
    
    def __init__(self):
        """Initialize the model discovery system."""
        self.discovered_models = []
        self.excluded_models = []
        
    def find_models(self,
                   directory: Union[str, Path],
                   pattern: str = "*.yml",
                   recursive: bool = False,
                   exclude_patterns: Optional[List[str]] = None,
                   validate: bool = True) -> List[Path]:
        """
        Find models matching criteria in a directory.
        
        Args:
            directory: Directory to search
            pattern: Glob or regex pattern to match
            recursive: Search subdirectories recursively
            exclude_patterns: Additional patterns to exclude
            validate: Validate if files are valid OrcaFlex models
        
        Returns:
            List of Path objects for discovered models
        """
        directory = Path(directory)
        
        if not directory.exists():
            logger.warning(f"Directory does not exist: {directory}")
            return []
        
        if not directory.is_dir():
            logger.warning(f"Path is not a directory: {directory}")
            return []
        
        # Combine default and custom exclude patterns
        all_exclude_patterns = self.DEFAULT_EXCLUDE_PATTERNS.copy()
        if exclude_patterns:
            all_exclude_patterns.extend(exclude_patterns)
        
        # Reset discovery lists
        self.discovered_models = []
        self.excluded_models = []
        
        # Determine if pattern is regex or glob
        is_regex = self._is_regex_pattern(pattern)
        
        # Find matching files
        if recursive:
            models = self._find_recursive(directory, pattern, is_regex)
        else:
            models = self._find_non_recursive(directory, pattern, is_regex)
        
        # Apply exclusion patterns
        filtered_models = []
        for model in models:
            if self._should_exclude(model, all_exclude_patterns):
                self.excluded_models.append(model)
                logger.debug(f"Excluded: {model.name}")
            else:
                filtered_models.append(model)
        
        # Validate models if requested
        if validate:
            valid_models = []
            for model in filtered_models:
                if self.validate_model(model):
                    valid_models.append(model)
                    self.discovered_models.append(model)
                else:
                    self.excluded_models.append(model)
                    logger.debug(f"Invalid model: {model.name}")
            filtered_models = valid_models
        else:
            self.discovered_models = filtered_models
        
        # Sort by name for consistent ordering
        filtered_models.sort(key=lambda p: p.name)
        
        logger.info(f"Found {len(filtered_models)} models matching '{pattern}'")
        if self.excluded_models:
            logger.info(f"Excluded {len(self.excluded_models)} files")
        
        return filtered_models
    
    def _is_regex_pattern(self, pattern: str) -> bool:
        """Determine if a pattern is regex or glob."""
        # Common glob patterns - treat as glob
        if pattern in ['*', '*.yml', '*.yaml', '*.dat', '*.sim', '**/*']:
            return False
        
        # Check for glob-only patterns
        if all(c in '*?[]' or c.isalnum() or c in '._-/' for c in pattern):
            # Looks like a simple glob pattern
            return False
        
        # Regex indicators
        regex_indicators = ['^', '$', '\\d', '\\w', '\\s', '.+', '.*', '|', '(', ')']
        for indicator in regex_indicators:
            if indicator in pattern:
                return True
        
        return False
    
    def _find_recursive(self, directory: Path, pattern: str, is_regex: bool) -> List[Path]:
        """Find files recursively matching pattern."""
        models = []
        
        if is_regex:
            regex = re.compile(pattern)
            for path in directory.rglob('*'):
                if path.is_file() and regex.match(path.name):
                    models.append(path)
        else:
            # Use glob pattern
            for path in directory.rglob(pattern):
                if path.is_file():
                    models.append(path)
        
        return models
    
    def _find_non_recursive(self, directory: Path, pattern: str, is_regex: bool) -> List[Path]:
        """Find files in directory (non-recursive) matching pattern."""
        models = []
        
        if is_regex:
            regex = re.compile(pattern)
            for path in directory.iterdir():
                if path.is_file() and regex.match(path.name):
                    models.append(path)
        else:
            # Use glob pattern
            for path in directory.glob(pattern):
                if path.is_file():
                    models.append(path)
        
        return models
    
    def _should_exclude(self, path: Path, exclude_patterns: List[str]) -> bool:
        """Check if a file should be excluded based on patterns."""
        for pattern in exclude_patterns:
            if fnmatch(path.name.lower(), pattern.lower()):
                return True
            # Also check against full path
            if fnmatch(str(path).lower(), pattern.lower()):
                return True
        return False
    
    def validate_model(self, model_path: Path) -> bool:
        """
        Validate if a file is a valid OrcaFlex model.
        
        Args:
            model_path: Path to model file
        
        Returns:
            True if file appears to be a valid OrcaFlex model
        """
        # Check extension
        if model_path.suffix.lower() not in self.ORCAFLEX_EXTENSIONS:
            return False
        
        # Check file size (OrcaFlex files shouldn't be empty)
        if not model_path.exists() or model_path.stat().st_size == 0:
            return False
        
        # For more thorough validation, check file content
        try:
            if model_path.suffix.lower() in {'.yml', '.yaml'}:
                return self._validate_yaml_model(model_path)
            elif model_path.suffix.lower() == '.dat':
                return self._validate_dat_model(model_path)
            elif model_path.suffix.lower() == '.sim':
                return self._validate_sim_model(model_path)
        except Exception as e:
            logger.debug(f"Validation error for {model_path.name}: {e}")
            return False
        
        return True
    
    def _validate_yaml_model(self, model_path: Path) -> bool:
        """Validate YAML model file."""
        try:
            import yaml
            with open(model_path, 'r') as f:
                data = yaml.safe_load(f)
            
            # Check for OrcaFlex-specific keys
            orcaflex_indicators = [
                'General', 'Environment', 'BaseFile',
                'UnitsSystem', 'Vessels', 'Lines', 'Winches'
            ]
            
            if isinstance(data, dict):
                # Check if any OrcaFlex indicators are present
                return any(key in data for key in orcaflex_indicators)
            
            return False
            
        except Exception:
            return False
    
    def _validate_dat_model(self, model_path: Path) -> bool:
        """Validate DAT model file."""
        try:
            # Read first few lines to check for OrcaFlex markers
            with open(model_path, 'r', errors='ignore') as f:
                first_lines = [f.readline() for _ in range(10)]
            
            # Look for OrcaFlex indicators
            indicators = ['OrcaFlex', 'Orcina', 'General Data', 'Environment']
            content = ' '.join(first_lines)
            
            return any(indicator in content for indicator in indicators)
            
        except Exception:
            return False
    
    def _validate_sim_model(self, model_path: Path) -> bool:
        """Validate SIM model file."""
        # .sim files are binary, just check they exist and aren't empty
        return model_path.exists() and model_path.stat().st_size > 100
    
    def find_related_models(self, base_model: Path) -> List[Path]:
        """
        Find models related to a base model.
        
        Looks for models with similar names or in the same directory.
        
        Args:
            base_model: Base model path
        
        Returns:
            List of related model paths
        """
        related = []
        base_stem = base_model.stem
        base_dir = base_model.parent
        
        # Look for variations of the base model
        patterns = [
            f"{base_stem}*.yml",
            f"{base_stem}*.dat",
            f"*{base_stem}*.yml",
            f"*{base_stem}*.dat",
        ]
        
        for pattern in patterns:
            for path in base_dir.glob(pattern):
                if path != base_model and path.is_file():
                    if self.validate_model(path):
                        related.append(path)
        
        return related
    
    def group_models_by_pattern(self, models: List[Path]) -> dict:
        """
        Group models by common patterns in their names.
        
        Args:
            models: List of model paths
        
        Returns:
            Dictionary with pattern groups
        """
        groups = {}
        
        # Common grouping patterns
        patterns = [
            (r'fsts_.*', 'FSTS Models'),
            (r'.*_test.*', 'Test Models'),
            (r'.*_6dof.*', '6DOF Models'),
            (r'.*mooring.*', 'Mooring Models'),
            (r'.*vessel.*', 'Vessel Models'),
        ]
        
        # Group models
        for model in models:
            grouped = False
            for pattern, group_name in patterns:
                if re.match(pattern, model.name, re.IGNORECASE):
                    if group_name not in groups:
                        groups[group_name] = []
                    groups[group_name].append(model)
                    grouped = True
                    break
            
            if not grouped:
                if 'Other' not in groups:
                    groups['Other'] = []
                groups['Other'].append(model)
        
        return groups
    
    def get_discovery_stats(self) -> dict:
        """Get statistics about the last discovery operation."""
        return {
            'total_discovered': len(self.discovered_models),
            'total_excluded': len(self.excluded_models),
            'by_extension': self._count_by_extension(self.discovered_models),
            'by_directory': self._count_by_directory(self.discovered_models),
        }
    
    def _count_by_extension(self, models: List[Path]) -> dict:
        """Count models by file extension."""
        counts = {}
        for model in models:
            ext = model.suffix.lower()
            counts[ext] = counts.get(ext, 0) + 1
        return counts
    
    def _count_by_directory(self, models: List[Path]) -> dict:
        """Count models by directory."""
        counts = {}
        for model in models:
            dir_name = model.parent.name
            counts[dir_name] = counts.get(dir_name, 0) + 1
        return counts