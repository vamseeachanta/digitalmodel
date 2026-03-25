"""
Pattern analyzer for Create Go-By Folder Tool
"""

import re
from pathlib import Path
from typing import Dict, List, Set, Any
from collections import defaultdict
import logging

logger = logging.getLogger(__name__)


class PatternAnalyzer:
    """Analyze file patterns and variations."""

    def __init__(self):
        """Initialize pattern analyzer."""
        self.patterns = defaultdict(list)
        self.parameter_patterns = {}
        self.naming_conventions = set()
        self.files: List[Path] = []
        self._file_groups: Dict[str, List[Path]] = {}

    def add_file(self, file_path: Path) -> None:
        """
        Add a file to the analyzer.

        Args:
            file_path: Path to file to add
        """
        self.files.append(file_path)
        
    def detect_patterns(self, files: List[Dict]) -> Dict:
        """
        Detect naming patterns and variations.
        
        Args:
            files: List of file information dictionaries
            
        Returns:
            Dictionary with detected patterns
        """
        # Group files by directory and base pattern
        for file_info in files:
            pattern = self.extract_pattern(file_info['relative_path'].name)
            self.patterns[pattern].append(file_info)
        
        # Detect parameter variations
        self.detect_parameter_patterns()
        
        # Detect naming conventions
        self.detect_naming_conventions(files)
        
        return {
            'file_patterns': dict(self.patterns),
            'parameter_patterns': self.parameter_patterns,
            'naming_conventions': list(self.naming_conventions),
            'variation_count': self.count_variations()
        }
    
    def extract_pattern(self, filename: str) -> str:
        """
        Extract base pattern from filename.
        
        Args:
            filename: File name
            
        Returns:
            Base pattern string
        """
        # Replace numbers with placeholders
        pattern = re.sub(r'\d+', '{N}', filename)
        
        # Replace common parameter patterns
        pattern = re.sub(r'_(low|medium|high)', '_{LEVEL}', pattern)
        pattern = re.sub(r'_v\d+', '_v{VERSION}', pattern)
        pattern = re.sub(r'_\d{4}-\d{2}-\d{2}', '_{DATE}', pattern)
        
        return pattern
    
    def detect_parameter_patterns(self):
        """Identify parameter sweep patterns."""
        for pattern, files in self.patterns.items():
            if len(files) > 5:  # Likely a parameter sweep
                self.analyze_parameter_variation(pattern, files)
    
    def analyze_parameter_variation(self, pattern: str, files: List[Dict]):
        """
        Analyze parameter variations in a group of files.
        
        Args:
            pattern: Base pattern
            files: List of files matching pattern
        """
        # Extract varying parts
        filenames = [f['relative_path'].name for f in files]
        
        # Find numeric sequences
        numeric_parts = self.find_numeric_sequences(filenames)
        if numeric_parts:
            self.parameter_patterns[pattern] = {
                'type': 'numeric_sequence',
                'values': numeric_parts
            }
    
    def find_numeric_sequences(self, filenames: List[str]) -> List[int]:
        """
        Find numeric sequences in filenames.
        
        Args:
            filenames: List of filenames
            
        Returns:
            List of numeric values found
        """
        numbers = []
        for filename in filenames:
            matches = re.findall(r'\d+', filename)
            for match in matches:
                try:
                    numbers.append(int(match))
                except ValueError:
                    pass
        
        return sorted(set(numbers))
    
    def detect_naming_conventions(self, files: List[Dict] = None) -> Dict[str, List[Any]]:
        """
        Detect naming conventions used.

        Args:
            files: List of file information (optional, uses self.files if None)

        Returns:
            Dictionary mapping convention names to files/file_info
        """
        if files is None:
            # Use self.files (Path objects)
            return self.detect_naming_conventions_simple()

        # Original behavior for dict-based file info
        for file_info in files:
            name = file_info['stem']

            # Check for camelCase
            if re.match(r'^[a-z]+[A-Z]', name):
                self.naming_conventions.add('camelCase')

            # Check for snake_case
            if '_' in name and name.islower():
                self.naming_conventions.add('snake_case')

            # Check for kebab-case
            if '-' in name:
                self.naming_conventions.add('kebab-case')

            # Check for UPPER_CASE
            if '_' in name and name.isupper():
                self.naming_conventions.add('UPPER_CASE')

        return {conv: [] for conv in self.naming_conventions}
    
    def count_variations(self) -> int:
        """
        Count total variations found.

        Returns:
            Total number of variations
        """
        return sum(len(files) for files in self.patterns.values())

    def detect_numeric_sequences(self) -> List[Dict]:
        """
        Detect numeric sequences in added files.

        Returns:
            List of detected sequence patterns
        """
        if len(self.files) < 2:
            return []

        # Group files by base pattern (numbers replaced)
        patterns: Dict[str, List[Path]] = defaultdict(list)
        for f in self.files:
            base = re.sub(r'\d+', '{N}', f.name.lower())
            patterns[base].append(f)

        sequences = []
        for pattern, files in patterns.items():
            if len(files) >= 2:
                # Extract numbers from each file
                nums = []
                for f in files:
                    matches = re.findall(r'\d+', f.name)
                    if matches:
                        nums.extend(int(m) for m in matches)
                if nums:
                    sequences.append({
                        'pattern': pattern,
                        'files': files,
                        'values': sorted(set(nums))
                    })

        return sequences

    def detect_naming_conventions_simple(self) -> Dict[str, List[Path]]:
        """
        Detect naming conventions in added files.

        Returns:
            Dictionary mapping convention names to files using that convention
        """
        conventions: Dict[str, List[Path]] = {}

        for f in self.files:
            name = f.stem

            # Check for camelCase
            if re.match(r'^[a-z]+[A-Z]', name):
                conventions.setdefault('camelCase', []).append(f)

            # Check for snake_case
            if '_' in name and name.replace('_', '').islower():
                conventions.setdefault('snake_case', []).append(f)

            # Check for kebab-case
            if '-' in name:
                conventions.setdefault('kebab-case', []).append(f)

            # Check for UPPER_CASE
            if '_' in name and name.isupper():
                conventions.setdefault('UPPER_CASE', []).append(f)

        return conventions

    def detect_parameter_variations(self) -> Dict[str, List[Any]]:
        """
        Detect parameter variations in filenames (e.g., temp=100, pressure=2.0).

        Returns:
            Dictionary mapping parameter names to their values
        """
        if not self.files:
            return {}

        variations: Dict[str, Set] = defaultdict(set)

        for f in self.files:
            # Match patterns like _param=value or param=value at start
            # The underscore before param name indicates it's a delimiter
            matches = re.findall(r'(?:^|_)([a-zA-Z][a-zA-Z0-9]*)=([^_\s.]+)', f.name)
            for param, value in matches:
                # Try to convert to number
                try:
                    if '.' in value:
                        variations[param].add(float(value))
                    else:
                        variations[param].add(int(value))
                except ValueError:
                    variations[param].add(value)

        # Convert sets to sorted lists
        return {k: sorted(v, key=lambda x: (isinstance(x, str), x))
                for k, v in variations.items()}

    def detect_file_groups(self) -> Dict[str, List[Path]]:
        """
        Detect file groups by directory or pattern.

        Returns:
            Dictionary mapping group names to files
        """
        if not self.files:
            return {}

        groups: Dict[str, List[Path]] = defaultdict(list)

        for f in self.files:
            # Group by parent directory
            parent = str(f.parent) if f.parent != Path('.') else 'root'
            groups[parent].append(f)

        # Only return groups with multiple files
        return {k: v for k, v in groups.items() if len(v) >= 1}