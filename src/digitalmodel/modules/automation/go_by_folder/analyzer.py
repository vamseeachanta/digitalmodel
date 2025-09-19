"""
Pattern analyzer for Create Go-By Folder Tool
"""

import re
from pathlib import Path
from typing import Dict, List, Set
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
    
    def detect_naming_conventions(self, files: List[Dict]):
        """
        Detect naming conventions used.
        
        Args:
            files: List of file information
        """
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
    
    def count_variations(self) -> int:
        """
        Count total variations found.
        
        Returns:
            Total number of variations
        """
        return sum(len(files) for files in self.patterns.values())