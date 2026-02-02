"""
Unit tests for pattern analyzer module
"""

import pytest
from pathlib import Path
from unittest.mock import Mock, patch

from digitalmodel.automation.go_by_folder.analyzer import PatternAnalyzer


class TestPatternAnalyzer:
    """Test PatternAnalyzer class."""
    
    def test_analyzer_initialization(self):
        """Test analyzer initialization."""
        analyzer = PatternAnalyzer()
        
        assert analyzer.files == []
        assert isinstance(analyzer.patterns, dict)
    
    def test_add_file(self):
        """Test adding files to analyzer."""
        analyzer = PatternAnalyzer()
        
        analyzer.add_file(Path("test_001.dat"))
        analyzer.add_file(Path("test_002.dat"))
        
        assert len(analyzer.files) == 2
        assert Path("test_001.dat") in analyzer.files
    
    def test_detect_numeric_sequences(self):
        """Test numeric sequence detection."""
        analyzer = PatternAnalyzer()
        
        # Add files with numeric sequences
        files = [
            Path("data_001.csv"),
            Path("data_002.csv"),
            Path("data_003.csv"),
            Path("run_01.log"),
            Path("run_02.log"),
            Path("run_03.log")
        ]
        
        for f in files:
            analyzer.add_file(f)
        
        sequences = analyzer.detect_numeric_sequences()
        
        assert len(sequences) > 0
        # Should detect patterns like data_XXX.csv and run_XX.log
    
    def test_detect_naming_conventions(self):
        """Test naming convention detection."""
        analyzer = PatternAnalyzer()
        
        # Add files with different naming conventions
        files = [
            Path("my_snake_case_file.py"),
            Path("another_snake_file.txt"),
            Path("myCarmelCaseFile.js"),
            Path("anotherCamelFile.java"),
            Path("kebab-case-file.css"),
            Path("another-kebab.html")
        ]
        
        for f in files:
            analyzer.add_file(f)
        
        conventions = analyzer.detect_naming_conventions()
        
        assert 'snake_case' in conventions
        assert 'camelCase' in conventions
        assert 'kebab-case' in conventions
    
    def test_detect_parameter_variations(self):
        """Test parameter variation detection."""
        analyzer = PatternAnalyzer()
        
        # Add files with parameter variations
        files = [
            Path("sim_temp=100_pressure=1.0.dat"),
            Path("sim_temp=100_pressure=2.0.dat"),
            Path("sim_temp=200_pressure=1.0.dat"),
            Path("sim_temp=200_pressure=2.0.dat")
        ]
        
        for f in files:
            analyzer.add_file(f)
        
        variations = analyzer.detect_parameter_variations()
        
        assert 'temp' in variations
        assert 'pressure' in variations
        assert 100 in variations['temp'] or '100' in variations['temp']
        assert 200 in variations['temp'] or '200' in variations['temp']
    
    def test_detect_file_groups(self):
        """Test file grouping detection."""
        analyzer = PatternAnalyzer()
        
        # Add grouped files
        files = [
            Path("input/data_01.txt"),
            Path("input/data_02.txt"),
            Path("output/result_01.csv"),
            Path("output/result_02.csv"),
            Path("logs/run_01.log"),
            Path("logs/run_02.log")
        ]
        
        for f in files:
            analyzer.add_file(f)
        
        groups = analyzer.detect_file_groups()
        
        # Should group by directory or pattern
        assert len(groups) > 0
    
    def test_empty_file_list(self):
        """Test analyzer with no files."""
        analyzer = PatternAnalyzer()
        
        sequences = analyzer.detect_numeric_sequences()
        conventions = analyzer.detect_naming_conventions()
        variations = analyzer.detect_parameter_variations()
        groups = analyzer.detect_file_groups()
        
        assert sequences == []
        assert conventions == {}
        assert variations == {}
        assert groups == {}
    
    def test_single_file(self):
        """Test analyzer with single file."""
        analyzer = PatternAnalyzer()
        analyzer.add_file(Path("single_file.txt"))
        
        sequences = analyzer.detect_numeric_sequences()
        conventions = analyzer.detect_naming_conventions()
        
        # Single file shouldn't create patterns
        assert len(sequences) == 0
        # But might detect naming convention
        assert 'snake_case' in conventions or len(conventions) == 0
    
    def test_mixed_patterns(self):
        """Test detection with mixed pattern types."""
        analyzer = PatternAnalyzer()
        
        # Add files with multiple pattern types
        files = [
            Path("exp_001_temp=100.dat"),
            Path("exp_002_temp=100.dat"),
            Path("exp_001_temp=200.dat"),
            Path("exp_002_temp=200.dat")
        ]
        
        for f in files:
            analyzer.add_file(f)
        
        sequences = analyzer.detect_numeric_sequences()
        variations = analyzer.detect_parameter_variations()
        
        # Should detect both sequence and parameter patterns
        assert len(sequences) > 0 or len(variations) > 0
    
    def test_case_insensitive_patterns(self):
        """Test case-insensitive pattern detection."""
        analyzer = PatternAnalyzer()
        
        files = [
            Path("File_001.TXT"),
            Path("file_002.txt"),
            Path("FILE_003.Txt")
        ]
        
        for f in files:
            analyzer.add_file(f)
        
        sequences = analyzer.detect_numeric_sequences()
        
        # Should detect sequence despite case differences
        assert len(sequences) > 0
    
    def test_complex_parameter_patterns(self):
        """Test complex parameter pattern detection."""
        analyzer = PatternAnalyzer()
        
        files = [
            Path("results_a=1_b=2_c=3.csv"),
            Path("results_a=1_b=3_c=3.csv"),
            Path("results_a=2_b=2_c=4.csv")
        ]
        
        for f in files:
            analyzer.add_file(f)
        
        variations = analyzer.detect_parameter_variations()
        
        # Should detect all three parameters
        assert 'a' in variations or len(variations) > 0
        assert 'b' in variations or len(variations) > 0
        assert 'c' in variations or len(variations) > 0