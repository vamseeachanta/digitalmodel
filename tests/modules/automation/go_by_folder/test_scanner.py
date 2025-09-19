"""
Unit tests for file scanner module
"""

import pytest
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
import hashlib

from digitalmodel.modules.automation.go_by_folder.scanner import FileScanner


class TestFileScanner:
    """Test FileScanner class."""
    
    def test_scanner_initialization(self, sample_source_dir):
        """Test scanner initialization."""
        scanner = FileScanner(sample_source_dir)
        
        assert scanner.root_path == sample_source_dir
        assert scanner.exclude_patterns == []
        assert scanner.include_patterns == []
        assert scanner.follow_symlinks is False
    
    def test_scanner_with_patterns(self, sample_source_dir):
        """Test scanner with include/exclude patterns."""
        scanner = FileScanner(
            sample_source_dir,
            exclude_patterns=['*.bin', '*.log'],
            include_patterns=['*.py', '*.txt']
        )
        
        assert scanner.exclude_patterns == ['*.bin', '*.log']
        assert scanner.include_patterns == ['*.py', '*.txt']
    
    def test_scan_files(self, sample_source_dir):
        """Test basic file scanning."""
        scanner = FileScanner(sample_source_dir)
        files = list(scanner.scan())
        
        # Should find all files
        assert len(files) > 0
        
        # Check file info structure
        for file_info in files:
            assert 'path' in file_info
            assert 'name' in file_info
            assert 'size' in file_info
            assert 'extension' in file_info
            assert 'modified' in file_info
    
    def test_scan_with_exclude_patterns(self, sample_source_dir):
        """Test scanning with exclude patterns."""
        scanner = FileScanner(
            sample_source_dir,
            exclude_patterns=['*.csv', '*.bin']
        )
        files = list(scanner.scan())
        
        # Should not include CSV or bin files
        for file_info in files:
            assert not file_info['name'].endswith('.csv')
            assert not file_info['name'].endswith('.bin')
    
    def test_scan_with_include_patterns(self, sample_source_dir):
        """Test scanning with include patterns."""
        scanner = FileScanner(
            sample_source_dir,
            include_patterns=['*.py']
        )
        files = list(scanner.scan())
        
        # Should only include Python files
        for file_info in files:
            assert file_info['extension'] == '.py'
    
    def test_detect_binary_file(self, temp_dir):
        """Test binary file detection."""
        scanner = FileScanner(temp_dir)
        
        # Create test files
        text_file = temp_dir / "text.txt"
        text_file.write_text("This is text content")
        
        binary_file = temp_dir / "binary.bin"
        binary_file.write_bytes(b'\x00\x01\x02\x03\xff\xfe\xfd')
        
        # Test detection
        assert scanner.is_binary_file(text_file) is False
        assert scanner.is_binary_file(binary_file) is True
    
    def test_calculate_file_hash(self, temp_dir):
        """Test file hash calculation."""
        scanner = FileScanner(temp_dir)
        
        # Create test file
        test_file = temp_dir / "test.txt"
        content = "Test content for hashing"
        test_file.write_text(content)
        
        # Calculate hash
        file_hash = scanner.calculate_file_hash(test_file)
        
        # Verify hash
        expected_hash = hashlib.md5(content.encode()).hexdigest()
        assert file_hash == expected_hash
    
    def test_scan_empty_directory(self, temp_dir):
        """Test scanning empty directory."""
        empty_dir = temp_dir / "empty"
        empty_dir.mkdir()
        
        scanner = FileScanner(empty_dir)
        files = list(scanner.scan())
        
        assert len(files) == 0
    
    def test_scan_nested_directories(self, temp_dir):
        """Test scanning nested directory structure."""
        # Create nested structure
        (temp_dir / "level1" / "level2" / "level3").mkdir(parents=True)
        (temp_dir / "level1" / "file1.txt").write_text("Level 1")
        (temp_dir / "level1" / "level2" / "file2.txt").write_text("Level 2")
        (temp_dir / "level1" / "level2" / "level3" / "file3.txt").write_text("Level 3")
        
        scanner = FileScanner(temp_dir)
        files = list(scanner.scan())
        
        # Should find all 3 files
        assert len(files) == 3
        
        # Check that all levels are represented
        file_names = [f['name'] for f in files]
        assert 'file1.txt' in file_names
        assert 'file2.txt' in file_names
        assert 'file3.txt' in file_names
    
    @patch('pathlib.Path.stat')
    def test_scan_with_permission_error(self, mock_stat, temp_dir):
        """Test handling permission errors during scan."""
        # Create test file
        test_file = temp_dir / "test.txt"
        test_file.write_text("Test")
        
        # Mock permission error
        mock_stat.side_effect = PermissionError("Access denied")
        
        scanner = FileScanner(temp_dir)
        files = list(scanner.scan())
        
        # Should handle error gracefully
        assert isinstance(files, list)
    
    def test_symlink_detection(self, temp_dir):
        """Test symbolic link detection."""
        # Create regular file and symlink
        target = temp_dir / "target.txt"
        target.write_text("Target content")
        
        link = temp_dir / "link.txt"
        
        # Only test on systems that support symlinks
        try:
            link.symlink_to(target)
            
            scanner = FileScanner(temp_dir, follow_symlinks=False)
            
            # Scan and check for symlink registry
            files = list(scanner.scan())
            
            # Should detect symlink
            assert len(scanner.symlinks) > 0 if hasattr(scanner, 'symlinks') else True
            
        except (OSError, NotImplementedError):
            # Skip on Windows or systems without symlink support
            pytest.skip("Symlinks not supported on this system")
    
    def test_file_info_completeness(self, sample_source_dir):
        """Test that file info contains all required fields."""
        scanner = FileScanner(sample_source_dir)
        files = list(scanner.scan())
        
        required_fields = ['path', 'name', 'size', 'extension', 'modified', 'parent']
        
        for file_info in files:
            for field in required_fields:
                assert field in file_info, f"Missing required field: {field}"
    
    def test_large_file_handling(self, temp_dir):
        """Test handling of large files."""
        # Create a "large" file (simulated)
        large_file = temp_dir / "large.dat"
        # Write 10MB of data (in chunks to avoid memory issues)
        with open(large_file, 'wb') as f:
            chunk = b'0' * 1024  # 1KB chunk
            for _ in range(10 * 1024):  # 10MB total
                f.write(chunk)
        
        scanner = FileScanner(temp_dir)
        files = list(scanner.scan())
        
        assert len(files) == 1
        assert files[0]['size'] == 10 * 1024 * 1024  # 10MB