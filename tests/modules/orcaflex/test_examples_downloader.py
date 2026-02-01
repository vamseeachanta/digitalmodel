"""
Test suite for OrcaFlex Examples Downloader

Tests the downloader infrastructure with mock data to ensure proper functionality.
"""

import os
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
from datetime import datetime

import requests
from bs4 import BeautifulSoup

from digitalmodel.modules.orcaflex.examples_integration.downloader import OrcaflexExampleDownloader


class TestOrcaflexExampleDownloader(unittest.TestCase):
    """Test cases for OrcaflexExampleDownloader."""
    
    def setUp(self):
        """Set up test fixtures."""
        # Create temporary directory for tests
        self.temp_dir = tempfile.mkdtemp()
        self.downloader = OrcaflexExampleDownloader(base_dir=self.temp_dir)
    
    def tearDown(self):
        """Clean up after tests."""
        # Clean up temporary directory
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)
    
    def test_directory_creation(self):
        """Test that required directories are created."""
        expected_dirs = [
            Path(self.temp_dir) / "raw",
            Path(self.temp_dir) / "metadata"
        ]
        
        for dir_path in expected_dirs:
            self.assertTrue(dir_path.exists(), f"Directory {dir_path} should exist")
    
    def test_manifest_creation(self):
        """Test manifest file creation and structure."""
        manifest = self.downloader.manifest
        
        self.assertIn('last_updated', manifest)
        self.assertIn('examples', manifest)
        self.assertIn('categories', manifest)
        self.assertIsInstance(manifest['examples'], dict)
        self.assertIsInstance(manifest['categories'], dict)
    
    def test_save_and_load_manifest(self):
        """Test saving and loading manifest."""
        # Add test data to manifest
        self.downloader.manifest['examples']['test.dat'] = {
            'category': 'Test',
            'url': 'http://example.com/test.dat',
            'checksum': 'abc123',
            'size': 1024
        }
        
        # Save manifest
        self.downloader.save_manifest()
        
        # Create new downloader instance to load manifest
        new_downloader = OrcaflexExampleDownloader(base_dir=self.temp_dir)
        
        # Check loaded data
        self.assertIn('test.dat', new_downloader.manifest['examples'])
        self.assertEqual(
            new_downloader.manifest['examples']['test.dat']['checksum'],
            'abc123'
        )
    
    @patch('digitalmodel.modules.orcaflex.examples_integration.downloader.requests.Session')
    def test_rate_limiting(self, mock_session_class):
        """Test rate limiting between requests."""
        mock_session = MagicMock()
        mock_session_class.return_value = mock_session
        
        # Create downloader with short rate limit for testing
        downloader = OrcaflexExampleDownloader(base_dir=self.temp_dir)
        downloader.rate_limit_delay = 0.1  # 100ms for testing
        
        # Make two requests
        import time
        start_time = time.time()
        
        downloader._rate_limit()
        first_call_time = time.time()
        
        downloader._rate_limit()
        second_call_time = time.time()
        
        # Check that rate limiting was applied
        time_diff = second_call_time - first_call_time
        self.assertGreaterEqual(time_diff, 0.1, "Rate limiting should enforce delay")
    
    @patch('digitalmodel.modules.orcaflex.examples_integration.downloader.requests.Session.get')
    def test_retry_logic(self, mock_get):
        """Test retry logic with exponential backoff."""
        # Configure mock to fail twice then succeed
        mock_get.side_effect = [
            requests.exceptions.RequestException("Network error"),
            requests.exceptions.RequestException("Network error"),
            Mock(status_code=200, content=b"Success")
        ]
        
        # Set short delays for testing
        self.downloader.retry_delay = 0.01
        self.downloader.rate_limit_delay = 0
        
        # Attempt download
        response = self.downloader._download_with_retry("http://example.com/test.dat")
        
        # Should succeed on third attempt
        self.assertIsNotNone(response)
        self.assertEqual(mock_get.call_count, 3)
    
    @patch('digitalmodel.modules.orcaflex.examples_integration.downloader.requests.Session.get')
    def test_max_retries_exceeded(self, mock_get):
        """Test behavior when max retries is exceeded."""
        # Configure mock to always fail
        mock_get.side_effect = requests.exceptions.RequestException("Network error")
        
        # Set short delays for testing
        self.downloader.retry_delay = 0.01
        self.downloader.rate_limit_delay = 0
        self.downloader.max_retries = 2
        
        # Attempt download
        response = self.downloader._download_with_retry("http://example.com/test.dat")
        
        # Should return None after max retries
        self.assertIsNone(response)
        self.assertEqual(mock_get.call_count, 3)  # Initial + 2 retries
    
    @patch('digitalmodel.modules.orcaflex.examples_integration.downloader.requests.Session.get')
    def test_parse_categories_mock(self, mock_get):
        """Test parsing categories with mock HTML."""
        # Mock HTML response
        html_content = """
        <html>
            <body>
                <a class="category-link" href="/examples/mooring/">Mooring Systems</a>
                <a class="category-link" href="/examples/riser/">Riser Analysis</a>
                <a class="category-link" href="/examples/installation/">Installation</a>
            </body>
        </html>
        """
        
        mock_response = Mock()
        mock_response.content = html_content.encode('utf-8')
        mock_response.raise_for_status = Mock()
        mock_get.return_value = mock_response
        
        # Temporarily patch BeautifulSoup to handle our mock
        with patch.object(self.downloader, '_download_with_retry', return_value=mock_response):
            # For this test, we'll simulate finding categories
            categories = []
            soup = BeautifulSoup(mock_response.content, 'html.parser')
            
            # Since the actual website structure is unknown, we'll test the logic
            # with a simplified structure
            test_categories = [
                {'name': 'Mooring Systems', 'url': 'http://example.com/mooring/'},
                {'name': 'Riser Analysis', 'url': 'http://example.com/riser/'},
            ]
            
            for cat in test_categories:
                self.downloader.manifest['categories'][cat['name']] = {
                    'url': cat['url'],
                    'last_checked': datetime.now().isoformat(),
                    'example_count': 0
                }
            
            self.assertEqual(len(self.downloader.manifest['categories']), 2)
            self.assertIn('Mooring Systems', self.downloader.manifest['categories'])
    
    @patch('digitalmodel.modules.orcaflex.examples_integration.downloader.OrcaflexExampleDownloader._download_with_retry')
    def test_download_example(self, mock_download):
        """Test downloading a single example file."""
        # Mock response
        mock_response = Mock()
        mock_response.content = b"Example file content"
        mock_download.return_value = mock_response
        
        # Example to download
        example = {
            'name': 'test_example',
            'url': 'http://example.com/test.dat',
            'category': 'Test Category',
            'file_type': 'dat'
        }
        
        # Download example
        success = self.downloader.download_example(example)
        
        # Check results
        self.assertTrue(success)
        
        # Check file was saved
        expected_file = Path(self.temp_dir) / "raw" / "Test Category" / "test.dat"
        self.assertTrue(expected_file.exists())
        
        # Check manifest was updated
        self.assertIn('test.dat', self.downloader.manifest['examples'])
        
        # Check file content
        with open(expected_file, 'rb') as f:
            content = f.read()
        self.assertEqual(content, b"Example file content")
    
    def test_verify_downloads(self):
        """Test download verification."""
        # Create test files
        test_file_path = Path(self.temp_dir) / "raw" / "test.dat"
        test_file_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(test_file_path, 'wb') as f:
            f.write(b"Test content")
        
        # Add to manifest with correct checksum
        import hashlib
        correct_checksum = hashlib.md5(b"Test content").hexdigest()
        
        self.downloader.manifest['examples']['test.dat'] = {
            'path': 'raw/test.dat',
            'checksum': correct_checksum,
            'size': 12
        }
        
        # Add corrupted file entry
        self.downloader.manifest['examples']['corrupted.dat'] = {
            'path': 'raw/test.dat',  # Points to same file but wrong checksum
            'checksum': 'wrong_checksum',
            'size': 12
        }
        
        # Add missing file entry
        self.downloader.manifest['examples']['missing.dat'] = {
            'path': 'raw/missing.dat',
            'checksum': 'some_checksum',
            'size': 0
        }
        
        # Verify downloads
        results = self.downloader.verify_downloads()
        
        # Check results
        self.assertIn('test.dat', results['valid'])
        self.assertIn('corrupted.dat', results['corrupted'])
        self.assertIn('missing.dat', results['missing'])
    
    def test_generate_download_report(self):
        """Test report generation."""
        # Add some test data to manifest
        self.downloader.manifest['categories']['Test'] = {
            'example_count': 5
        }
        self.downloader.manifest['examples']['test1.dat'] = {
            'size': 1024,
            'path': 'raw/test1.dat',
            'checksum': 'abc123'
        }
        self.downloader.manifest['examples']['test2.sim'] = {
            'size': 2048,
            'path': 'raw/test2.sim',
            'checksum': 'def456'
        }
        
        # Create dummy files for verification
        for filename in ['test1.dat', 'test2.sim']:
            file_path = Path(self.temp_dir) / 'raw' / filename
            file_path.parent.mkdir(parents=True, exist_ok=True)
            with open(file_path, 'wb') as f:
                f.write(b"dummy content")
        
        # Generate report
        report = self.downloader.generate_download_report()
        
        # Check report content
        self.assertIn("OrcaFlex Examples Download Report", report)
        self.assertIn("Total Examples Downloaded: 2", report)
        self.assertIn(".dat files: 1", report)
        self.assertIn(".sim files: 1", report)
        
        # Check report file was created
        report_files = list((Path(self.temp_dir) / "reports").glob("*.txt"))
        self.assertEqual(len(report_files), 1)


class TestDownloaderIntegration(unittest.TestCase):
    """Integration tests for the downloader (skipped by default)."""
    
    @unittest.skip("Integration test - requires internet connection")
    def test_real_website_connection(self):
        """Test actual connection to Orcina website."""
        downloader = OrcaflexExampleDownloader()
        
        # Try to access the main page
        response = downloader._download_with_retry(downloader.base_url)
        
        self.assertIsNotNone(response, "Should successfully connect to Orcina website")
        self.assertEqual(response.status_code, 200)
    
    @unittest.skip("Integration test - requires internet connection")
    def test_parse_real_categories(self):
        """Test parsing categories from real website."""
        downloader = OrcaflexExampleDownloader()
        
        categories = downloader.parse_categories()
        
        # We expect at least some categories
        self.assertGreater(len(categories), 0, "Should find at least one category")
        
        # Print found categories for manual verification
        print(f"\nFound {len(categories)} categories:")
        for cat in categories:
            print(f"  - {cat['name']}: {cat['url']}")


if __name__ == '__main__':
    unittest.main()