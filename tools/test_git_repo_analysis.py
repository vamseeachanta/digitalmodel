"""
Test suite for git repository analysis and optimization functionality.

This module tests the repository size analysis, large object identification,
user configuration, and backup procedures for git repository optimization.
"""

import unittest
from unittest.mock import patch, MagicMock, call
import subprocess
import tempfile
import os
from datetime import datetime, timedelta
import json


class TestRepositoryAnalysis(unittest.TestCase):
    """Test repository size analysis functionality."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.mock_repo_path = "/test/repo"
        
    @patch('subprocess.run')
    def test_git_count_objects_analysis(self, mock_run):
        """Test git count-objects command execution and parsing."""
        # Mock git count-objects output
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="count 1247\nsize 1234567890\nin-pack 1200\npacks 3\nsize-pack 987654321\nprune-packable 0\ngarbage 0\nsize-garbage 0\n"
        )
        
        from git_repo_analyzer import RepositoryAnalyzer
        analyzer = RepositoryAnalyzer(self.mock_repo_path)
        result = analyzer.analyze_repository_size()
        
        # Verify git command was called correctly
        mock_run.assert_called_with(
            ['git', 'count-objects', '-vH'],
            cwd=self.mock_repo_path,
            capture_output=True,
            text=True,
            check=True
        )
        
        # Verify analysis results (keys are normalized with underscores)
        self.assertEqual(result['count'], 1247)
        self.assertEqual(result['size'], 1234567890)
        self.assertEqual(result['in_pack'], 1200)
        self.assertEqual(result['packs'], 3)
        self.assertEqual(result['size_pack'], 987654321)
        self.assertEqual(result['total_size_bytes'], 1234567890 + 987654321)
        
    @patch('subprocess.run')
    def test_large_object_identification(self, mock_run):
        """Test identification of large objects in repository."""
        # Mock git rev-list output and cat-file responses
        mock_responses = [
            # First call: git rev-list --objects --all
            MagicMock(returncode=0, stdout="abc123 large_file1.zip\ndef456 backup_data.tar\nghi789 medium_file.pdf\n"),
            # Subsequent calls: git cat-file -s for each object
            MagicMock(returncode=0, stdout="5242880\n"),  # large_file1.zip
            MagicMock(returncode=0, stdout="10485760\n"), # backup_data.tar  
            MagicMock(returncode=0, stdout="2097152\n"),  # medium_file.pdf
        ]
        mock_run.side_effect = mock_responses
        
        from git_repo_analyzer import RepositoryAnalyzer
        analyzer = RepositoryAnalyzer(self.mock_repo_path)
        large_objects = analyzer.identify_large_objects(min_size_mb=2)
        
        # Verify large objects were identified correctly (only files >= 2MB)
        self.assertEqual(len(large_objects), 3)  # All files are >= 2MB
        
        # Check largest first (sorted by size descending)
        self.assertEqual(large_objects[0]['filename'], 'backup_data.tar')
        self.assertEqual(large_objects[0]['size_mb'], 10.0)
        self.assertEqual(large_objects[1]['filename'], 'large_file1.zip')
        self.assertEqual(large_objects[1]['size_mb'], 5.0)
        self.assertEqual(large_objects[2]['filename'], 'medium_file.pdf')
        self.assertEqual(large_objects[2]['size_mb'], 2.0)
        
    @patch('subprocess.run')
    def test_commit_history_analysis(self, mock_run):
        """Test commit history analysis for retention policies."""
        # Mock git log output with proper format (date time timezone hash)
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="2025-01-15 10:30:00 +0000 abc123def456\n2024-12-01 14:15:30 +0000 def456ghi789\n2024-06-15 09:45:20 +0000 ghi789jkl012\n2024-01-01 16:20:10 +0000 jkl012mno345\n"
        )
        
        from git_repo_analyzer import RepositoryAnalyzer
        analyzer = RepositoryAnalyzer(self.mock_repo_path)
        history = analyzer.analyze_commit_history()
        
        # Verify git log command was called
        mock_run.assert_called_with(
            ['git', 'log', '--oneline', '--pretty=format:%ci %H'],
            cwd=self.mock_repo_path,
            capture_output=True,
            text=True,
            check=True
        )
        
        # Verify history analysis results
        self.assertEqual(history['total_commits'], 4)
        self.assertIn('oldest_commit', history)
        self.assertIn('newest_commit', history)
        self.assertIn('commit_dates', history)
        self.assertIn('repository_age_days', history)
        self.assertIn('repository_age_years', history)


class TestUserConfiguration(unittest.TestCase):
    """Test user configuration and input validation."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.config_handler = None
        
    def test_history_retention_menu_display(self):
        """Test interactive menu display for history retention."""
        from git_repo_analyzer import UserConfigurationHandler
        handler = UserConfigurationHandler()
        
        # Mock repository status for menu display
        repo_status = {
            'total_commits': 1247,
            'repository_age_years': 2.3,
            'total_size_gb': 1.2,
            'large_files_size_mb': 450
        }
        
        menu_text = handler.generate_retention_menu(repo_status)
        
        # Verify menu contains required sections
        self.assertIn('Git Repository Optimization - History Retention Policy', menu_text)
        self.assertIn('Total commits: 1,247', menu_text)
        self.assertIn('Repository age: 2.3 years', menu_text)
        self.assertIn('Current size: 1.2 GB', menu_text)
        self.assertIn('1. Keep last N commits', menu_text)
        self.assertIn('2. Keep last N months', menu_text)
        self.assertIn('3. Keep all commits', menu_text)
        self.assertIn('4. Custom date cutoff', menu_text)
        
    def test_input_validation_commits(self):
        """Test validation of commit count input."""
        from git_repo_analyzer import UserConfigurationHandler
        handler = UserConfigurationHandler()
        
        # Test valid commit counts
        self.assertTrue(handler.validate_commit_count("100"))
        self.assertTrue(handler.validate_commit_count("500"))
        self.assertTrue(handler.validate_commit_count("1000"))
        
        # Test invalid commit counts
        self.assertFalse(handler.validate_commit_count("-10"))
        self.assertFalse(handler.validate_commit_count("0"))
        self.assertFalse(handler.validate_commit_count("abc"))
        self.assertFalse(handler.validate_commit_count(""))
        
    def test_input_validation_months(self):
        """Test validation of month count input."""
        from git_repo_analyzer import UserConfigurationHandler
        handler = UserConfigurationHandler()
        
        # Test valid month counts
        self.assertTrue(handler.validate_month_count("6"))
        self.assertTrue(handler.validate_month_count("12"))
        self.assertTrue(handler.validate_month_count("24"))
        
        # Test invalid month counts
        self.assertFalse(handler.validate_month_count("-1"))
        self.assertFalse(handler.validate_month_count("0"))
        self.assertFalse(handler.validate_month_count("xyz"))
        self.assertFalse(handler.validate_month_count("120"))  # Too many months
        
    def test_input_validation_dates(self):
        """Test validation of date input."""
        from git_repo_analyzer import UserConfigurationHandler
        handler = UserConfigurationHandler()
        
        # Test valid dates
        self.assertTrue(handler.validate_date("2024-07-27"))
        self.assertTrue(handler.validate_date("2023-01-01"))
        self.assertTrue(handler.validate_date("2022-12-31"))
        
        # Test invalid dates
        self.assertFalse(handler.validate_date("2024-13-01"))  # Invalid month
        self.assertFalse(handler.validate_date("2024-07-32"))  # Invalid day
        self.assertFalse(handler.validate_date("24-07-27"))    # Wrong format
        self.assertFalse(handler.validate_date("2024/07/27"))  # Wrong format
        self.assertFalse(handler.validate_date("invalid"))
        
    def test_preview_generation(self):
        """Test preview generation for retention policies."""
        from git_repo_analyzer import UserConfigurationHandler
        handler = UserConfigurationHandler()
        
        # Mock repository data
        repo_data = {
            'total_commits': 1247,
            'commit_dates': [
                datetime(2025, 1, 15),
                datetime(2024, 12, 1),
                datetime(2024, 6, 15),
                datetime(2023, 1, 1)
            ]
        }
        
        # Test commit count preview
        preview = handler.generate_preview('commits', 500, repo_data)
        self.assertIn('Commits to preserve: 500', preview)
        self.assertIn('Commits to remove: 747', preview)
        
        # Test month count preview
        preview = handler.generate_preview('months', 12, repo_data)
        self.assertIn('Date cutoff:', preview)
        self.assertIn('months ago', preview)


class TestBackupProcedures(unittest.TestCase):
    """Test backup creation and verification procedures."""
    
    @patch('subprocess.run')
    @patch('tempfile.mkdtemp')
    def test_backup_creation(self, mock_mkdtemp, mock_run):
        """Test repository backup creation."""
        # Mock temporary directory creation
        mock_mkdtemp.return_value = '/tmp/repo_backup_123'
        
        # Mock git clone success
        mock_run.return_value = MagicMock(returncode=0)
        
        from git_repo_analyzer import BackupManager
        backup_manager = BackupManager('/test/repo')
        backup_path = backup_manager.create_backup()
        
        # Verify backup directory was created
        mock_mkdtemp.assert_called_once()
        
        # Verify git clone was called correctly
        mock_run.assert_called_with(
            ['git', 'clone', '--mirror', '/test/repo', '/tmp/repo_backup_123'],
            check=True
        )
        
        # Verify backup path is returned
        self.assertEqual(backup_path, '/tmp/repo_backup_123')
        
    @patch('subprocess.run')
    def test_backup_verification(self, mock_run):
        """Test backup verification procedures."""
        # Mock git fsck success on backup
        mock_run.return_value = MagicMock(returncode=0, stdout="")
        
        from git_repo_analyzer import BackupManager
        backup_manager = BackupManager('/test/repo')
        is_valid = backup_manager.verify_backup('/tmp/backup')
        
        # Verify git fsck was called on backup
        mock_run.assert_called_with(
            ['git', 'fsck', '--full'],
            cwd='/tmp/backup',
            capture_output=True,
            text=True,
            check=True
        )
        
        # Verify backup is considered valid
        self.assertTrue(is_valid)
        
    @patch('subprocess.run')
    def test_backup_verification_failure(self, mock_run):
        """Test handling of backup verification failure."""
        # Mock git fsck failure
        mock_run.side_effect = subprocess.CalledProcessError(1, 'git fsck')
        
        from git_repo_analyzer import BackupManager
        backup_manager = BackupManager('/test/repo')
        is_valid = backup_manager.verify_backup('/tmp/backup')
        
        # Verify backup is considered invalid
        self.assertFalse(is_valid)


class TestIntegrationWorkflow(unittest.TestCase):
    """Test integrated workflow functionality."""
    
    @patch('builtins.input')
    @patch('subprocess.run')
    def test_complete_analysis_workflow(self, mock_run, mock_input):
        """Test complete repository analysis workflow."""
        # Mock user inputs for interactive configuration
        mock_input.side_effect = ['1', '500', 'y']  # Choose commits, 500 commits, confirm
        
        # Mock git command responses in order
        mock_responses = [
            # count-objects
            MagicMock(returncode=0, stdout="count 1247\nsize 1234567890\nsize-pack 100000000\n"),
            # commit history log
            MagicMock(returncode=0, stdout="2025-01-15 10:30:00 +0000 abc123\n2024-12-01 14:15:30 +0000 def456\n"),
            # rev-list for large objects
            MagicMock(returncode=0, stdout="abc123 large_file.zip\n"),
            # cat-file for size
            MagicMock(returncode=0, stdout="5242880\n")
        ]
        mock_run.side_effect = mock_responses
        
        from git_repo_analyzer import GitRepositoryOptimizer
        optimizer = GitRepositoryOptimizer('/test/repo')
        config = optimizer.run_analysis_and_configuration()
        
        # Verify configuration was generated correctly
        self.assertEqual(config['retention_type'], 'commits')
        self.assertEqual(config['retention_value'], 500)
        self.assertTrue(config['confirmed'])


if __name__ == '__main__':
    unittest.main()