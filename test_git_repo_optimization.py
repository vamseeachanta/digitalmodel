"""
Test suite for git repository optimization functionality.

This module tests git-filter-repo integration, large file removal,
repository optimization, and integrity verification.
"""

import unittest
from unittest.mock import patch, MagicMock, call
import subprocess
import os
import tempfile
from datetime import datetime, timedelta


class TestGitFilterRepoIntegration(unittest.TestCase):
    """Test git-filter-repo integration and configuration."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.repo_path = "/test/repo"
        self.backup_path = "/test/backup"
        
    @patch('subprocess.run')
    def test_git_filter_repo_installation_check(self, mock_run):
        """Test checking for git-filter-repo installation."""
        # Mock successful git-filter-repo check
        mock_run.return_value = MagicMock(returncode=0, stdout="git-filter-repo version 2.38.0\n")
        
        from git_repo_optimizer import GitFilterRepoManager
        manager = GitFilterRepoManager()
        is_installed = manager.check_installation()
        
        # Verify command was called
        mock_run.assert_called_with(
            ['git-filter-repo', '--version'],
            capture_output=True,
            text=True
        )
        
        self.assertTrue(is_installed)
        
    @patch('subprocess.run')
    def test_git_filter_repo_not_installed(self, mock_run):
        """Test handling when git-filter-repo is not installed."""
        # Mock command not found error
        mock_run.side_effect = subprocess.CalledProcessError(1, 'git-filter-repo')
        
        from git_repo_optimizer import GitFilterRepoManager
        manager = GitFilterRepoManager()
        is_installed = manager.check_installation()
        
        self.assertFalse(is_installed)
        
    @patch('subprocess.run')
    def test_large_file_removal_command_construction(self, mock_run):
        """Test construction of git-filter-repo commands for large file removal."""
        mock_run.return_value = MagicMock(returncode=0)
        
        from git_repo_optimizer import GitFilterRepoManager
        manager = GitFilterRepoManager()
        
        # Test removing files larger than 10MB
        large_files = [
            {'filename': 'large_file1.zip', 'size_mb': 15.0},
            {'filename': 'backup/data.tar', 'size_mb': 25.0}
        ]
        
        command = manager.build_removal_command(
            repo_path=self.repo_path,
            large_files=large_files,
            retention_type='all'  # Keep all commits
        )
        
        # Verify command structure
        self.assertIn('git-filter-repo', command)
        self.assertIn('--path', command)
        self.assertIn('large_file1.zip', command)
        self.assertIn('backup/data.tar', command)
        self.assertIn('--invert-paths', command)
        
    @patch('subprocess.run')
    def test_history_truncation_with_commit_limit(self, mock_run):
        """Test history truncation with commit count limit."""
        mock_run.return_value = MagicMock(returncode=0)
        
        from git_repo_optimizer import GitFilterRepoManager
        manager = GitFilterRepoManager()
        
        # Test keeping last 500 commits
        command = manager.build_removal_command(
            repo_path=self.repo_path,
            large_files=[],
            retention_type='commits',
            retention_value=500
        )
        
        # Should include commit limiting options
        self.assertIn('--refs', command)
        self.assertIn('HEAD~500..HEAD', command)
        
    @patch('subprocess.run')
    def test_history_truncation_with_date_cutoff(self, mock_run):
        """Test history truncation with date cutoff."""
        mock_run.return_value = MagicMock(returncode=0)
        
        from git_repo_optimizer import GitFilterRepoManager
        manager = GitFilterRepoManager()
        
        # Test keeping commits after specific date
        cutoff_date = '2024-01-01'
        command = manager.build_removal_command(
            repo_path=self.repo_path,
            large_files=[],
            retention_type='date',
            retention_value=cutoff_date
        )
        
        # Should include date-based filtering
        self.assertIn('--commit-callback', command)
        # Check that callback contains timestamp logic
        callback_found = any('commit.committer_date' in str(item) for item in command)
        self.assertTrue(callback_found)


class TestRepositoryOptimization(unittest.TestCase):
    """Test repository optimization operations."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.repo_path = "/test/repo"
        
    @patch('subprocess.run')
    def test_garbage_collection_aggressive(self, mock_run):
        """Test aggressive garbage collection."""
        mock_run.return_value = MagicMock(returncode=0)
        
        from git_repo_optimizer import RepositoryOptimizer
        optimizer = RepositoryOptimizer(self.repo_path)
        optimizer.run_garbage_collection(aggressive=True)
        
        # Verify git gc command
        mock_run.assert_called_with(
            ['git', 'gc', '--aggressive', '--prune=now'],
            cwd=self.repo_path,
            check=True
        )
        
    @patch('subprocess.run')
    def test_repository_repacking(self, mock_run):
        """Test repository repacking for optimization."""
        mock_run.return_value = MagicMock(returncode=0)
        
        from git_repo_optimizer import RepositoryOptimizer
        optimizer = RepositoryOptimizer(self.repo_path)
        optimizer.repack_repository()
        
        # Verify repack command
        mock_run.assert_called_with(
            ['git', 'repack', '-a', '-d', '-f', '--depth=250', '--window=250'],
            cwd=self.repo_path,
            check=True
        )
        
    @patch('subprocess.run')
    def test_repository_integrity_verification(self, mock_run):
        """Test repository integrity verification after optimization."""
        mock_run.return_value = MagicMock(returncode=0, stdout="")
        
        from git_repo_optimizer import RepositoryOptimizer
        optimizer = RepositoryOptimizer(self.repo_path)
        is_valid = optimizer.verify_integrity()
        
        # Verify fsck command
        mock_run.assert_called_with(
            ['git', 'fsck', '--full', '--strict'],
            cwd=self.repo_path,
            capture_output=True,
            text=True,
            check=True
        )
        
        self.assertTrue(is_valid)
        
    @patch('subprocess.run')
    def test_size_comparison_measurement(self, mock_run):
        """Test repository size measurement before/after optimization."""
        # Mock size before optimization (format matching git count-objects output)
        mock_run.side_effect = [
            MagicMock(returncode=0, stdout="count 1000\nsize 1234567890\nsize-pack 987654321\n"),
            MagicMock(returncode=0, stdout="count 800\nsize 234567890\nsize-pack 187654321\n")
        ]
        
        from git_repo_optimizer import RepositoryOptimizer
        optimizer = RepositoryOptimizer(self.repo_path)
        
        size_before = optimizer.measure_repository_size()
        size_after = optimizer.measure_repository_size()
        
        comparison = optimizer.compare_sizes(size_before, size_after)
        
        # Verify size reduction (total_size should be calculated correctly)
        self.assertGreater(size_before['total_size'], 0)
        self.assertGreater(size_after['total_size'], 0)
        self.assertLess(size_after['total_size'], size_before['total_size'])
        self.assertIn('reduction_percentage', comparison)
        self.assertIn('size_saved_mb', comparison)


class TestOptimizationWorkflow(unittest.TestCase):
    """Test complete optimization workflow."""
    
    @patch('subprocess.run')
    @patch('os.path.exists')
    @patch('os.chdir')
    def test_complete_optimization_workflow(self, mock_chdir, mock_exists, mock_run):
        """Test complete repository optimization workflow."""
        # Mock file existence checks
        mock_exists.return_value = True
        
        # Mock command responses in order
        mock_responses = [
            # git-filter-repo --version check
            MagicMock(returncode=0, stdout="git-filter-repo version 2.38.0\n"),
            # Size measurement before (git count-objects)
            MagicMock(returncode=0, stdout="count 1000\nsize 1234567890\nsize-pack 987654321\n"),
            # git-filter-repo execution
            MagicMock(returncode=0, stdout="", stderr=""),
            # Garbage collection
            MagicMock(returncode=0),
            # Repacking
            MagicMock(returncode=0),
            # Integrity check
            MagicMock(returncode=0, stdout=""),
            # Size measurement after (git count-objects)
            MagicMock(returncode=0, stdout="count 800\nsize 234567890\nsize-pack 187654321\n")
        ]
        mock_run.side_effect = mock_responses
        
        from git_repo_optimizer import GitRepositoryOptimizer
        optimizer = GitRepositoryOptimizer('/test/repo')
        
        config = {
            'retention_type': 'commits',
            'retention_value': 500,
            'large_objects': [
                {'filename': 'large.zip', 'size_mb': 10.0}
            ],
            'backup_path': '/test/backup'
        }
        
        result = optimizer.optimize_repository(config)
        
        # Verify workflow completed
        self.assertTrue(result['success'])
        self.assertIn('size_before', result)
        self.assertIn('size_after', result)
        self.assertIn('reduction_percentage', result)
        
    @patch('subprocess.run')
    @patch('os.chdir')
    def test_optimization_with_error_handling(self, mock_chdir, mock_run):
        """Test optimization error handling and recovery."""
        # Mock git-filter-repo installation check succeeds, but execution fails
        mock_responses = [
            # git-filter-repo --version check succeeds
            MagicMock(returncode=0, stdout="git-filter-repo version 2.38.0\n"),
            # Size measurement before
            MagicMock(returncode=0, stdout="count 1000\nsize 1234567890\nsize-pack 987654321\n"),
            # git-filter-repo execution fails
            subprocess.CalledProcessError(1, 'git-filter-repo', stderr="Repository filtering failed")
        ]
        mock_run.side_effect = mock_responses
        
        from git_repo_optimizer import GitRepositoryOptimizer
        optimizer = GitRepositoryOptimizer('/test/repo')
        
        config = {
            'retention_type': 'commits',
            'retention_value': 100,
            'large_objects': [{'filename': 'large.zip', 'size_mb': 10.0}],
            'backup_path': '/test/backup'
        }
        
        result = optimizer.optimize_repository(config)
        
        # Verify error handling
        self.assertFalse(result['success'])
        self.assertIn('error', result)
        self.assertIn('recovery_instructions', result)


class TestProgressReporting(unittest.TestCase):
    """Test progress reporting during optimization."""
    
    def test_progress_callback_integration(self):
        """Test progress callback during optimization steps."""
        progress_updates = []
        
        def progress_callback(message, percentage):
            progress_updates.append({
                'message': message,
                'percentage': percentage
            })
            
        from git_repo_optimizer import GitRepositoryOptimizer
        optimizer = GitRepositoryOptimizer('/test/repo')
        optimizer.set_progress_callback(progress_callback)
        
        # Simulate progress updates
        optimizer._report_progress("Analyzing repository...", 10)
        optimizer._report_progress("Removing large files...", 40)
        optimizer._report_progress("Optimizing repository...", 70)
        optimizer._report_progress("Verifying integrity...", 90)
        optimizer._report_progress("Complete!", 100)
        
        # Verify progress updates
        self.assertEqual(len(progress_updates), 5)
        self.assertEqual(progress_updates[0]['percentage'], 10)
        self.assertEqual(progress_updates[-1]['percentage'], 100)


if __name__ == '__main__':
    unittest.main()