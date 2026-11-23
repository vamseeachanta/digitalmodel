"""
Git Repository Optimization Module.

This module provides git-filter-repo integration, large file removal,
repository optimization through garbage collection and repacking,
and comprehensive progress reporting.
"""

import subprocess
import os
import sys
import json
import shutil
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional, Any, Callable
from git_repo_analyzer import RepositoryAnalyzer, BackupManager


class GitFilterRepoManager:
    """Manages git-filter-repo operations for repository optimization."""
    
    def check_installation(self) -> bool:
        """Check if git-filter-repo is installed.
        
        Returns:
            True if git-filter-repo is available
        """
        try:
            result = subprocess.run(
                ['git-filter-repo', '--version'],
                capture_output=True,
                text=True
            )
            return result.returncode == 0
        except (subprocess.CalledProcessError, FileNotFoundError):
            return False
            
    def install_instructions(self) -> str:
        """Provide installation instructions for git-filter-repo.
        
        Returns:
            Installation instructions string
        """
        return """
git-filter-repo is not installed. Please install it using one of these methods:

1. Using pip (recommended):
   pip install git-filter-repo

2. Using package manager:
   - Ubuntu/Debian: apt install git-filter-repo
   - MacOS: brew install git-filter-repo
   - Windows: Download from https://github.com/newren/git-filter-repo/releases

3. Manual installation:
   - Download git-filter-repo script from GitHub
   - Make it executable and place in PATH

After installation, run this command again.
"""
    
    def build_removal_command(self, repo_path: str, large_files: List[Dict[str, Any]], 
                            retention_type: str, retention_value: Optional[Any] = None) -> List[str]:
        """Build git-filter-repo command for large file removal.
        
        Args:
            repo_path: Path to repository
            large_files: List of large files to remove
            retention_type: Type of history retention (commits, months, date, all)
            retention_value: Value for retention policy
            
        Returns:
            Command list for subprocess
        """
        cmd = ['git-filter-repo']
        
        # Add path exclusions for large files
        for file_info in large_files:
            cmd.extend(['--path', file_info['filename']])
        
        # Invert paths to remove specified files
        if large_files:
            cmd.append('--invert-paths')
            
        # Handle history retention
        if retention_type == 'commits' and retention_value:
            # Keep only last N commits
            cmd.extend(['--refs', f'HEAD~{retention_value}..HEAD'])
            
        elif retention_type == 'months' and retention_value:
            # Calculate date cutoff
            cutoff_date = datetime.now() - timedelta(days=retention_value * 30)
            date_str = cutoff_date.strftime('%Y-%m-%d')
            
            # Use commit callback to filter by date
            callback = f"""
if commit.committer_date < {int(cutoff_date.timestamp())}:
    commit.skip()
"""
            cmd.extend(['--commit-callback', callback])
            
        elif retention_type == 'date' and retention_value:
            # Parse date string
            cutoff_date = datetime.strptime(retention_value, '%Y-%m-%d')
            
            callback = f"""
if commit.committer_date < {int(cutoff_date.timestamp())}:
    commit.skip()
"""
            cmd.extend(['--commit-callback', callback])
            
        # Force operation (required for non-fresh clones)
        cmd.append('--force')
        
        return cmd
        
    def execute_filtering(self, repo_path: str, command: List[str]) -> Tuple[bool, str]:
        """Execute git-filter-repo command.
        
        Args:
            repo_path: Repository path
            command: Command to execute
            
        Returns:
            Tuple of (success, message)
        """
        try:
            # Change to repository directory
            original_dir = os.getcwd()
            os.chdir(repo_path)
            
            # Execute git-filter-repo
            result = subprocess.run(
                command,
                capture_output=True,
                text=True,
                check=True
            )
            
            os.chdir(original_dir)
            return True, "Repository filtering completed successfully"
            
        except subprocess.CalledProcessError as e:
            os.chdir(original_dir)
            return False, f"Filtering failed: {e.stderr}"
        except Exception as e:
            os.chdir(original_dir)
            return False, f"Unexpected error: {str(e)}"


class RepositoryOptimizer:
    """Handles repository optimization operations."""
    
    def __init__(self, repo_path: str):
        """Initialize optimizer.
        
        Args:
            repo_path: Path to repository
        """
        self.repo_path = repo_path
        
    def run_garbage_collection(self, aggressive: bool = True) -> None:
        """Run git garbage collection.
        
        Args:
            aggressive: Use aggressive gc settings
        """
        cmd = ['git', 'gc']
        
        if aggressive:
            cmd.append('--aggressive')
            
        cmd.append('--prune=now')
        
        subprocess.run(cmd, cwd=self.repo_path, check=True)
        
    def repack_repository(self) -> None:
        """Repack repository for optimization."""
        cmd = [
            'git', 'repack',
            '-a',  # Pack all objects
            '-d',  # Remove redundant packs
            '-f',  # Force recomputation
            '--depth=250',  # Delta chain depth
            '--window=250'  # Window size for delta compression
        ]
        
        subprocess.run(cmd, cwd=self.repo_path, check=True)
        
    def verify_integrity(self) -> bool:
        """Verify repository integrity.
        
        Returns:
            True if repository is valid
        """
        try:
            result = subprocess.run(
                ['git', 'fsck', '--full', '--strict'],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            return True
        except subprocess.CalledProcessError:
            return False
            
    def measure_repository_size(self) -> Dict[str, int]:
        """Measure current repository size.
        
        Returns:
            Dictionary with size metrics
        """
        analyzer = RepositoryAnalyzer(self.repo_path)
        size_data = analyzer.analyze_repository_size()
        
        return {
            'total_size': size_data.get('total_size_bytes', 0),
            'object_count': size_data.get('count', 0),
            'pack_size': size_data.get('size_pack', 0)
        }
        
    def compare_sizes(self, before: Dict[str, int], after: Dict[str, int]) -> Dict[str, Any]:
        """Compare repository sizes.
        
        Args:
            before: Size metrics before optimization
            after: Size metrics after optimization
            
        Returns:
            Comparison metrics
        """
        size_saved = before['total_size'] - after['total_size']
        reduction_percentage = (size_saved / before['total_size'] * 100) if before['total_size'] > 0 else 0
        
        return {
            'size_before_mb': round(before['total_size'] / (1024 * 1024), 2),
            'size_after_mb': round(after['total_size'] / (1024 * 1024), 2),
            'size_saved_mb': round(size_saved / (1024 * 1024), 2),
            'reduction_percentage': round(reduction_percentage, 1),
            'objects_before': before['object_count'],
            'objects_after': after['object_count']
        }


class GitRepositoryOptimizer:
    """Main orchestrator for complete repository optimization workflow."""
    
    def __init__(self, repo_path: str):
        """Initialize optimizer.
        
        Args:
            repo_path: Path to repository
        """
        self.repo_path = repo_path
        self.filter_manager = GitFilterRepoManager()
        self.optimizer = RepositoryOptimizer(repo_path)
        self.progress_callback: Optional[Callable[[str, int], None]] = None
        
    def set_progress_callback(self, callback: Callable[[str, int], None]) -> None:
        """Set progress callback function.
        
        Args:
            callback: Function that receives (message, percentage)
        """
        self.progress_callback = callback
        
    def _report_progress(self, message: str, percentage: int) -> None:
        """Report progress if callback is set.
        
        Args:
            message: Progress message
            percentage: Progress percentage (0-100)
        """
        if self.progress_callback:
            self.progress_callback(message, percentage)
        else:
            print(f"[{percentage}%] {message}")
            
    def optimize_repository(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """Execute complete repository optimization workflow.
        
        Args:
            config: Configuration from analysis phase
            
        Returns:
            Result dictionary with optimization metrics
        """
        try:
            # Check git-filter-repo installation
            self._report_progress("Checking git-filter-repo installation...", 5)
            if not self.filter_manager.check_installation():
                return {
                    'success': False,
                    'error': 'git-filter-repo not installed',
                    'instructions': self.filter_manager.install_instructions()
                }
                
            # Measure size before optimization
            self._report_progress("Measuring repository size...", 10)
            size_before = self.optimizer.measure_repository_size()
            
            # Build filter-repo command
            self._report_progress("Preparing optimization strategy...", 20)
            command = self.filter_manager.build_removal_command(
                repo_path=self.repo_path,
                large_files=config.get('large_objects', []),
                retention_type=config.get('retention_type', 'all'),
                retention_value=config.get('retention_value')
            )
            
            # Execute filtering if needed
            if config.get('large_objects') or config.get('retention_type') != 'all':
                self._report_progress("Removing large files and filtering history...", 40)
                success, message = self.filter_manager.execute_filtering(
                    self.repo_path, 
                    command
                )
                
                if not success:
                    return {
                        'success': False,
                        'error': message,
                        'recovery_instructions': self._get_recovery_instructions(config)
                    }
                    
            # Run garbage collection
            self._report_progress("Running garbage collection...", 60)
            self.optimizer.run_garbage_collection(aggressive=True)
            
            # Repack repository
            self._report_progress("Repacking repository...", 75)
            self.optimizer.repack_repository()
            
            # Verify integrity
            self._report_progress("Verifying repository integrity...", 90)
            if not self.optimizer.verify_integrity():
                return {
                    'success': False,
                    'error': 'Repository integrity check failed',
                    'recovery_instructions': self._get_recovery_instructions(config)
                }
                
            # Measure size after optimization
            self._report_progress("Calculating optimization results...", 95)
            size_after = self.optimizer.measure_repository_size()
            
            # Compare sizes
            comparison = self.optimizer.compare_sizes(size_before, size_after)
            
            self._report_progress("Optimization complete!", 100)
            
            return {
                'success': True,
                'size_before': size_before,
                'size_after': size_after,
                **comparison,
                'message': f"Repository optimized successfully. Size reduced by {comparison['reduction_percentage']}%"
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'recovery_instructions': self._get_recovery_instructions(config)
            }
            
    def _get_recovery_instructions(self, config: Dict[str, Any]) -> str:
        """Get recovery instructions in case of failure.
        
        Args:
            config: Original configuration
            
        Returns:
            Recovery instructions string
        """
        backup_path = config.get('backup_path', 'backup')
        
        return f"""
Repository optimization failed. To recover:

1. If you have a backup at {backup_path}:
   - Delete current repository: rm -rf .git
   - Restore from backup: cp -r {backup_path} .git

2. If no backup available:
   - Clone fresh from origin: git clone [origin-url]
   - Re-apply any local changes

3. Report the error for troubleshooting.

Always ensure you have a backup before attempting optimization.
"""