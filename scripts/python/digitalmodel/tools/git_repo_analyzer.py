"""
Git Repository Analysis and Optimization Module.

This module provides comprehensive analysis of git repository size, identification
of large objects, user configuration for history retention policies, and backup
procedures for safe repository optimization.
"""

import subprocess
import tempfile
import os
import re
import json
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional, Any
import shutil


class RepositoryAnalyzer:
    """Analyzes git repository size and identifies optimization targets."""
    
    def __init__(self, repo_path: str):
        """Initialize analyzer with repository path.
        
        Args:
            repo_path: Path to git repository root
        """
        self.repo_path = repo_path
        
    def analyze_repository_size(self) -> Dict[str, Any]:
        """Analyze repository size using git count-objects.
        
        Returns:
            Dictionary containing repository size analysis
        """
        try:
            result = subprocess.run(
                ['git', 'count-objects', '-vH'],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            
            # Parse git count-objects output
            analysis = {}
            for line in result.stdout.strip().split('\n'):
                if ' ' in line:
                    key, value = line.split(' ', 1)
                    # Convert numeric values
                    if value.isdigit():
                        analysis[key.replace('-', '_')] = int(value)
                    else:
                        analysis[key.replace('-', '_')] = value
                        
            # Add human-readable size calculations
            total_size = analysis.get('size', 0) + analysis.get('size_pack', 0)
            analysis['total_size_bytes'] = total_size
            analysis['total_size_mb'] = round(total_size / (1024 * 1024), 2)
            analysis['total_size_gb'] = round(total_size / (1024 * 1024 * 1024), 2)
            
            return analysis
            
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Failed to analyze repository size: {e}")
            
    def identify_large_objects(self, min_size_mb: float = 1.0) -> List[Dict[str, Any]]:
        """Identify large objects in repository history.
        
        Args:
            min_size_mb: Minimum size in MB to consider as large
            
        Returns:
            List of large objects with metadata
        """
        try:
            # Get all objects in repository
            result = subprocess.run(
                ['git', 'rev-list', '--objects', '--all'],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            
            large_objects = []
            min_size_bytes = int(min_size_mb * 1024 * 1024)
            
            # Process each object
            for line in result.stdout.strip().split('\n'):
                if not line.strip():
                    continue
                    
                parts = line.split(' ', 1)
                if len(parts) < 2:
                    continue
                    
                obj_hash = parts[0]
                filename = parts[1] if len(parts) > 1 else 'unknown'
                
                # Get object size
                try:
                    size_result = subprocess.run(
                        ['git', 'cat-file', '-s', obj_hash],
                        cwd=self.repo_path,
                        capture_output=True,
                        text=True,
                        check=True
                    )
                    
                    size_bytes = int(size_result.stdout.strip())
                    
                    # Include if size exceeds threshold
                    if size_bytes >= min_size_bytes:
                        large_objects.append({
                            'hash': obj_hash,
                            'filename': filename,
                            'size_bytes': size_bytes,
                            'size_mb': round(size_bytes / (1024 * 1024), 2)
                        })
                        
                except (subprocess.CalledProcessError, ValueError):
                    # Skip objects that can't be analyzed
                    continue
                    
            # Sort by size (largest first)
            large_objects.sort(key=lambda x: x['size_bytes'], reverse=True)
            
            return large_objects
            
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Failed to identify large objects: {e}")
            
    def analyze_commit_history(self) -> Dict[str, Any]:
        """Analyze commit history for retention policy decisions.
        
        Returns:
            Dictionary containing commit history analysis
        """
        try:
            result = subprocess.run(
                ['git', 'log', '--oneline', '--pretty=format:%ci %H'],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            
            commit_lines = result.stdout.strip().split('\n')
            commit_dates = []
            commit_hashes = []
            
            # Parse commit dates and hashes
            for line in commit_lines:
                if not line.strip():
                    continue
                    
                parts = line.split(' ')
                if len(parts) >= 4:
                    # Parse ISO date format
                    date_str = f"{parts[0]} {parts[1]}"
                    try:
                        commit_date = datetime.strptime(date_str, '%Y-%m-%d %H:%M:%S')
                        commit_dates.append(commit_date)
                        commit_hashes.append(parts[3])  # Commit hash
                    except ValueError:
                        continue
                        
            if not commit_dates:
                raise RuntimeError("No valid commits found in repository")
                
            # Sort dates (newest first)
            commit_dates.sort(reverse=True)
            
            history = {
                'total_commits': len(commit_dates),
                'oldest_commit': commit_dates[-1].isoformat(),
                'newest_commit': commit_dates[0].isoformat(),
                'commit_dates': commit_dates,
                'repository_age_days': (commit_dates[0] - commit_dates[-1]).days,
                'repository_age_years': round((commit_dates[0] - commit_dates[-1]).days / 365.25, 1)
            }
            
            return history
            
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Failed to analyze commit history: {e}")


class UserConfigurationHandler:
    """Handles user configuration and input validation for retention policies."""
    
    def generate_retention_menu(self, repo_status: Dict[str, Any]) -> str:
        """Generate interactive menu for history retention configuration.
        
        Args:
            repo_status: Repository status information
            
        Returns:
            Formatted menu string
        """
        menu = f"""
Git Repository Optimization - History Retention Policy
====================================================

Current repository status:
- Total commits: {repo_status.get('total_commits', 'Unknown'):,}
- Repository age: {repo_status.get('repository_age_years', 'Unknown')} years
- Current size: {repo_status.get('total_size_gb', 'Unknown')} GB
- Estimated large files: {repo_status.get('large_files_size_mb', 'Unknown')} MB

Choose history retention policy:

1. Keep last N commits
   └─ Enter number of recent commits to preserve (recommended: 100-1000)
   
2. Keep last N months  
   └─ Enter number of months to preserve (recommended: 6-24)
   
3. Keep all commits
   └─ Preserve complete history, only remove large files
   
4. Custom date cutoff
   └─ Enter specific date (YYYY-MM-DD format)
   
5. Preview mode
   └─ Show what would be removed without making changes

Enter your choice (1-5): """
        
        return menu.strip()
        
    def validate_commit_count(self, value: str) -> bool:
        """Validate commit count input.
        
        Args:
            value: User input string
            
        Returns:
            True if valid commit count
        """
        try:
            count = int(value)
            return count > 0 and count <= 10000  # Reasonable upper limit
        except ValueError:
            return False
            
    def validate_month_count(self, value: str) -> bool:
        """Validate month count input.
        
        Args:
            value: User input string
            
        Returns:
            True if valid month count
        """
        try:
            months = int(value)
            return months > 0 and months <= 60  # Max 5 years
        except ValueError:
            return False
            
    def validate_date(self, value: str) -> bool:
        """Validate date input in YYYY-MM-DD format.
        
        Args:
            value: User input string
            
        Returns:
            True if valid date
        """
        try:
            datetime.strptime(value, '%Y-%m-%d')
            return True
        except ValueError:
            return False
            
    def generate_preview(self, retention_type: str, retention_value: Any, 
                        repo_data: Dict[str, Any]) -> str:
        """Generate preview of retention policy impact.
        
        Args:
            retention_type: Type of retention (commits, months, date, all)
            retention_value: Value for retention policy
            repo_data: Repository analysis data
            
        Returns:
            Preview text showing impact
        """
        total_commits = repo_data.get('total_commits', 0)
        commit_dates = repo_data.get('commit_dates', [])
        
        if retention_type == 'commits':
            commits_to_preserve = min(retention_value, total_commits)
            commits_to_remove = total_commits - commits_to_preserve
            
            preview = f"""
Preview:
- Commits to preserve: {commits_to_preserve} (most recent)
- Commits to remove: {commits_to_remove} (older history)  
- Estimated size reduction: ~{min(60, int(commits_to_remove / total_commits * 80))}%"""

            if commit_dates and commits_to_preserve < len(commit_dates):
                cutoff_date = commit_dates[commits_to_preserve - 1]
                preview += f"\n- Date range preserved: {cutoff_date.strftime('%Y-%m-%d')} to present"
                
        elif retention_type == 'months':
            cutoff_date = datetime.now() - timedelta(days=retention_value * 30)
            preserved_commits = sum(1 for date in commit_dates if date >= cutoff_date)
            removed_commits = total_commits - preserved_commits
            
            preview = f"""
Preview:
- Date cutoff: {cutoff_date.strftime('%Y-%m-%d')} ({retention_value} months ago)
- Commits to preserve: {preserved_commits}
- Commits to remove: {removed_commits}
- Estimated size reduction: ~{min(60, int(removed_commits / total_commits * 80))}%"""

        elif retention_type == 'all':
            preview = f"""
Preview:
- Commits to preserve: {total_commits} (all)
- Large files to remove: ~{repo_data.get('large_files_size_mb', 0)} MB
- Estimated size reduction: ~35%
- History preserved: Complete"""

        else:
            preview = "Preview not available for this option."
            
        return preview.strip()


class BackupManager:
    """Manages repository backup creation and verification."""
    
    def __init__(self, repo_path: str):
        """Initialize backup manager.
        
        Args:
            repo_path: Path to repository to backup
        """
        self.repo_path = repo_path
        
    def create_backup(self) -> str:
        """Create a complete backup of the repository.
        
        Returns:
            Path to backup directory
        """
        try:
            # Create temporary backup directory
            backup_dir = tempfile.mkdtemp(prefix='repo_backup_')
            
            # Clone repository as mirror (includes all refs and history)
            subprocess.run(
                ['git', 'clone', '--mirror', self.repo_path, backup_dir],
                check=True
            )
            
            return backup_dir
            
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Failed to create repository backup: {e}")
            
    def verify_backup(self, backup_path: str) -> bool:
        """Verify backup integrity.
        
        Args:
            backup_path: Path to backup directory
            
        Returns:
            True if backup is valid
        """
        try:
            # Run git fsck on backup to verify integrity
            subprocess.run(
                ['git', 'fsck', '--full'],
                cwd=backup_path,
                capture_output=True,
                text=True,
                check=True
            )
            
            return True
            
        except subprocess.CalledProcessError:
            return False
            
    def cleanup_backup(self, backup_path: str) -> None:
        """Remove backup directory.
        
        Args:
            backup_path: Path to backup directory to remove
        """
        try:
            shutil.rmtree(backup_path)
        except OSError as e:
            raise RuntimeError(f"Failed to cleanup backup: {e}")


class GitRepositoryOptimizer:
    """Main orchestrator for git repository optimization workflow."""
    
    def __init__(self, repo_path: str):
        """Initialize optimizer.
        
        Args:
            repo_path: Path to git repository
        """
        self.repo_path = repo_path
        self.analyzer = RepositoryAnalyzer(repo_path)
        self.config_handler = UserConfigurationHandler()
        self.backup_manager = BackupManager(repo_path)
        
    def run_analysis_and_configuration(self) -> Dict[str, Any]:
        """Run complete analysis and user configuration workflow.
        
        Returns:
            Configuration dictionary for optimization
        """
        print("Analyzing repository...")
        
        # Perform repository analysis
        size_analysis = self.analyzer.analyze_repository_size()
        history_analysis = self.analyzer.analyze_commit_history()
        large_objects = self.analyzer.identify_large_objects()
        
        # Combine analysis data
        repo_status = {
            **size_analysis,
            **history_analysis,
            'large_files_size_mb': sum(obj['size_mb'] for obj in large_objects[:10])  # Top 10
        }
        
        print(f"Repository analysis complete:")
        print(f"- Total size: {repo_status['total_size_gb']} GB")
        print(f"- Total commits: {repo_status['total_commits']:,}")
        print(f"- Large objects found: {len(large_objects)}")
        print()
        
        # Display configuration menu
        menu = self.config_handler.generate_retention_menu(repo_status)
        print(menu)
        
        # Get user choice (in real usage, this would be input())
        # For testing, we'll return a default configuration
        choice = input().strip()
        
        config = {'repo_status': repo_status, 'large_objects': large_objects}
        
        if choice == '1':
            # Keep last N commits
            while True:
                commits_str = input("Enter number of commits to preserve: ").strip()
                if self.config_handler.validate_commit_count(commits_str):
                    commits = int(commits_str)
                    config.update({
                        'retention_type': 'commits',
                        'retention_value': commits
                    })
                    break
                else:
                    print("Invalid input. Please enter a positive number (1-10000).")
                    
        elif choice == '2':
            # Keep last N months
            while True:
                months_str = input("Enter number of months to preserve: ").strip()
                if self.config_handler.validate_month_count(months_str):
                    months = int(months_str)
                    config.update({
                        'retention_type': 'months',
                        'retention_value': months
                    })
                    break
                else:
                    print("Invalid input. Please enter a positive number (1-60).")
                    
        elif choice == '3':
            # Keep all commits
            config.update({
                'retention_type': 'all',
                'retention_value': None
            })
            
        elif choice == '4':
            # Custom date cutoff
            while True:
                date_str = input("Enter cutoff date (YYYY-MM-DD): ").strip()
                if self.config_handler.validate_date(date_str):
                    config.update({
                        'retention_type': 'date',
                        'retention_value': date_str
                    })
                    break
                else:
                    print("Invalid date format. Please use YYYY-MM-DD.")
                    
        else:
            print("Invalid choice. Defaulting to preview mode.")
            config.update({
                'retention_type': 'preview',
                'retention_value': None
            })
            
        # Show preview if not preview mode
        if config['retention_type'] != 'preview':
            preview = self.config_handler.generate_preview(
                config['retention_type'],
                config['retention_value'],
                repo_status
            )
            print(preview)
            print()
            
            # Get confirmation
            confirm = input("Continue? (y/N): ").strip().lower()
            config['confirmed'] = confirm == 'y'
        else:
            config['confirmed'] = False
            
        return config