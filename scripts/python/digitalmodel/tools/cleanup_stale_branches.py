#!/usr/bin/env python3
"""
Automated Stale Branch Cleanup Script for Git Repositories

This script identifies and cleans up stale branches across multiple repositories.
Features:
- Dry-run mode for safety
- Archive branches before deletion
- Categorize branches by safety level
- Generate detailed cleanup reports
- Support for both local and remote cleanup
"""

import os
import sys
import json
import argparse
import subprocess
from datetime import datetime, timedelta
from pathlib import Path
from typing import List, Dict, Tuple, Optional
from dataclasses import dataclass
from enum import Enum


class BranchStatus(Enum):
    """Branch status categories"""
    MERGED = "merged"
    UNMERGED_STALE = "unmerged_stale"
    UNMERGED_ACTIVE = "unmerged_active"
    PROTECTED = "protected"
    RELEASE = "release"


@dataclass
class BranchInfo:
    """Information about a git branch"""
    name: str
    last_commit_date: datetime
    is_merged: bool
    ahead_count: int
    behind_count: int
    last_commit_message: str
    status: BranchStatus
    is_remote: bool = False


class GitBranchCleaner:
    """Git branch cleanup utility"""
    
    # Protected branch patterns
    PROTECTED_BRANCHES = ['master', 'main', 'develop', 'staging', 'production']
    
    # Release branch patterns
    RELEASE_PATTERNS = ['release/', 'hotfix/', r'^\d{6}$', r'^\d{4}\d{2}$']
    
    def __init__(self, repo_path: str, dry_run: bool = True, stale_days: int = 60):
        """
        Initialize the branch cleaner
        
        Args:
            repo_path: Path to git repository
            dry_run: If True, only show what would be done
            stale_days: Number of days to consider a branch stale
        """
        self.repo_path = Path(repo_path)
        self.dry_run = dry_run
        self.stale_days = stale_days
        self.stale_date = datetime.now() - timedelta(days=stale_days)
        
    def run_git_command(self, *args) -> Optional[str]:
        """Run a git command and return output"""
        try:
            result = subprocess.run(
                ['git'] + list(args),
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            return result.stdout.strip()
        except subprocess.CalledProcessError as e:
            print(f"Git command failed: {e}")
            return None
    
    def get_branch_info(self, branch: str, is_remote: bool = False) -> Optional[BranchInfo]:
        """Get detailed information about a branch"""
        # Skip if it's the HEAD pointer
        if '->' in branch or 'HEAD' in branch:
            return None
            
        # Clean branch name
        if is_remote and branch.startswith('origin/'):
            branch_name = branch[7:]  # Remove 'origin/' prefix
        else:
            branch_name = branch.strip()
        
        # Get last commit date
        date_output = self.run_git_command(
            'log', '-1', '--format=%ci', branch
        )
        if not date_output:
            return None
            
        last_commit_date = datetime.strptime(date_output[:19], '%Y-%m-%d %H:%M:%S')
        
        # Get last commit message
        msg_output = self.run_git_command(
            'log', '-1', '--format=%s', branch
        )
        
        # Check if merged to master/main
        main_branch = self.get_main_branch()
        merged_branches = self.run_git_command('branch', '--merged', main_branch)
        is_merged = branch_name in (merged_branches or '')
        
        # Get ahead/behind counts
        rev_output = self.run_git_command(
            'rev-list', '--left-right', '--count', 
            f'{main_branch}...{branch}'
        )
        behind_count, ahead_count = 0, 0
        if rev_output:
            parts = rev_output.split()
            if len(parts) == 2:
                behind_count, ahead_count = int(parts[0]), int(parts[1])
        
        # Determine status
        status = self.categorize_branch(
            branch_name, is_merged, last_commit_date, ahead_count
        )
        
        return BranchInfo(
            name=branch_name,
            last_commit_date=last_commit_date,
            is_merged=is_merged,
            ahead_count=ahead_count,
            behind_count=behind_count,
            last_commit_message=msg_output or '',
            status=status,
            is_remote=is_remote
        )
    
    def categorize_branch(self, name: str, is_merged: bool, 
                         last_commit_date: datetime, ahead_count: int) -> BranchStatus:
        """Categorize a branch based on its characteristics"""
        # Check if protected
        if name in self.PROTECTED_BRANCHES:
            return BranchStatus.PROTECTED
        
        # Check if release branch
        for pattern in self.RELEASE_PATTERNS:
            if pattern in name or name.startswith('release/'):
                return BranchStatus.RELEASE
        
        # Check if merged
        if is_merged:
            return BranchStatus.MERGED
        
        # Check if stale
        if last_commit_date < self.stale_date:
            return BranchStatus.UNMERGED_STALE
        
        return BranchStatus.UNMERGED_ACTIVE
    
    def get_main_branch(self) -> str:
        """Detect the main branch (master or main)"""
        branches = self.run_git_command('branch', '-a')
        if 'main' in branches:
            return 'main'
        return 'master'
    
    def get_all_branches(self) -> Tuple[List[BranchInfo], List[BranchInfo]]:
        """Get all local and remote branches"""
        local_branches = []
        remote_branches = []
        
        # Get local branches
        local_output = self.run_git_command('branch')
        if local_output:
            for line in local_output.split('\n'):
                branch = line.strip().replace('* ', '')
                if branch:
                    info = self.get_branch_info(branch)
                    if info:
                        local_branches.append(info)
        
        # Get remote branches
        remote_output = self.run_git_command('branch', '-r')
        if remote_output:
            for line in remote_output.split('\n'):
                branch = line.strip()
                if branch and 'HEAD' not in branch:
                    info = self.get_branch_info(branch, is_remote=True)
                    if info:
                        remote_branches.append(info)
        
        return local_branches, remote_branches
    
    def archive_branch(self, branch: BranchInfo) -> bool:
        """Archive a branch by creating a tag"""
        if self.dry_run:
            print(f"  [DRY RUN] Would archive branch '{branch.name}' as tag 'archive/{branch.name}'")
            return True
        
        tag_name = f"archive/{branch.name}_{datetime.now().strftime('%Y%m%d')}"
        result = self.run_git_command(
            'tag', '-a', tag_name, branch.name,
            '-m', f"Archived branch {branch.name} on {datetime.now().strftime('%Y-%m-%d')}"
        )
        
        if result is not None:
            print(f"  [OK] Archived branch '{branch.name}' as tag '{tag_name}'")
            # Push tag to remote
            self.run_git_command('push', 'origin', tag_name)
            return True
        return False
    
    def delete_local_branch(self, branch: BranchInfo) -> bool:
        """Delete a local branch"""
        if self.dry_run:
            print(f"  [DRY RUN] Would delete local branch '{branch.name}'")
            return True
        
        # Use -D for force delete if unmerged
        flag = '-d' if branch.is_merged else '-D'
        result = self.run_git_command('branch', flag, branch.name)
        
        if result is not None:
            print(f"  [OK] Deleted local branch '{branch.name}'")
            return True
        return False
    
    def delete_remote_branch(self, branch: BranchInfo) -> bool:
        """Delete a remote branch"""
        if self.dry_run:
            print(f"  [DRY RUN] Would delete remote branch 'origin/{branch.name}'")
            return True
        
        result = self.run_git_command('push', 'origin', '--delete', branch.name)
        
        if result is not None:
            print(f"  [OK] Deleted remote branch 'origin/{branch.name}'")
            return True
        return False
    
    def generate_report(self, local_branches: List[BranchInfo], 
                       remote_branches: List[BranchInfo]) -> Dict:
        """Generate a cleanup report"""
        report = {
            'repository': str(self.repo_path),
            'scan_date': datetime.now().isoformat(),
            'dry_run': self.dry_run,
            'stale_days': self.stale_days,
            'summary': {
                'total_local': len(local_branches),
                'total_remote': len(remote_branches),
                'merged': 0,
                'stale': 0,
                'active': 0,
                'protected': 0,
                'release': 0
            },
            'branches': {
                'local': [],
                'remote': []
            }
        }
        
        # Process branches
        for branches, key in [(local_branches, 'local'), (remote_branches, 'remote')]:
            for branch in branches:
                branch_data = {
                    'name': branch.name,
                    'status': branch.status.value,
                    'last_commit': branch.last_commit_date.isoformat(),
                    'days_old': (datetime.now() - branch.last_commit_date).days,
                    'is_merged': branch.is_merged,
                    'ahead': branch.ahead_count,
                    'behind': branch.behind_count,
                    'last_message': branch.last_commit_message[:100]
                }
                report['branches'][key].append(branch_data)
                
                # Update summary
                if branch.status == BranchStatus.MERGED:
                    report['summary']['merged'] += 1
                elif branch.status == BranchStatus.UNMERGED_STALE:
                    report['summary']['stale'] += 1
                elif branch.status == BranchStatus.UNMERGED_ACTIVE:
                    report['summary']['active'] += 1
                elif branch.status == BranchStatus.PROTECTED:
                    report['summary']['protected'] += 1
                elif branch.status == BranchStatus.RELEASE:
                    report['summary']['release'] += 1
        
        return report
    
    def cleanup(self) -> Dict:
        """Main cleanup process"""
        print(f"\n{'='*60}")
        print(f"Branch Cleanup for: {self.repo_path}")
        print(f"Mode: {'DRY RUN' if self.dry_run else 'EXECUTE'}")
        print(f"Stale threshold: {self.stale_days} days")
        print(f"{'='*60}\n")
        
        # Get all branches
        local_branches, remote_branches = self.get_all_branches()
        
        # Generate report
        report = self.generate_report(local_branches, remote_branches)
        
        # Print summary
        print("Branch Summary:")
        print(f"  Local branches: {report['summary']['total_local']}")
        print(f"  Remote branches: {report['summary']['total_remote']}")
        print(f"  Merged: {report['summary']['merged']}")
        print(f"  Stale: {report['summary']['stale']}")
        print(f"  Active: {report['summary']['active']}")
        print(f"  Protected: {report['summary']['protected']}")
        print(f"  Release: {report['summary']['release']}")
        print()
        
        # Process branches for cleanup
        branches_to_delete = []
        branches_to_archive = []
        
        for branch in local_branches + remote_branches:
            if branch.status == BranchStatus.MERGED:
                branches_to_delete.append(branch)
            elif branch.status == BranchStatus.UNMERGED_STALE:
                branches_to_archive.append(branch)
        
        # Archive stale unmerged branches
        if branches_to_archive:
            print(f"\n[ARCHIVE] Archiving {len(branches_to_archive)} stale unmerged branches:")
            for branch in branches_to_archive:
                print(f"\n  Branch: {branch.name}")
                print(f"  Last commit: {branch.last_commit_date.strftime('%Y-%m-%d')} ({(datetime.now() - branch.last_commit_date).days} days ago)")
                print(f"  Commits ahead: {branch.ahead_count}")
                self.archive_branch(branch)
                if not self.dry_run or True:  # Archive creates tag, then delete
                    branches_to_delete.append(branch)
        
        # Delete branches
        if branches_to_delete:
            print(f"\n[DELETE] Deleting {len(branches_to_delete)} branches:")
            for branch in branches_to_delete:
                print(f"\n  Branch: {branch.name}")
                print(f"  Status: {branch.status.value}")
                if branch.is_remote:
                    self.delete_remote_branch(branch)
                else:
                    self.delete_local_branch(branch)
        
        # Prune remote tracking branches
        if not self.dry_run:
            print("\n[PRUNE] Pruning remote tracking branches...")
            self.run_git_command('remote', 'prune', 'origin')
        else:
            print("\n[DRY RUN] Would prune remote tracking branches")
        
        print(f"\n{'='*60}")
        print("Cleanup complete!")
        if self.dry_run:
            print("This was a DRY RUN. No changes were made.")
            print("Run with --execute to perform actual cleanup.")
        print(f"{'='*60}\n")
        
        return report


def cleanup_all_repos(base_path: str, dry_run: bool = True, stale_days: int = 60):
    """Clean up branches in all repositories"""
    base_path = Path(base_path)
    reports = []
    
    # Find all git repositories
    repos = []
    for item in base_path.iterdir():
        if item.is_dir() and (item / '.git').exists():
            repos.append(item)
    
    print(f"Found {len(repos)} repositories to clean\n")
    
    for repo in repos:
        cleaner = GitBranchCleaner(repo, dry_run=dry_run, stale_days=stale_days)
        report = cleaner.cleanup()
        reports.append(report)
    
    # Save consolidated report
    report_file = base_path / f"branch_cleanup_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(report_file, 'w') as f:
        json.dump(reports, f, indent=2, default=str)
    
    print(f"\n[REPORT] Full report saved to: {report_file}")
    
    # Print summary
    total_deleted = sum(r['summary']['merged'] + r['summary']['stale'] for r in reports)
    print(f"\n[TOTAL] Total branches to clean across all repos: {total_deleted}")


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description='Clean up stale git branches across repositories'
    )
    parser.add_argument(
        'path',
        nargs='?',
        default='.',
        help='Path to repository or parent directory of multiple repos'
    )
    parser.add_argument(
        '--execute',
        action='store_true',
        help='Actually perform cleanup (default is dry-run)'
    )
    parser.add_argument(
        '--stale-days',
        type=int,
        default=60,
        help='Number of days to consider a branch stale (default: 60)'
    )
    parser.add_argument(
        '--all-repos',
        action='store_true',
        help='Process all repositories in the given directory'
    )
    
    args = parser.parse_args()
    
    dry_run = not args.execute
    
    if args.all_repos:
        cleanup_all_repos(args.path, dry_run=dry_run, stale_days=args.stale_days)
    else:
        cleaner = GitBranchCleaner(args.path, dry_run=dry_run, stale_days=args.stale_days)
        cleaner.cleanup()


if __name__ == '__main__':
    main()