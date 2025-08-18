#!/usr/bin/env python3
"""
Unified Git Command - Consolidates all git operations

This command replaces:
- git-sync, git-sync-all-enhanced
- git-trunk-flow, git-trunk-flow-enhanced  
- git-trunk-status, git-trunk-sync-all
- git-commit-push-merge-all

Usage:
    /git status              # Check status of current repo or all repos
    /git sync                # Sync current repo with remote
    /git sync --all          # Sync all repositories
    /git trunk               # Ensure trunk-based development
    /git commit "message"    # Commit, push and merge
    /git clean               # Clean stale branches and data
    /git help                # Show this help
"""

import os
import sys
import argparse
import subprocess
from pathlib import Path
from typing import List, Tuple, Dict, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
import json
from datetime import datetime

class UnifiedGitCommand:
    """Unified handler for all git operations."""
    
    def __init__(self):
        self.base_path = Path("/mnt/github/github")
        self.current_repo = self._get_current_repo()
        self.all_repos = self._get_all_repos()
        
    def _get_current_repo(self) -> Optional[str]:
        """Get current repository name."""
        cwd = Path.cwd()
        if self.base_path in cwd.parents or cwd == self.base_path:
            # Extract repo name from path
            relative = cwd.relative_to(self.base_path)
            parts = relative.parts
            return parts[0] if parts else None
        return None
    
    def _get_all_repos(self) -> List[str]:
        """Get list of all repositories."""
        repos = []
        for item in self.base_path.iterdir():
            if item.is_dir() and (item / '.git').exists():
                repos.append(item.name)
        return sorted(repos)
    
    def status(self, all_repos: bool = False) -> Dict:
        """Check git status of repositories."""
        if all_repos:
            print("📊 Checking status of all repositories...\n")
            return self._status_all()
        else:
            if self.current_repo:
                print(f"📊 Status of {self.current_repo}:\n")
                return self._status_single(self.current_repo)
            else:
                print("⚠️  Not in a git repository. Use --all to check all repos.")
                return {}
    
    def _status_single(self, repo: str) -> Dict:
        """Get status of a single repository."""
        repo_path = self.base_path / repo
        
        try:
            os.chdir(repo_path)
            
            # Get current branch
            branch = subprocess.run(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                capture_output=True, text=True
            ).stdout.strip()
            
            # Get status
            status = subprocess.run(
                ["git", "status", "--porcelain"],
                capture_output=True, text=True
            ).stdout
            
            # Check if up to date with remote
            subprocess.run(["git", "fetch"], capture_output=True)
            behind = subprocess.run(
                ["git", "rev-list", f"HEAD..origin/{branch}", "--count"],
                capture_output=True, text=True
            ).stdout.strip()
            
            ahead = subprocess.run(
                ["git", "rev-list", f"origin/{branch}..HEAD", "--count"],
                capture_output=True, text=True
            ).stdout.strip()
            
            # Get stash count
            stash_list = subprocess.run(
                ["git", "stash", "list"],
                capture_output=True, text=True
            ).stdout
            stash_count = len(stash_list.strip().split('\n')) if stash_list.strip() else 0
            
            result = {
                'repo': repo,
                'branch': branch,
                'clean': len(status) == 0,
                'behind': int(behind) if behind else 0,
                'ahead': int(ahead) if ahead else 0,
                'stashes': stash_count,
                'changes': status.strip().split('\n') if status.strip() else []
            }
            
            # Print status
            status_icon = "✅" if result['clean'] else "⚠️"
            print(f"{status_icon} {repo} [{branch}]")
            
            if not result['clean']:
                print(f"   Uncommitted changes: {len(result['changes'])} files")
            
            if result['behind'] > 0:
                print(f"   Behind remote: {result['behind']} commits")
            
            if result['ahead'] > 0:
                print(f"   Ahead of remote: {result['ahead']} commits")
                
            if result['stashes'] > 0:
                print(f"   Stashes: {result['stashes']}")
            
            print()
            return result
            
        except Exception as e:
            print(f"❌ Error checking {repo}: {e}\n")
            return {'repo': repo, 'error': str(e)}
    
    def _status_all(self) -> Dict:
        """Get status of all repositories."""
        results = {}
        
        with ThreadPoolExecutor(max_workers=10) as executor:
            futures = {executor.submit(self._status_single, repo): repo 
                      for repo in self.all_repos}
            
            for future in as_completed(futures):
                repo = futures[future]
                results[repo] = future.result()
        
        # Summary
        clean_repos = sum(1 for r in results.values() if r.get('clean', False))
        print(f"\n📈 Summary:")
        print(f"   Total repos: {len(self.all_repos)}")
        print(f"   Clean repos: {clean_repos}")
        print(f"   Repos with changes: {len(self.all_repos) - clean_repos}")
        
        return results
    
    def sync(self, all_repos: bool = False) -> Dict:
        """Sync repositories with remote."""
        if all_repos:
            print("🔄 Syncing all repositories...\n")
            return self._sync_all()
        else:
            if self.current_repo:
                print(f"🔄 Syncing {self.current_repo}...\n")
                return self._sync_single(self.current_repo)
            else:
                print("⚠️  Not in a git repository. Use --all to sync all repos.")
                return {}
    
    def _sync_single(self, repo: str) -> Dict:
        """Sync a single repository."""
        repo_path = self.base_path / repo
        
        try:
            os.chdir(repo_path)
            
            # Fetch latest
            print(f"📡 Fetching {repo}...")
            subprocess.run(["git", "fetch", "--all"], check=True)
            
            # Get current branch
            branch = subprocess.run(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                capture_output=True, text=True
            ).stdout.strip()
            
            # Pull latest changes
            print(f"⬇️  Pulling latest changes for {repo}...")
            result = subprocess.run(
                ["git", "pull", "origin", branch],
                capture_output=True, text=True
            )
            
            if "Already up to date" in result.stdout:
                print(f"✅ {repo} is up to date")
            else:
                print(f"✅ {repo} synced successfully")
            
            return {'repo': repo, 'status': 'synced', 'branch': branch}
            
        except subprocess.CalledProcessError as e:
            print(f"❌ Failed to sync {repo}: {e}")
            return {'repo': repo, 'status': 'failed', 'error': str(e)}
    
    def _sync_all(self) -> Dict:
        """Sync all repositories."""
        results = {}
        
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = {executor.submit(self._sync_single, repo): repo 
                      for repo in self.all_repos}
            
            for future in as_completed(futures):
                repo = futures[future]
                results[repo] = future.result()
        
        # Summary
        synced = sum(1 for r in results.values() if r.get('status') == 'synced')
        print(f"\n✅ Synced {synced}/{len(self.all_repos)} repositories")
        
        return results
    
    def trunk(self, all_repos: bool = False) -> Dict:
        """Ensure trunk-based development."""
        if all_repos:
            print("🌳 Enforcing trunk-based development for all repos...\n")
            repos = self.all_repos
        else:
            if self.current_repo:
                print(f"🌳 Enforcing trunk-based development for {self.current_repo}...\n")
                repos = [self.current_repo]
            else:
                print("⚠️  Not in a git repository. Use --all for all repos.")
                return {}
        
        results = {}
        for repo in repos:
            results[repo] = self._enforce_trunk(repo)
        
        return results
    
    def _enforce_trunk(self, repo: str) -> Dict:
        """Enforce trunk-based development for a repository."""
        repo_path = self.base_path / repo
        
        try:
            os.chdir(repo_path)
            
            # Get current branch
            current = subprocess.run(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                capture_output=True, text=True
            ).stdout.strip()
            
            # Determine trunk branch (main or master)
            branches = subprocess.run(
                ["git", "branch", "-r"],
                capture_output=True, text=True
            ).stdout
            
            trunk = "main" if "origin/main" in branches else "master"
            
            if current != trunk:
                print(f"⚠️  {repo}: Switching from {current} to {trunk}")
                
                # Stash changes if any
                status = subprocess.run(
                    ["git", "status", "--porcelain"],
                    capture_output=True, text=True
                ).stdout
                
                if status:
                    print(f"   Stashing changes in {repo}...")
                    subprocess.run(["git", "stash"], check=True)
                
                # Switch to trunk
                subprocess.run(["git", "checkout", trunk], check=True)
                
                # Pull latest
                subprocess.run(["git", "pull", "origin", trunk], check=True)
                
                print(f"✅ {repo}: Now on {trunk} branch")
            else:
                print(f"✅ {repo}: Already on {trunk} branch")
            
            # Clean up old branches
            self._clean_branches(repo)
            
            return {'repo': repo, 'trunk': trunk, 'status': 'success'}
            
        except Exception as e:
            print(f"❌ {repo}: Failed to enforce trunk - {e}")
            return {'repo': repo, 'status': 'failed', 'error': str(e)}
    
    def _clean_branches(self, repo: str):
        """Clean up stale branches."""
        try:
            # Get all local branches
            branches = subprocess.run(
                ["git", "branch"],
                capture_output=True, text=True
            ).stdout.strip().split('\n')
            
            # Clean up merged branches
            for branch in branches:
                branch = branch.strip().replace('* ', '')
                if branch not in ['main', 'master']:
                    try:
                        subprocess.run(
                            ["git", "branch", "-d", branch],
                            capture_output=True, check=True
                        )
                        print(f"   Deleted merged branch: {branch}")
                    except:
                        pass  # Branch not fully merged, skip
                        
        except Exception:
            pass
    
    def commit(self, message: str, push: bool = True) -> Dict:
        """Commit changes with automatic push."""
        if not self.current_repo:
            print("❌ Not in a git repository")
            return {'status': 'failed', 'error': 'Not in a repository'}
        
        try:
            repo_path = self.base_path / self.current_repo
            os.chdir(repo_path)
            
            # Check for changes
            status = subprocess.run(
                ["git", "status", "--porcelain"],
                capture_output=True, text=True
            ).stdout
            
            if not status:
                print("✅ No changes to commit")
                return {'status': 'no_changes'}
            
            # Add all changes
            print("📝 Adding changes...")
            subprocess.run(["git", "add", "-A"], check=True)
            
            # Commit
            print(f"💾 Committing: {message}")
            subprocess.run(["git", "commit", "-m", message], check=True)
            
            if push:
                # Get current branch
                branch = subprocess.run(
                    ["git", "rev-parse", "--abbrev-ref", "HEAD"],
                    capture_output=True, text=True
                ).stdout.strip()
                
                # Push
                print(f"⬆️  Pushing to origin/{branch}...")
                subprocess.run(["git", "push", "origin", branch], check=True)
                
                print(f"✅ Changes committed and pushed successfully")
            else:
                print(f"✅ Changes committed locally")
            
            return {'status': 'success', 'message': message}
            
        except subprocess.CalledProcessError as e:
            print(f"❌ Failed to commit: {e}")
            return {'status': 'failed', 'error': str(e)}
    
    def clean(self, all_repos: bool = False) -> Dict:
        """Clean stale branches and data."""
        if all_repos:
            print("🧹 Cleaning all repositories...\n")
            repos = self.all_repos
        else:
            if self.current_repo:
                print(f"🧹 Cleaning {self.current_repo}...\n")
                repos = [self.current_repo]
            else:
                print("⚠️  Not in a git repository. Use --all for all repos.")
                return {}
        
        results = {}
        for repo in repos:
            results[repo] = self._clean_repo(repo)
        
        return results
    
    def _clean_repo(self, repo: str) -> Dict:
        """Clean a single repository."""
        repo_path = self.base_path / repo
        
        try:
            os.chdir(repo_path)
            
            cleaned = []
            
            # Clean merged branches
            branches = subprocess.run(
                ["git", "branch", "--merged"],
                capture_output=True, text=True
            ).stdout.strip().split('\n')
            
            for branch in branches:
                branch = branch.strip().replace('* ', '')
                if branch not in ['main', 'master']:
                    try:
                        subprocess.run(
                            ["git", "branch", "-d", branch],
                            check=True, capture_output=True
                        )
                        cleaned.append(f"branch: {branch}")
                    except:
                        pass
            
            # Clean remote tracking branches
            subprocess.run(["git", "remote", "prune", "origin"], check=True)
            
            # Clean up git objects
            subprocess.run(["git", "gc", "--auto"], check=True)
            
            if cleaned:
                print(f"✅ {repo}: Cleaned {len(cleaned)} items")
                for item in cleaned:
                    print(f"   - {item}")
            else:
                print(f"✅ {repo}: Already clean")
            
            return {'repo': repo, 'cleaned': cleaned}
            
        except Exception as e:
            print(f"❌ {repo}: Cleaning failed - {e}")
            return {'repo': repo, 'error': str(e)}
    
    def help(self):
        """Show help information."""
        help_text = """
🔧 Unified Git Command

Usage: /git [subcommand] [options]

Subcommands:
  status          Check repository status
                  Options: --all (check all repos)
  
  sync            Sync with remote repository  
                  Options: --all (sync all repos)
  
  trunk           Enforce trunk-based development
                  Options: --all (apply to all repos)
  
  commit MESSAGE  Commit and push changes
                  Example: /git commit "Fix bug in module"
  
  clean           Clean stale branches and data
                  Options: --all (clean all repos)
  
  help            Show this help message

Examples:
  /git status                # Check current repo
  /git status --all          # Check all repos
  /git sync                  # Sync current repo
  /git sync --all            # Sync all repos
  /git trunk                 # Switch to trunk branch
  /git commit "Add feature"  # Commit and push
  /git clean                 # Clean current repo

This command consolidates all git operations into one unified interface.
"""
        print(help_text)
        return {'status': 'help_shown'}

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        prog='git',
        description='Unified git command for all operations',
        add_help=False  # We'll handle help ourselves
    )
    
    parser.add_argument('subcommand', nargs='?', default='help',
                       choices=['status', 'sync', 'trunk', 'commit', 'clean', 'help'])
    parser.add_argument('message', nargs='?', help='Commit message')
    parser.add_argument('--all', action='store_true', help='Apply to all repositories')
    
    # Parse args
    args, unknown = parser.parse_known_args()
    
    # Handle the case where message contains spaces
    if args.subcommand == 'commit' and unknown:
        # Combine message with unknown args
        full_message = ' '.join([args.message] + unknown) if args.message else ' '.join(unknown)
        args.message = full_message
    
    # Create command instance
    git_cmd = UnifiedGitCommand()
    
    # Execute subcommand
    if args.subcommand == 'status':
        result = git_cmd.status(all_repos=args.all)
    elif args.subcommand == 'sync':
        result = git_cmd.sync(all_repos=args.all)
    elif args.subcommand == 'trunk':
        result = git_cmd.trunk(all_repos=args.all)
    elif args.subcommand == 'commit':
        if not args.message:
            print("❌ Commit message required")
            print("Usage: /git commit \"your message\"")
            sys.exit(1)
        result = git_cmd.commit(args.message)
    elif args.subcommand == 'clean':
        result = git_cmd.clean(all_repos=args.all)
    else:  # help
        result = git_cmd.help()
    
    # Return success/failure
    if isinstance(result, dict) and result.get('status') == 'failed':
        sys.exit(1)

if __name__ == '__main__':
    main()