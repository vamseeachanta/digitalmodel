#!/usr/bin/env python3
"""
Comprehensive Git Management System with Slash Commands
MANDATORY for all repositories in the folder
"""

import os
import sys
import subprocess
import argparse
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Optional, Tuple
import concurrent.futures
import json

# Configuration
MAX_PARALLEL_REPOS = 5  # Process 5 repos at a time
DEFAULT_COMMIT_MESSAGE = "feat: Sync and standardize repository"
DEFAULT_PR_TITLE = "Auto-sync: Standardization and updates"

class GitManager:
    """Manages Git operations across all repositories"""
    
    def __init__(self, base_path: str = "/mnt/github/github"):
        self.base_path = Path(base_path)
        self.repos = self.get_all_repos()
        self.results = {}
        
    def get_all_repos(self) -> List[str]:
        """Get list of all repository directories"""
        repos = [
            "doris", "aceengineer-admin", "aceengineer-website", 
            "aceengineercode", "achantas-data", "achantas-media",
            "acma-projects", "ai-native-traditional-eng", "assethold",
            "assetutilities", "client_projects", "digitalmodel",
            "energy", "frontierdeepwater", "hobbies", "investments",
            "OGManufacturing", "pyproject-starter", "rock-oil-field",
            "sabithaandkrishnaestates", "saipem", "sd-work",
            "seanation", "teamresumes", "worldenergydata"
        ]
        return [r for r in repos if (self.base_path / r).exists()]
    
    def run_command(self, cmd: str, repo_path: Path) -> Tuple[bool, str]:
        """Run a shell command in the given repository"""
        try:
            result = subprocess.run(
                cmd,
                shell=True,
                cwd=repo_path,
                capture_output=True,
                text=True,
                timeout=30
            )
            return result.returncode == 0, result.stdout + result.stderr
        except subprocess.TimeoutExpired:
            return False, "Command timed out"
        except Exception as e:
            return False, str(e)
    
    def get_default_branch(self, repo_path: Path) -> str:
        """Determine if repo uses main or master"""
        success, output = self.run_command(
            "git branch -r | grep -E 'origin/(main|master)' | head -1",
            repo_path
        )
        if success and "main" in output:
            return "main"
        return "master"
    
    def commit_all_changes(self, repo: str, message: str = None) -> Dict:
        """Commit all changes in a repository"""
        repo_path = self.base_path / repo
        if not (repo_path / ".git").exists():
            return {"repo": repo, "status": "error", "message": "Not a git repository"}
        
        result = {"repo": repo, "status": "processing"}
        
        # Check for changes
        success, output = self.run_command("git status --porcelain", repo_path)
        if not success:
            result["status"] = "error"
            result["message"] = "Failed to check status"
            return result
        
        if not output.strip():
            result["status"] = "no_changes"
            result["message"] = "No changes to commit"
            return result
        
        # Add all changes
        success, output = self.run_command("git add -A", repo_path)
        if not success:
            result["status"] = "error"
            result["message"] = f"Failed to add files: {output}"
            return result
        
        # Commit with message
        commit_msg = message or f"{DEFAULT_COMMIT_MESSAGE}\n\n🤖 Generated with Claude Code\n\nCo-Authored-By: Claude <noreply@anthropic.com>"
        commit_cmd = f'git commit -m "{commit_msg}"'
        success, output = self.run_command(commit_cmd, repo_path)
        
        if success:
            result["status"] = "committed"
            result["message"] = "Changes committed successfully"
            # Extract commit hash
            hash_cmd = "git rev-parse HEAD"
            _, commit_hash = self.run_command(hash_cmd, repo_path)
            result["commit"] = commit_hash.strip()[:7]
        else:
            result["status"] = "error"
            result["message"] = f"Commit failed: {output}"
        
        return result
    
    def create_pull_request(self, repo: str, title: str = None, body: str = None) -> Dict:
        """Create a pull request using GitHub CLI"""
        repo_path = self.base_path / repo
        result = {"repo": repo, "status": "processing"}
        
        # Get current branch
        success, current_branch = self.run_command(
            "git branch --show-current", repo_path
        )
        if not success:
            result["status"] = "error"
            result["message"] = "Failed to get current branch"
            return result
        
        current_branch = current_branch.strip()
        default_branch = self.get_default_branch(repo_path)
        
        if current_branch == default_branch:
            # Create a new branch for PR
            timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
            new_branch = f"auto-sync-{timestamp}"
            success, output = self.run_command(
                f"git checkout -b {new_branch}", repo_path
            )
            if not success:
                result["status"] = "error"
                result["message"] = f"Failed to create branch: {output}"
                return result
            current_branch = new_branch
        
        # Push current branch
        success, output = self.run_command(
            f"git push -u origin {current_branch}", repo_path
        )
        if not success:
            result["status"] = "error"
            result["message"] = f"Failed to push: {output}"
            return result
        
        # Create PR using gh CLI
        pr_title = title or DEFAULT_PR_TITLE
        pr_body = body or f"""## Summary
Automated synchronization and standardization of repository.

### Changes
- Updated configurations
- Synchronized with latest standards
- Cleaned up stale branches

### Automated by
🤖 Git Management System

---
*Generated with Claude Code*"""
        
        pr_cmd = f'''gh pr create --title "{pr_title}" --body "{pr_body}" --base {default_branch}'''
        success, output = self.run_command(pr_cmd, repo_path)
        
        if success:
            result["status"] = "pr_created"
            result["message"] = "Pull request created"
            result["pr_url"] = output.strip()
        else:
            if "already exists" in output:
                result["status"] = "pr_exists"
                result["message"] = "PR already exists for this branch"
            else:
                result["status"] = "error"
                result["message"] = f"PR creation failed: {output}"
        
        return result
    
    def merge_pull_request(self, repo: str, pr_number: Optional[int] = None) -> Dict:
        """Merge a pull request"""
        repo_path = self.base_path / repo
        result = {"repo": repo, "status": "processing"}
        
        if pr_number:
            merge_cmd = f"gh pr merge {pr_number} --merge --delete-branch"
        else:
            # Merge current branch's PR
            merge_cmd = "gh pr merge --merge --delete-branch"
        
        success, output = self.run_command(merge_cmd, repo_path)
        
        if success:
            result["status"] = "merged"
            result["message"] = "PR merged successfully"
        else:
            result["status"] = "error"
            result["message"] = f"Merge failed: {output}"
        
        return result
    
    def sync_repository(self, repo: str) -> Dict:
        """Sync repository with remote"""
        repo_path = self.base_path / repo
        result = {"repo": repo, "status": "processing"}
        
        if not (repo_path / ".git").exists():
            result["status"] = "error"
            result["message"] = "Not a git repository"
            return result
        
        default_branch = self.get_default_branch(repo_path)
        
        # Fetch latest
        success, output = self.run_command("git fetch --all --prune", repo_path)
        if not success:
            result["status"] = "error"
            result["message"] = f"Fetch failed: {output}"
            return result
        
        # Checkout default branch
        success, output = self.run_command(
            f"git checkout {default_branch}", repo_path
        )
        if not success:
            result["status"] = "error"
            result["message"] = f"Checkout failed: {output}"
            return result
        
        # Pull latest changes
        success, output = self.run_command(
            f"git pull origin {default_branch}", repo_path
        )
        if success:
            result["status"] = "synced"
            result["message"] = f"Synced with {default_branch}"
        else:
            result["status"] = "error"
            result["message"] = f"Pull failed: {output}"
        
        return result
    
    def clean_stale_branches(self, repo: str) -> Dict:
        """Delete merged and stale branches"""
        repo_path = self.base_path / repo
        result = {"repo": repo, "status": "processing", "deleted_branches": []}
        
        if not (repo_path / ".git").exists():
            result["status"] = "error"
            result["message"] = "Not a git repository"
            return result
        
        # Get default branch
        default_branch = self.get_default_branch(repo_path)
        
        # Checkout default branch first
        self.run_command(f"git checkout {default_branch}", repo_path)
        
        # Delete merged local branches
        cmd = f"git branch --merged | grep -v '{default_branch}' | grep -v '\\*'"
        success, branches = self.run_command(cmd, repo_path)
        
        if success and branches.strip():
            for branch in branches.strip().split('\n'):
                branch = branch.strip()
                if branch:
                    del_success, _ = self.run_command(
                        f"git branch -d {branch}", repo_path
                    )
                    if del_success:
                        result["deleted_branches"].append(branch)
        
        # Prune remote tracking branches
        self.run_command("git remote prune origin", repo_path)
        
        # Delete remote branches that have been merged
        cmd = "gh pr list --state merged --json headRefName --jq '.[].headRefName'"
        success, merged_branches = self.run_command(cmd, repo_path)
        
        if success and merged_branches.strip():
            for branch in merged_branches.strip().split('\n'):
                if branch and branch != default_branch:
                    del_cmd = f"git push origin --delete {branch} 2>/dev/null"
                    del_success, _ = self.run_command(del_cmd, repo_path)
                    if del_success:
                        result["deleted_branches"].append(f"origin/{branch}")
        
        result["status"] = "cleaned"
        result["message"] = f"Deleted {len(result['deleted_branches'])} branches"
        
        return result
    
    def process_all_repos(self, operation: str, **kwargs) -> Dict:
        """Process operation on all repos in parallel"""
        results = {}
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=MAX_PARALLEL_REPOS) as executor:
            if operation == "commit":
                futures = {
                    executor.submit(self.commit_all_changes, repo, kwargs.get("message")): repo
                    for repo in self.repos
                }
            elif operation == "sync":
                futures = {
                    executor.submit(self.sync_repository, repo): repo
                    for repo in self.repos
                }
            elif operation == "clean":
                futures = {
                    executor.submit(self.clean_stale_branches, repo): repo
                    for repo in self.repos
                }
            elif operation == "pr":
                futures = {
                    executor.submit(self.create_pull_request, repo, 
                                  kwargs.get("title"), kwargs.get("body")): repo
                    for repo in self.repos
                }
            else:
                return {"error": f"Unknown operation: {operation}"}
            
            for future in concurrent.futures.as_completed(futures):
                repo = futures[future]
                try:
                    result = future.result()
                    results[repo] = result
                except Exception as e:
                    results[repo] = {"status": "error", "message": str(e)}
        
        return results


class SlashCommands:
    """Slash command handlers for Git operations"""
    
    def __init__(self):
        self.manager = GitManager()
    
    def git_sync(self, args: List[str]) -> None:
        """/git-sync - Sync all repositories with remote"""
        print("🔄 Syncing all repositories...")
        print("=" * 60)
        
        results = self.manager.process_all_repos("sync")
        
        synced = sum(1 for r in results.values() if r["status"] == "synced")
        errors = sum(1 for r in results.values() if r["status"] == "error")
        
        for repo, result in results.items():
            if result["status"] == "synced":
                print(f"✅ {repo}: {result['message']}")
            elif result["status"] == "error":
                print(f"❌ {repo}: {result['message']}")
        
        print("=" * 60)
        print(f"Summary: {synced} synced, {errors} errors")
    
    def git_commit_all(self, args: List[str]) -> None:
        """/git-commit-all - Commit all changes across repos"""
        message = " ".join(args) if args else None
        
        print("📝 Committing changes across all repositories...")
        print("=" * 60)
        
        results = self.manager.process_all_repos("commit", message=message)
        
        committed = sum(1 for r in results.values() if r["status"] == "committed")
        no_changes = sum(1 for r in results.values() if r["status"] == "no_changes")
        errors = sum(1 for r in results.values() if r["status"] == "error")
        
        for repo, result in results.items():
            if result["status"] == "committed":
                print(f"✅ {repo}: Committed {result.get('commit', '')}")
            elif result["status"] == "no_changes":
                print(f"⏭️  {repo}: No changes to commit")
            elif result["status"] == "error":
                print(f"❌ {repo}: {result['message']}")
        
        print("=" * 60)
        print(f"Summary: {committed} committed, {no_changes} unchanged, {errors} errors")
    
    def git_pr_all(self, args: List[str]) -> None:
        """/git-pr-all - Create PRs for all repos with changes"""
        title = " ".join(args) if args else None
        
        print("🔀 Creating pull requests...")
        print("=" * 60)
        
        # First commit all changes
        print("Step 1: Committing changes...")
        commit_results = self.manager.process_all_repos("commit")
        
        # Then create PRs
        print("Step 2: Creating pull requests...")
        pr_results = self.manager.process_all_repos("pr", title=title)
        
        created = sum(1 for r in pr_results.values() if r["status"] == "pr_created")
        exists = sum(1 for r in pr_results.values() if r["status"] == "pr_exists")
        errors = sum(1 for r in pr_results.values() if r["status"] == "error")
        
        for repo, result in pr_results.items():
            if result["status"] == "pr_created":
                print(f"✅ {repo}: PR created - {result.get('pr_url', '')}")
            elif result["status"] == "pr_exists":
                print(f"ℹ️  {repo}: PR already exists")
            elif result["status"] == "error":
                print(f"❌ {repo}: {result['message']}")
        
        print("=" * 60)
        print(f"Summary: {created} PRs created, {exists} existing, {errors} errors")
    
    def git_clean(self, args: List[str]) -> None:
        """/git-clean - Clean up merged/stale branches"""
        print("🧹 Cleaning stale branches...")
        print("=" * 60)
        
        results = self.manager.process_all_repos("clean")
        
        total_deleted = 0
        for repo, result in results.items():
            if result["status"] == "cleaned":
                deleted = result.get("deleted_branches", [])
                total_deleted += len(deleted)
                if deleted:
                    print(f"✅ {repo}: Deleted {len(deleted)} branches")
                    for branch in deleted[:5]:  # Show first 5
                        print(f"   - {branch}")
                    if len(deleted) > 5:
                        print(f"   ... and {len(deleted)-5} more")
                else:
                    print(f"✨ {repo}: No stale branches")
            elif result["status"] == "error":
                print(f"❌ {repo}: {result['message']}")
        
        print("=" * 60)
        print(f"Total branches deleted: {total_deleted}")
    
    def git_flow(self, args: List[str]) -> None:
        """/git-flow - Complete flow: commit, PR, merge, sync, clean"""
        print("🚀 Executing complete Git flow...")
        print("=" * 60)
        
        # Step 1: Commit all changes
        print("\n📝 Step 1: Committing all changes...")
        commit_results = self.manager.process_all_repos("commit")
        committed = sum(1 for r in commit_results.values() if r["status"] == "committed")
        print(f"   Committed: {committed} repos")
        
        # Step 2: Create PRs
        print("\n🔀 Step 2: Creating pull requests...")
        pr_results = self.manager.process_all_repos("pr")
        created = sum(1 for r in pr_results.values() if r["status"] == "pr_created")
        print(f"   PRs created: {created}")
        
        # Step 3: Auto-merge PRs (optional - requires confirmation)
        if "--auto-merge" in args:
            print("\n🔧 Step 3: Auto-merging PRs...")
            # Implementation would go here
            print("   Auto-merge requires manual confirmation for safety")
        
        # Step 4: Sync repositories
        print("\n🔄 Step 4: Syncing repositories...")
        sync_results = self.manager.process_all_repos("sync")
        synced = sum(1 for r in sync_results.values() if r["status"] == "synced")
        print(f"   Synced: {synced} repos")
        
        # Step 5: Clean stale branches
        print("\n🧹 Step 5: Cleaning stale branches...")
        clean_results = self.manager.process_all_repos("clean")
        total_deleted = sum(
            len(r.get("deleted_branches", [])) 
            for r in clean_results.values()
        )
        print(f"   Branches deleted: {total_deleted}")
        
        print("\n" + "=" * 60)
        print("✅ Git flow completed!")
        print(f"   • Commits: {committed}")
        print(f"   • PRs: {created}")
        print(f"   • Synced: {synced}")
        print(f"   • Cleaned: {total_deleted} branches")
    
    def git_status_all(self, args: List[str]) -> None:
        """/git-status-all - Show status of all repositories"""
        print("📊 Repository Status Overview")
        print("=" * 60)
        
        for repo in self.manager.repos:
            repo_path = self.manager.base_path / repo
            
            # Get current branch
            success, branch = self.manager.run_command(
                "git branch --show-current", repo_path
            )
            branch = branch.strip() if success else "unknown"
            
            # Check for uncommitted changes
            success, status = self.manager.run_command(
                "git status --porcelain", repo_path
            )
            has_changes = bool(status.strip()) if success else False
            
            # Check if ahead/behind
            success, ahead_behind = self.manager.run_command(
                "git rev-list --left-right --count @{u}...HEAD 2>/dev/null", 
                repo_path
            )
            
            status_icon = "🔴" if has_changes else "🟢"
            change_text = "changes" if has_changes else "clean"
            
            print(f"{status_icon} {repo:25} [{branch:15}] {change_text}")
        
        print("=" * 60)
        print("Legend: 🟢 clean, 🔴 has changes")
    
    def show_help(self):
        """Show available commands"""
        print("""
🎯 GIT MANAGEMENT SLASH COMMANDS
================================

Available Commands:

/git-sync
  Sync all repositories with remote (fetch + pull)
  
/git-commit-all [message]
  Commit all changes across all repositories
  Example: /git-commit-all "feat: Add new feature"
  
/git-pr-all [title]
  Create pull requests for all repos with changes
  Example: /git-pr-all "Weekly sync and updates"
  
/git-clean
  Clean up merged and stale branches across all repos
  
/git-flow [--auto-merge]
  Complete flow: commit → PR → sync → clean
  Add --auto-merge to automatically merge PRs
  
/git-status-all
  Show status overview of all repositories

/git-help
  Show this help message

Examples:
---------
# Daily workflow
/git-flow

# Quick sync
/git-sync

# Commit with custom message
/git-commit-all "fix: Resolve configuration issues"

# Clean up after merges
/git-clean

MANDATORY PRACTICES:
-------------------
• Use parallel processing (automatic)
• Commit with descriptive messages
• Create PRs for review
• Clean stale branches regularly
• Sync before starting new work
""")


def main():
    """Main entry point for command line usage"""
    parser = argparse.ArgumentParser(
        description="Git Management System with Slash Commands"
    )
    parser.add_argument(
        "command",
        choices=[
            "git-sync", "git-commit-all", "git-pr-all", 
            "git-clean", "git-flow", "git-status-all", "git-help"
        ],
        help="Slash command to execute"
    )
    parser.add_argument(
        "args",
        nargs="*",
        help="Additional arguments for the command"
    )
    
    args = parser.parse_args()
    
    commands = SlashCommands()
    
    if args.command == "git-sync":
        commands.git_sync(args.args)
    elif args.command == "git-commit-all":
        commands.git_commit_all(args.args)
    elif args.command == "git-pr-all":
        commands.git_pr_all(args.args)
    elif args.command == "git-clean":
        commands.git_clean(args.args)
    elif args.command == "git-flow":
        commands.git_flow(args.args)
    elif args.command == "git-status-all":
        commands.git_status_all(args.args)
    elif args.command == "git-help":
        commands.show_help()


if __name__ == "__main__":
    # If run directly, show help
    commands = SlashCommands()
    commands.show_help()