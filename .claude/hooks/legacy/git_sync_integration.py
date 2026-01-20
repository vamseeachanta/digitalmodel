#!/usr/bin/env python
"""
Git Sync Integration with Verification Hooks
Ensures verification hooks are distributed across all repositories during sync
"""

import os
import sys
import shutil
import subprocess
from pathlib import Path
from typing import List, Dict, Tuple, Optional
import json
import time

# Add hooks to path
sys.path.append(str(Path(__file__).parent))
from verification_hooks import VerificationHooks, Colors


class GitSyncWithHooks:
    """Git sync operations with integrated verification hooks"""
    
    def __init__(self, base_dir: str = "/d/github", source_repo: str = "digitalmodel"):
        self.base_dir = Path(base_dir)
        self.source_repo = source_repo
        self.hooks = VerificationHooks()
        self.source_hooks_dir = self.base_dir / source_repo / ".agent-os" / "hooks"
        
    def sync_repository(self, repo_path: Path, branch: str = "master") -> bool:
        """Sync single repository with verification"""
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        print(f"{Colors.YELLOW}GIT SYNC WITH VERIFICATION{Colors.NC}")
        print(f"{Colors.YELLOW}Repository: {repo_path}{Colors.NC}")
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        
        # Pre-sync verification
        print(f"{Colors.BLUE}[PHASE 1]{Colors.NC} Pre-sync verification")
        if not self.hooks.verify_dir_exists(str(repo_path / ".git"), "Git repository"):
            return False
        
        # Check current status
        print(f"{Colors.BLUE}[PHASE 2]{Colors.NC} Checking repository status")
        status_cmd = f"cd {repo_path} && git status --short"
        subprocess.run(status_cmd, shell=True)
        
        # Fetch updates
        print(f"{Colors.BLUE}[PHASE 3]{Colors.NC} Fetching from remote")
        fetch_cmd = f"cd {repo_path} && git fetch --all --prune"
        if not self.hooks.verify_return_code(fetch_cmd, "Fetch all remotes"):
            return False
        
        # Pull changes
        print(f"{Colors.BLUE}[PHASE 4]{Colors.NC} Pulling changes")
        pull_cmd = f"cd {repo_path} && git pull origin {branch}"
        if not self.hooks.verify_return_code(pull_cmd, f"Pull from origin/{branch}"):
            # Try to handle merge conflicts
            print(f"{Colors.YELLOW}[WARN WARN]{Colors.NC} Pull failed, checking for conflicts")
            status_result = subprocess.run(
                f"cd {repo_path} && git status",
                shell=True, capture_output=True, text=True
            )
            if "conflict" in status_result.stdout.lower():
                print(f"{Colors.RED}[FAIL CONFLICT]{Colors.NC} Merge conflicts detected")
                return False
        
        # Verify hooks are present
        print(f"{Colors.BLUE}[PHASE 5]{Colors.NC} Verifying hooks")
        hooks_present = all([
            (repo_path / ".agent-os" / "hooks" / "verification_hooks.sh").exists(),
            (repo_path / ".agent-os" / "hooks" / "verification_hooks.py").exists()
        ])
        
        if hooks_present:
            print(f"{Colors.GREEN}[OK HOOKS]{Colors.NC} Verification hooks present")
        else:
            print(f"{Colors.YELLOW}[WARN HOOKS]{Colors.NC} Verification hooks missing")
        
        # Check for uncommitted changes
        print(f"{Colors.BLUE}[PHASE 6]{Colors.NC} Post-sync status")
        status_result = subprocess.run(
            f"cd {repo_path} && git status --porcelain",
            shell=True, capture_output=True, text=True
        )
        
        if status_result.stdout.strip():
            print(f"{Colors.YELLOW}[WARN WARN]{Colors.NC} Uncommitted changes present")
            print(status_result.stdout)
        else:
            print(f"{Colors.GREEN}[OK CLEAN]{Colors.NC} Working tree clean")
        
        print(f"{Colors.GREEN}[OK COMPLETE]{Colors.NC} Git sync complete")
        return True
    
    def copy_hooks_to_repo(self, target_repo: Path) -> bool:
        """Copy verification hooks to target repository"""
        print(f"{Colors.BLUE}[SYNC]{Colors.NC} Syncing hooks to: {target_repo}")
        
        if not target_repo.exists():
            print(f"{Colors.RED}[FAIL SKIP]{Colors.NC} Repository not found: {target_repo}")
            return False
        
        # Create hooks directory if it doesn't exist
        target_hooks_dir = target_repo / ".agent-os" / "hooks"
        if not target_hooks_dir.exists():
            print(f"{Colors.YELLOW}[CREATE]{Colors.NC} Creating hooks directory")
            target_hooks_dir.mkdir(parents=True, exist_ok=True)
        
        # Hook files to copy
        hook_files = [
            "verification_hooks.sh",
            "verification_hooks.py",
            "README.md",
            "example_usage.sh",
            "git_sync_integration.sh",
            "git_sync_integration.py"
        ]
        
        # Copy hook files
        print(f"{Colors.BLUE}[COPY]{Colors.NC} Copying hook files")
        copied = 0
        for hook_file in hook_files:
            source = self.source_hooks_dir / hook_file
            target = target_hooks_dir / hook_file
            
            if source.exists():
                shutil.copy2(source, target)
                copied += 1
                print(f"  {Colors.GREEN}OK{Colors.NC} {hook_file}")
            else:
                print(f"  {Colors.YELLOW}WARN{Colors.NC} {hook_file} not found in source")
        
        print(f"{Colors.GREEN}[OK COPIED]{Colors.NC} {copied} hook files")
        
        # Add to git if needed
        status_result = subprocess.run(
            f"cd {target_repo} && git status --porcelain .agent-os/hooks/",
            shell=True, capture_output=True, text=True
        )
        
        if status_result.stdout.strip():
            print(f"{Colors.YELLOW}[GIT]{Colors.NC} Adding hooks to git")
            add_result = subprocess.run(
                f"cd {target_repo} && git add .agent-os/hooks/",
                shell=True, capture_output=True
            )
            if add_result.returncode == 0:
                print(f"{Colors.GREEN}[OK ADDED]{Colors.NC} Hooks added to git")
            else:
                print(f"{Colors.RED}[FAIL ERROR]{Colors.NC} Failed to add hooks to git")
                return False
        else:
            print(f"{Colors.GREEN}[OK CURRENT]{Colors.NC} Hooks already up to date")
        
        return True
    
    def sync_all_repositories(self) -> Dict[str, bool]:
        """Sync all repositories with hooks distribution"""
        print(f"{Colors.YELLOW}╔{'═'*55}╗{Colors.NC}")
        print(f"{Colors.YELLOW}║     SYNCING ALL REPOSITORIES WITH HOOKS              ║{Colors.NC}")
        print(f"{Colors.YELLOW}╚{'═'*55}╝{Colors.NC}")
        
        # Find all git repositories
        repos = []
        for item in self.base_dir.iterdir():
            if item.is_dir() and (item / ".git").exists():
                repos.append(item)
        
        print(f"{Colors.BLUE}[INFO]{Colors.NC} Found {len(repos)} repositories")
        
        results = {}
        success_count = 0
        fail_count = 0
        
        # Verify source hooks exist
        if not self.source_hooks_dir.exists():
            print(f"{Colors.RED}[FAIL ERROR]{Colors.NC} Source hooks not found: {self.source_hooks_dir}")
            return results
        
        # Process each repository
        for repo in repos:
            repo_name = repo.name
            
            print(f"\n{Colors.BLUE}{'='*60}{Colors.NC}")
            print(f"{Colors.BLUE}Repository: {repo_name}{Colors.NC}")
            print(f"{Colors.BLUE}{'='*60}{Colors.NC}")
            
            # Skip source repository for hook copying
            if repo_name == self.source_repo:
                # Still sync it, just don't copy hooks to itself
                if self.sync_repository(repo):
                    results[repo_name] = True
                    success_count += 1
                    print(f"{Colors.GREEN}[OK SUCCESS]{Colors.NC} {repo_name} synced")
                else:
                    results[repo_name] = False
                    fail_count += 1
                    print(f"{Colors.RED}[FAIL FAIL]{Colors.NC} {repo_name} sync failed")
                continue
            
            # First sync the repository
            if self.sync_repository(repo):
                # Then copy the hooks
                if self.copy_hooks_to_repo(repo):
                    results[repo_name] = True
                    success_count += 1
                    print(f"{Colors.GREEN}[OK SUCCESS]{Colors.NC} {repo_name} synced with hooks")
                else:
                    results[repo_name] = False
                    fail_count += 1
                    print(f"{Colors.RED}[FAIL FAIL]{Colors.NC} {repo_name} hook sync failed")
            else:
                results[repo_name] = False
                fail_count += 1
                print(f"{Colors.RED}[FAIL FAIL]{Colors.NC} {repo_name} git sync failed")
        
        # Summary
        print(f"\n{Colors.YELLOW}{'='*60}{Colors.NC}")
        print(f"{Colors.YELLOW}SYNC SUMMARY{Colors.NC}")
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        print(f"{Colors.GREEN}Successful:{Colors.NC} {success_count}")
        print(f"{Colors.RED}Failed:{Colors.NC} {fail_count}")
        print(f"{Colors.BLUE}Total:{Colors.NC} {len(repos)}")
        
        if fail_count == 0:
            print(f"{Colors.GREEN}╔{'═'*35}╗{Colors.NC}")
            print(f"{Colors.GREEN}║     ALL REPOS SYNCED OK           ║{Colors.NC}")
            print(f"{Colors.GREEN}╚{'═'*35}╝{Colors.NC}")
        else:
            print(f"{Colors.RED}╔{'═'*35}╗{Colors.NC}")
            print(f"{Colors.RED}║     SOME REPOS FAILED FAIL          ║{Colors.NC}")
            print(f"{Colors.RED}╚{'═'*35}╝{Colors.NC}")
        
        # Save results to file
        self._save_sync_results(results)
        
        return results
    
    def _save_sync_results(self, results: Dict[str, bool]):
        """Save sync results to a JSON file"""
        results_file = self.source_hooks_dir / "last_sync_results.json"
        
        sync_data = {
            "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
            "base_dir": str(self.base_dir),
            "source_repo": self.source_repo,
            "results": results,
            "summary": {
                "total": len(results),
                "successful": sum(1 for v in results.values() if v),
                "failed": sum(1 for v in results.values() if not v)
            }
        }
        
        with open(results_file, 'w') as f:
            json.dump(sync_data, f, indent=2)
        
        print(f"{Colors.BLUE}[SAVED]{Colors.NC} Results saved to {results_file}")
    
    def verify_hooks_in_all_repos(self) -> Dict[str, Dict[str, bool]]:
        """Verify hooks are present in all repositories"""
        print(f"{Colors.YELLOW}VERIFYING HOOKS IN ALL REPOSITORIES{Colors.NC}")
        
        verification_results = {}
        
        for item in self.base_dir.iterdir():
            if item.is_dir() and (item / ".git").exists():
                repo_name = item.name
                hooks_dir = item / ".agent-os" / "hooks"
                
                results = {
                    "directory_exists": hooks_dir.exists(),
                    "bash_hooks": (hooks_dir / "verification_hooks.sh").exists(),
                    "python_hooks": (hooks_dir / "verification_hooks.py").exists(),
                    "readme": (hooks_dir / "README.md").exists()
                }
                
                all_present = all(results.values())
                
                if all_present:
                    print(f"{Colors.GREEN}[OK]{Colors.NC} {repo_name}: All hooks present")
                else:
                    missing = [k for k, v in results.items() if not v]
                    print(f"{Colors.YELLOW}[WARN]{Colors.NC} {repo_name}: Missing {missing}")
                
                verification_results[repo_name] = results
        
        return verification_results


def main():
    """Main execution"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Git sync with verification hooks")
    parser.add_argument("command", choices=["sync", "sync-all", "copy-hooks", "verify"],
                       help="Command to execute")
    parser.add_argument("--repo", default=".", help="Repository path for sync")
    parser.add_argument("--branch", default="master", help="Branch to sync")
    parser.add_argument("--base-dir", default="/d/github", help="Base directory for all repos")
    parser.add_argument("--source-repo", default="digitalmodel", 
                       help="Source repository for hooks")
    
    args = parser.parse_args()
    
    syncer = GitSyncWithHooks(args.base_dir, args.source_repo)
    
    if args.command == "sync":
        # Sync single repository
        repo_path = Path(args.repo).resolve()
        success = syncer.sync_repository(repo_path, args.branch)
        sys.exit(0 if success else 1)
        
    elif args.command == "sync-all":
        # Sync all repositories
        results = syncer.sync_all_repositories()
        failed = sum(1 for v in results.values() if not v)
        sys.exit(0 if failed == 0 else 1)
        
    elif args.command == "copy-hooks":
        # Copy hooks to specific repository
        target = Path(args.repo).resolve()
        success = syncer.copy_hooks_to_repo(target)
        sys.exit(0 if success else 1)
        
    elif args.command == "verify":
        # Verify hooks in all repositories
        results = syncer.verify_hooks_in_all_repos()
        missing = sum(1 for r in results.values() if not all(r.values()))
        print(f"\n{Colors.BLUE}Summary:{Colors.NC} {missing} repositories missing hooks")
        sys.exit(0 if missing == 0 else 1)


if __name__ == "__main__":
    main()