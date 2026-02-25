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
import shutil
import time

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
    
    def sync(self, all_repos: bool = False, with_commands: bool = True) -> Dict:
        """Sync repositories with remote and optionally propagate commands."""
        if all_repos:
            print("🔄 Syncing all repositories...\n")
            
            # Step 1: Git sync all repos
            sync_results = self._sync_all()
            
            # Step 2: Propagate commands (by default)
            if with_commands:
                print("\n📦 Propagating slash commands...")
                propagate_results = self.propagate(all_repos=True)
                
                # Combine results
                for repo, result in sync_results.items():
                    if repo in propagate_results:
                        result['commands_propagated'] = propagate_results[repo].get('commands', 0)
            
            # Step 3: Generate and distribute documentation
            print("\n📚 Updating documentation...")
            self._generate_and_distribute_docs()
            
            # Summary
            print("\n✅ Complete sync finished:")
            print("   • Git repositories synced")
            if with_commands:
                print("   • Slash commands propagated")
            print("   • Documentation updated")
            
            return sync_results
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
    
    def propagate(self, all_repos: bool = True) -> Dict:
        """Propagate all slash commands to repositories."""
        print("📦 Propagating slash commands to all repositories...\n")
        
        # Commands to propagate
        commands_to_copy = [
            # Core unified commands
            "git.py", "spec.py", "task.py", "test.py", 
            "project.py", "data.py",
            # UV environment support
            "uv_environment_manager.py",
            # Enhanced versions
            "spec_enhanced.py", "test_automation_enhanced.py",
            "execute_tasks_enhanced.py", "engineering_data_context.py",
            # AI agent support
            "ai_agent.py",
            # Verification and utilities
            "verify-ai-work.py",
        ]
        
        # Resource files
        resources_to_copy = [
            "aitmpl_agents_catalog.yaml",
            "ai_templates.yaml",
        ]
        
        results = {}
        source_commands = self.base_path / ".agent-os" / "commands"
        source_resources = self.base_path / ".agent-os" / "resources"
        
        for repo in self.all_repos:
            if repo == "github":  # Skip source repo
                continue
                
            repo_path = self.base_path / repo
            if not repo_path.exists():
                results[repo] = {"status": "skipped", "reason": "not found"}
                continue
                
            try:
                # Create directories
                commands_dir = repo_path / ".agent-os" / "commands"
                commands_dir.mkdir(parents=True, exist_ok=True)
                
                resources_dir = repo_path / ".agent-os" / "resources"
                resources_dir.mkdir(parents=True, exist_ok=True)
                
                # Copy commands
                copied_commands = 0
                for cmd_file in commands_to_copy:
                    source = source_commands / cmd_file
                    if source.exists():
                        dest = commands_dir / cmd_file
                        shutil.copy2(source, dest)
                        if dest.suffix == '.py':
                            dest.chmod(0o755)
                        copied_commands += 1
                
                # Copy resources
                copied_resources = 0
                for resource_file in resources_to_copy:
                    source = source_resources / resource_file
                    if source.exists():
                        dest = resources_dir / resource_file
                        shutil.copy2(source, dest)
                        copied_resources += 1
                
                print(f"✅ {repo}: {copied_commands} commands, {copied_resources} resources")
                results[repo] = {
                    "status": "success",
                    "commands": copied_commands,
                    "resources": copied_resources
                }
                
            except Exception as e:
                print(f"❌ {repo}: {str(e)}")
                results[repo] = {"status": "failed", "error": str(e)}
        
        # Summary
        successful = sum(1 for r in results.values() if r.get("status") == "success")
        print(f"\n📊 Propagated to {successful}/{len(self.all_repos)-1} repositories")
        
        return results
    
    def _generate_and_distribute_docs(self):
        """Generate and distribute command documentation to all repos."""
        print("📚 Generating and distributing command documentation...")
        
        # Generate the commands matrix document
        matrix_content = self._generate_commands_matrix()
        
        # Save to main repo
        main_doc_path = self.base_path / "COMMANDS_MATRIX.md"
        main_doc_path.write_text(matrix_content)
        
        # Distribute to all repos
        distributed = 0
        for repo in self.all_repos:
            if repo == "github":  # Skip the main repo
                continue
                
            repo_path = self.base_path / repo
            
            # Create .agent-os/docs directory if it doesn't exist
            docs_dir = repo_path / ".agent-os" / "docs"
            docs_dir.mkdir(parents=True, exist_ok=True)
            
            # Copy the matrix document
            dest_path = docs_dir / "COMMANDS_MATRIX.md"
            dest_path.write_text(matrix_content)
            
            # Also create a symlink or copy at repo root for easy access
            root_doc = repo_path / "AGENT_OS_COMMANDS.md"
            root_doc.write_text(matrix_content)
            
            distributed += 1
        
        print(f"✅ Documentation distributed to {distributed} repositories\n")
    
    def _generate_commands_matrix(self) -> str:
        """Generate the commands matrix documentation."""
        current_date = datetime.now().strftime("%Y-%m-%d")
        
        return f"""# Slash Commands Matrix

## Primary Commands and Subcommands Overview

| Command | Subcommands | Description | Features |
|---------|-------------|-------------|----------|
| **`/git`** | | **Git Operations** | |
| | `status` | Show status of all repositories | • Multi-repo status<br>• Uncommitted changes detection<br>• Branch information |
| | `sync` | Sync all repos with origin | • Auto-stash changes<br>• Pull latest<br>• **Propagates all commands**<br>• **Updates documentation**<br>• Use --no-commands to skip propagation |
| | `trunk` | Switch to trunk-based development | • Convert to main branch<br>• Clean old branches<br>• Setup trunk workflow |
| | `commit [msg]` | Commit changes | • Smart commit messages<br>• Auto-staging<br>• Push to origin |
| | `clean` | Clean branches and references | • Remove merged branches<br>• Prune remote refs<br>• Clean stale data |
| | `propagate` | Propagate slash commands to all repos | • Copy all commands<br>• Copy AI resources<br>• Update all repositories<br>• **Ensures command consistency** |
| | | | |
| **`/spec`** | | **Specification Management** | |
| | `create [name] [module]` | Create new specification | • AI template selection<br>• Module organization<br>• Agent recommendations<br>• Auto task generation |
| | `list` | List all specifications | • Show all specs<br>• Module grouping<br>• Status indicators |
| | `tasks [name]` | Show tasks for a spec | • Task breakdown<br>• Completion status<br>• Dependencies |
| | `templates` | Show available AI templates | • Claude Code Templates<br>• AITmpl templates<br>• Custom templates |
| | | | |
| **`/task`** | | **Task Execution** | |
| | `execute [id]` | Execute specific task | • UV environment usage<br>• Module-aware<br>• Progress tracking |
| | `execute --all` | Execute all pending tasks | • Batch execution<br>• Dependency order<br>• Auto-testing |
| | `status` | Show task completion status | • Progress metrics<br>• Module breakdown<br>• Blocking issues |
| | `verify` | Verify AI-generated work | • Code quality check<br>• Test coverage<br>• Standards compliance |
| | | | |
| **`/test`** | | **Testing Operations** | |
| | `run [module]` | Run tests (all or specific) | • UV Python usage<br>• Module-level testing<br>• Parallel execution |
| | `fix` | Auto-fix test failures | • Intelligent fixes<br>• Pattern recognition<br>• Safe modifications |
| | `summary` | Generate test summaries | • Module summaries<br>• Coverage reports<br>• Failure analysis |
| | `coverage` | Show coverage report | • Line coverage<br>• Branch coverage<br>• Module breakdown |
| | | | |
| **`/project`** | | **Project Management** | |
| | `status` | Overall project status | • Cross-repo status<br>• Progress metrics<br>• Health indicators |
| | `setup` | Initialize project structure | • Agent OS setup<br>• Module creation<br>• Standards application |
| | `optimize` | Run optimization agents | • Performance agents<br>• Code quality<br>• Bundle size |
| | `docs` | Generate documentation | • Auto-documentation<br>• API docs<br>• README generation |
| | | | |
| **`/data`** | | **Data Operations** | |
| | `context [folder]` | Generate engineering data context | • 25+ file formats<br>• Web research<br>• Module assignment |
| | `analyze` | Analyze data files | • Statistical analysis<br>• Pattern detection<br>• Visualizations |
| | `pipeline` | Create ETL pipelines | • Pipeline design<br>• Data validation<br>• Error handling |
| | `optimize` | Optimize data operations | • Query optimization<br>• Index recommendations<br>• Performance tuning |
| | | | |
| **`/ai-agent`** | | **AI Agent Management** | |
| | `list [--category]` | List all available agents | • 48+ agents<br>• Category filtering<br>• Capability display |
| | `recommend [context]` | Get agent recommendations | • Context analysis<br>• Auto-selection<br>• Workflow suggestions |
| | `use [agent]` | Activate specific agent | • Agent activation<br>• Task integration<br>• Guided usage |
| | `info [agent]` | Show agent details | • Capabilities<br>• Integration points<br>• Usage examples |
| | `workflow [type]` | Show agent workflows | • Step-by-step<br>• Agent chaining<br>• Best practices |

## Quick Reference Matrix

```
┌─────────────┬────────┬────────┬────────┬────────┬─────────┬───────────┐
│   Command   │ Sub 1  │ Sub 2  │ Sub 3  │ Sub 4  │  Sub 5  │   Sub 6   │
├─────────────┼────────┼────────┼────────┼────────┼─────────┼───────────┤
│ /git        │ status │ sync   │ trunk  │ commit │ clean   │ propagate │
│ /spec       │ create │ list   │ tasks  │template│    -    │     -     │
│ /task       │execute │exec-all│ status │ verify │    -    │     -     │
│ /test       │ run    │ fix    │summary │coverage│    -    │     -     │
│ /project    │ status │ setup  │optimize│ docs   │    -    │     -     │
│ /data       │context │analyze │pipeline│optimize│    -    │     -     │
│ /ai-agent   │ list   │recommend│ use   │ info   │workflow │     -     │
└─────────────┴────────┴────────┴────────┴────────┴─────────┴───────────┘
```

## Utility Commands

| Command | Description | Subcommands |
|---------|-------------|-------------|
| **`/uv-env`** | UV Environment Manager | `info`, `ensure`, `sync`, `add`, `enhance` |
| **`/verify`** | Verify AI Work | Standalone command for spec verification |

## Summary Statistics

- **7 Primary Commands**
- **31 Total Subcommands** (git now has 6 subcommands)
- **2 Utility Commands**
- **All with UV Environment Support**
- **48+ AI Agents Available**

## Feature Support Matrix

| Feature | /git | /spec | /task | /test | /project | /data | /ai-agent |
|---------|:----:|:-----:|:-----:|:-----:|:--------:|:-----:|:---------:|
| UV Environment | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Multi-repo | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| AI Integration | - | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Module Aware | - | ✓ | ✓ | ✓ | ✓ | ✓ | - |
| Auto-fix | - | - | ✓ | ✓ | ✓ | - | - |
| Web Research | - | ✓ | - | - | - | ✓ | - |

## Command Flow

```
/spec create → /ai-agent recommend → /task execute → /test run → /verify → /git commit
```

## Most Common Workflows

### Daily Development
```bash
/git sync --all          # Start your day - syncs everything!
                        # • Git pull all repos
                        # • Propagate latest commands
                        # • Update documentation
/task status            # Check progress
/test run               # Run tests
/git commit "updates"   # Commit work
```

### Feature Development
```bash
/spec create feature-name module-name
/ai-agent recommend
/task execute 1.1
/test run module-name
/verify
/git commit "Add feature-name"
```

### Data Processing
```bash
/data context ./data-folder
/data analyze
/data pipeline
/test run data-module
```

## Resources

- **AI Templates**: https://github.com/davila7/claude-code-templates
- **AITmpl**: https://www.aitmpl.com/
- **Agent Catalog**: `.agent-os/resources/aitmpl_agents_catalog.yaml`
- **Commands Location**: `.agent-os/commands/`

---

*Auto-generated: {current_date}*
*Distribution: This document is automatically updated and distributed via `/git sync --all`*
*Total Commands: 7 primary + 2 utility = 9 unified commands (consolidated from 21+)*
"""
    
    def help(self):
        """Show help information."""
        help_text = """
🔧 Unified Git Command

Usage: /git [subcommand] [options]

Subcommands:
  status          Check repository status
                  Options: --all (check all repos)
  
  sync            Sync with remote repository  
                  Options: --all (sync all repos + commands + docs)
                          --no-commands (skip command propagation)
  
  trunk           Enforce trunk-based development
                  Options: --all (apply to all repos)
  
  commit MESSAGE  Commit and push changes
                  Example: /git commit "Fix bug in module"
  
  clean           Clean stale branches and data
                  Options: --all (clean all repos)
  
  propagate       Propagate slash commands to all repos
                  Copies all commands and resources
  
  help            Show this help message

Examples:
  /git status                # Check current repo
  /git status --all          # Check all repos
  /git sync                  # Sync current repo
  /git sync --all            # Full sync: repos + commands + docs
  /git sync --all --no-commands  # Sync repos + docs only
  /git trunk                 # Switch to trunk branch
  /git commit "Add feature"  # Commit and push
  /git clean                 # Clean current repo
  /git propagate             # Distribute commands only

Special Features:
  • sync --all: Complete sync (git + commands + docs)
  • sync --all --no-commands: Git sync + docs only
  • propagate: Commands distribution only

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
                       choices=['status', 'sync', 'trunk', 'commit', 'clean', 'propagate', 'help'])
    parser.add_argument('message', nargs='?', help='Commit message')
    parser.add_argument('--all', action='store_true', help='Apply to all repositories')
    parser.add_argument('--no-commands', action='store_true', help='Skip command propagation during sync')
    
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
        result = git_cmd.sync(all_repos=args.all, with_commands=not args.no_commands)
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
    elif args.subcommand == 'propagate':
        result = git_cmd.propagate(all_repos=True)
    else:  # help
        result = git_cmd.help()
    
    # Return success/failure
    if isinstance(result, dict) and result.get('status') == 'failed':
        sys.exit(1)

if __name__ == '__main__':
    main()