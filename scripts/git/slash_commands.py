#!/usr/bin/env python3
"""
Local Slash Commands for Git Operations
Available in this repository
"""

import os
import sys
import subprocess
from pathlib import Path

class LocalGitCommands:
    """Git commands for current repository"""
    
    def __init__(self):
        self.repo_path = Path.cwd()
        self.repo_name = self.repo_path.name
    
    def run_command(self, cmd: str) -> tuple:
        """Run a shell command"""
        try:
            result = subprocess.run(
                cmd, shell=True, capture_output=True, 
                text=True, timeout=30
            )
            return result.returncode == 0, result.stdout + result.stderr
        except Exception as e:
            return False, str(e)
    
    def git_sync(self):
        """/git-sync - Sync this repository"""
        print(f"🔄 Syncing {self.repo_name}...")
        
        cmds = [
            ("Fetching", "git fetch --all --prune"),
            ("Pulling", "git pull"),
        ]
        
        for desc, cmd in cmds:
            print(f"  {desc}...")
            success, output = self.run_command(cmd)
            if not success:
                print(f"  ❌ Error: {output}")
                return False
        
        print(f"✅ {self.repo_name} synced successfully!")
        return True
    
    def git_commit(self, message: str = None):
        """/git-commit - Commit changes in this repo"""
        print(f"📝 Committing changes in {self.repo_name}...")
        
        # Check for changes
        success, output = self.run_command("git status --porcelain")
        if not output.strip():
            print("  ℹ️ No changes to commit")
            return True
        
        # Add all changes
        self.run_command("git add -A")
        
        # Commit
        msg = message or f"feat: Update {self.repo_name}\n\n🤖 Generated with Claude Code"
        cmd = f'git commit -m "{msg}"'
        success, output = self.run_command(cmd)
        
        if success:
            print(f"✅ Changes committed successfully!")
        else:
            print(f"❌ Commit failed: {output}")
        
        return success
    
    def git_push(self):
        """/git-push - Push changes to remote"""
        print(f"⬆️ Pushing {self.repo_name}...")
        
        success, output = self.run_command("git push")
        if success:
            print("✅ Pushed successfully!")
        else:
            # Try push with upstream
            success, output = self.run_command("git push -u origin HEAD")
            if success:
                print("✅ Pushed successfully (set upstream)!")
            else:
                print(f"❌ Push failed: {output}")
        
        return success
    
    def git_pr(self, title: str = None):
        """/git-pr - Create a pull request"""
        print(f"🔀 Creating pull request for {self.repo_name}...")
        
        # Push first
        self.git_push()
        
        # Create PR
        pr_title = title or f"Update {self.repo_name}"
        pr_body = "Automated update\n\n🤖 Generated with Claude Code"
        
        cmd = f'''gh pr create --title "{pr_title}" --body "{pr_body}"'''
        success, output = self.run_command(cmd)
        
        if success:
            print(f"✅ PR created: {output.strip()}")
        else:
            if "already exists" in output:
                print("ℹ️ PR already exists for this branch")
            else:
                print(f"❌ PR creation failed: {output}")
        
        return success
    
    def git_clean(self):
        """/git-clean - Clean merged branches"""
        print(f"🧹 Cleaning branches in {self.repo_name}...")
        
        # Get default branch
        success, default = self.run_command(
            "git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'"
        )
        default = default.strip() or "main"
        
        # Checkout default branch
        self.run_command(f"git checkout {default}")
        
        # Delete merged branches
        cmd = f"git branch --merged | grep -v '{default}' | grep -v '\\*'"
        success, branches = self.run_command(cmd)
        
        deleted = []
        if success and branches.strip():
            for branch in branches.strip().split('\n'):
                branch = branch.strip()
                if branch:
                    del_success, _ = self.run_command(f"git branch -d {branch}")
                    if del_success:
                        deleted.append(branch)
        
        # Prune remotes
        self.run_command("git remote prune origin")
        
        if deleted:
            print(f"✅ Deleted {len(deleted)} branches: {', '.join(deleted)}")
        else:
            print("✨ No stale branches to clean")
        
        return True
    
    def git_status(self):
        """/git-status - Show repository status"""
        print(f"📊 Status of {self.repo_name}")
        print("=" * 40)
        
        # Current branch
        success, branch = self.run_command("git branch --show-current")
        print(f"Branch: {branch.strip()}")
        
        # Check for changes
        success, status = self.run_command("git status --short")
        if status.strip():
            print("\nChanges:")
            print(status)
        else:
            print("Status: ✨ Clean")
        
        # Ahead/behind
        success, ab = self.run_command(
            "git rev-list --left-right --count @{u}...HEAD 2>/dev/null"
        )
        if success and ab.strip():
            behind, ahead = ab.strip().split('\t')
            if int(behind) > 0:
                print(f"⬇️ Behind by {behind} commits")
            if int(ahead) > 0:
                print(f"⬆️ Ahead by {ahead} commits")
        
        print("=" * 40)
        return True
    
    def git_flow(self):
        """/git-flow - Complete workflow"""
        print(f"🚀 Running Git flow for {self.repo_name}")
        print("=" * 40)
        
        steps = [
            ("Committing", self.git_commit),
            ("Pushing", self.git_push),
            ("Syncing", self.git_sync),
            ("Cleaning", self.git_clean),
        ]
        
        for step_name, step_func in steps:
            print(f"\n{step_name}...")
            if not step_func():
                print(f"⚠️ {step_name} had issues, continuing...")
        
        print("\n✅ Git flow completed!")
        return True
    
    def show_help(self):
        """Show available commands"""
        print(f"""
🎯 LOCAL GIT COMMANDS FOR: {self.repo_name}
{'=' * 50}

/git-sync       - Sync with remote
/git-commit     - Commit all changes
/git-push       - Push to remote
/git-pr         - Create pull request
/git-clean      - Clean merged branches
/git-status     - Show repository status
/git-flow       - Run complete workflow
/git-help       - Show this help

Examples:
  python .git-commands/slash_commands.py sync
  python .git-commands/slash_commands.py commit "fix: Bug fix"
  python .git-commands/slash_commands.py flow
""")


def main():
    """Main entry point"""
    import sys
    
    commands = LocalGitCommands()
    
    if len(sys.argv) < 2:
        commands.show_help()
        return
    
    cmd = sys.argv[1].replace("/", "").replace("git-", "")
    args = " ".join(sys.argv[2:]) if len(sys.argv) > 2 else None
    
    if cmd == "sync":
        commands.git_sync()
    elif cmd == "commit":
        commands.git_commit(args)
    elif cmd == "push":
        commands.git_push()
    elif cmd == "pr":
        commands.git_pr(args)
    elif cmd == "clean":
        commands.git_clean()
    elif cmd == "status":
        commands.git_status()
    elif cmd == "flow":
        commands.git_flow()
    elif cmd == "help":
        commands.show_help()
    else:
        print(f"Unknown command: {cmd}")
        commands.show_help()


if __name__ == "__main__":
    main()
