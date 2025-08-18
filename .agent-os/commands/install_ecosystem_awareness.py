#!/usr/bin/env python3
"""
Install Ecosystem Awareness - Add slash command suggestions to all repos
This command updates all CLAUDE.md files to include mandatory ecosystem awareness.
"""

import os
import sys
from pathlib import Path
from typing import List, Dict, Tuple
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed

class EcosystemAwarenessInstaller:
    """Install ecosystem awareness across all repositories."""
    
    def __init__(self, workspace_dir: Path):
        self.workspace_dir = Path(workspace_dir)
        self.ecosystem_section = """
## 🚀 MANDATORY: Slash Command Ecosystem Integration

**CRITICAL DIRECTIVE**: For EVERY improvement, automation, or repetitive task:

### AI Agents MUST:
1. **EVALUATE** if the improvement could benefit multiple repositories
2. **SUGGEST** creating a slash command when applicable  
3. **DESIGN** for reusability across all 25 repos
4. **INTEGRATE** with existing ecosystem via `/sync-all-commands`

### Immediate Triggers for Command Creation:
- ✅ Repetitive tasks that could be automated
- ✅ Functionality that would benefit other repos
- ✅ Multi-step processes that could be scripted
- ✅ Regular developer workflows
- ✅ Best practice standardization
- ✅ Common problem solutions

### Example Response Template:
```
💡 Slash Command Opportunity Detected!
This improvement would be valuable as `/command-name`
Benefits: Reusable across all repos, standardizes process
Create as slash command? (Recommended)
```

### Available Ecosystem Commands:
- `/git-trunk-flow` - Complete git workflow with best practices
- `/sync-all-commands` - Sync commands from all repos to master
- `/propagate-commands` - Distribute commands to all repos
- `/modernize-deps` - Update dependency management
- `/organize-structure` - Organize project structure

### Creating New Commands:
1. Implement in `.agent-os/commands/`
2. Test locally with `./slash_commands.py /command-name`
3. Sync to master with `/sync-all-commands`
4. Distribute with `/propagate-commands`

**Full Guidelines**: See MANDATORY_SLASH_COMMAND_ECOSYSTEM.md in assetutilities
"""
        
    def find_repositories(self) -> List[Path]:
        """Find all repositories in the workspace."""
        repos = []
        for item in self.workspace_dir.iterdir():
            if item.is_dir() and not item.name.startswith('.'):
                if (item / ".git").exists():
                    repos.append(item)
        return sorted(repos)
    
    def check_claude_md(self, repo_path: Path) -> Tuple[bool, bool]:
        """Check if CLAUDE.md exists and has ecosystem section."""
        claude_file = repo_path / "CLAUDE.md"
        
        if not claude_file.exists():
            return False, False
        
        content = claude_file.read_text()
        has_ecosystem = "MANDATORY: Slash Command Ecosystem" in content
        
        return True, has_ecosystem
    
    def update_claude_md(self, repo_path: Path) -> Dict:
        """Update CLAUDE.md with ecosystem awareness."""
        result = {
            "repo": repo_path.name,
            "status": "pending",
            "message": ""
        }
        
        claude_file = repo_path / "CLAUDE.md"
        
        try:
            # Check current state
            exists, has_ecosystem = self.check_claude_md(repo_path)
            
            if has_ecosystem:
                result["status"] = "skipped"
                result["message"] = "Already has ecosystem awareness"
                return result
            
            if exists:
                # Read existing content
                content = claude_file.read_text()
                
                # Find insertion point (after initial header or at top)
                lines = content.split('\n')
                insert_index = 0
                
                # Look for a good insertion point after any title
                for i, line in enumerate(lines):
                    if line.startswith('# '):
                        insert_index = i + 1
                        # Skip any immediate description lines
                        while insert_index < len(lines) and lines[insert_index].strip():
                            insert_index += 1
                        break
                
                # Insert ecosystem section
                lines.insert(insert_index, self.ecosystem_section)
                
                # Write updated content
                claude_file.write_text('\n'.join(lines))
                
                result["status"] = "updated"
                result["message"] = "Added ecosystem awareness section"
            else:
                # Create new CLAUDE.md
                content = f"""# CLAUDE.md - AI Agent Instructions

{self.ecosystem_section}

## Project-Specific Instructions

Add any project-specific instructions below.
"""
                claude_file.write_text(content)
                
                result["status"] = "created"
                result["message"] = "Created CLAUDE.md with ecosystem awareness"
            
        except Exception as e:
            result["status"] = "error"
            result["message"] = str(e)
        
        return result
    
    def copy_mandatory_file(self, repo_path: Path) -> bool:
        """Copy MANDATORY_SLASH_COMMAND_ECOSYSTEM.md to repo."""
        source_file = self.workspace_dir / "assetutilities" / "MANDATORY_SLASH_COMMAND_ECOSYSTEM.md"
        target_file = repo_path / "MANDATORY_SLASH_COMMAND_ECOSYSTEM.md"
        
        if not source_file.exists():
            return False
        
        try:
            import shutil
            shutil.copy2(source_file, target_file)
            return True
        except Exception:
            return False
    
    def install_to_repository(self, repo_path: Path) -> Dict:
        """Install ecosystem awareness to a single repository."""
        # Update CLAUDE.md
        result = self.update_claude_md(repo_path)
        
        # Copy mandatory file
        if result["status"] in ["updated", "created"]:
            if self.copy_mandatory_file(repo_path):
                result["mandatory_file"] = "copied"
            else:
                result["mandatory_file"] = "failed"
        
        return result
    
    def install_all(self, dry_run: bool = False) -> Dict:
        """Install ecosystem awareness to all repositories."""
        repos = self.find_repositories()
        results = []
        
        print(f"🎯 Installing Ecosystem Awareness to {len(repos)} repositories")
        print("=" * 60)
        
        if dry_run:
            print("🔍 DRY RUN MODE - No changes will be made\n")
            for repo in repos:
                exists, has_ecosystem = self.check_claude_md(repo)
                status = "skip" if has_ecosystem else ("update" if exists else "create")
                print(f"  {repo.name}: Would {status}")
            return {"dry_run": True, "repos": len(repos)}
        
        # Process repositories in parallel
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = {
                executor.submit(self.install_to_repository, repo): repo
                for repo in repos
            }
            
            for future in as_completed(futures):
                repo = futures[future]
                try:
                    result = future.result(timeout=10)
                    results.append(result)
                    
                    # Display progress
                    icon = {
                        "created": "✅",
                        "updated": "✅", 
                        "skipped": "⏭️",
                        "error": "❌"
                    }.get(result["status"], "❓")
                    
                    print(f"  {icon} {result['repo']}: {result['message']}")
                    
                except Exception as e:
                    print(f"  ❌ {repo.name}: Error - {e}")
                    results.append({
                        "repo": repo.name,
                        "status": "error",
                        "message": str(e)
                    })
        
        # Summary
        created = sum(1 for r in results if r["status"] == "created")
        updated = sum(1 for r in results if r["status"] == "updated")
        skipped = sum(1 for r in results if r["status"] == "skipped")
        errors = sum(1 for r in results if r["status"] == "error")
        
        print("\n" + "=" * 60)
        print("📊 Installation Summary")
        print("=" * 60)
        print(f"✅ Created: {created} new CLAUDE.md files")
        print(f"✅ Updated: {updated} existing files")
        print(f"⏭️  Skipped: {skipped} (already have ecosystem)")
        if errors:
            print(f"❌ Errors: {errors}")
        
        return {
            "created": created,
            "updated": updated,
            "skipped": skipped,
            "errors": errors,
            "total": len(results)
        }


def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Install slash command ecosystem awareness to all repositories"
    )
    parser.add_argument(
        "--workspace",
        default="/mnt/github/github",
        help="Workspace directory containing repositories"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Preview changes without making them"
    )
    
    args = parser.parse_args()
    
    workspace = Path(args.workspace)
    if not workspace.exists():
        print(f"❌ Workspace not found: {workspace}")
        return 1
    
    print("🚀 Slash Command Ecosystem Awareness Installer")
    print("=" * 60)
    print("This will update all repositories to suggest slash commands")
    print("for reusable improvements and automations.")
    print("=" * 60)
    print()
    
    installer = EcosystemAwarenessInstaller(workspace)
    results = installer.install_all(dry_run=args.dry_run)
    
    print("\n✨ Ecosystem awareness installation complete!")
    print("\nAll AI agents will now suggest creating slash commands")
    print("when implementing reusable improvements.")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())