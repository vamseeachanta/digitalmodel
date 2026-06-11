#!/usr/bin/env python3
"""
Sync All Commands - Bidirectional Command Synchronization
Collects slash commands from all repositories and syncs them to the master directory.
Also identifies new commands and can distribute updates.
"""

import os
import sys
import json
import shutil
import hashlib
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Set
from datetime import datetime
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed
import difflib
import argparse

class CommandSynchronizer:
    """Synchronize slash commands across all repositories."""
    
    def __init__(self, master_repo: Path, workspace_dir: Path):
        self.master_repo = Path(master_repo)
        self.workspace_dir = Path(workspace_dir)
        self.master_commands_dir = self.master_repo / ".agent-os" / "commands"
        self.command_inventory = {}
        self.new_commands = {}
        self.modified_commands = {}
        self.conflicts = {}
        self.backup_dir = self.master_repo / ".command-backups" / datetime.now().strftime("%Y%m%d_%H%M%S")
        
    def discover_all_repositories(self) -> List[Path]:
        """Find all repositories in the workspace."""
        repos = []
        for item in self.workspace_dir.iterdir():
            if item.is_dir() and not item.name.startswith('.'):
                # Check if it has .git directory
                if (item / ".git").exists():
                    repos.append(item)
                # Also check if it has slash commands
                elif (item / ".agent-os" / "commands").exists():
                    repos.append(item)
        return sorted(repos)
    
    def get_command_hash(self, file_path: Path) -> str:
        """Calculate hash of a command file for comparison."""
        try:
            content = file_path.read_bytes()
            return hashlib.sha256(content).hexdigest()
        except Exception:
            return ""
    
    def extract_command_metadata(self, file_path: Path) -> Dict:
        """Extract metadata from a command file."""
        metadata = {
            "path": str(file_path),
            "size": file_path.stat().st_size if file_path.exists() else 0,
            "modified": datetime.fromtimestamp(file_path.stat().st_mtime).isoformat() if file_path.exists() else None,
            "hash": self.get_command_hash(file_path),
            "description": "",
            "version": "1.0.0",
            "author": "unknown"
        }
        
        try:
            content = file_path.read_text()
            lines = content.split('\n')[:50]  # Check first 50 lines
            
            for line in lines:
                # Extract description from docstring
                if '"""' in line and not metadata["description"]:
                    start_idx = content.find('"""')
                    end_idx = content.find('"""', start_idx + 3)
                    if end_idx > start_idx:
                        docstring = content[start_idx+3:end_idx].strip()
                        first_line = docstring.split('\n')[0] if docstring else ""
                        metadata["description"] = first_line[:200]
                
                # Extract version if present
                if 'version' in line.lower() and '=' in line:
                    version_match = line.split('=')[1].strip().strip('"\'')
                    if version_match and '.' in version_match:
                        metadata["version"] = version_match
                
                # Extract author from comments
                if '# Author:' in line or '# @author' in line:
                    metadata["author"] = line.split(':', 1)[1].strip() if ':' in line else "unknown"
        
        except Exception:
            pass
        
        return metadata
    
    def scan_repository_commands(self, repo_path: Path) -> Dict[str, Dict]:
        """Scan a repository for all slash commands."""
        commands = {}
        commands_dir = repo_path / ".agent-os" / "commands"
        
        if not commands_dir.exists():
            return commands
        
        for cmd_file in commands_dir.glob("*.py"):
            if cmd_file.name == "__init__.py":
                continue
            
            # Convert filename to command name
            cmd_name = "/" + cmd_file.stem.replace('_', '-')
            
            # Get metadata
            metadata = self.extract_command_metadata(cmd_file)
            metadata["repository"] = repo_path.name
            metadata["command_name"] = cmd_name
            
            commands[cmd_name] = metadata
        
        return commands
    
    def scan_all_repositories(self) -> Dict[str, Dict]:
        """Scan all repositories for commands."""
        print("🔍 Scanning all repositories for slash commands...")
        all_commands = {}
        
        repos = self.discover_all_repositories()
        
        with ThreadPoolExecutor(max_workers=10) as executor:
            futures = {
                executor.submit(self.scan_repository_commands, repo): repo
                for repo in repos
            }
            
            for future in as_completed(futures):
                repo = futures[future]
                try:
                    repo_commands = future.result(timeout=10)
                    if repo_commands:
                        print(f"  📦 {repo.name}: {len(repo_commands)} commands found")
                        for cmd_name, cmd_data in repo_commands.items():
                            if cmd_name not in all_commands:
                                all_commands[cmd_name] = []
                            all_commands[cmd_name].append(cmd_data)
                except Exception as e:
                    print(f"  ❌ Error scanning {repo.name}: {e}")
        
        self.command_inventory = all_commands
        return all_commands
    
    def identify_new_commands(self) -> Dict[str, List[Dict]]:
        """Identify commands that don't exist in the master repository."""
        new_commands = {}
        
        for cmd_name, instances in self.command_inventory.items():
            master_cmd_file = self.master_commands_dir / (cmd_name.lstrip('/').replace('-', '_') + '.py')
            
            if not master_cmd_file.exists():
                new_commands[cmd_name] = instances
        
        self.new_commands = new_commands
        return new_commands
    
    def identify_modified_commands(self) -> Dict[str, List[Dict]]:
        """Identify commands that have been modified in other repositories."""
        modified_commands = {}
        
        for cmd_name, instances in self.command_inventory.items():
            master_cmd_file = self.master_commands_dir / (cmd_name.lstrip('/').replace('-', '_') + '.py')
            
            if master_cmd_file.exists():
                master_hash = self.get_command_hash(master_cmd_file)
                
                for instance in instances:
                    if instance["hash"] != master_hash and instance["repository"] != self.master_repo.name:
                        if cmd_name not in modified_commands:
                            modified_commands[cmd_name] = {
                                "master_hash": master_hash,
                                "master_path": str(master_cmd_file),
                                "instances": []
                            }
                        modified_commands[cmd_name]["instances"].append(instance)
        
        self.modified_commands = modified_commands
        return modified_commands
    
    def backup_command(self, cmd_file: Path):
        """Backup a command file before modifying."""
        if not cmd_file.exists():
            return
        
        self.backup_dir.mkdir(parents=True, exist_ok=True)
        backup_file = self.backup_dir / cmd_file.name
        shutil.copy2(cmd_file, backup_file)
        print(f"    📋 Backed up: {cmd_file.name}")
    
    def show_diff(self, file1: Path, file2: Path, context_lines: int = 3) -> str:
        """Show differences between two files."""
        try:
            content1 = file1.read_text().splitlines() if file1.exists() else []
            content2 = file2.read_text().splitlines() if file2.exists() else []
            
            diff = difflib.unified_diff(
                content1, content2,
                fromfile=str(file1),
                tofile=str(file2),
                lineterm='',
                n=context_lines
            )
            
            return '\n'.join(diff)
        except Exception as e:
            return f"Error generating diff: {e}"
    
    def resolve_conflict(self, cmd_name: str, instances: List[Dict]) -> Optional[Dict]:
        """Resolve conflicts when multiple versions of a command exist."""
        print(f"\n⚠️  Conflict detected for {cmd_name}")
        print(f"   Found {len(instances)} different versions:")
        
        # Sort by modification time, newest first
        sorted_instances = sorted(
            instances,
            key=lambda x: x.get('modified', ''),
            reverse=True
        )
        
        for i, instance in enumerate(sorted_instances, 1):
            print(f"   {i}. From {instance['repository']}")
            print(f"      Modified: {instance.get('modified', 'unknown')}")
            print(f"      Size: {instance.get('size', 0)} bytes")
            print(f"      Description: {instance.get('description', 'No description')[:100]}")
        
        # Auto-resolve by taking the newest version
        print(f"\n   Auto-selecting newest version from {sorted_instances[0]['repository']}")
        return sorted_instances[0]
    
    def sync_new_command(self, cmd_name: str, instances: List[Dict]) -> bool:
        """Sync a new command to the master repository."""
        # If multiple instances exist, resolve conflict
        if len(instances) > 1:
            selected = self.resolve_conflict(cmd_name, instances)
        else:
            selected = instances[0]
        
        if not selected:
            return False
        
        source_file = Path(selected["path"])
        target_file = self.master_commands_dir / (cmd_name.lstrip('/').replace('-', '_') + '.py')
        
        try:
            # Create commands directory if it doesn't exist
            self.master_commands_dir.mkdir(parents=True, exist_ok=True)
            
            # Copy the command file
            shutil.copy2(source_file, target_file)
            print(f"  ✅ Synced new command: {cmd_name} from {selected['repository']}")
            return True
        except Exception as e:
            print(f"  ❌ Failed to sync {cmd_name}: {e}")
            return False
    
    def sync_modified_command(self, cmd_name: str, modifications: Dict) -> bool:
        """Sync a modified command to the master repository."""
        master_file = Path(modifications["master_path"])
        
        # Select the best version
        selected = self.resolve_conflict(cmd_name, modifications["instances"])
        if not selected:
            return False
        
        source_file = Path(selected["path"])
        
        try:
            # Backup the existing master version
            self.backup_command(master_file)
            
            # Show diff if verbose
            print(f"\n  📝 Changes for {cmd_name}:")
            diff = self.show_diff(master_file, source_file)
            if diff:
                diff_lines = diff.split('\n')[:20]  # Show first 20 lines
                for line in diff_lines:
                    if line.startswith('+'):
                        print(f"    {line}")
                    elif line.startswith('-'):
                        print(f"    {line}")
                all_diff_lines = diff.split('\n')
                if len(all_diff_lines) > 20:
                    print(f"    ... and {len(all_diff_lines) - 20} more lines")
            
            # Copy the updated version
            shutil.copy2(source_file, master_file)
            print(f"  ✅ Updated command: {cmd_name} from {selected['repository']}")
            return True
        except Exception as e:
            print(f"  ❌ Failed to update {cmd_name}: {e}")
            return False
    
    def create_sync_report(self) -> Dict:
        """Create a detailed sync report."""
        report = {
            "timestamp": datetime.now().isoformat(),
            "workspace": str(self.workspace_dir),
            "master_repo": str(self.master_repo),
            "statistics": {
                "total_repositories": len(self.discover_all_repositories()),
                "total_commands": len(self.command_inventory),
                "new_commands": len(self.new_commands),
                "modified_commands": len(self.modified_commands),
                "conflicts": len(self.conflicts)
            },
            "new_commands": {},
            "modified_commands": {},
            "all_commands": {}
        }
        
        # Add new commands to report
        for cmd_name, instances in self.new_commands.items():
            report["new_commands"][cmd_name] = {
                "found_in": [inst["repository"] for inst in instances],
                "selected_from": instances[0]["repository"] if instances else "none"
            }
        
        # Add modified commands to report
        for cmd_name, mods in self.modified_commands.items():
            report["modified_commands"][cmd_name] = {
                "modified_in": [inst["repository"] for inst in mods["instances"]],
                "versions": len(mods["instances"])
            }
        
        # Add complete command inventory
        for cmd_name, instances in self.command_inventory.items():
            report["all_commands"][cmd_name] = {
                "repositories": list(set(inst["repository"] for inst in instances)),
                "versions": len(set(inst["hash"] for inst in instances))
            }
        
        return report
    
    def save_sync_report(self, report: Dict):
        """Save the sync report to a file."""
        report_dir = self.master_repo / ".sync-reports"
        report_dir.mkdir(exist_ok=True)
        
        report_file = report_dir / f"sync_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"\n📊 Sync report saved to: {report_file}")
    
    def sync_all(self, dry_run: bool = False, force: bool = False) -> Dict:
        """Perform complete synchronization."""
        print("\n🔄 Starting Command Synchronization")
        print("=" * 60)
        
        # Step 1: Scan all repositories
        self.scan_all_repositories()
        
        # Step 2: Identify new commands
        new_commands = self.identify_new_commands()
        if new_commands:
            print(f"\n🆕 Found {len(new_commands)} new commands:")
            for cmd_name, instances in new_commands.items():
                repos = ', '.join(set(inst["repository"] for inst in instances))
                print(f"  • {cmd_name} (in {repos})")
        
        # Step 3: Identify modified commands
        modified_commands = self.identify_modified_commands()
        if modified_commands:
            print(f"\n🔧 Found {len(modified_commands)} modified commands:")
            for cmd_name, mods in modified_commands.items():
                repos = ', '.join(set(inst["repository"] for inst in mods["instances"]))
                print(f"  • {cmd_name} (modified in {repos})")
        
        if dry_run:
            print("\n🔍 DRY RUN MODE - No changes will be made")
            report = self.create_sync_report()
            return report
        
        # Step 4: Sync new commands
        if new_commands:
            print("\n📥 Syncing new commands to master...")
            for cmd_name, instances in new_commands.items():
                self.sync_new_command(cmd_name, instances)
        
        # Step 5: Sync modified commands
        if modified_commands and (force or self.prompt_for_updates()):
            print("\n📤 Updating modified commands in master...")
            for cmd_name, mods in modified_commands.items():
                self.sync_modified_command(cmd_name, mods)
        
        # Step 6: Create and save report
        report = self.create_sync_report()
        self.save_sync_report(report)
        
        return report
    
    def prompt_for_updates(self) -> bool:
        """Prompt user to confirm updates."""
        response = input("\n⚠️  Update modified commands in master? (y/n): ")
        return response.lower() == 'y'
    
    def distribute_updates(self) -> bool:
        """Distribute updated commands from master to all repositories."""
        print("\n📤 Distributing updates from master to all repositories...")
        
        # Use the propagate_commands module
        propagate_script = self.master_commands_dir / "propagate_commands.py"
        if propagate_script.exists():
            import subprocess
            try:
                result = subprocess.run(
                    [sys.executable, str(propagate_script), 
                     "--source", str(self.master_repo),
                     "--target-dir", str(self.workspace_dir),
                     "--force"],
                    capture_output=True,
                    text=True
                )
                print(result.stdout)
                return result.returncode == 0
            except Exception as e:
                print(f"❌ Failed to distribute updates: {e}")
                return False
        else:
            print("❌ Propagate commands script not found")
            return False


def main():
    """Main entry point for sync-all-commands."""
    parser = argparse.ArgumentParser(
        description="Synchronize slash commands across all repositories"
    )
    parser.add_argument(
        "--master-repo",
        default=".",
        help="Master repository path (default: current directory)"
    )
    parser.add_argument(
        "--workspace",
        default="/mnt/github/github",
        help="Workspace directory containing all repositories"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be done without making changes"
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Force update modified commands without prompting"
    )
    parser.add_argument(
        "--distribute",
        action="store_true",
        help="After sync, distribute updates to all repositories"
    )
    parser.add_argument(
        "--report-only",
        action="store_true",
        help="Only generate a report without syncing"
    )
    
    args = parser.parse_args()
    
    # Initialize synchronizer
    master_repo = Path(args.master_repo).resolve()
    workspace_dir = Path(args.workspace).resolve()
    
    if not master_repo.exists():
        print(f"❌ Master repository not found: {master_repo}")
        return 1
    
    if not workspace_dir.exists():
        print(f"❌ Workspace directory not found: {workspace_dir}")
        return 1
    
    print("🎯 Command Synchronization System")
    print("=" * 60)
    print(f"📦 Master Repository: {master_repo.name}")
    print(f"🗂️  Workspace: {workspace_dir}")
    print("=" * 60)
    
    synchronizer = CommandSynchronizer(master_repo, workspace_dir)
    
    if args.report_only:
        synchronizer.scan_all_repositories()
        synchronizer.identify_new_commands()
        synchronizer.identify_modified_commands()
        report = synchronizer.create_sync_report()
        synchronizer.save_sync_report(report)
        print("\n📊 Report generated successfully!")
    else:
        # Perform synchronization
        report = synchronizer.sync_all(dry_run=args.dry_run, force=args.force)
        
        # Print summary
        print("\n" + "=" * 60)
        print("📊 Synchronization Summary")
        print("=" * 60)
        print(f"✅ Total repositories scanned: {report['statistics']['total_repositories']}")
        print(f"📦 Total unique commands found: {report['statistics']['total_commands']}")
        print(f"🆕 New commands synced: {report['statistics']['new_commands']}")
        print(f"🔧 Modified commands: {report['statistics']['modified_commands']}")
        
        if args.distribute and not args.dry_run:
            print("\n" + "=" * 60)
            if synchronizer.distribute_updates():
                print("✅ Updates distributed to all repositories!")
            else:
                print("❌ Failed to distribute updates")
    
    print("\n✨ Command synchronization complete!")
    return 0


if __name__ == "__main__":
    sys.exit(main())