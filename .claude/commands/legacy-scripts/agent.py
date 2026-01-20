#!/usr/bin/env python3
"""
Unified Agent Command - Agent OS management
"""

import sys
import os
import subprocess
import argparse
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
import shutil

class UnifiedAgentCommand:
    def __init__(self):
        self.base_path = Path("/mnt/github/github")
        self.commands_dir = self.base_path / ".agent-os" / "commands"
        
    def sync(self):
        """Sync all agent commands to all repos."""
        print("🔄 Syncing agent commands to all repositories...\n")
        
        repos = [d.name for d in self.base_path.iterdir() 
                if d.is_dir() and (d / '.git').exists()]
        
        success = 0
        for repo in repos:
            repo_cmd_dir = self.base_path / repo / ".agent-os" / "commands"
            repo_cmd_dir.mkdir(parents=True, exist_ok=True)
            
            # Copy all command files
            for cmd_file in self.commands_dir.glob("*.py"):
                dest = repo_cmd_dir / cmd_file.name
                shutil.copy2(cmd_file, dest)
                os.chmod(dest, 0o755)
            
            print(f"✅ {repo}")
            success += 1
        
        print(f"\n✅ Synced to {success}/{len(repos)} repositories")
    
    def list(self):
        """List available commands."""
        print("📚 Available Agent Commands\n")
        
        commands = {
            '/git': 'Git operations (status, sync, trunk, commit, clean)',
            '/spec': 'Specification management (create, list, tasks)',
            '/task': 'Task execution (execute, status, verify)',
            '/test': 'Testing suite (run, fix, summary, coverage)',
            '/data': 'Engineering data (scan, context, research, query)',
            '/project': 'Project management (setup, deps, structure, health)',
            '/agent': 'Agent OS management (sync, list, help)'
        }
        
        for cmd, desc in commands.items():
            print(f"  {cmd:12} - {desc}")
        
        print("\nUse '/[command] help' for detailed usage")
    
    def help(self, command: str = None):
        """Show help for a command."""
        if command:
            # Run the command's help
            cmd_path = self.commands_dir / f"{command}.py"
            if cmd_path.exists():
                subprocess.run([sys.executable, str(cmd_path), "help"])
            else:
                print(f"Command '{command}' not found")
        else:
            self.list()

def main():
    parser = argparse.ArgumentParser(prog='agent', add_help=False)
    parser.add_argument('subcommand', nargs='?', default='help',
                       choices=['sync', 'list', 'help', 'install'])
    parser.add_argument('command', nargs='?')
    
    args = parser.parse_args()
    
    cmd = UnifiedAgentCommand()
    
    if args.subcommand == 'sync':
        cmd.sync()
    elif args.subcommand == 'list':
        cmd.list()
    elif args.subcommand == 'help':
        cmd.help(args.command)
    elif args.subcommand == 'install':
        # Run ecosystem awareness installation if available
        install_cmd = Path(__file__).parent / "install-ecosystem-awareness.py"
        if install_cmd.exists():
            subprocess.run([sys.executable, str(install_cmd)])
        else:
            print("Ecosystem awareness installer not available")
    else:
        print("""
🤖 Agent Command

Usage: /agent [subcommand] [options]

Subcommands:
  sync       Sync commands to all repos
  list       List available commands
  help CMD   Get help for a command
  install    Install ecosystem awareness
  
Examples:
  /agent sync
  /agent list
  /agent help git
  /agent install
""")

if __name__ == '__main__':
    main()
