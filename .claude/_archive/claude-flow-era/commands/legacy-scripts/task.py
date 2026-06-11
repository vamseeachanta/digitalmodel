#!/usr/bin/env python3
"""
Unified Task Command - Consolidates task operations

Replaces: execute-tasks, execute-tasks-enhanced
"""

import sys
import os
import subprocess
import argparse
from pathlib import Path

# Import UV manager if available
try:
    from uv_environment_manager import UVEnvironmentManager
    HAS_UV_MANAGER = True
except ImportError:
    HAS_UV_MANAGER = False

class UnifiedTaskCommand:
    def __init__(self):
        self.base_path = Path.cwd()
        self.python_exe = sys.executable
        
        # Try to use UV environment if available
        if HAS_UV_MANAGER:
            try:
                self.uv_manager = UVEnvironmentManager(self.base_path)
                # Ensure UV environment exists
                success, msg = self.uv_manager.ensure_uv_environment()
                if success:
                    print(f"✅ Using UV environment")
                    # Get UV Python executable
                    uv_python = self.uv_manager.get_python_executable()
                    if uv_python:
                        self.python_exe = str(uv_python)
                else:
                    print(f"ℹ️  UV not configured, using system Python")
            except Exception as e:
                print(f"ℹ️  UV manager not available: {e}")
                self.uv_manager = None
        else:
            self.uv_manager = None
        
    def execute(self, task_id: str = None, all_tasks: bool = False):
        """Execute tasks."""
        # Use the enhanced version with UV Python
        cmd = [self.python_exe, str(Path(__file__).parent / "execute_tasks_enhanced.py")]
        
        if all_tasks:
            cmd.append("--all")
        elif task_id:
            cmd.extend(["--task", task_id])
        
        subprocess.run(cmd)
    
    def status(self):
        """Show task status."""
        # Look for tasks.md files
        for base in [self.base_path / "specs" / "modules",
                    self.base_path / ".agent-os" / "specs"]:
            if base.exists():
                print("📊 Task Status:\n")
                for spec_dir in base.glob("**/tasks.md"):
                    content = spec_dir.read_text()
                    completed = content.count("[x]")
                    total = content.count("- [")
                    print(f"  {spec_dir.parent.name}: {completed}/{total} completed")
    
    def verify(self):
        """Verify AI work."""
        # Use verify-ai-work if available
        verify_cmd = Path(__file__).parent / "verify-ai-work.py"
        if verify_cmd.exists():
            subprocess.run([self.python_exe, str(verify_cmd)])
        else:
            print("Verify command not available")

def main():
    parser = argparse.ArgumentParser(prog='task', add_help=False)
    parser.add_argument('subcommand', nargs='?', default='help',
                       choices=['execute', 'status', 'verify', 'help'])
    parser.add_argument('task_id', nargs='?')
    parser.add_argument('--all', action='store_true')
    
    args = parser.parse_args()
    
    cmd = UnifiedTaskCommand()
    
    if args.subcommand == 'execute':
        cmd.execute(args.task_id, args.all)
    elif args.subcommand == 'status':
        cmd.status()
    elif args.subcommand == 'verify':
        cmd.verify()
    else:
        print("""
📋 Unified Task Command

Usage: /task [subcommand] [options]

Subcommands:
  execute [TASK_ID]   Execute specific task
  execute --all       Execute all pending tasks
  status             Show task status
  verify             Verify AI work
  help               Show this help

Examples:
  /task execute 1.2
  /task execute --all
  /task status
  /task verify
""")

if __name__ == '__main__':
    main()
