#!/usr/bin/env python3
"""
Unified Spec Command - Consolidates spec operations

Replaces: create-spec, create-spec-enhanced
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

class UnifiedSpecCommand:
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
        
    def create(self, name: str, module: str = None):
        """Create a new spec."""
        # Use the new enhanced version with AI templates and UV Python
        cmd = [self.python_exe, str(Path(__file__).parent / "spec_enhanced.py"), "create"]
        if name:
            cmd.append(name)
        if module:
            cmd.append(module)
        
        subprocess.run(cmd)
        
    def list(self):
        """List all specs."""
        specs_path = self.base_path / "specs" / "modules"
        if not specs_path.exists():
            specs_path = self.base_path / ".agent-os" / "specs"
        
        if specs_path.exists():
            print("📋 Available Specs:\n")
            for spec_dir in sorted(specs_path.glob("*")):
                if spec_dir.is_dir():
                    spec_file = spec_dir / "spec.md"
                    if spec_file.exists():
                        print(f"  • {spec_dir.name}")
        else:
            print("No specs found")
    
    def tasks(self, spec_name: str = None):
        """Show tasks for a spec."""
        if spec_name:
            # Find spec directory
            for base in [self.base_path / "specs" / "modules",
                        self.base_path / ".agent-os" / "specs"]:
                spec_dir = base / spec_name
                if spec_dir.exists():
                    tasks_file = spec_dir / "tasks.md"
                    if tasks_file.exists():
                        print(f"📝 Tasks for {spec_name}:\n")
                        print(tasks_file.read_text())
                        return
            print(f"Spec '{spec_name}' not found")
        else:
            print("Please specify a spec name")
    
    def templates(self):
        """Show available AI templates."""
        cmd = [self.python_exe, str(Path(__file__).parent / "spec_enhanced.py"), "templates"]
        subprocess.run(cmd)

def main():
    parser = argparse.ArgumentParser(prog='spec', add_help=False)
    parser.add_argument('subcommand', nargs='?', default='help',
                       choices=['create', 'list', 'tasks', 'templates', 'help'])
    parser.add_argument('name', nargs='?')
    parser.add_argument('module', nargs='?')
    parser.add_argument('--module', dest='module_flag')
    
    args = parser.parse_args()
    
    cmd = UnifiedSpecCommand()
    
    if args.subcommand == 'create':
        module = args.module_flag or args.module
        cmd.create(args.name, module)
    elif args.subcommand == 'list':
        cmd.list()
    elif args.subcommand == 'tasks':
        cmd.tasks(args.name)
    elif args.subcommand == 'templates':
        cmd.templates()
    else:
        print("""
📝 Unified Spec Command

Usage: /spec [subcommand] [options]

Subcommands:
  create NAME MODULE   Create new specification with AI templates
  list                List all specifications  
  tasks SPEC          Show tasks for a spec
  templates           Show available AI templates
  help                Show this help

Examples:
  /spec create user-auth authentication
  /spec list
  /spec tasks user-auth
  /spec templates

Resources:
  • Claude Code Templates: https://github.com/davila7/claude-code-templates
  • AITmpl: https://www.aitmpl.com/
""")

if __name__ == '__main__':
    main()
