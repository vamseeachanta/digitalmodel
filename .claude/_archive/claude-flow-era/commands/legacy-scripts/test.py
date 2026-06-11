#!/usr/bin/env python3
"""
Unified Test Command - Consolidates testing operations

Replaces: test-automation, test-automation-enhanced
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

class UnifiedTestCommand:
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
        
    def run(self, module: str = None):
        """Run tests."""
        # Use the enhanced version with UV Python
        cmd = [self.python_exe, str(Path(__file__).parent / "test_automation_enhanced.py")]
        
        if module:
            cmd.extend(["run-module", module])
        else:
            cmd.append("run-all")
        
        subprocess.run(cmd)
    
    def fix(self):
        """Auto-fix test failures."""
        cmd = [self.python_exe, str(Path(__file__).parent / "test_automation_enhanced.py"),
               "run-all", "--auto-fix"]
        subprocess.run(cmd)
    
    def summary(self):
        """Generate test summaries."""
        cmd = [self.python_exe, str(Path(__file__).parent / "test_automation_enhanced.py"),
               "generate-summary"]
        subprocess.run(cmd)
    
    def coverage(self):
        """Show coverage report."""
        cmd = [self.python_exe, str(Path(__file__).parent / "test_automation_enhanced.py"),
               "run-all", "--coverage"]
        subprocess.run(cmd)

def main():
    parser = argparse.ArgumentParser(prog='test', add_help=False)
    parser.add_argument('subcommand', nargs='?', default='help',
                       choices=['run', 'fix', 'summary', 'coverage', 'help'])
    parser.add_argument('module', nargs='?')
    
    args = parser.parse_args()
    
    cmd = UnifiedTestCommand()
    
    if args.subcommand == 'run':
        cmd.run(args.module)
    elif args.subcommand == 'fix':
        cmd.fix()
    elif args.subcommand == 'summary':
        cmd.summary()
    elif args.subcommand == 'coverage':
        cmd.coverage()
    else:
        print("""
🧪 Unified Test Command

Usage: /test [subcommand] [options]

Subcommands:
  run [MODULE]    Run all tests or module tests
  fix            Auto-fix test failures
  summary        Generate test summaries
  coverage       Show coverage report
  help           Show this help

Examples:
  /test run
  /test run authentication
  /test fix
  /test summary
  /test coverage
""")

if __name__ == '__main__':
    main()
