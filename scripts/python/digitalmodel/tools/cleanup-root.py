#!/usr/bin/env python
"""
Automatic Repository Root Cleanup Tool

This script enforces repository hygiene by cleaning up the root directory.
It moves misplaced files to appropriate locations and removes temporary files.
"""

import os
import shutil
from pathlib import Path
from datetime import datetime
import argparse

class RootCleaner:
    """Clean up repository root directory according to hygiene standards."""
    
    # Whitelisted files allowed in root
    WHITELIST = {
        # Config files
        '.gitignore', '.gitmodules', '.editorconfig', '.coveragerc',
        'pyproject.toml', 'uv.toml', 'setup.py', 'setup.cfg',
        
        # Documentation
        'README.md', 'LICENSE', 'CLAUDE.md', 'Makefile',
        
        # Docker
        'docker-compose.yml', 'Dockerfile',
        
        # Package files
        'package.json', 'package-lock.json',
        
        # Convenience scripts
        'create-spec.sh', 'slash.sh',
    }
    
    # Whitelisted directories
    WHITELIST_DIRS = {
        '.agent-os', '.git', '.github', '.vscode', '.idea',
        'agents', 'config', 'docs', 'examples', 'scripts',
        'specs', 'src', 'tests', 'tools', 'venv', '.venv',
        'htmlcov', 'logs', 'test_baselines', 'test_reports'
    }
    
    def __init__(self, root_path='.', dry_run=False, verbose=False):
        """Initialize the cleaner."""
        self.root = Path(root_path).resolve()
        self.dry_run = dry_run
        self.verbose = verbose
        self.actions = []
        
    def log(self, message):
        """Log a message."""
        if self.verbose:
            print(message)
        self.actions.append(message)
        
    def clean(self):
        """Run the cleanup process."""
        print(f"{'DRY RUN: ' if self.dry_run else ''}Cleaning repository root: {self.root}")
        print("=" * 60)
        
        moved = 0
        deleted = 0
        
        # Process all items in root
        for item in self.root.iterdir():
            # Skip whitelisted directories
            if item.is_dir() and item.name in self.WHITELIST_DIRS:
                continue
                
            # Skip whitelisted files
            if item.is_file() and item.name in self.WHITELIST:
                continue
                
            # Skip hidden files (except specific ones)
            if item.name.startswith('.') and item.name not in ['.command-link', '.command-registry.json']:
                continue
                
            # Process violations
            if item.is_file():
                action = self.process_file(item)
                if action == 'moved':
                    moved += 1
                elif action == 'deleted':
                    deleted += 1
            elif item.is_dir():
                # Unexpected directory in root
                self.log(f"[!] Unexpected directory in root: {item.name}")
                
        # Summary
        print("\n" + "=" * 60)
        print(f"Summary: {moved} files moved, {deleted} files deleted")
        
        if self.dry_run:
            print("\n[!] This was a DRY RUN. No actual changes were made.")
            print("Run without --dry-run to apply changes.")
            
        return moved, deleted
        
    def process_file(self, file_path):
        """Process a single file."""
        name = file_path.name
        
        # Test files
        if name.startswith('test_') or name.endswith('_test.py'):
            return self.move_file(file_path, 'tests/modules/orcaflex/test_scripts')
            
        # Backup files
        if '.backup' in name or name.endswith(('.bak', '~', '.old')):
            return self.delete_file(file_path, "backup file")
            
        # Migration files
        if 'migration' in name.lower() or 'upgrade' in name.lower():
            return self.delete_file(file_path, "migration file")
            
        # Temporary files
        if name in ['nul', 'coverage.xml', '.command-link', '.command-registry.json', 'agos']:
            return self.delete_file(file_path, "temporary file")
            
        if name.startswith(('tmp', 'temp')) or name.endswith('.tmp'):
            return self.delete_file(file_path, "temporary file")
            
        # Batch config files
        if name.startswith('batch_') and name.endswith('.yml'):
            return self.move_file(file_path, 'tests/modules/orcaflex/batch_processing/test_configs')
            
        # FSTS config files
        if name.startswith('fsts_') and name.endswith('.yml'):
            return self.move_file(file_path, 'tests/modules/orcaflex/batch_processing/test_configs')
            
        # Python tools
        if name.startswith('create-') and name.endswith('.py'):
            return self.move_file(file_path, 'tools')
            
        if name.startswith('generate_') and name.endswith('.py'):
            return self.move_file(file_path, 'tools')
            
        # Other YAML/JSON configs
        if name.endswith(('.yml', '.yaml', '.json')) and name not in self.WHITELIST:
            return self.move_file(file_path, 'config')
            
        # Requirements files
        if name.startswith('requirements') and name.endswith('.txt'):
            return self.delete_file(file_path, "consolidated requirements file")
            
        # Log files
        if name.endswith('.log'):
            return self.move_file(file_path, 'logs')
            
        # Unknown file
        self.log(f"[?] Unknown file (manual review needed): {name}")
        return None
        
    def move_file(self, file_path, destination):
        """Move a file to the specified destination."""
        dest_dir = self.root / destination
        dest_path = dest_dir / file_path.name
        
        self.log(f"[MOVE] {file_path.name} -> {destination}/")
        
        if not self.dry_run:
            dest_dir.mkdir(parents=True, exist_ok=True)
            shutil.move(str(file_path), str(dest_path))
            
        return 'moved'
        
    def delete_file(self, file_path, reason):
        """Delete a file."""
        self.log(f"[DELETE] {reason}: {file_path.name}")
        
        if not self.dry_run:
            file_path.unlink()
            
        return 'deleted'
        

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Clean up repository root directory'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without making changes'
    )
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Show detailed output'
    )
    parser.add_argument(
        '--root',
        default='.',
        help='Repository root directory (default: current directory)'
    )
    
    args = parser.parse_args()
    
    cleaner = RootCleaner(
        root_path=args.root,
        dry_run=args.dry_run,
        verbose=args.verbose
    )
    
    moved, deleted = cleaner.clean()
    
    # Show all actions
    if args.verbose and cleaner.actions:
        print("\n" + "=" * 60)
        print("Actions taken:")
        for action in cleaner.actions:
            print(f"  {action}")
            
    # Create report
    if not args.dry_run:
        report_file = Path('logs') / f'cleanup_{datetime.now().strftime("%Y%m%d_%H%M%S")}.log'
        report_file.parent.mkdir(exist_ok=True)
        with open(report_file, 'w') as f:
            f.write(f"Repository Root Cleanup Report\n")
            f.write(f"{'=' * 60}\n")
            f.write(f"Date: {datetime.now()}\n")
            f.write(f"Files moved: {moved}\n")
            f.write(f"Files deleted: {deleted}\n")
            f.write(f"\nActions:\n")
            for action in cleaner.actions:
                f.write(f"  {action}\n")
        print(f"\n[REPORT] Saved to: {report_file}")


if __name__ == '__main__':
    main()