#!/usr/bin/env python
"""
/propagate-commands - Distribute Slash Commands Across All Repositories

This command transfers custom slash commands from a source repository
to all other repositories in a directory, ensuring consistent tooling
across your entire codebase.
"""

import os
import sys
import json
import shutil
import subprocess
from pathlib import Path
from typing import List, Dict, Tuple, Optional, Set
from concurrent.futures import ThreadPoolExecutor, as_completed
import tempfile
import logging
from datetime import datetime
import hashlib

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)

class CommandPropagator:
    """Propagate slash commands across multiple repositories."""
    
    # Standard locations to look for custom commands
    COMMAND_LOCATIONS = [
        ".agent-os/commands",
        ".git-commands",
        "scripts/slash_commands",
        ".claude/commands",
        "slash_commands"
    ]
    
    def __init__(self, source_repo: Path, target_dir: Path, force: bool = False):
        self.source_repo = Path(source_repo)
        self.target_dir = Path(target_dir)
        self.force = force
        self.commands_found = {}
        self.command_registry = {}
        
    def discover_commands(self) -> Dict[str, Dict]:
        """Discover all custom commands in the source repository."""
        commands = {}
        
        for location in self.COMMAND_LOCATIONS:
            cmd_path = self.source_repo / location
            if cmd_path.exists():
                if cmd_path.is_dir():
                    # Look for Python files in the directory
                    for py_file in cmd_path.glob("*.py"):
                        if py_file.name != "__init__.py":
                            command_name = self._extract_command_name(py_file)
                            if command_name:
                                commands[command_name] = {
                                    "file": py_file,
                                    "location": location,
                                    "type": "python",
                                    "description": self._extract_description(py_file)
                                }
                elif cmd_path.suffix == ".py":
                    # Single Python file
                    command_name = self._extract_command_name(cmd_path)
                    if command_name:
                        commands[command_name] = {
                            "file": cmd_path,
                            "location": str(cmd_path.parent.relative_to(self.source_repo)),
                            "type": "python",
                            "description": self._extract_description(cmd_path)
                        }
        
        # Look for command registry file
        registry_file = self.source_repo / ".command-registry.json"
        if registry_file.exists():
            with open(registry_file, 'r') as f:
                self.command_registry = json.load(f)
        
        self.commands_found = commands
        return commands
    
    def _extract_command_name(self, file_path: Path) -> Optional[str]:
        """Extract command name from file content or filename."""
        try:
            content = file_path.read_text()
            
            # Look for command name in docstring or comments
            import re
            
            # Pattern 1: /command-name in docstring
            pattern1 = r'"""\s*/([a-z-]+)'
            match = re.search(pattern1, content)
            if match:
                return f"/{match.group(1)}"
            
            # Pattern 2: Command name in first line comment
            pattern2 = r'^#\s*/([a-z-]+)'
            match = re.search(pattern2, content, re.MULTILINE)
            if match:
                return f"/{match.group(1)}"
            
            # Fallback to filename
            name = file_path.stem.replace('_', '-')
            if name and not name.startswith('.'):
                return f"/{name}"
                
        except Exception as e:
            logger.debug(f"Could not extract command name from {file_path}: {e}")
        
        return None
    
    def _extract_description(self, file_path: Path) -> str:
        """Extract description from file docstring."""
        try:
            content = file_path.read_text()
            
            # Extract docstring
            import ast
            tree = ast.parse(content)
            docstring = ast.get_docstring(tree)
            
            if docstring:
                # Get first non-empty line after command name
                lines = docstring.split('\n')
                for line in lines:
                    line = line.strip()
                    if line and not line.startswith('/'):
                        return line[:100]  # Limit description length
            
        except Exception as e:
            logger.debug(f"Could not extract description from {file_path}: {e}")
        
        return "Custom slash command"
    
    def create_command_structure(self, repo_path: Path) -> bool:
        """Create the standard command structure in a repository."""
        try:
            # Create .agent-os/commands directory
            commands_dir = repo_path / ".agent-os" / "commands"
            commands_dir.mkdir(parents=True, exist_ok=True)
            
            # Create __init__.py
            init_file = commands_dir / "__init__.py"
            if not init_file.exists():
                init_file.write_text('"""Agent OS Custom Commands"""\n')
            
            # Create command wrapper
            wrapper_file = repo_path / "slash_commands.py"
            if not wrapper_file.exists() or self.force:
                wrapper_content = self._generate_command_wrapper()
                wrapper_file.write_text(wrapper_content)
                wrapper_file.chmod(0o755)
            
            return True
            
        except Exception as e:
            logger.error(f"Failed to create command structure: {e}")
            return False
    
    def _generate_command_wrapper(self) -> str:
        """Generate the command wrapper script."""
        return '''#!/usr/bin/env python
"""
Slash Command Wrapper - Auto-generated by /propagate-commands
This file provides a unified interface for all custom slash commands.
"""

import sys
import os
from pathlib import Path
import importlib.util
import argparse

# Add command directory to path
COMMAND_DIR = Path(__file__).parent / ".agent-os/commands"
sys.path.insert(0, str(COMMAND_DIR))

def load_command(command_name: str):
    """Dynamically load a command module."""
    # Remove leading slash and convert to module name
    module_name = command_name.lstrip('/').replace('-', '_')
    module_path = COMMAND_DIR / f"{module_name}.py"
    
    if not module_path.exists():
        print(f"❌ Command {command_name} not found!")
        print(f"   Looked for: {module_path}")
        return None
    
    # Load the module
    spec = importlib.util.spec_from_file_location(module_name, module_path)
    if spec and spec.loader:
        module = importlib.util.module_from_spec(spec)
        sys.modules[module_name] = module
        spec.loader.exec_module(module)
        return module
    
    return None

def list_available_commands():
    """List all available commands."""
    print("📋 Available Slash Commands:")
    print("=" * 40)
    
    # Load command registry if exists
    registry_file = Path(__file__).parent / ".command-registry.json"
    if registry_file.exists():
        import json
        with open(registry_file, 'r') as f:
            registry = json.load(f)
            for cmd, info in registry.get("commands", {}).items():
                print(f"  {cmd:<20} - {info.get('description', 'No description')}")
    else:
        # Scan command directory
        if COMMAND_DIR.exists():
            for py_file in sorted(COMMAND_DIR.glob("*.py")):
                if py_file.name != "__init__.py":
                    cmd_name = "/" + py_file.stem.replace('_', '-')
                    print(f"  {cmd_name}")
    
    print()
    print("Usage: ./slash_commands.py <command> [args...]")
    print("Example: ./slash_commands.py /modernize-deps --parallel=5")

def main():
    """Main entry point for slash commands."""
    if len(sys.argv) < 2:
        list_available_commands()
        sys.exit(0)
    
    command = sys.argv[1]
    
    # Special case: list commands
    if command in ["--list", "-l", "list"]:
        list_available_commands()
        sys.exit(0)
    
    # Load and execute command
    module = load_command(command)
    if module:
        # Check if module has a main function
        if hasattr(module, 'main'):
            # Pass remaining arguments to the command
            sys.argv = [command] + sys.argv[2:]
            sys.exit(module.main())
        else:
            print(f"❌ Command {command} does not have a main() function!")
            sys.exit(1)
    else:
        print(f"❌ Unknown command: {command}")
        print("Use --list to see available commands")
        sys.exit(1)

if __name__ == "__main__":
    main()
'''
    
    def copy_command_files(self, repo_path: Path) -> List[str]:
        """Copy command files to the target repository."""
        copied_files = []
        
        commands_dir = repo_path / ".agent-os" / "commands"
        
        for cmd_name, cmd_info in self.commands_found.items():
            source_file = cmd_info["file"]
            
            # Generate target filename
            target_name = cmd_name.lstrip('/').replace('-', '_') + ".py"
            target_file = commands_dir / target_name
            
            try:
                # Copy the file
                shutil.copy2(source_file, target_file)
                copied_files.append(cmd_name)
                logger.debug(f"    Copied {cmd_name}")
                
            except Exception as e:
                logger.error(f"    Failed to copy {cmd_name}: {e}")
        
        return copied_files
    
    def create_command_registry(self, repo_path: Path, commands: List[str]):
        """Create or update the command registry file."""
        registry_file = repo_path / ".command-registry.json"
        
        # Load existing registry if exists
        existing_registry = {}
        if registry_file.exists() and not self.force:
            try:
                with open(registry_file, 'r') as f:
                    existing_registry = json.load(f)
            except:
                pass
        
        # Build new registry
        registry = {
            "version": "1.0.0",
            "last_updated": datetime.now().isoformat(),
            "source": str(self.source_repo),
            "commands": existing_registry.get("commands", {})
        }
        
        # Update with new commands
        for cmd_name in commands:
            if cmd_name in self.commands_found:
                cmd_info = self.commands_found[cmd_name]
                registry["commands"][cmd_name] = {
                    "description": cmd_info["description"],
                    "source": f".agent-os/commands/{cmd_name.lstrip('/').replace('-', '_')}.py",
                    "type": cmd_info["type"],
                    "last_updated": datetime.now().isoformat()
                }
        
        # Write registry
        with open(registry_file, 'w') as f:
            json.dump(registry, f, indent=2)
        
        return True
    
    def create_commands_documentation(self, repo_path: Path, commands: List[str]):
        """Create COMMANDS.md documentation file."""
        doc_file = repo_path / "COMMANDS.md"
        
        content = [
            "# Custom Slash Commands",
            "",
            f"Last updated: {datetime.now().strftime('%Y-%m-%d %H:%M')}",
            f"Propagated from: {self.source_repo.name}",
            "",
            "## Available Commands",
            ""
        ]
        
        for cmd_name in sorted(commands):
            if cmd_name in self.commands_found:
                cmd_info = self.commands_found[cmd_name]
                content.append(f"### `{cmd_name}`")
                content.append(f"{cmd_info['description']}")
                content.append("")
        
        content.extend([
            "## Usage",
            "",
            "```bash",
            "# List all available commands",
            "./slash_commands.py --list",
            "",
            "# Execute a specific command",
            "./slash_commands.py /modernize-deps",
            "",
            "# Get help for a command",
            "./slash_commands.py /modernize-deps --help",
            "```",
            "",
            "## Adding New Commands",
            "",
            "Place new command files in `.agent-os/commands/` directory.",
            "Use `/propagate-commands` to distribute to other repositories.",
            ""
        ])
        
        doc_file.write_text('\n'.join(content))
        return True
    
    def validate_installation(self, repo_path: Path, expected_commands: List[str]) -> Tuple[bool, List[str]]:
        """Validate that commands were properly installed."""
        issues = []
        
        # Check command files exist
        commands_dir = repo_path / ".agent-os" / "commands"
        for cmd_name in expected_commands:
            target_name = cmd_name.lstrip('/').replace('-', '_') + ".py"
            target_file = commands_dir / target_name
            if not target_file.exists():
                issues.append(f"Missing command file: {target_name}")
        
        # Check wrapper exists
        wrapper_file = repo_path / "slash_commands.py"
        if not wrapper_file.exists():
            issues.append("Missing wrapper file: slash_commands.py")
        
        # Check registry exists
        registry_file = repo_path / ".command-registry.json"
        if not registry_file.exists():
            issues.append("Missing command registry")
        
        return len(issues) == 0, issues
    
    def propagate_to_repository(self, repo_path: Path) -> Dict:
        """Propagate commands to a single repository."""
        result = {
            "repo": repo_path.name,
            "success": False,
            "commands_installed": [],
            "errors": []
        }
        
        try:
            # Skip source repository
            if repo_path.resolve() == self.source_repo.resolve():
                result["success"] = True
                result["skipped"] = True
                return result
            
            # Step 1: Create command structure
            if not self.create_command_structure(repo_path):
                result["errors"].append("Failed to create command structure")
                return result
            
            # Step 2: Copy command files
            copied_commands = self.copy_command_files(repo_path)
            result["commands_installed"] = copied_commands
            
            # Step 3: Create command registry
            self.create_command_registry(repo_path, copied_commands)
            
            # Step 4: Create documentation
            self.create_commands_documentation(repo_path, copied_commands)
            
            # Step 5: Validate installation
            valid, issues = self.validate_installation(repo_path, copied_commands)
            if not valid:
                result["errors"].extend(issues)
            else:
                result["success"] = True
            
        except Exception as e:
            result["errors"].append(str(e))
        
        return result


def find_repositories(base_dir: Path, include_non_git: bool = False) -> List[Path]:
    """Find all repositories in the given directory."""
    repos = []
    
    for item in base_dir.iterdir():
        if item.is_dir() and not item.name.startswith('.'):
            # Check if it's a git repository
            if (item / ".git").exists():
                repos.append(item)
            elif include_non_git:
                # Include non-git directories that look like projects
                if any((item / indicator).exists() for indicator in 
                       ["setup.py", "pyproject.toml", "package.json", "Makefile"]):
                    repos.append(item)
    
    return sorted(repos)


def main(source: Optional[str] = None, target_dir: str = ".", 
         commands: Optional[List[str]] = None, parallel: int = 5,
         force: bool = False, dry_run: bool = False,
         repos: Optional[List[str]] = None):
    """
    Main entry point for the propagate-commands command.
    
    Args:
        source: Source repository path (defaults to current directory)
        target_dir: Directory containing target repositories
        commands: Specific commands to propagate
        parallel: Number of parallel workers
        force: Force overwrite existing files
        dry_run: Show what would be done without making changes
        repos: Specific repositories to target
    """
    # Determine source repository
    if source:
        source_repo = Path(source).resolve()
    else:
        source_repo = Path.cwd()
    
    target_base = Path(target_dir).resolve()
    
    logger.info("🚀 Propagating Commands to Repositories")
    logger.info("=" * 50)
    logger.info(f"📦 Source: {source_repo}")
    
    # Initialize propagator
    propagator = CommandPropagator(source_repo, target_base, force)
    
    # Discover commands
    discovered_commands = propagator.discover_commands()
    
    if not discovered_commands:
        logger.error("❌ No commands found in source repository!")
        logger.info("\nSearched in:")
        for location in CommandPropagator.COMMAND_LOCATIONS:
            logger.info(f"  • {location}")
        return 1
    
    # Filter commands if specified
    if commands:
        discovered_commands = {
            k: v for k, v in discovered_commands.items()
            if k in commands
        }
    
    logger.info(f"📋 Commands to propagate:")
    for cmd_name in discovered_commands:
        logger.info(f"  • {cmd_name}")
    
    # Find target repositories
    if repos:
        repo_paths = [target_base / repo for repo in repos]
        repo_paths = [p for p in repo_paths if p.exists()]
    else:
        repo_paths = find_repositories(target_base)
    
    # Filter out source repository
    repo_paths = [r for r in repo_paths if r.resolve() != source_repo.resolve()]
    
    if not repo_paths:
        logger.error("❌ No target repositories found!")
        return 1
    
    logger.info(f"\n🎯 Target repositories: {len(repo_paths)}")
    
    if dry_run:
        logger.info("\n🔍 DRY RUN MODE - No changes will be made")
        for repo in repo_paths:
            logger.info(f"  Would propagate to: {repo.name}")
        return 0
    
    logger.info(f"🔧 Using {parallel} parallel workers")
    logger.info("")
    
    # Process repositories in parallel
    results = []
    logger.info("🔄 Processing repositories:")
    
    with ThreadPoolExecutor(max_workers=parallel) as executor:
        futures = {
            executor.submit(propagator.propagate_to_repository, repo): repo
            for repo in repo_paths
        }
        
        for future in as_completed(futures):
            repo = futures[future]
            try:
                result = future.result(timeout=30)
                results.append(result)
                
                # Log progress
                if result.get("skipped"):
                    logger.info(f"  ⏭️  {result['repo']} [skipped - source repo]")
                elif result["success"]:
                    cmd_count = len(result["commands_installed"])
                    logger.info(f"  ✅ {result['repo']} [{cmd_count} commands installed]")
                else:
                    logger.info(f"  ❌ {result['repo']} [failed]")
                    
            except Exception as e:
                logger.error(f"  ❌ {repo.name} [error: {e}]")
                results.append({
                    "repo": repo.name,
                    "success": False,
                    "errors": [str(e)]
                })
    
    # Generate summary
    logger.info("\n" + "=" * 50)
    logger.info("📊 Summary")
    logger.info("=" * 50)
    
    successful = sum(1 for r in results if r.get("success", False))
    failed = len(results) - successful
    
    logger.info(f"✅ Success: {successful}/{len(results)} repositories")
    
    if failed > 0:
        logger.info(f"❌ Failed: {failed} repositories")
        logger.info("\nFailed repositories:")
        for result in results:
            if not result.get("success", False):
                errors = result.get("errors", ["Unknown error"])
                logger.info(f"  • {result['repo']}: {', '.join(errors)}")
    
    # Command statistics
    total_commands_installed = sum(
        len(r.get("commands_installed", [])) for r in results
    )
    
    logger.info(f"\n📈 Statistics:")
    logger.info(f"  • Commands propagated: {len(discovered_commands)}")
    logger.info(f"  • Total installations: {total_commands_installed}")
    logger.info(f"  • Time: {datetime.now().strftime('%H:%M:%S')}")
    
    logger.info("\n✨ Command propagation complete!")
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Propagate slash commands across repositories")
    parser.add_argument("--source", help="Source repository (default: current directory)")
    parser.add_argument("--target-dir", default=".", help="Directory containing target repositories")
    parser.add_argument("--commands", nargs="+", help="Specific commands to propagate")
    parser.add_argument("--parallel", type=int, default=5, help="Number of parallel workers")
    parser.add_argument("--force", action="store_true", help="Force overwrite existing files")
    parser.add_argument("--dry-run", action="store_true", help="Show what would be done")
    parser.add_argument("--repos", nargs="+", help="Specific repositories to target")
    
    args = parser.parse_args()
    
    sys.exit(main(
        source=args.source,
        target_dir=args.target_dir,
        commands=args.commands,
        parallel=args.parallel,
        force=args.force,
        dry_run=args.dry_run,
        repos=args.repos
    ))