#!/usr/bin/env python3
"""
Search Commands - Easy discovery and exploration of slash commands
Provides intelligent search, filtering, and detailed help for all commands.
"""

import os
import sys
import json
import re
import ast
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from datetime import datetime
import argparse
import subprocess

class CommandSearcher:
    """Intelligent search system for slash commands."""
    
    def __init__(self):
        self.workspace = Path("/mnt/github/github")
        self.current_repo = Path.cwd()
        self.command_cache = {}
        self.load_commands()
    
    def load_commands(self):
        """Load all available commands with metadata."""
        # Check current repo first
        local_commands = self.scan_repo_commands(self.current_repo)
        
        # Check AssetUtilities master
        master_repo = self.workspace / "assetutilities"
        master_commands = {}
        if master_repo.exists():
            master_commands = self.scan_repo_commands(master_repo)
        
        # Merge commands (local overrides master)
        self.command_cache = {**master_commands, **local_commands}
    
    def scan_repo_commands(self, repo_path: Path) -> Dict:
        """Scan a repository for slash commands."""
        commands = {}
        
        # Check .agent-os/commands directory
        cmd_dir = repo_path / ".agent-os/commands"
        if cmd_dir.exists():
            for py_file in cmd_dir.glob("*.py"):
                if py_file.name == "__init__.py":
                    continue
                
                cmd_info = self.extract_command_info(py_file)
                if cmd_info:
                    cmd_name = "/" + py_file.stem.replace("_", "-")
                    commands[cmd_name] = cmd_info
        
        # Check .git-commands directory
        git_cmd_dir = repo_path / ".git-commands"
        if git_cmd_dir.exists():
            for py_file in git_cmd_dir.glob("*.py"):
                cmd_info = self.extract_command_info(py_file)
                if cmd_info:
                    cmd_name = "/" + py_file.stem.replace("_", "-")
                    commands[cmd_name] = cmd_info
        
        # Check for slash_commands.py wrapper
        wrapper = repo_path / "slash_commands.py"
        if wrapper.exists():
            for cmd_name, info in commands.items():
                info["runner"] = str(wrapper)
        
        return commands
    
    def extract_command_info(self, file_path: Path) -> Optional[Dict]:
        """Extract metadata from a command file."""
        try:
            content = file_path.read_text()
            lines = content.split('\n')
            
            info = {
                "path": str(file_path),
                "repo": file_path.parts[-4] if len(file_path.parts) > 4 else "local",
                "description": "No description available",
                "usage": None,
                "examples": [],
                "tags": [],
                "hooks": False,
                "multi_repo": False,
                "size": file_path.stat().st_size,
                "modified": datetime.fromtimestamp(file_path.stat().st_mtime).isoformat()
            }
            
            # Extract description from docstring
            in_docstring = False
            docstring_lines = []
            for i, line in enumerate(lines[:50]):
                if '"""' in line:
                    if not in_docstring:
                        in_docstring = True
                        if line.count('"""') == 2:
                            # Single line docstring
                            info["description"] = line.split('"""')[1].strip()
                            break
                    else:
                        break
                elif in_docstring:
                    docstring_lines.append(line.strip())
            
            if docstring_lines:
                info["description"] = docstring_lines[0] if docstring_lines[0] else docstring_lines[1] if len(docstring_lines) > 1 else "No description"
                # Look for usage examples in docstring
                for line in docstring_lines:
                    if "example" in line.lower() or "usage" in line.lower():
                        info["examples"].append(line)
            
            # Extract features
            if "hook" in content.lower():
                info["hooks"] = True
            if "multi" in content.lower() and "repo" in content.lower():
                info["multi_repo"] = True
            
            # Extract tags from content
            if "git" in content.lower():
                info["tags"].append("git")
            if "test" in content.lower():
                info["tags"].append("testing")
            if "dependen" in content.lower():
                info["tags"].append("dependencies")
            if "organize" in content.lower() or "structure" in content.lower():
                info["tags"].append("organization")
            if "sync" in content.lower() or "propagate" in content.lower():
                info["tags"].append("distribution")
            
            # Extract usage from argparse
            if "argparse" in content:
                parser_match = re.search(r'parser\.add_argument.*?\((.*?)\)', content, re.DOTALL)
                if parser_match:
                    info["usage"] = "Supports command-line arguments"
            
            return info
            
        except Exception as e:
            return None
    
    def search(self, query: str = "", filters: Dict = None) -> List[Tuple[str, Dict]]:
        """Search commands with optional filters."""
        results = []
        query_lower = query.lower()
        
        for cmd_name, cmd_info in self.command_cache.items():
            # Text search
            if query and not any([
                query_lower in cmd_name.lower(),
                query_lower in cmd_info["description"].lower(),
                any(query_lower in tag for tag in cmd_info["tags"])
            ]):
                continue
            
            # Apply filters
            if filters:
                if filters.get("hooks") and not cmd_info["hooks"]:
                    continue
                if filters.get("multi_repo") and not cmd_info["multi_repo"]:
                    continue
                if filters.get("tags"):
                    if not any(tag in cmd_info["tags"] for tag in filters["tags"]):
                        continue
                if filters.get("repo") and cmd_info["repo"] != filters["repo"]:
                    continue
            
            results.append((cmd_name, cmd_info))
        
        # Sort by relevance (name match first, then description)
        def sort_key(item):
            cmd_name, _ = item
            if query_lower in cmd_name.lower():
                return (0, cmd_name)
            else:
                return (1, cmd_name)
        
        results.sort(key=sort_key)
        return results
    
    def display_results(self, results: List[Tuple[str, Dict]], detailed: bool = False):
        """Display search results."""
        if not results:
            print("❌ No commands found matching your search.")
            return
        
        print(f"\n🔍 Found {len(results)} command(s):\n")
        print("=" * 70)
        
        for cmd_name, cmd_info in results:
            # Command header
            tags = " ".join([f"[{tag}]" for tag in cmd_info["tags"]]) if cmd_info["tags"] else ""
            features = []
            if cmd_info["hooks"]:
                features.append("🎣 hooks")
            if cmd_info["multi_repo"]:
                features.append("📦 multi-repo")
            features_str = " ".join(features)
            
            print(f"\n✨ {cmd_name}")
            if tags:
                print(f"   Tags: {tags}")
            if features_str:
                print(f"   Features: {features_str}")
            print(f"   {cmd_info['description']}")
            
            if detailed:
                print(f"   Repository: {cmd_info['repo']}")
                print(f"   Path: {cmd_info['path']}")
                print(f"   Size: {cmd_info['size']:,} bytes")
                print(f"   Modified: {cmd_info['modified']}")
                
                if cmd_info["usage"]:
                    print(f"   Usage: {cmd_info['usage']}")
                
                if cmd_info["examples"]:
                    print("   Examples:")
                    for example in cmd_info["examples"]:
                        print(f"     • {example}")
        
        print("\n" + "=" * 70)
        print("\n💡 Run a command with: ./slash_commands.py /command-name")
        print("   Get help with: ./slash_commands.py /command-name --help")
    
    def show_command_help(self, command: str):
        """Show detailed help for a specific command."""
        if command not in self.command_cache:
            print(f"❌ Command {command} not found.")
            self.suggest_similar(command)
            return
        
        info = self.command_cache[command]
        
        print(f"\n📚 Help for {command}")
        print("=" * 70)
        print(f"\n{info['description']}\n")
        
        print("📍 Location:")
        print(f"   Repository: {info['repo']}")
        print(f"   File: {info['path']}")
        
        print("\n⚙️  Features:")
        if info["hooks"]:
            print("   • Supports hooks for extending functionality")
        if info["multi_repo"]:
            print("   • Can operate across multiple repositories")
        if info["tags"]:
            print(f"   • Categories: {', '.join(info['tags'])}")
        
        # Try to get actual help from the command
        wrapper = Path.cwd() / "slash_commands.py"
        if wrapper.exists():
            print("\n📖 Command Help:")
            try:
                result = subprocess.run(
                    [sys.executable, str(wrapper), command, "--help"],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                if result.returncode == 0 and result.stdout:
                    for line in result.stdout.split('\n'):
                        print(f"   {line}")
                else:
                    print("   Run with --help for detailed options")
            except:
                print("   Run with --help for detailed options")
        
        print("\n🚀 Usage:")
        print(f"   ./slash_commands.py {command} [options]")
        
        if info["examples"]:
            print("\n📝 Examples:")
            for example in info["examples"]:
                print(f"   {example}")
        
        print("\n" + "=" * 70)
    
    def suggest_similar(self, query: str):
        """Suggest similar commands."""
        from difflib import get_close_matches
        
        all_commands = list(self.command_cache.keys())
        similar = get_close_matches(query, all_commands, n=3, cutoff=0.5)
        
        if similar:
            print("\n💡 Did you mean:")
            for cmd in similar:
                print(f"   • {cmd}")
    
    def list_by_category(self):
        """List all commands organized by category."""
        categories = {
            "git": [],
            "testing": [],
            "dependencies": [],
            "organization": [],
            "distribution": [],
            "other": []
        }
        
        for cmd_name, cmd_info in self.command_cache.items():
            if "git" in cmd_info["tags"]:
                categories["git"].append((cmd_name, cmd_info))
            elif "testing" in cmd_info["tags"]:
                categories["testing"].append((cmd_name, cmd_info))
            elif "dependencies" in cmd_info["tags"]:
                categories["dependencies"].append((cmd_name, cmd_info))
            elif "organization" in cmd_info["tags"]:
                categories["organization"].append((cmd_name, cmd_info))
            elif "distribution" in cmd_info["tags"]:
                categories["distribution"].append((cmd_name, cmd_info))
            else:
                categories["other"].append((cmd_name, cmd_info))
        
        print("\n📚 Commands by Category")
        print("=" * 70)
        
        category_names = {
            "git": "🔀 Git & Version Control",
            "testing": "🧪 Testing & Quality",
            "dependencies": "📦 Dependencies & Packages",
            "organization": "📁 Code Organization",
            "distribution": "🔄 Command Distribution",
            "other": "🔧 Other Tools"
        }
        
        for cat_key, cat_name in category_names.items():
            if categories[cat_key]:
                print(f"\n{cat_name}:")
                for cmd_name, cmd_info in sorted(categories[cat_key]):
                    print(f"  {cmd_name:<30} - {cmd_info['description'][:50]}")
        
        print("\n" + "=" * 70)
    
    def export_registry(self, output_file: Path):
        """Export command registry to JSON."""
        registry = {
            "version": "1.0",
            "generated": datetime.now().isoformat(),
            "repository": str(self.current_repo),
            "commands": self.command_cache
        }
        
        with open(output_file, 'w') as f:
            json.dump(registry, f, indent=2)
        
        print(f"✅ Command registry exported to {output_file}")


def main():
    """Main entry point for search-commands."""
    parser = argparse.ArgumentParser(
        description="Search and discover slash commands",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Search for git-related commands
  ./slash_commands.py /search-commands git
  
  # List all commands with hooks
  ./slash_commands.py /search-commands --hooks
  
  # Show detailed help for a command
  ./slash_commands.py /search-commands --help-for /git-trunk-flow
  
  # List commands by category
  ./slash_commands.py /search-commands --categories
  
  # Export command registry
  ./slash_commands.py /search-commands --export registry.json
        """
    )
    
    parser.add_argument(
        "query",
        nargs="?",
        default="",
        help="Search query (searches names, descriptions, tags)"
    )
    
    parser.add_argument(
        "--detailed", "-d",
        action="store_true",
        help="Show detailed information for each command"
    )
    
    parser.add_argument(
        "--hooks",
        action="store_true",
        help="Filter for commands with hook support"
    )
    
    parser.add_argument(
        "--multi-repo",
        action="store_true",
        help="Filter for multi-repository commands"
    )
    
    parser.add_argument(
        "--tags",
        nargs="+",
        help="Filter by tags (git, testing, dependencies, etc.)"
    )
    
    parser.add_argument(
        "--repo",
        help="Filter by repository name"
    )
    
    parser.add_argument(
        "--help-for",
        metavar="COMMAND",
        help="Show detailed help for a specific command"
    )
    
    parser.add_argument(
        "--categories", "-c",
        action="store_true",
        help="List all commands organized by category"
    )
    
    parser.add_argument(
        "--export",
        metavar="FILE",
        help="Export command registry to JSON file"
    )
    
    args = parser.parse_args()
    
    # Initialize searcher
    searcher = CommandSearcher()
    
    # Handle different modes
    if args.help_for:
        searcher.show_command_help(args.help_for)
    elif args.categories:
        searcher.list_by_category()
    elif args.export:
        searcher.export_registry(Path(args.export))
    else:
        # Build filters
        filters = {}
        if args.hooks:
            filters["hooks"] = True
        if args.multi_repo:
            filters["multi_repo"] = True
        if args.tags:
            filters["tags"] = args.tags
        if args.repo:
            filters["repo"] = args.repo
        
        # Search and display
        results = searcher.search(args.query, filters)
        searcher.display_results(results, detailed=args.detailed)
    
    return 0


if __name__ == "__main__":
    sys.exit(main())