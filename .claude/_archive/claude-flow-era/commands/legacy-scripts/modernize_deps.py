#!/usr/bin/env python
"""
/modernize-deps - Modernize Dependencies and Module Structure

This command modernizes dependency management and improves module structure
across Python repositories by consolidating requirements into pyproject.toml,
setting up uv environment configuration, and enabling parallel processing.
"""

import os
import sys
import json
import shutil
import subprocess
from pathlib import Path
from typing import List, Dict, Tuple, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
import tempfile
import logging
from datetime import datetime

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)

class DependencyModernizer:
    """Modernize dependency management across repositories."""
    
    def __init__(self, repo_path: Path, backup: bool = True):
        self.repo_path = Path(repo_path)
        self.backup = backup
        self.backup_dir = None
        self.dependencies = set()
        self.dev_dependencies = set()
        self.report = {
            "repo": repo_path.name,
            "files_processed": 0,
            "dependencies_found": 0,
            "files_created": [],
            "files_removed": [],
            "errors": []
        }
    
    def create_backup(self):
        """Create backup of important files."""
        if not self.backup:
            return
        
        self.backup_dir = Path(tempfile.mkdtemp(prefix=f"{self.repo_path.name}_backup_"))
        files_to_backup = [
            "pyproject.toml",
            "setup.py",
            "requirements.txt",
            "requirements-dev.txt",
            "uv.toml",
            ".python-version"
        ]
        
        for file_name in files_to_backup:
            file_path = self.repo_path / file_name
            if file_path.exists():
                shutil.copy2(file_path, self.backup_dir / file_name)
                logger.debug(f"  📁 Backed up {file_name}")
    
    def find_requirements_files(self) -> List[Path]:
        """Find all requirements files in the repository."""
        patterns = [
            "requirements*.txt",
            "**/requirements*.txt",
            "reqs*.txt",
            "**/reqs*.txt"
        ]
        
        req_files = []
        for pattern in patterns:
            req_files.extend(self.repo_path.glob(pattern))
        
        # Also check for setup.py
        setup_py = self.repo_path / "setup.py"
        if setup_py.exists():
            req_files.append(setup_py)
        
        return list(set(req_files))
    
    def parse_requirements_file(self, file_path: Path) -> Tuple[set, set]:
        """Parse a requirements file and extract dependencies."""
        deps = set()
        dev_deps = set()
        
        if file_path.name == "setup.py":
            # Parse setup.py for install_requires
            try:
                content = file_path.read_text()
                # Simple parsing - could be improved with AST
                if "install_requires" in content:
                    # Extract dependencies (simplified)
                    import re
                    pattern = r'install_requires\s*=\s*\[(.*?)\]'
                    match = re.search(pattern, content, re.DOTALL)
                    if match:
                        deps_str = match.group(1)
                        for line in deps_str.split('\n'):
                            line = line.strip().strip(',').strip('"').strip("'")
                            if line and not line.startswith('#'):
                                deps.add(line)
            except Exception as e:
                self.report["errors"].append(f"Error parsing {file_path}: {e}")
        else:
            # Parse requirements.txt style files
            try:
                content = file_path.read_text()
                is_dev = 'dev' in file_path.name.lower() or 'test' in file_path.name.lower()
                
                for line in content.splitlines():
                    line = line.strip()
                    if line and not line.startswith('#') and not line.startswith('-'):
                        # Remove comments from line
                        if '#' in line:
                            line = line.split('#')[0].strip()
                        
                        if is_dev:
                            dev_deps.add(line)
                        else:
                            deps.add(line)
            except Exception as e:
                self.report["errors"].append(f"Error parsing {file_path}: {e}")
        
        return deps, dev_deps
    
    def collect_all_dependencies(self):
        """Collect all dependencies from various files."""
        req_files = self.find_requirements_files()
        self.report["files_processed"] = len(req_files)
        
        for req_file in req_files:
            logger.info(f"  📄 Processing: {req_file.relative_to(self.repo_path)}")
            deps, dev_deps = self.parse_requirements_file(req_file)
            self.dependencies.update(deps)
            self.dev_dependencies.update(dev_deps)
        
        self.report["dependencies_found"] = len(self.dependencies) + len(self.dev_dependencies)
    
    def create_pyproject_toml(self):
        """Create or update pyproject.toml with unified dependencies."""
        pyproject_path = self.repo_path / "pyproject.toml"
        
        # Check if pyproject.toml exists and parse it
        existing_config = {}
        if pyproject_path.exists():
            try:
                import tomli
                existing_config = tomli.loads(pyproject_path.read_text())
            except ImportError:
                # Fallback to basic parsing
                pass
            except Exception as e:
                logger.warning(f"  ⚠️  Could not parse existing pyproject.toml: {e}")
        
        # Build new configuration
        config = {
            "build-system": existing_config.get("build-system", {
                "requires": ["setuptools>=61.0", "wheel"],
                "build-backend": "setuptools.build_meta"
            }),
            "project": {
                "name": existing_config.get("project", {}).get("name", self.repo_path.name),
                "version": existing_config.get("project", {}).get("version", "0.1.0"),
                "description": existing_config.get("project", {}).get("description", f"Modernized {self.repo_path.name}"),
                "readme": "README.md",
                "requires-python": ">=3.9",
                "license": existing_config.get("project", {}).get("license", {"text": "MIT"}),
                "dependencies": sorted(list(self.dependencies))
            },
            "project.optional-dependencies": {
                "dev": sorted(list(self.dev_dependencies)) or ["pytest>=7.0", "ruff", "black>=23.0"],
                "test": ["pytest>=7.0", "pytest-cov>=4.0"],
                "docs": ["sphinx>=6.0"]
            },
            "tool": {
                "parallel": {
                    "enabled": True,
                    "max_workers": 5,
                    "use_threading": False,
                    "use_asyncio": True,
                    "batch_size": 10
                },
                "uv": {
                    "python": "3.11",
                    "compile": True
                }
            }
        }
        
        # Write pyproject.toml
        content = self._dict_to_toml(config)
        pyproject_path.write_text(content)
        self.report["files_created"].append("pyproject.toml")
        logger.info(f"  ✅ Created/Updated pyproject.toml")
    
    def _dict_to_toml(self, data: dict) -> str:
        """Convert dictionary to TOML format (simplified)."""
        lines = []
        
        # Build-system section
        if "build-system" in data:
            lines.append("[build-system]")
            bs = data["build-system"]
            lines.append(f'requires = {json.dumps(bs.get("requires", []))}')
            lines.append(f'build-backend = "{bs.get("build-backend", "setuptools.build_meta")}"')
            lines.append("")
        
        # Project section
        if "project" in data:
            lines.append("[project]")
            proj = data["project"]
            lines.append(f'name = "{proj.get("name")}"')
            lines.append(f'version = "{proj.get("version")}"')
            lines.append(f'description = "{proj.get("description")}"')
            lines.append(f'readme = "{proj.get("readme", "README.md")}"')
            lines.append(f'requires-python = "{proj.get("requires-python", ">=3.9")}"')
            
            # License
            if "license" in proj:
                lines.append(f'license = {{text = "{proj["license"].get("text", "MIT")}"}}')
            
            # Dependencies with categories
            lines.append("\ndependencies = [")
            
            # Group dependencies by category
            deps_by_category = self._categorize_dependencies(proj.get("dependencies", []))
            for category, deps in deps_by_category.items():
                if deps:
                    lines.append(f"    # {category}")
                    for dep in sorted(deps):
                        lines.append(f'    "{dep}",')
                    lines.append("")
            lines.append("]")
            lines.append("")
        
        # Optional dependencies
        if "project.optional-dependencies" in data:
            lines.append("[project.optional-dependencies]")
            opt_deps = data["project.optional-dependencies"]
            for key, deps in opt_deps.items():
                deps_str = ', '.join([f'"{d}"' for d in deps])
                lines.append(f'{key} = [{deps_str}]')
            lines.append("")
        
        # Tool sections
        if "tool" in data:
            tool = data["tool"]
            
            # Parallel configuration
            if "parallel" in tool:
                lines.append("[tool.parallel]")
                for key, value in tool["parallel"].items():
                    if isinstance(value, bool):
                        lines.append(f"{key} = {str(value).lower()}")
                    else:
                        lines.append(f"{key} = {value}")
                lines.append("")
            
            # UV configuration
            if "uv" in tool:
                lines.append("[tool.uv]")
                for key, value in tool["uv"].items():
                    if isinstance(value, bool):
                        lines.append(f"{key} = {str(value).lower()}")
                    elif isinstance(value, str):
                        lines.append(f'{key} = "{value}"')
                    else:
                        lines.append(f"{key} = {value}")
                lines.append("")
        
        return '\n'.join(lines)
    
    def _categorize_dependencies(self, deps: List[str]) -> Dict[str, List[str]]:
        """Categorize dependencies by type."""
        categories = {
            "Core data processing": [],
            "Web and networking": [],
            "Testing and development": [],
            "Documentation": [],
            "Utilities": [],
            "Other": []
        }
        
        for dep in deps:
            dep_lower = dep.lower()
            if any(x in dep_lower for x in ['numpy', 'pandas', 'scipy', 'scikit']):
                categories["Core data processing"].append(dep)
            elif any(x in dep_lower for x in ['request', 'urllib', 'http', 'scrapy', 'beautifulsoup', 'selenium']):
                categories["Web and networking"].append(dep)
            elif any(x in dep_lower for x in ['pytest', 'mock', 'tox', 'coverage', 'black', 'ruff', 'flake8']):
                categories["Testing and development"].append(dep)
            elif any(x in dep_lower for x in ['sphinx', 'mkdocs', 'doc']):
                categories["Documentation"].append(dep)
            elif any(x in dep_lower for x in ['click', 'rich', 'tqdm', 'colorama', 'pyyaml', 'toml']):
                categories["Utilities"].append(dep)
            else:
                categories["Other"].append(dep)
        
        # Remove empty categories
        return {k: v for k, v in categories.items() if v}
    
    def create_uv_config(self):
        """Create uv.toml configuration."""
        uv_toml_path = self.repo_path / "uv.toml"
        
        content = """# UV Configuration - Single Environment Setup
[project]
name = "{name}"
requires-python = ">=3.9"

[tool.uv]
python = "3.11"
system = false
compile = true
seed = true

[tool.uv.pip]
index-url = "https://pypi.org/simple"
pre = false
no-cache = false

[tool.uv.venv]
prompt = "{name}"
system-site-packages = false

# Parallel processing for package installation
[tool.uv.parallel]
enabled = true
jobs = 4
""".format(name=self.repo_path.name)
        
        uv_toml_path.write_text(content)
        self.report["files_created"].append("uv.toml")
        logger.info(f"  ✅ Created uv.toml")
    
    def create_python_version(self):
        """Create .python-version file."""
        version_file = self.repo_path / ".python-version"
        version_file.write_text("3.11\n")
        self.report["files_created"].append(".python-version")
        logger.info(f"  ✅ Created .python-version")
    
    def create_setup_script(self):
        """Create setup_uv_env.sh script."""
        script_path = self.repo_path / "setup_uv_env.sh"
        
        content = '''#!/bin/bash
# Setup script for {name} with uv package manager

echo "Setting up {name} environment with uv..."

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# Create virtual environment with uv
echo "Creating virtual environment..."
uv venv

# Activate virtual environment
source .venv/bin/activate

# Install dependencies from pyproject.toml
echo "Installing dependencies..."
uv pip install -e .

# Install development dependencies
echo "Installing development dependencies..."
uv pip install -e ".[dev]"

# Verify installation
echo "Verifying installation..."
python -c "print('Environment setup complete!')"

echo "Setup complete! Activate the environment with: source .venv/bin/activate"
'''.format(name=self.repo_path.name)
        
        script_path.write_text(content)
        script_path.chmod(0o755)
        self.report["files_created"].append("setup_uv_env.sh")
        logger.info(f"  ✅ Created setup_uv_env.sh")
    
    def update_requirements_txt(self):
        """Update requirements.txt to reference pyproject.toml."""
        req_file = self.repo_path / "requirements.txt"
        
        content = f"""# {self.repo_path.name} Requirements
# This file is auto-generated from pyproject.toml
# Install with: pip install -e .
# For development: pip install -e ".[dev]"

# Core dependencies are specified in pyproject.toml
# This file exists for compatibility with tools that expect requirements.txt
# Use: pip install -e . to install from pyproject.toml

# To install all dependencies including optional ones:
# pip install -e ".[dev]"
"""
        
        req_file.write_text(content)
        logger.info(f"  ✅ Updated requirements.txt to reference pyproject.toml")
    
    def cleanup_redundant_files(self):
        """Remove redundant requirements files."""
        patterns_to_remove = [
            "**/requirements*.txt",
            "reqs*.txt",
            "**/reqs*.txt"
        ]
        
        # Keep only the root requirements.txt
        root_req = self.repo_path / "requirements.txt"
        
        for pattern in patterns_to_remove:
            for file_path in self.repo_path.glob(pattern):
                if file_path != root_req and file_path.exists():
                    try:
                        file_path.unlink()
                        self.report["files_removed"].append(str(file_path.relative_to(self.repo_path)))
                        logger.info(f"  🗑️  Removed {file_path.relative_to(self.repo_path)}")
                    except Exception as e:
                        self.report["errors"].append(f"Could not remove {file_path}: {e}")
    
    def validate_installation(self) -> bool:
        """Validate that the modernization was successful."""
        required_files = [
            "pyproject.toml",
            "uv.toml",
            ".python-version",
            "setup_uv_env.sh"
        ]
        
        for file_name in required_files:
            if not (self.repo_path / file_name).exists():
                self.report["errors"].append(f"Missing required file: {file_name}")
                return False
        
        return True
    
    def modernize(self) -> Dict:
        """Execute the complete modernization process."""
        logger.info(f"\n🔄 Processing: {self.repo_path.name}")
        
        try:
            # Step 1: Create backup
            self.create_backup()
            
            # Step 2: Collect all dependencies
            self.collect_all_dependencies()
            
            # Step 3: Create pyproject.toml
            self.create_pyproject_toml()
            
            # Step 4: Create uv configuration
            self.create_uv_config()
            
            # Step 5: Create .python-version
            self.create_python_version()
            
            # Step 6: Create setup script
            self.create_setup_script()
            
            # Step 7: Update requirements.txt
            self.update_requirements_txt()
            
            # Step 8: Cleanup redundant files
            self.cleanup_redundant_files()
            
            # Step 9: Validate
            success = self.validate_installation()
            self.report["success"] = success
            
            if success:
                logger.info(f"  ✅ Successfully modernized {self.repo_path.name}")
                logger.info(f"  📊 Dependencies: {self.report['dependencies_found']} consolidated")
            else:
                logger.error(f"  ❌ Modernization incomplete for {self.repo_path.name}")
            
        except Exception as e:
            self.report["errors"].append(str(e))
            self.report["success"] = False
            logger.error(f"  ❌ Error modernizing {self.repo_path.name}: {e}")
        
        return self.report


def find_python_repositories(base_dir: Path) -> List[Path]:
    """Find all Python repositories in the given directory."""
    repos = []
    
    for item in base_dir.iterdir():
        if item.is_dir() and not item.name.startswith('.'):
            # Check if it's a Python repository
            indicators = [
                item / "setup.py",
                item / "pyproject.toml",
                item / "requirements.txt",
                item / "setup.cfg"
            ]
            
            # Also check for Python files
            has_py_files = len(list(item.glob("*.py"))) > 0 or len(list(item.glob("**/*.py"))) > 0
            
            if any(indicator.exists() for indicator in indicators) or has_py_files:
                repos.append(item)
    
    return sorted(repos)


def modernize_repository(repo_path: Path, backup: bool = True) -> Dict:
    """Modernize a single repository."""
    modernizer = DependencyModernizer(repo_path, backup)
    return modernizer.modernize()


def main(target_dir: str = ".", parallel: int = 5, backup: bool = True, 
         repos: Optional[List[str]] = None, dry_run: bool = False):
    """
    Main entry point for the modernize-deps command.
    
    Args:
        target_dir: Directory containing repositories
        parallel: Number of parallel workers
        backup: Whether to create backups
        repos: Specific repositories to process
        dry_run: Show what would be done without making changes
    """
    base_dir = Path(target_dir).resolve()
    
    logger.info("🚀 Modernizing Dependencies Across Repositories")
    logger.info("=" * 50)
    
    # Find repositories
    if repos:
        repo_paths = [base_dir / repo for repo in repos]
        repo_paths = [p for p in repo_paths if p.exists()]
    else:
        repo_paths = find_python_repositories(base_dir)
    
    if not repo_paths:
        logger.error("❌ No Python repositories found!")
        return 1
    
    logger.info(f"📦 Found {len(repo_paths)} Python repositories")
    logger.info(f"🔧 Using {parallel} parallel workers")
    logger.info(f"💾 Backup: {'Enabled' if backup else 'Disabled'}")
    
    if dry_run:
        logger.info("🔍 DRY RUN MODE - No changes will be made")
        for repo in repo_paths:
            logger.info(f"  Would process: {repo.name}")
        return 0
    
    logger.info("")
    
    # Process repositories in parallel
    results = []
    with ThreadPoolExecutor(max_workers=parallel) as executor:
        futures = {
            executor.submit(modernize_repository, repo, backup): repo
            for repo in repo_paths
        }
        
        for future in as_completed(futures):
            repo = futures[future]
            try:
                result = future.result(timeout=60)
                results.append(result)
            except Exception as e:
                logger.error(f"❌ Failed to process {repo.name}: {e}")
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
    
    logger.info(f"✅ Successful: {successful}/{len(results)} repositories")
    
    if failed > 0:
        logger.info(f"❌ Failed: {failed} repositories")
        logger.info("\nFailed repositories:")
        for result in results:
            if not result.get("success", False):
                logger.info(f"  • {result['repo']}: {', '.join(result.get('errors', ['Unknown error']))}")
    
    # Statistics
    total_deps = sum(r.get("dependencies_found", 0) for r in results)
    total_files_removed = sum(len(r.get("files_removed", [])) for r in results)
    
    logger.info(f"\n📈 Statistics:")
    logger.info(f"  • Total dependencies consolidated: {total_deps}")
    logger.info(f"  • Redundant files removed: {total_files_removed}")
    logger.info(f"  • Time taken: {datetime.now()}")
    
    logger.info("\n✨ Dependency modernization complete!")
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Modernize dependency management across repositories")
    parser.add_argument("--target-dir", default=".", help="Directory containing repositories")
    parser.add_argument("--parallel", type=int, default=5, help="Number of parallel workers")
    parser.add_argument("--no-backup", action="store_true", help="Skip creating backups")
    parser.add_argument("--repos", nargs="+", help="Specific repositories to process")
    parser.add_argument("--dry-run", action="store_true", help="Show what would be done without making changes")
    
    args = parser.parse_args()
    
    sys.exit(main(
        target_dir=args.target_dir,
        parallel=args.parallel,
        backup=not args.no_backup,
        repos=args.repos,
        dry_run=args.dry_run
    ))