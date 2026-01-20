#!/usr/bin/env python3
"""
UV Environment Manager - Manages UV environments across repositories

This command ensures all tests, specs, and tasks use the existing UV environment
in each repository instead of creating new virtual environments. It also handles
environment enhancement when new dependencies are needed for specs.

Key Features:
- Detects existing UV environments in repositories
- Prevents creation of duplicate virtual environments
- Enhances UV environment with spec-specific dependencies
- Provides consistent Python environment across all operations
"""

import os
import sys
import subprocess
import json
import yaml
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import shutil

class UVEnvironmentManager:
    """Manages UV environments for repositories."""
    
    def __init__(self, repo_path: Path = None):
        self.repo_path = repo_path or Path.cwd()
        self.uv_path = self._find_uv_executable()
        self.env_info = self._detect_uv_environment()
        
    def _find_uv_executable(self) -> Optional[Path]:
        """Find UV executable in system."""
        # Check common locations
        uv_locations = [
            shutil.which("uv"),
            Path.home() / ".cargo" / "bin" / "uv",
            Path("/usr/local/bin/uv"),
            Path("/opt/homebrew/bin/uv"),
        ]
        
        for location in uv_locations:
            if location and Path(location).exists():
                return Path(location)
        
        # Try to find via subprocess
        try:
            result = subprocess.run(
                ["which", "uv"],
                capture_output=True,
                text=True
            )
            if result.returncode == 0:
                return Path(result.stdout.strip())
        except:
            pass
        
        return None
    
    def _detect_uv_environment(self) -> Dict:
        """Detect UV environment in repository."""
        env_info = {
            'has_uv': False,
            'uv_lock': None,
            'venv_path': None,
            'python_version': None,
            'dependencies': [],
            'pyproject_toml': None,
            'requirements_txt': None
        }
        
        # Check for uv.lock
        uv_lock = self.repo_path / "uv.lock"
        if uv_lock.exists():
            env_info['has_uv'] = True
            env_info['uv_lock'] = str(uv_lock)
        
        # Check for .venv directory
        venv_path = self.repo_path / ".venv"
        if venv_path.exists():
            env_info['venv_path'] = str(venv_path)
            
            # Get Python version from venv
            python_exe = venv_path / "bin" / "python"
            if not python_exe.exists():
                python_exe = venv_path / "Scripts" / "python.exe"
            
            if python_exe.exists():
                try:
                    result = subprocess.run(
                        [str(python_exe), "--version"],
                        capture_output=True,
                        text=True
                    )
                    env_info['python_version'] = result.stdout.strip()
                except:
                    pass
        
        # Check for pyproject.toml
        pyproject = self.repo_path / "pyproject.toml"
        if pyproject.exists():
            env_info['pyproject_toml'] = str(pyproject)
            env_info['has_uv'] = True  # UV can work with pyproject.toml
            
            # Parse dependencies from pyproject.toml
            try:
                import tomli
                with open(pyproject, 'rb') as f:
                    data = tomli.load(f)
                    deps = data.get('project', {}).get('dependencies', [])
                    env_info['dependencies'].extend(deps)
            except ImportError:
                # Fallback to basic parsing
                content = pyproject.read_text()
                if 'dependencies = [' in content:
                    start = content.find('dependencies = [')
                    end = content.find(']', start)
                    if start != -1 and end != -1:
                        deps_str = content[start:end]
                        # Basic extraction (not perfect but works for simple cases)
                        import re
                        deps = re.findall(r'"([^"]+)"', deps_str)
                        env_info['dependencies'].extend(deps)
        
        # Check for requirements.txt
        requirements = self.repo_path / "requirements.txt"
        if requirements.exists():
            env_info['requirements_txt'] = str(requirements)
            with open(requirements, 'r') as f:
                deps = [line.strip() for line in f if line.strip() and not line.startswith('#')]
                env_info['dependencies'].extend(deps)
        
        return env_info
    
    def ensure_uv_environment(self) -> Tuple[bool, str]:
        """Ensure UV environment exists and is properly configured."""
        if not self.uv_path:
            return False, "UV not found. Please install UV first: curl -LsSf https://astral.sh/uv/install.sh | sh"
        
        # If UV environment doesn't exist, create it
        if not self.env_info['has_uv']:
            return self.create_uv_environment()
        
        # If venv doesn't exist but we have config files, sync
        if not self.env_info['venv_path'] and (self.env_info['pyproject_toml'] or self.env_info['uv_lock']):
            return self.sync_uv_environment()
        
        return True, "UV environment already exists and is configured"
    
    def create_uv_environment(self) -> Tuple[bool, str]:
        """Create a new UV environment."""
        try:
            # Initialize UV project if pyproject.toml doesn't exist
            if not (self.repo_path / "pyproject.toml").exists():
                subprocess.run(
                    [str(self.uv_path), "init", "--name", self.repo_path.name],
                    cwd=self.repo_path,
                    check=True,
                    capture_output=True
                )
            
            # Create virtual environment
            subprocess.run(
                [str(self.uv_path), "venv"],
                cwd=self.repo_path,
                check=True,
                capture_output=True
            )
            
            # Sync dependencies
            subprocess.run(
                [str(self.uv_path), "sync"],
                cwd=self.repo_path,
                check=True,
                capture_output=True
            )
            
            return True, "UV environment created successfully"
            
        except subprocess.CalledProcessError as e:
            return False, f"Failed to create UV environment: {e}"
    
    def sync_uv_environment(self) -> Tuple[bool, str]:
        """Sync UV environment with current dependencies."""
        try:
            subprocess.run(
                [str(self.uv_path), "sync"],
                cwd=self.repo_path,
                check=True,
                capture_output=True
            )
            return True, "UV environment synced successfully"
        except subprocess.CalledProcessError as e:
            return False, f"Failed to sync UV environment: {e}"
    
    def add_dependency(self, package: str, dev: bool = False) -> Tuple[bool, str]:
        """Add a dependency to the UV environment."""
        if not self.uv_path:
            return False, "UV not found"
        
        try:
            cmd = [str(self.uv_path), "add"]
            if dev:
                cmd.append("--dev")
            cmd.append(package)
            
            subprocess.run(
                cmd,
                cwd=self.repo_path,
                check=True,
                capture_output=True
            )
            
            # Sync after adding
            self.sync_uv_environment()
            
            return True, f"Added {package} to UV environment"
            
        except subprocess.CalledProcessError as e:
            return False, f"Failed to add {package}: {e}"
    
    def enhance_for_spec(self, spec_requirements: List[str]) -> Tuple[bool, str]:
        """Enhance UV environment with spec-specific requirements."""
        if not spec_requirements:
            return True, "No additional requirements needed"
        
        added = []
        failed = []
        
        for req in spec_requirements:
            success, msg = self.add_dependency(req)
            if success:
                added.append(req)
            else:
                failed.append(req)
        
        if failed:
            return False, f"Failed to add: {', '.join(failed)}"
        
        return True, f"Enhanced UV environment with: {', '.join(added)}"
    
    def get_python_executable(self) -> Optional[Path]:
        """Get the Python executable from UV environment."""
        if self.env_info['venv_path']:
            venv_path = Path(self.env_info['venv_path'])
            
            # Try different locations
            python_paths = [
                venv_path / "bin" / "python",
                venv_path / "Scripts" / "python.exe",
                venv_path / "bin" / "python3",
            ]
            
            for python_path in python_paths:
                if python_path.exists():
                    return python_path
        
        return None
    
    def run_in_uv_env(self, command: List[str]) -> subprocess.CompletedProcess:
        """Run a command in the UV environment."""
        python_exe = self.get_python_executable()
        
        if python_exe:
            # Modify command to use UV environment Python
            if command[0] in ['python', 'python3']:
                command[0] = str(python_exe)
            elif command[0] == 'pip':
                command = [str(python_exe), '-m', 'pip'] + command[1:]
            elif command[0] == 'pytest':
                command = [str(python_exe), '-m', 'pytest'] + command[1:]
        
        # Run with UV if available
        if self.uv_path and command[0] != str(python_exe):
            command = [str(self.uv_path), "run"] + command
        
        return subprocess.run(
            command,
            cwd=self.repo_path,
            capture_output=True,
            text=True
        )
    
    def display_info(self):
        """Display UV environment information."""
        print("\n🔷 UV Environment Information")
        print("=" * 50)
        
        if self.env_info['has_uv']:
            print("✅ UV environment detected")
            
            if self.env_info['venv_path']:
                print(f"📁 Virtual environment: {self.env_info['venv_path']}")
            
            if self.env_info['python_version']:
                print(f"🐍 Python version: {self.env_info['python_version']}")
            
            if self.env_info['uv_lock']:
                print(f"🔒 UV lock file: {self.env_info['uv_lock']}")
            
            if self.env_info['pyproject_toml']:
                print(f"📦 Project config: {self.env_info['pyproject_toml']}")
            
            if self.env_info['dependencies']:
                print(f"\n📚 Dependencies ({len(self.env_info['dependencies'])} packages):")
                for dep in self.env_info['dependencies'][:5]:
                    print(f"   • {dep}")
                if len(self.env_info['dependencies']) > 5:
                    print(f"   ... and {len(self.env_info['dependencies']) - 5} more")
        else:
            print("❌ No UV environment found")
            print("\n💡 To create one:")
            print("   uv init")
            print("   uv venv")
            print("   uv sync")
        
        print("\n" + "=" * 50)


class UVGlobalConfig:
    """Global configuration for UV usage across all commands."""
    
    CONFIG_FILE = Path.home() / ".agent-os" / "uv_config.yaml"
    
    @classmethod
    def save_config(cls, config: Dict):
        """Save UV configuration."""
        cls.CONFIG_FILE.parent.mkdir(parents=True, exist_ok=True)
        with open(cls.CONFIG_FILE, 'w') as f:
            yaml.dump(config, f)
    
    @classmethod
    def load_config(cls) -> Dict:
        """Load UV configuration."""
        if cls.CONFIG_FILE.exists():
            with open(cls.CONFIG_FILE, 'r') as f:
                return yaml.safe_load(f) or {}
        
        # Default configuration
        return {
            'always_use_uv': True,
            'auto_enhance': True,
            'create_if_missing': False,
            'python_version': '3.11',
            'default_packages': [
                'pytest',
                'pytest-cov',
                'black',
                'ruff',
                'mypy'
            ]
        }
    
    @classmethod
    def ensure_uv_usage(cls):
        """Ensure UV is used for all Python operations."""
        config = cls.load_config()
        
        if not config.get('always_use_uv', True):
            return
        
        # Set environment variables to prefer UV
        os.environ['VIRTUAL_ENV_DISABLE_PROMPT'] = '1'
        os.environ['UV_SYSTEM_PYTHON'] = '0'  # Don't use system Python
        
        # Add UV bin to PATH if not already there
        uv_bin = Path.home() / ".cargo" / "bin"
        if uv_bin.exists() and str(uv_bin) not in os.environ.get('PATH', ''):
            os.environ['PATH'] = f"{uv_bin}:{os.environ.get('PATH', '')}"


def integrate_with_test_command():
    """Generate code to integrate UV with test commands."""
    return '''
# Add this to test_automation_enhanced.py __init__ method:

def __init__(self):
    self.adapter = RepositoryAdapter()
    
    # Initialize UV environment manager
    from uv_environment_manager import UVEnvironmentManager
    self.uv_manager = UVEnvironmentManager(self.adapter.repo_root)
    
    # Ensure UV environment is used
    success, msg = self.uv_manager.ensure_uv_environment()
    if success:
        print(f"✅ Using UV environment: {msg}")
    else:
        print(f"⚠️  UV environment issue: {msg}")
    
    # Display UV info
    self.uv_manager.display_info()
    
    # Use UV Python for all test operations
    self.python_exe = self.uv_manager.get_python_executable() or sys.executable
    
    # Rest of initialization...
    self.discovery = EnhancedTestDiscovery(self.adapter)
    self.runner = IntelligentTestRunner(self.adapter, python_exe=self.python_exe)
'''


def integrate_with_spec_command():
    """Generate code to integrate UV with spec commands."""
    return '''
# Add this to spec execution:

def execute_spec_tasks(self, spec_path: Path):
    """Execute spec tasks with UV environment."""
    
    # Initialize UV manager for the repository
    from uv_environment_manager import UVEnvironmentManager
    uv_manager = UVEnvironmentManager(spec_path.parent.parent)
    
    # Check spec for required dependencies
    spec_deps = self._extract_spec_dependencies(spec_path)
    
    if spec_deps:
        print(f"📦 Spec requires: {', '.join(spec_deps)}")
        
        # Enhance UV environment with spec dependencies
        success, msg = uv_manager.enhance_for_spec(spec_deps)
        if success:
            print(f"✅ UV environment enhanced: {msg}")
        else:
            print(f"⚠️  Failed to enhance: {msg}")
    
    # Run all tasks in UV environment
    for task in tasks:
        result = uv_manager.run_in_uv_env(task_command)
        # Process result...
'''


def main():
    """Main entry point for UV environment manager."""
    import argparse
    
    parser = argparse.ArgumentParser(
        prog='uv-env',
        description='UV Environment Manager for repositories'
    )
    
    parser.add_argument('command', nargs='?', default='info',
                       choices=['info', 'ensure', 'sync', 'add', 'enhance', 'run'])
    parser.add_argument('--package', help='Package to add')
    parser.add_argument('--dev', action='store_true', help='Add as dev dependency')
    parser.add_argument('--spec', help='Spec file to enhance for')
    parser.add_argument('--repo', help='Repository path', default='.')
    parser.add_argument('args', nargs='*', help='Command arguments for run')
    
    args = parser.parse_args()
    
    # Initialize manager
    repo_path = Path(args.repo).resolve()
    manager = UVEnvironmentManager(repo_path)
    
    if args.command == 'info':
        manager.display_info()
    
    elif args.command == 'ensure':
        success, msg = manager.ensure_uv_environment()
        print(f"{'✅' if success else '❌'} {msg}")
    
    elif args.command == 'sync':
        success, msg = manager.sync_uv_environment()
        print(f"{'✅' if success else '❌'} {msg}")
    
    elif args.command == 'add':
        if not args.package:
            print("❌ Package name required: --package NAME")
            sys.exit(1)
        success, msg = manager.add_dependency(args.package, dev=args.dev)
        print(f"{'✅' if success else '❌'} {msg}")
    
    elif args.command == 'enhance':
        if args.spec:
            # Extract dependencies from spec file
            spec_path = Path(args.spec)
            if spec_path.exists():
                # Simple extraction - could be enhanced
                deps = []
                content = spec_path.read_text()
                if 'fastapi' in content.lower():
                    deps.extend(['fastapi', 'uvicorn'])
                if 'django' in content.lower():
                    deps.append('django')
                if 'flask' in content.lower():
                    deps.append('flask')
                if 'pandas' in content.lower():
                    deps.append('pandas')
                if 'numpy' in content.lower():
                    deps.append('numpy')
                
                if deps:
                    success, msg = manager.enhance_for_spec(deps)
                    print(f"{'✅' if success else '❌'} {msg}")
                else:
                    print("ℹ️  No specific dependencies found in spec")
            else:
                print(f"❌ Spec file not found: {args.spec}")
        else:
            print("❌ Spec file required: --spec PATH")
    
    elif args.command == 'run':
        if not args.args:
            print("❌ Command required")
            sys.exit(1)
        
        result = manager.run_in_uv_env(args.args)
        print(result.stdout)
        if result.stderr:
            print(result.stderr, file=sys.stderr)
        sys.exit(result.returncode)


if __name__ == '__main__':
    # Ensure UV is preferred globally
    UVGlobalConfig.ensure_uv_usage()
    main()