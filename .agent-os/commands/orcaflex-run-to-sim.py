#!/usr/bin/env python
"""
Universal slash command for OrcaFlex run-to-sim functionality.
Can be used on any computer with the digitalmodel repository.

Usage:
    /orcaflex-run-to-sim --all                    # Run all models in current directory
    /orcaflex-run-to-sim --models file1.yml       # Run specific model
    /orcaflex-run-to-sim --mock --all             # Run in mock mode (no license needed)
    /orcaflex-run-to-sim --help                   # Show help
"""

import os
import sys
import subprocess
import argparse
from pathlib import Path
import json
import logging

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%H:%M:%S'
)
logger = logging.getLogger(__name__)


class OrcaFlexRunToSimCommand:
    """Universal command for running OrcaFlex models to generate .sim files."""
    
    def __init__(self):
        """Initialize the command."""
        self.repo_root = self._find_repo_root()
        self.module_path = self.repo_root / "src" / "digitalmodel" / "modules" / "orcaflex"
        self.cli_script = self.module_path / "run_to_sim_cli.py"
        
    def _find_repo_root(self) -> Path:
        """Find the digitalmodel repository root."""
        # Try multiple strategies to find the repo
        current = Path.cwd()
        
        # Strategy 1: Look for .git directory going up
        search_path = current
        while search_path != search_path.parent:
            if (search_path / ".git").exists() and (search_path / "src" / "digitalmodel").exists():
                return search_path
            search_path = search_path.parent
        
        # Strategy 2: Check common locations
        common_paths = [
            Path.home() / "github" / "digitalmodel",
            Path("D:/github/digitalmodel"),
            Path("/mnt/github/digitalmodel"),
            Path.cwd(),
        ]
        
        for path in common_paths:
            if path.exists() and (path / "src" / "digitalmodel").exists():
                return path
        
        # Strategy 3: Use environment variable if set
        if "DIGITALMODEL_ROOT" in os.environ:
            path = Path(os.environ["DIGITALMODEL_ROOT"])
            if path.exists():
                return path
        
        raise RuntimeError(
            "Could not find digitalmodel repository. "
            "Please run from within the repository or set DIGITALMODEL_ROOT environment variable."
        )
    
    def validate_environment(self) -> bool:
        """Validate that the required files exist."""
        if not self.cli_script.exists():
            logger.error(f"CLI script not found: {self.cli_script}")
            logger.info("Please ensure the digitalmodel repository is properly set up.")
            return False
        
        return True
    
    def run(self, args: list = None) -> int:
        """
        Run the OrcaFlex run-to-sim command.
        
        Args:
            args: Command line arguments (if None, uses sys.argv)
            
        Returns:
            Exit code (0 for success, non-zero for failure)
        """
        if not self.validate_environment():
            return 1
        
        # Build the command
        cmd = [sys.executable, str(self.cli_script)]
        if args:
            cmd.extend(args)
        else:
            cmd.extend(sys.argv[1:])
        
        logger.info(f"Repository root: {self.repo_root}")
        logger.info(f"Running command: {' '.join(cmd)}")
        
        # Execute the command
        try:
            result = subprocess.run(
                cmd,
                cwd=str(Path.cwd()),
                capture_output=False,
                text=True
            )
            return result.returncode
            
        except subprocess.CalledProcessError as e:
            logger.error(f"Command failed with error: {e}")
            return e.returncode
        except Exception as e:
            logger.error(f"Unexpected error: {e}")
            return 1


def create_wrapper_script():
    """Create a wrapper script that can be placed anywhere."""
    wrapper_content = '''#!/usr/bin/env python
"""
OrcaFlex Run-to-Sim Wrapper
This wrapper can be placed anywhere and will find the digitalmodel repository.
"""
import sys
import os
from pathlib import Path

# Try to find the digitalmodel repository
def find_digitalmodel():
    # Check environment variable
    if "DIGITALMODEL_ROOT" in os.environ:
        return Path(os.environ["DIGITALMODEL_ROOT"])
    
    # Check common locations
    locations = [
        Path.home() / "github" / "digitalmodel",
        Path("D:/github/digitalmodel"),
        Path("/mnt/github/digitalmodel"),
    ]
    
    for loc in locations:
        if loc.exists() and (loc / "src" / "digitalmodel").exists():
            return loc
    
    return None

repo = find_digitalmodel()
if repo:
    sys.path.insert(0, str(repo / ".agent-os" / "commands"))
    from orcaflex_run_to_sim import main
    sys.exit(main())
else:
    print("Error: Could not find digitalmodel repository")
    print("Set DIGITALMODEL_ROOT environment variable to the repository path")
    sys.exit(1)
'''
    
    wrapper_path = Path.home() / ".local" / "bin" / "orcaflex-run-to-sim"
    wrapper_path.parent.mkdir(parents=True, exist_ok=True)
    wrapper_path.write_text(wrapper_content)
    wrapper_path.chmod(0o755)
    
    logger.info(f"Created wrapper script at: {wrapper_path}")
    logger.info("Add ~/.local/bin to your PATH to use the command from anywhere")


def main():
    """Main entry point for the slash command."""
    # Check if user wants to install the wrapper
    if len(sys.argv) > 1 and sys.argv[1] == "--install-wrapper":
        create_wrapper_script()
        return 0
    
    # Run the command
    command = OrcaFlexRunToSimCommand()
    return command.run()


if __name__ == "__main__":
    sys.exit(main())