#!/usr/bin/env python
"""
ABOUTME: Script to run quality gates validation.
Simple wrapper for easy execution without remembering CLI commands.
"""

import sys
from pathlib import Path

# Add src to path for development
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.automation.quality_gates_cli import cli

if __name__ == "__main__":
    cli()
