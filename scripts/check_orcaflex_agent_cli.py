#!/usr/bin/env python3
"""
Test script for OrcaFlex Agent CLI
Run from repository root: python scripts/test_orcaflex_agent_cli.py
"""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.agents.orcaflex.cli import cli

if __name__ == '__main__':
    cli(obj={})
