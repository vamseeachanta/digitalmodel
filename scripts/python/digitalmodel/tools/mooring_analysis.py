#!/usr/bin/env python
"""
Convenience script for running OrcaFlex mooring comparative analysis.

This script provides a simple entry point to the analysis module.
"""

import sys
from pathlib import Path

# Add src to path for module imports
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.modules.orcaflex.analysis import run_comparative_analysis

if __name__ == '__main__':
    sys.exit(run_comparative_analysis())