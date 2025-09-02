#!/usr/bin/env python
"""Run universal OrcaFlex analysis."""

import sys
import os
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parents[5]
sys.path.insert(0, str(project_root))

def run_analysis():
    """Run the OrcaFlex analysis for universal."""
    from digitalmodel.modules.orcaflex.universal import UniversalOrcaFlexRunner
    
    current_dir = Path(__file__).parent.parent
    config_file = Path(__file__).parent / 'universal_config.yml'
    
    runner = UniversalOrcaFlexRunner()
    result = runner.run(
        input_directory=str(current_dir),
        config=str(config_file) if config_file.exists() else None,
        pattern='*.dat',
        parallel=4
    )
    
    return 0 if result else 1

if __name__ == '__main__':
    sys.exit(run_analysis())
