#!/usr/bin/env python
"""Run OrcaFlex analysis for this test folder."""

import sys
from pathlib import Path
import subprocess

def main():
    """Run the analysis using the universal OrcaFlex runner."""
    current_dir = Path(__file__).parent.parent
    
    # Run universal runner with this directory
    cmd = [
        sys.executable, '-m', 'digitalmodel.orcaflex.universal',
        '--input-directory', str(current_dir),
        '--pattern', '*.yml',
        '--parallel', '4'
    ]
    
    print(f"Running: {' '.join(cmd)}")
    result = subprocess.run(cmd)
    return result.returncode

if __name__ == '__main__':
    sys.exit(main())
