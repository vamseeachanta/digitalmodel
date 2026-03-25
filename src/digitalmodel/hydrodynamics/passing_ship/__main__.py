"""
Module entry point for running the passing ship forces calculator.

This allows the module to be executed with:
    python -m digitalmodel.hydrodynamics.passing_ship
"""

import sys
from .cli import main

if __name__ == '__main__':
    sys.exit(main())