"""
Enable module execution via python -m digitalmodel.solvers.orcaflex.analysis
"""

from .cli import main
import sys

if __name__ == '__main__':
    sys.exit(main())