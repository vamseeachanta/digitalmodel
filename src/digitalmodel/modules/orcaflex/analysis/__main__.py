"""
Enable module execution via python -m digitalmodel.modules.orcaflex.analysis
"""

from .cli import main
import sys

if __name__ == '__main__':
    sys.exit(main())