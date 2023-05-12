import os
import sys

from digitalmodel.engine import engine

# Run by file
ymlfile = 'test_data/orcaflex_analysis.yml'
if not os.path.isfile(ymlfile):
    ymlfile = os.path.join(os.path.dirname(__file__), ymlfile)
    print(os.path.isfile(ymlfile))
    engine(ymlfile)

# Run by args
ymlfile = 'test_data/orcaflex_analysis.yml'
if len(sys.argv) <= 1:
    sys.argv.append(ymlfile)
    engine()
