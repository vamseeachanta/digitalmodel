import os
import sys

from digitalmodel.engine import engine

# Run by file
ymlfile = 'test_data/umbilical_end.yml'
if not os.path.isfile(ymlfile):
    ymlfile = os.path.join(os.path.dirname(__file__), ymlfile)
    print(os.path.isfile(ymlfile))
engine(ymlfile)