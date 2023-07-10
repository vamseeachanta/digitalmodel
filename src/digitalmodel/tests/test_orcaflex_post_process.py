import os

from digitalmodel.engine import engine

# Run by file
ymlfile = 'test_data/orcaflex_post_process/orcaflex_post_process.yml'
if not os.path.isfile(ymlfile):
    ymlfile = os.path.join(os.path.dirname(__file__), ymlfile)
    print(os.path.isfile(ymlfile))
engine(ymlfile)
