import os
import sys

from digitalmodel.engine import engine

# Run by file

ymlfile = '../test_data/code_dnvrph103_rectangular_uth1.yml'


def test_dnvrph103_rectangular(input_file):
    if not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
        print(os.path.isfile(input_file))
    cfg = engine(input_file)


# ymlfile = 'test_data/code_dnvrph103_rectangular_uth2.yml'
# if not os.path.isfile(ymlfile):
#     ymlfile = os.path.join(os.path.dirname(__file__), ymlfile)
#     print(os.path.isfile(ymlfile))
# engine(ymlfile)

ymlfile = '../test_data/code_dnvrph103_rectangular_uth1.yml'
test_dnvrph103_rectangular(ymlfile)

# Run by file
ymlfile = 'test_data/230kgbuoy.yml'
if not os.path.isfile(ymlfile):
    ymlfile = os.path.join(os.path.dirname(__file__), ymlfile)
    print(os.path.isfile(ymlfile))
engine(ymlfile)
