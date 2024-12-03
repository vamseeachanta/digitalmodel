# Standard library imports
import os
import sys

# Reader imports
from digitalmodel.engine import engine


def run_process(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)


def test_process():
    input_file = 'fsts_post_dev.yml'

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result={})

test_process()
