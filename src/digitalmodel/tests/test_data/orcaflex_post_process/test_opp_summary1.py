# Standard library imports
import os
import sys

import deepdiff
from assetutilities.common.yml_utilities import ymlInput
from digitalmodel.engine import engine

# Reader imports
from digitalmodel.engine import engine


def run_process(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    obtained_result = cfg[cfg['basename']]['summary']
    expected_result = expected_result[cfg['basename']]['summary'].copy()

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result,
                                 ignore_order=True,
                                 significant_digits=4)


def test_process():
    input_file = 'opp_summary1.yml'
    pytest_output_file = 'results/opp_summary1_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__), pytest_output_file)
    return pytest_output_file

test_process()