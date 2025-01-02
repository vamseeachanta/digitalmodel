# Standard library imports
import os
import sys

# Third party imports
import pytest  # noqa
import deepdiff

# Reader imports
from digitalmodel.engine import engine
from assetutilities.common.yml_utilities import ymlInput


def run_process(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    output_file = 'results/Data/118kgbuoy_deep.yml'
    output_file = get_valid_pytest_output_file(output_file)
    obtained_result = ymlInput(output_file, updateYml=None)

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result,
                                 ignore_order=True,
                                 significant_digits=4)

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__), pytest_output_file)
    return pytest_output_file

def test_run_process():
    input_file = '118kgbuoy.yml'
    pytest_output_file = 'results/Data/118kgbuoy_deep_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)


test_run_process()
