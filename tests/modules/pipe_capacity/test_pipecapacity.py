import pytest
import os
import sys
import deepdiff

from digitalmodel.engine import engine

from assetutilities.common.yml_utilities import ymlInput


def run_pipecapacity(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    obtained_result = cfg['Result'].copy()
    expected_result = expected_result['Result'].copy()

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result,
                                 ignore_order=True,
                                 significant_digits=4)

    assert (True)


def test_pipecapacity():
    input_file = 'pipe_capacity.yml'
    pytest_output_file = 'pipe_capacity_pytest.yml'
    if not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__),
                                          pytest_output_file)
        expected_result = ymlInput(pytest_output_file, updateYml=None)
    else:
        expected_result = {}

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_pipecapacity(input_file, expected_result)


test_pipecapacity()
