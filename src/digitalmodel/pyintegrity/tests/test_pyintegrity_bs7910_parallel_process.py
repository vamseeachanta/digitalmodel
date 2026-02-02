import pytest
import deepdiff
import os
import sys

from pyintegrity.engine import engine
from pyintegrity.common.yml_utilities import ymlInput


def run_bs7910(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    obtained_result = cfg['Result']
    expected_result = expected_result['Result'].copy()

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result,
                                 ignore_order=True,
                                 significant_digits=4)


def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(
            pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__),
                                          pytest_output_file)
    return pytest_output_file


def test_bs7910_axial():
    input_file = 'test_data/fracture_mechanics/fracture_mechanics_py_axial_parallel_process.yml'
    pytest_output_file = 'test_data/API579/app_API579_16in_gas_b318_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()
    run_bs7910(input_file, expected_result)


test_bs7910_axial()
