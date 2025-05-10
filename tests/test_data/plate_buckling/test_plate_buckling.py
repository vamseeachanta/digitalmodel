# Standard library imports
import os
import sys

# Third party imports
import deepdiff
from assetutilities.common.yml_utilities import ymlInput

# Reader imports
from digitalmodel.engine import engine


def run_plate_buckling(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)
    
    obtained_result = cfg[cfg['basename']]
    expected_result = expected_result[cfg['basename']].copy()

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result,
                                 ignore_order=True,
                                 significant_digits=4)

    return cfg

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(
            pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__),
                                          pytest_output_file)
    return pytest_output_file


def test_plate_buckling():
    input_file = 'src/digitalmodel/tests/test_data/plate_buckling/plate_buckling.yml'
    pytest_output_file = 'src/digitalmodel/tests/test_data/plate_buckling/results/plate_buckling_py_test.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    cfg = run_plate_buckling(input_file, expected_result)


test_plate_buckling()