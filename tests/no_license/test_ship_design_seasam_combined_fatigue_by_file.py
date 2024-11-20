import pytest
import deepdiff
import os
import sys

from assetutilities.common.yml_utilities import ymlInput

from digitalmodel.engine import engine


def run_ship_design(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)


def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__), pytest_output_file)
    return pytest_output_file


def test_ship_design():
    input_file = "../test_data/ship_design/ship_design_seasam_combined_fatigue_by_file.yml"
    pytest_output_file = None
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_ship_design(input_file, expected_result={})


test_ship_design()
