# Standard library imports
import os
import sys
from typing import Any, Dict

# Reader imports
from assetutilities.common.yml_utilities import ymlInput
from assetutilities.modules.test_utilities.test_utilities import TestUtilities
from digitalmodel.engine import engine

tu = TestUtilities()

def run_process(input_file: str, expected_result: Dict[str, Any] = {}) -> None:
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    return cfg

def test_process() -> None:
    input_file = 'ofx_mooring_analysis.yml'

    # pytest_output_file = 'results/opp_summary1_pytest.yml'
    # pytest_output_file = tu.get_valid_pytest_output_file(os.path.dirname(__file__), pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result={})

test_process()