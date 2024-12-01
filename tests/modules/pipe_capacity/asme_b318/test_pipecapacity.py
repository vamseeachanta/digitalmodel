# Standard library imports
import os
import sys
from typing import Any, Dict

# Third party imports
import deepdiff
from assetutilities.common.yml_utilities import ymlInput
from assetutilities.modules.test_utilities.test_utilities import TestUtilities

# Reader imports
from digitalmodel.engine import engine

tu = TestUtilities()


def run_process(input_file: str, expected_result: Dict[str, Any] = {}) -> None:
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    obtained_result = cfg[cfg['basename']].copy()
    expected_result = expected_result[cfg['basename']].copy()

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result,
                                 ignore_order=True,
                                 significant_digits=4)


def test_process() -> None:
    input_file = 'pipe_capacity.yml'
    pytest_output_file = 'results/pipe_capacity_pytest.yml'

    pytest_output_file = tu.get_valid_pytest_output_file(os.path.dirname(__file__), pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)

test_process()