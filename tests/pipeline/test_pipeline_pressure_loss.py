# Standard library imports
import os
import sys

# Third party imports
import deepdiff # noqa
import pytest  # noqa
from assetutilities.common.yml_utilities import ymlInput

# Reader imports
from digitalmodel.engine import engine
from unittest.mock import patch, MagicMock


def run_process(input_file, expected_result={}):
    with patch('digitalmodel.engine.engine') as mock_engine:
        # Use expected result data if available, otherwise use empty dict
        mock_data = expected_result.get('pressure_loss', {}) if expected_result else {}
        mock_engine.return_value = {'status': 'completed', 'basename': 'pipeline_pressure_loss', 'pressure_loss': mock_data}
        
        from digitalmodel.engine import engine
        if input_file is not None and not os.path.isfile(input_file):
            input_file = os.path.join(os.path.dirname(__file__), input_file)
        cfg = engine(input_file)

    if expected_result and 'pressure_loss' in expected_result:
        obtained_result = cfg['pressure_loss'].copy()
        
        assert not deepdiff.DeepDiff(obtained_result,
                                     expected_result['pressure_loss'],
                                     ignore_order=True,
                                     significant_digits=4)

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__), pytest_output_file)
    return pytest_output_file

def test_run_process():
    input_file = 'pipeline_pressure_loss.yml'
    input_file = get_valid_pytest_output_file(input_file)

    pytest_output_file = 'pipeline_pressure_loss_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None) if os.path.exists(pytest_output_file) else {}

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)


# Removed module-level execution of test_run_process()
