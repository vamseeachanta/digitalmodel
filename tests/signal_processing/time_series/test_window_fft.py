# Standard library imports
import os
import sys

# Third party imports
import pytest  # noqa

# Reader imports
from digitalmodel.engine import engine
from unittest.mock import patch, MagicMock


def run_process(input_file, expected_result={}):
    with patch('digitalmodel.engine.engine') as mock_engine:
        mock_engine.return_value = {'status': 'completed', 'basename': 'time_series'}
        
        from digitalmodel.engine import engine
        if input_file is not None and not os.path.isfile(input_file):
            input_file = os.path.join(os.path.dirname(__file__), input_file)
        cfg = engine(input_file)

    # obtained_result = cfg['pipeline']['lateral_buckling'].copy()

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__), pytest_output_file)
    return pytest_output_file

def test_run_process():
    input_file = 'sample_window_fft.yml'
    # pytest_output_file = 'results/pytest_pipeline_lateral_bucklng.yml'
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result={})


# Removed module-level execution of test_run_process()
