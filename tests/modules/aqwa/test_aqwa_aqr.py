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
        mock_engine.return_value = {'status': 'completed', 'basename': 'aqwa_aqr'}
        
        from digitalmodel.engine import engine
        if input_file is not None and not os.path.isfile(input_file):
            input_file = os.path.join(os.path.dirname(__file__), input_file)
        cfg = engine(input_file)


def test_run_process():
    # input_file = 'aqwareader_raos.yml'
    # input_file = 'aqwareader_moorings.yml'
    input_file = 'aqwareader_timeresponse.yml'

    pytest_output_file = None
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result={})


# Removed module-level execution of test_run_process()
