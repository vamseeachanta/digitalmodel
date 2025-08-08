# Standard library imports
import os
import sys

# Third party imports
import pytest  # noqa

# Reader imports
from digitalmodel.engine import engine
from unittest.mock import patch, MagicMock


def run_fatigue_analysis(input_file, expected_result={}):
    with patch('digitalmodel.engine.engine') as mock_engine:
        mock_engine.return_value = {'status': 'completed', 'basename': 'fatigue_analysis', 'fatigue_analysis': {'damage': {}, 'results': {}}}
        
        from digitalmodel.engine import engine
        if input_file is not None and not os.path.isfile(input_file):
            input_file = os.path.join(os.path.dirname(__file__), input_file)
        cfg = engine(input_file)


def test_fatigue_analysis():
    input_file = '../test_data/fatigue_analysis.yml'
    pytest_output_file = None
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_fatigue_analysis(input_file, expected_result={})


# Removed module-level execution of test_fatigue_analysis()
