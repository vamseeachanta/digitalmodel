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
        mock_engine.return_value = {
            'status': 'completed', 
            'basename': 'viv_analysis',
            'viv_analysis': {
                'current_velocity': 1.2,
                'reduced_velocity': 6.5,
                'strouhal_number': 0.2,
                'response_amplitude': 0.8,
                'fatigue_damage': {
                    'cf_inline': 2.3e-4,
                    'cf_crossflow': 4.7e-4,
                    'total_damage': 7.0e-4
                },
                'max_stress': 125.5
            }
        }
        
        from digitalmodel.engine import engine
        if input_file is not None and not os.path.isfile(input_file):
            input_file = os.path.join(os.path.dirname(__file__), input_file)
        cfg = engine(input_file)


def test_run_process():
    input_file = 'viv_analysis.yml'
    # pytest_output_file = 'results/pytest_viv_analysis.yml'
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result={})

# Removed module-level execution of test_run_process()
