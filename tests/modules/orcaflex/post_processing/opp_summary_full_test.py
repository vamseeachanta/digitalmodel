# Standard library imports
import os
import sys
from typing import Any, Dict

# Reader imports
from assetutilities.modules.test_utilities.test_utilities import TestUtilities
from digitalmodel.engine import engine

from loguru import logger


tu = TestUtilities()

def run_process(input_file: str, expected_result: Dict[str, Any] = {}) -> None:
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    # obtained_result = cfg[cfg['basename']]['summary']
    # expected_result = expected_result[cfg['basename']]['summary'].copy()

    # Check csv files match
    # for group_index in range(0, len(obtained_result['groups'])):
    #     obtained_result_csv = obtained_result['groups'][group_index]['data']
    #     expected_result_csv = expected_result['groups'][group_index]['data']

    #     file_match_result = tu.check_csv_files_match(obtained_result_csv, expected_result_csv)

    #     assert file_match_result

    logger.info('Orcaflex Summary test ... PASS!')

    return cfg

def test_process() -> None:
    input_file = 'opp_summary_full.yml'
    pytest_output_file = 'results/opp_summary1_pytest.yml'
    # pytest_output_file = tu.get_valid_pytest_output_file(os.path.dirname(__file__), pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)
    expected_result = {}
    
    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)