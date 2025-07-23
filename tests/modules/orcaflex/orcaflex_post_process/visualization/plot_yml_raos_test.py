# Standard library imports
import os
import sys
from typing import Any, Dict

# Third party imports
from assetutilities.modules.test_utilities.test_utilities import TestUtilities
from colorama import Fore, Style

# Reader imports
from digitalmodel.engine import engine

import colorama
colorama.init(autoreset=True)

tu = TestUtilities()

def run_process(input_file: str, expected_result: Dict[str, Any] = {}) -> None:

    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    obtained_result = cfg[cfg['basename']]['time_series']
    expected_result = expected_result[cfg['basename']]['time_series'].copy()

    # Check csv files match
    for file_index in range(0, len(obtained_result)):
        for group_index in range(0, len(obtained_result[file_index]['time_series']['groups'])):
            obtained_result_csv = obtained_result[file_index]['time_series']['groups'][group_index]['data']
            expected_result_csv = expected_result[file_index]['time_series']['groups'][group_index]['data']

            file_match_result = tu.check_csv_files_match(obtained_result_csv, expected_result_csv)

            assert file_match_result


    print(Fore.GREEN + 'Orcaflex time series test ... PASS' + Style.RESET_ALL)


def test_process() -> None:
    input_file = 'plot_yml_raos.yml'
    # pytest_output_file = 'results/opp_time_series1_pytest.yml'
    # pytest_output_file = tu.get_valid_pytest_output_file(os.path.dirname(__file__), pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)
    expected_result = {}

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)


if __name__ == '__main__':
    test_process()