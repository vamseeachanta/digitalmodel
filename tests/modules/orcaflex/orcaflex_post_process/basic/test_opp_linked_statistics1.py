# Standard library imports
import os
import sys
# Reader imports
import colorama
from assetutilities.common.yml_utilities import ymlInput
from assetutilities.modules.test_utilities.test_utilities import TestUtilities
from colorama import Fore, Style
from digitalmodel.engine import engine

colorama.init(autoreset=True)

from typing import Any, Dict

tu = TestUtilities()

# Standard library imports


def run_process(input_file: str, expected_result: Dict[str, Any] = {}) -> None:
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    obtained_result = cfg[cfg['basename']]['linked_statistics']
    expected_result = expected_result[cfg['basename']]['linked_statistics'].copy()

    # Check csv files match
    for group_index in range(0, len(obtained_result['groups'])):
        obtained_result_csv = obtained_result['groups'][group_index]['data']
        expected_result_csv = expected_result['groups'][group_index]['data']

        file_match_result = tu.check_csv_files_match(obtained_result_csv, expected_result_csv)

        assert file_match_result

    print(Fore.GREEN + 'Orcaflex Linked Statistic test ... PASS' + Style.RESET_ALL)

    return cfg

def test_process() -> None:
    input_file = 'opp_linked_statistics1.yml'
    pytest_output_file = 'results/opp_linked_statistics1_pytest.yml'
    pytest_output_file = tu.get_valid_pytest_output_file(os.path.dirname(__file__), pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)

test_process()