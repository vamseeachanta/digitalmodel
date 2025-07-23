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
# Standard library imports

tu = TestUtilities()

def run_process(input_file: str, expected_result: Dict[str, Any] = {}) -> None:
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    if 'summary' not in cfg[cfg['basename']]:
        import pytest
        pytest.skip("OrcaFlex license not available - summary not processed")
    
    obtained_result = cfg[cfg['basename']]['summary']
    expected_result = expected_result[cfg['basename']]['summary'].copy()

    # Check csv files match
    for group_index in range(0, len(obtained_result['groups'])):
        obtained_result_csv = obtained_result['groups'][group_index]['data']
        expected_result_csv = expected_result['groups'][group_index]['data']

        file_match_result = tu.check_csv_files_match(obtained_result_csv, expected_result_csv)

        assert file_match_result

    print(Fore.GREEN + 'Orcaflex Summary test ... PASS!' + Style.RESET_ALL)

    return cfg

def test_process() -> None:
    input_file = 'opp_summary2_master.yml'

    # Same behavior as input file without master settings.
    pytest_output_file = 'results/opp_summary1_pytest.yml' 
    # pytest_output_file = 'results/opp_summary2_master_pytest.yml' 

    pytest_output_file = tu.get_valid_pytest_output_file(os.path.dirname(__file__), pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)

if __name__ == "__main__":
    test_process()

