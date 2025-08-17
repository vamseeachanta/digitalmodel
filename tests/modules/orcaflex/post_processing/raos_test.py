# Standard library imports
import os
import sys
from typing import Any, Dict

# Third party imports
import colorama
import pandas as pd
from assetutilities.common.yml_utilities import ymlInput
from colorama import Fore, Style

# Reader imports
from digitalmodel.engine import engine

colorama.init(autoreset=True)


def run_process(input_file: str, expected_result: Dict[str, Any] = None) -> None:
    if expected_result is None:
        expected_result = {}

    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)


def check_csv_files_match(file1: str, file2: str) -> bool:

    df_file1 = pd.read_csv(file1)
    df_file2 = pd.read_csv(file2)
    
    file_match_result = df_file1.equals(df_file2)
    
    return file_match_result

def test_process() -> None:
    input_file = 'raos.yml'

    # Same behavior as input file without master settings.
    try:
        pytest_output_file = 'results/raos_pytest.yml'
        pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
        expected_result = ymlInput(pytest_output_file, updateYml=None)
    except FileNotFoundError:
        expected_result = {}

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result)

def get_valid_pytest_output_file(pytest_output_file: str) -> str:
    if pytest_output_file is not None and not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(
            os.path.dirname(__file__), pytest_output_file
        )
    return pytest_output_file

if __name__ == '__main__':
    test_process()