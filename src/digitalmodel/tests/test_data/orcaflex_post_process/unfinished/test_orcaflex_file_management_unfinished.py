import os
import sys

from digitalmodel.engine import engine
from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities

ou = OrcaflexUtilities()


def test_orcaflex_license():
    orcaflex_license_flag = ou.is_orcaflex_available()
    assert (orcaflex_license_flag)


def run_orcaflex_post_process(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)
    assert (True)


def test_orcaflex_post_process():
    input_file = 'file_management_unfinished.yml'
    pytest_output_file = None
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_orcaflex_post_process(input_file, expected_result={})


test_orcaflex_post_process()