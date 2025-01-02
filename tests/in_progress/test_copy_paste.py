# Not working due to lack of appropriate input files
import os
import sys

from digitalmodel.engine import engine


def run_copy_and_paste(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)
    assert (True)


def test_copy_and_paste():
    input_file = '../test_data/copy_and_paste.yml'
    pytest_output_file = None
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_copy_and_paste(input_file, expected_result={})


test_copy_and_paste()
