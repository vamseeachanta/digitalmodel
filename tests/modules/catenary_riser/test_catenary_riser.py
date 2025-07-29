# Not working. Properties and dictionaries are mismatched.
import os
import sys

from digitalmodel.engine import engine


def run_catenary_riser(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)
    assert (True)


def test_catenary_riser():
    input_file = '../test_data/catenary_riser.yml'
    pytest_output_file = None
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_catenary_riser(input_file, expected_result={})


test_catenary_riser()
