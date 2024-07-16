# Standard library imports
import os
import sys

# Third party imports
import deepdiff

# Reader imports
from digitalmodel.engine import engine


def run_cathodic_protection(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    obtained_result = cfg['cathodic_protection'].copy()

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result['cathodic_protection'],
                                 ignore_order=True,
                                 significant_digits=4)

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(
            pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__),
                                          pytest_output_file)
    return pytest_output_file


def test_cathodic_protection():
    input_file = 'cathodic_protection_DNV_RP_F103_2010.yml'
    input_file = get_valid_pytest_output_file(input_file)

    pytest_output_file = 'cathodic_protection_DNV_RP_F103_2010_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)
    # expected_result = expected_result['cathodic_protection']


    if len(sys.argv) > 1:
        sys.argv.pop()

    run_cathodic_protection(input_file, expected_result)


test_cathodic_protection()