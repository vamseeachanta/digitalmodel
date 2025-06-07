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

    obtained_result = cfg['outputs']['summary'].copy()

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result,
                                 ignore_order=True,
                                 significant_digits=4)

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(
            pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__),
                                          pytest_output_file)
    return pytest_output_file


def test_cathodic_protection():
    input_file = 'cathodic_protection_ABS_gn_ships_2018.yml'
    input_file = get_valid_pytest_output_file(input_file)

    pytest_output_file = None
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)
    expected_result = {'Number_of_anodes_mean_current':113.9,'total_net_mass':3301.68,'Number_of_anodes_initial_current':106.6,
                       'Number_of_anodes_final_current':121.2
                       }

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_cathodic_protection(input_file, expected_result)


test_cathodic_protection()