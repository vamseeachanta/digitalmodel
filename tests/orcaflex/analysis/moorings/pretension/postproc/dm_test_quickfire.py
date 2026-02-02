# Standard library imports
import os
import sys

from digitalmodel.engine import engine

# Reader imports


def run_process(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)



def test_process():
    # input_file = 'fsts_rev_a03.yml'
    # input_file = 'opp_fsts_mva.yml'
    # input_file = 'FST2L_FST1L_HWL_345deg.yml'
    # input_file = 'opp_fsts_simultaneous.yml'
    input_file = 'dm_fsts_lngc_pretension.yml'

    # pytest_output_file = 'results/orcaflex_post_process_summary_pytest.yml'
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result={})

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__), pytest_output_file)
    return pytest_output_file

if __name__ == '__main__':
    test_process()