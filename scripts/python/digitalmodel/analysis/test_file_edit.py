# TODO fix the code
import os
import sys

from assetutilities.engine import engine


def run_file(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    # obtained_result = cfg[cfg['basename']]['properties']
    # expected_result = expected_result[cfg['basename']]['properties'].copy()

    # assert not deepdiff.DeepDiff(obtained_result,
    #                              expected_result,
    #                              ignore_order=True,
    #                              significant_digits=4)


def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(
            pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__),
                                          pytest_output_file)
    return pytest_output_file


def test_file_run():
    input_file = 'preproc_dat_moor.yml'
    # input_file = 'preproc_dat_wnch.yml'

    # pytest_output_file = '../test_data/6d_buoy/buoy_6d_circular_px_0_pytest.yml'
    # pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = {}
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_file(input_file, expected_result)


test_file_run()