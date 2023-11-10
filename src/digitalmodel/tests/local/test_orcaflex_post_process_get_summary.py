import os
import sys
import deepdiff

from digitalmodel.engine import engine
from digitalmodel.custom.orcaflex_utilities import OrcaflexUtilities
from assetutilities.common.yml_utilities import ymlInput

ou = OrcaflexUtilities()


def test_orcaflex_license():
    orcaflex_license_flag = ou.is_orcaflex_available()
    assert (orcaflex_license_flag)


def run_orcaflex_post_process(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    obtained_result = cfg[cfg['basename']]
    expected_result = expected_result[cfg['basename']].copy()

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


def test_orcaflex_post_process():
    input_file = '../test_data/orcaflex_post_process/orcaflex_post_process_summary.yml'
    pytest_output_file = '../test_data/orcaflex_post_process/orcaflex_post_process_summary_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_orcaflex_post_process(input_file, expected_result)


test_orcaflex_post_process()
