import pytest
import deepdiff
import os
import sys

from assetutilities.common.yml_utilities import ymlInput

from digitalmodel.engine import engine


def run_fatigue_analysis(input_file, expected_result={}):
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    total_damage_comparison = deepdiff.DeepDiff(expected_result['fatigue_analysis'], cfg['fatigue_analysis'], ignore_order=True)
    timetrace_comparison = deepdiff.DeepDiff(expected_result['fatigue_analysis']['timetraces'], cfg['fatigue_analysis']['timetraces'], ignore_order=True)

    assert total_damage_comparison == {}
    assert timetrace_comparison == {}

def get_valid_pytest_output_file(pytest_output_file):
    if pytest_output_file is not None and not os.path.isfile(pytest_output_file):
        pytest_output_file = os.path.join(os.path.dirname(__file__), pytest_output_file)
    return pytest_output_file


def test_fatigue_analysis():
    input_file = "../test_data/fatigue_analysis/fatigue_analysis_timetrace.yml"
    pytest_output_file = "../test_data/fatigue_analysis/app_fatigue_analysis_fatigue_analysis_timetrace_pytest.yml"
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_fatigue_analysis(input_file, expected_result)


test_fatigue_analysis()
