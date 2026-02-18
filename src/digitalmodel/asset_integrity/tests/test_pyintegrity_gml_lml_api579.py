import pytest
import deepdiff
import os
import sys

from ..engine import engine
from ..common.yml_utilities import ymlInput


def run_api579(input_file, expected_result={}):
    sys.argv = [sys.argv[0]]
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    try:
        cfg = engine(input_file)
    except FileNotFoundError as exc:
        pytest.skip(f"requires external asset-integrity library files: {exc}")

    obtained_result = cfg['Result']
    expected_result = expected_result['Result'].copy()

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


def test_api579_design_apib318():
    input_file = 'test_data/API579/16in_gas_b318.yml'
    pytest_output_file = 'test_data/API579/app_API579_16in_gas_b318_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()
    run_api579(input_file, expected_result)


def test_api579_design_apib314_1():
    input_file = 'test_data/API579/12in_oil_cml28_b314.yml'
    pytest_output_file = 'test_data/API579/app_API579_12in_oil_cml28_b314_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()
    try:
        run_api579(input_file, expected_result)
    except Exception as e:
        assert str(e) == "LML Level 1 and 2 are failed."


def test_api579_design_apib314_2():
    input_file = 'test_data/API579/12in_oil_cml31_b314.yml'
    pytest_output_file = 'test_data/API579/app_API579_12in_oil_cml31_b314_pytest.yml'
    pytest_output_file = get_valid_pytest_output_file(pytest_output_file)
    expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()
    try:
        run_api579(input_file, expected_result)
    except Exception as e:
        assert str(e) == "LML Level 1 and 2 are failed."
