import deepdiff
from assetutilities.common.pytest_utilities import pytest_approx_float_dict, pytest_deepdiff_nested_dict


def test_pytest_nested_dict():

    obtained_result = {'foo': {'bar': 0.30000001}}
    expected_result = {'foo': {'bar': 0.30000002}}
    rel = None
    abs = None
    nan_ok = False
    assert obtained_result == pytest_approx_float_dict(expected_result, rel,
                                                       abs, nan_ok)


def test_pytest_nested_2_layer_dict():

    obtained_result = {'foo': {'bar': 0.30000001, 'abc': 0.25006}}
    expected_result = {'foo': {'bar': 0.30000002, 'abc': 0.25007}}

    # Working only if used directly in a test function but not outside
    # pytest_deepdiff_nested_dict(obtained_result,
    #                             expected_result,
    #                             ignore_order=True,
    #                             significant_digits=2)

    assert not deepdiff.DeepDiff(obtained_result,
                                 expected_result,
                                 ignore_order=True,
                                 significant_digits=4)


def test_pytest_nested_dict_abs_criteria_1():

    obtained_result = {'foo': {'bar': 0.30000001}}
    expected_result = {'foo': {'bar': 0.40000002}}
    rel = None
    abs = None
    nan_ok = False
    if not obtained_result == pytest_approx_float_dict(expected_result, rel,
                                                       abs, nan_ok):
        pass


def test_pytest_nested_dict_abs_criteria():

    obtained_result = {'foo': {'bar': 0.30000001}}
    expected_result = {'foo': {'bar': 0.40000002}}
    rel = None
    abs = 0.11
    nan_ok = False
    assert obtained_result == pytest_approx_float_dict(expected_result, rel,
                                                       abs, nan_ok)


def test_pytest_nested_dict_rel_criteria():
    # https://stackoverflow.com/questions/69078845/what-is-the-rationale-behind-pytest-approxs-rel-parameter
    obtained_result = {'foo': {'bar': 0.30000001}}
    expected_result = {'foo': {'bar': 0.40000002}}
    rel = 0.26
    abs = None
    nan_ok = False
    assert obtained_result == pytest_approx_float_dict(expected_result, rel,
                                                       abs, nan_ok)


test_pytest_nested_2_layer_dict()