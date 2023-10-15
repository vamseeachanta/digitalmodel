import deepdiff
import pytest
from collections.abc import Mapping
from _pytest.python_api import ApproxMapping


def pytest_deepdiff_nested_dict(obtained_result,
                                expected_result,
                                ignore_order=True,
                                significant_digits=6):
    # https://github.com/pytest-dev/pytest/issues/3164
    # Working only if used directly in a test function but not outside
    assert not deepdiff.DeepDiff(obtained_result, expected_result, ignore_order,
                                 significant_digits)


def pytest_approx_float_dict(expected, rel=None, abs=None, nan_ok=False):
    # https://stackoverflow.com/questions/56046524/check-if-python-dictionaries-are-equal-allowing-small-difference-for-floats
    if isinstance(expected, Mapping):
        return ApproxNestedMapping(expected, rel, abs, nan_ok)
    return pytest.approx(expected, rel, abs, nan_ok)


class ApproxNestedMapping(ApproxMapping):

    def _yield_comparisons(self, actual):
        for k in self.expected.keys():
            if isinstance(actual[k], type(self.expected)):
                gen = ApproxNestedMapping(
                    self.expected[k],
                    rel=self.rel,
                    abs=self.abs,
                    nan_ok=self.nan_ok)._yield_comparisons(actual[k])
                for el in gen:
                    yield el
            else:
                yield actual[k], self.expected[k]

    def _check_type(self):
        for key, value in self.expected.items():
            if not isinstance(value, type(self.expected)):
                super()._check_type()
