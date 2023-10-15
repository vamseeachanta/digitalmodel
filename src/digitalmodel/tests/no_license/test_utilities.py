import os
import pytest

from digitalmodel.common.utilities import get_module_path


def run_get_module_path(module=None):
    module_path = get_module_path(module)
    # Check if valid directory
    try:
        os.path.isdir(module_path)
    except ImportError as exc:
        pytest.fail(exc, pytrace=True)


def test_no_module(module=None):
    run_get_module_path(module=module)


def test_with_module(module="digitalmodel"):
    run_get_module_path(module=module)


# def test_with_non_existent_module(module="fakemodule"):
#     run_get_module_path(module=module)
