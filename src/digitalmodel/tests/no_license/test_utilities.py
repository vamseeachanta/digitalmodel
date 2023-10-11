import os
import pytest

from digitalmodel.common.utilities import get_module_path


def test_module_path(module=None):
    module_path = get_module_path(module)
    # Check if valid directory
    try:
        os.path.isdir(module_path)
    except ImportError as exc:
        pytest.fail(exc, pytrace=True)


test_module_path()
test_module_path(module="digitalmodel")
# test_module_path(module="mydigitalmodel")
