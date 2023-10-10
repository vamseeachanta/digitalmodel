import os
import sys

from digitalmodel.common.utilities import get_module_path

module_path = get_module_path(module="digitalmodel")
print(module_path)

module_path = get_module_path()
print(module_path)
