import os
import sys
import yaml
import types

from digitalmodel.common.yml_utilities import WorkingWithYAML
from digitalmodel.common.yml_utilities import ymlInput

file_name1 = 'github/digitalmodel/src/digitalmodel/tests/test_data/orcaflex/SZ_trim_VA.yml'
file_name2 = 'github/digitalmodel/src/digitalmodel/tests/test_data/orcaflex/SZ_trim_QA.yml'

wwyaml = WorkingWithYAML()

# wwyaml.analyze_yaml_keys(file_name1)
# wwyaml.compare_yaml_root_keys(file_name1, file_name2)

wwyaml.compare_yaml_files_deepdiff(file_name1, file_name2)
