import os

from digitalmodel.common.utilities import is_file_valid, add_cwd_to_filename
from digitalmodel.common.yml_utilities import WorkingWithYAML

file_name1 = 'test_data/orcaflex/SZ_trim_VA.yml'
file_name2 = 'test_data/orcaflex/SZ_trim_QA.yml'

file_name1 = add_cwd_to_filename(file_name1, cwd=os.path.dirname(__file__))
file_name2 = add_cwd_to_filename(file_name2, cwd=os.path.dirname(__file__))

wwyaml = WorkingWithYAML()

# wwyaml.analyze_yaml_keys(file_name1)
# wwyaml.compare_yaml_root_keys(file_name1, file_name2)

wwyaml.compare_yaml_files_deepdiff(file_name1, file_name2)
