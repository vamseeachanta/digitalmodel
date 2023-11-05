import os

from assetutilities.common.utilities import add_cwd_to_filename
from assetutilities.common.yml_utilities import WorkingWithYAML


def compare_yaml_file_content_deepdiff():

    file_name1 = '../test_data/orcaflex/SZ_trim_VA.yml'
    file_name2 = '../test_data/orcaflex/SZ_trim_QA.yml'

    file_name1 = add_cwd_to_filename(file_name1, cwd=os.path.dirname(__file__))
    file_name2 = add_cwd_to_filename(file_name2, cwd=os.path.dirname(__file__))

    cfg = {
        'file_name1': file_name1,
        'file_name2': file_name2,
        'map_list': {
            'file_name1': ['General'],
            'file_name2': ['General']
        }
    }

    wwyaml = WorkingWithYAML()

    # wwyaml.analyze_yaml_keys(file_name1)
    # wwyaml.compare_yaml_root_keys(file_name1, file_name2)
    # wwyaml.compare_yaml_files_deepdiff(cfg)
    wwyaml.compare_yaml_file_contents_deepdiff(cfg)


compare_yaml_file_content_deepdiff()