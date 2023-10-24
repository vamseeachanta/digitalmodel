import os
import yaml
import pkgutil
import types
from deepdiff import DeepDiff
from collections import OrderedDict
import importlib.util
from pathlib import Path

from pathlib import Path
from collections.abc import Mapping
from digitalmodel.common.saveData import saveDataYaml
from digitalmodel.common.utilities import is_file_valid_func, get_common_name_from_2_filenames
from digitalmodel.common.data import ReadData

read_data = ReadData()

library_name = 'digitalmodel'


def ymlInput(defaultYml, updateYml=None):

    if not is_file_valid_func(defaultYml):
        raise Exception("Not valid file. Please check the file path.")

    with open(defaultYml, 'r') as ymlfile:
        try:
            cfg = yaml.safe_load(ymlfile)
        except yaml.composer.ComposerError:
            cfg = yml_read_stream(defaultYml)

    if updateYml != None:
        #  Update values file
        try:
            with open(updateYml, 'r') as ymlfile:
                cfgUpdateValues = yaml.safe_load(ymlfile)
    #  Convert to logs
    # print(cfgUpdateValues)
            cfg = update_deep(cfg, cfgUpdateValues)
        except:
            print(
                "Update Input file could not be loaded successfully. Running program default values"
            )

    return cfg


def update_deep(d, u):
    for k, v in u.items():
        # this condition handles the problem
        if not isinstance(d, Mapping):
            d = u
        elif isinstance(v, Mapping):
            r = update_deep(d.get(k, {}), v)
            d[k] = r
        else:
            d[k] = u[k]

    return d


def yml_read_stream(yaml_file_name):
    stream_dict = {}
    try:
        with open(yaml_file_name, 'r') as ymlfile:
            docs = yaml.safe_load_all(ymlfile)
            if type(docs) is types.GeneratorType:
                for doc in docs:
                    if type(doc) is dict:
                        stream_dict = update_deep(stream_dict, doc)
    except:
        raise Exception("Stopping Program")

    return stream_dict


class WorkingWithYAML():

    # Analyze Yaml file
    def analyze_yaml_keys(self, file_name):
        file_name_content = ymlInput(file_name)
        print(file_name_content.keys())

    # Compare 2 yaml files
    def compare_yaml_root_keys(self, file_name1, file_name2):
        file_name1_content = ymlInput(file_name1)
        file_name2_content = ymlInput(file_name2)
        file_name1_keys = file_name1_content.keys()
        file_name2_keys = file_name2_content.keys()
        if file_name1_keys == file_name2_keys:
            print("Yaml files have the same root keys")
        else:
            print(f"The root keys for {file_name1}: {file_name1_keys}")
            print(f"The root keys for {file_name2}: {file_name2_keys}")

    # Compare 2 yaml files using DeepDiff
    def compare_yaml_files_deepdiff(self, cfg):
        file_name1 = cfg['file_name1']
        file_name2 = cfg['file_name2']
        file_name1_content = ymlInput(file_name1)
        file_name2_content = ymlInput(file_name2)
        file_diff = DeepDiff(file_name1_content,
                             file_name2_content,
                             ignore_order=True)
        if file_diff == {}:    # if there is no difference
            print("Yaml files are the same")
        else:
            # get file root directory
            file_directory = os.path.dirname(file_name1)
            uniquebasename = get_common_name_from_2_filenames(
                file_name1, file_name2)
            self.save_diff_files(file_diff, file_directory, uniquebasename)

    def compare_yaml_file_contents_deepdiff(self, cfg):
        file_name1 = cfg['file_name1']
        file_name2 = cfg['file_name2']

        file_name1_content = read_data.key_chain(ymlInput(file_name1),
                                                 *cfg['map_list']['file_name1'])
        file_name2_content = read_data.key_chain(ymlInput(file_name2),
                                                 *cfg['map_list']['file_name2'])

        file_diff = DeepDiff(file_name1_content,
                             file_name2_content,
                             ignore_order=True)

        self.save_diff_files(file_diff, cfg)

    def save_diff_files(self, file_diff, cfg, deepdiff_save=False):
        file_name1 = cfg['file_name1']
        file_name2 = cfg['file_name2']
        if file_diff == {}:    # if there is no difference
            print("Yaml files are the same")
        else:
            # get file root directory
            file_directory = os.path.dirname(file_name1)
            uniquebasename = get_common_name_from_2_filenames(
                file_name1, file_name2)

            # save the entire diff file. A very messy and overwhelming file
            if deepdiff_save:
                saveDataYaml(
                    file_diff,
                    f"{file_directory}/wwyaml_{uniquebasename}_deepdiff")

            file_name = f"{file_directory}/wwyaml_{uniquebasename}_updated_values.yml"
            with open(file_name, 'w') as f:
                for key, value in file_diff.items():
                    if key == "values_changed":
                        for k, v in value.items():
                            f.write(f"{k}: {v['new_value']}\n")

            saveDataYaml(dict(file_diff),
                         f"{file_directory}/wwyaml_{uniquebasename}_items")

            saveDataYaml(
                dict(file_diff)['values_changed'],
                f"{file_directory}/wwyaml_{uniquebasename}_values_changed")

            print(
                "Yaml files are different. See wwyaml files saved in the current file directory"
            )

    def get_library_yaml_file(self, cfg):
        library_yaml_filename = cfg['filename']
        if os.path.isfile(library_yaml_filename):
            with open(library_yaml_filename, 'r') as ymlfile:
                library_yaml = yaml.load(ymlfile, Loader=yaml.Loader)
        else:
            data = pkgutil.get_data(library_name, cfg['filename'])
            library_yaml = yaml.safe_load(data)

        return library_yaml

    def get_library_filename(self, cfg):

        filename_with_lib_path = cfg['filename']
        if not os.path.isfile(cfg['filename']):
            lib_spec = importlib.util.find_spec(library_name)
            lib_path = Path(lib_spec.origin).parent
            filename_with_lib_path = os.path.join(lib_path, cfg['filename'])
            if not os.path.isfile(filename_with_lib_path):
                raise FileNotFoundError()

        return filename_with_lib_path