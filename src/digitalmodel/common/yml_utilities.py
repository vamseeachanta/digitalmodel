import os
import yaml
import types
from deepdiff import DeepDiff

from pathlib import Path
from collections.abc import Mapping
from digitalmodel.common.saveData import saveDataYaml
from digitalmodel.common.utilities import is_file_valid, get_common_name_from_2_filenames


def ymlInput(defaultYml, updateYml=None):

    if not is_file_valid(defaultYml):
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
    def compare_yaml_files_deepdiff(self, file_name1, file_name2):
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
            print(
                "Yaml files are different. See wwyaml files saved in the current file directory"
            )

    def save_diff_files(self,
                        file_diff,
                        file_directory,
                        uniquebasename="",
                        deepdiff_save=False):
        # save the entire diff file. A very messy and overwhelming file
        if deepdiff_save:
            saveDataYaml(file_diff,
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

    # Compare 2 yaml files using sets
