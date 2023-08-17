import os
import imp
import logging
from pathlib import Path


def get_valid_file_name(file_name):
    pass


def get_module_path(module=None):
    if module is None:
        module_path = __name__
        module = module_path.split(".")[0]
    module_info = imp.find_module(module)
    module_path = module_info[1]
    return module_path


# Determine if file is valid
def is_file_valid_func(file_name, analysis_root_folder=None):
    file_is_valid = True
    if not os.path.isfile(file_name):
        file_name_cwd = os.path.join(os.getcwd(), file_name)
        if os.path.isfile(file_name_cwd):
            file_name = file_name_cwd
        elif analysis_root_folder is not None:
            file_name_analysis_root = os.path.join(analysis_root_folder,
                                                   file_name)
            if os.path.isfile(file_name_analysis_root):
                file_name = file_name_analysis_root
            else:
                file_is_valid = False
                logging.error(f'File not found: {file_name}')
        else:
            file_is_valid = False
            logging.error(f'File not found: {file_name}')

    return file_is_valid, file_name


def add_cwd_to_filename(file_name, cwd=None):
    if not file_name.startswith(os.path.sep):
        file_name = os.path.join(cwd, file_name)

    return file_name


def get_common_name_from_2_filenames(filename1, filename2):
    # get file root directory
    stem1 = Path(filename1).stem
    stem2 = Path(filename2).stem
    commonprefix = os.path.commonprefix([stem1, stem2])
    uniquebasename = stem1 + "_" + Path(filename2).stem.replace(
        commonprefix, "")

    return uniquebasename