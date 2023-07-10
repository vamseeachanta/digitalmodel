import os
import logging
from pathlib import Path


# Determine if file is valid
def is_file_valid_func(file_name):
    file_is_valid = True
    if not os.path.isfile(file_name):
        file_name = os.path.join(os.getcwd(), file_name)
        if not os.path.isfile(file_name):
            file_is_valid = False
            logging.error(f'File not found: {file_name}')

    return is_file_valid_func, file_name


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