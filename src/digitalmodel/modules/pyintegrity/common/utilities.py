import os
import imp
import logging
from pathlib import Path
from webcolors import rgb_to_hex


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


def get_colors(set='single', n=15):
    '''
    https://help.tableau.com/current/pro/desktop/en-us/formatting_create_custom_colors.htm
    '''

    if set == 'single':
        if n <= 10:
            colors = [
                "#17becf", "#bcbd22", "#7f7f7f", "#e377c2", "#8c564b",
                "#9467bd", "#d62728", "#2ca02c", "#ff7f0e", "#1f77b4"
            ]
        else:
            colors = [(31, 119, 180), (174, 199, 232), (255, 127, 14),
                      (255, 187, 120), (44, 160, 44), (152, 223, 138),
                      (214, 39, 40), (255, 152, 150), (148, 103, 189),
                      (197, 176, 213), (140, 86, 75), (196, 156, 148),
                      (227, 119, 194), (247, 182, 210), (127, 127, 127),
                      (199, 199, 199), (188, 189, 34), (219, 219, 141),
                      (23, 190, 207), (158, 218, 229)]
            colors = [rgb_to_hex(color) for color in colors]
    elif set == 'multi':
        if n <= 8:
            color_1 = ["#0F6BE9", "#043F8F", "#001E45"]
            color_2 = ["#D1350A", "#B23000", "#2F1202"]
            color_3 = ["#A06900", "#5D4101", "#2C1B07"]
            color_4 = ["#05B25D", "#044B32", "#012B1"]
            color_5 = ["#A37BFA", "#492D99", "#019OA52"]
            color_6 = ["#017C9D", "#D1485B", "#062530"]
            color_7 = ["#D2186D", "#880C3E", "#3A0116"]
            color_8 = ["#DC1830", "#830A12", "#340301"]

            set1 = [
                color_1[0], color_2[0], color_3[0], color_4[0], color_5[0],
                color_6[0], color_7[0], color_8[0]
            ]
            set2 = [
                color_1[1], color_2[1], color_3[1], color_4[1], color_5[1],
                color_6[1], color_7[1], color_8[1]
            ]
            set3 = [
                color_1[2], color_2[2], color_3[2], color_4[2], color_5[2],
                color_6[2], color_7[2], color_8[2]
            ]
            set1 = [rgb_to_hex(color) for color in set1]
            set2 = [rgb_to_hex(color) for color in set2]
            set3 = [rgb_to_hex(color) for color in set3]
            colors = {'set1': set1, 'set2': set2, 'set3': set3}
        else:
            raise ValueError('Number of colors must be less than 9')

    return colors
