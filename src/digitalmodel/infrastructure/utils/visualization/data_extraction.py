import datetime
import logging
import re
import traceback

import urllib3
import xmltodict
from bs4 import BeautifulSoup

headers_default = {
    'User-Agent': 'Vamsee Achanta support@aceengineer.com',
    'Accept-Encoding': 'gzip, deflate',
    'Host': 'www.sec.gov'
}


class ReadDataFromSystemFiles():

    def get_file_list_from_folder(self, folder_with_file_type, with_path=True, with_extension=True):
        folder_with_file_type_example = 'Q:\projects\Mole\log_files\*.log'

        import glob
        import os
        file_list = []
        for file in glob.glob(folder_with_file_type):
            file_list.append(file)
        if with_path:
            if not with_extension:
                file_list = [os.path.splitext(file)[0] for file in file_list]
        else:
            if with_extension:
                file_list = [os.path.basename(file) for file in file_list]
            else:
                file_list = [os.path.splitext(os.path.basename(file))[0] for file in file_list]

        file_list.sort()
        return file_list

    def get_data_from_json(self, filename):
        import json
        import os
        if os.path.isfile(filename):
            with open(filename, 'r') as fp:
                data = json.load(fp)
        else:
            data = False

        return data

    def get_data_from_yaml(self, filename):
        import os

        import yaml
        if os.path.isfile(filename):
            with open(filename, 'r') as fp:
                data = yaml.load(fp, Loader=yaml.Loader)
        else:
            data = None

        return data


class ReadDataFromString():

    def __init__(self):
        pass

    def get_line_numbers_containing_keyword(self, cfg):
        sample_data_1 = {
            'data_string': '<XML>, what can I do \n  abc \n <XML>',
            'line': {
                'key_word': ['<XML>'],
                'transform': {
                    'scale': 1,
                    'shift': 0
                }
            }
        }
        if cfg is None:
            cfg = sample_data_1

        data_string = cfg.get('data_string')
        line_list = data_string.split('\n')

        line_numbers = self.get_row_numbers_containing_keyword(line_list, cfg['line']['key_word'])

        if cfg['line'].__contains__('transform'):
            scale = cfg['line']['transform']['scale']
            shift = cfg['line']['transform']['shift']
            line_numbers = [line_num * scale + shift for line_num in line_numbers]

        return line_numbers

    def get_row_numbers_containing_keyword(self, array, key_word):
        line_numbers = []
        for rownum in range(0, len(array)):
            if key_word in array[rownum]:
                line_numbers.append(rownum + 1)

        return line_numbers

    def get_array_row_containing_any_of_keywords(self, array, key_words):
        for rownum in range(0, len(array)):
            if any(keyword in array[rownum] for keyword in key_words):
                return rownum + 1

    def get_substrings_by_line_number_array(self, cfg):
        sample_data_1 = {
            'data_string': '<XML>, what can I do \n  abc \n <\XML>',
            'start_line_number': [1],
            'end_line_number': [3]
        }
        if cfg is None:
            cfg = sample_data_1
        start_line_number = cfg.get('start_line_number')
        end_line_number = cfg.get('end_line_number')
        start_line_count = len(start_line_number)
        end_line_count = len(end_line_number)
        sub_string_array = []
        if start_line_count == end_line_count:
            for sub_string_index in range(0, start_line_count):
                start_line = start_line_number[sub_string_index]
                end_line = end_line_number[sub_string_index]
                cfg_temp = cfg.copy()
                cfg_temp.update({'start_line_number': start_line, 'end_line_number': end_line})
                sub_string = self.get_sub_string_by_line_numbers(cfg_temp)
                sub_string_array.append(sub_string)
        elif start_line_count > end_line_count:
            raise ("End Line array/keyword is missing. Missing count is {}".format(start_line_count - end_line_count))
        else:
            raise ("End Line array/keyword is missing. Missing count is {}".format(end_line_count - start_line_count))

        return sub_string_array

    def get_sub_string_by_line_numbers(self, cfg):
        data_string = cfg.get('data_string')
        line_list = data_string.split('\n')
        start_line_number = cfg.get('start_line_number')
        end_line_number = cfg.get('end_line_number')
        sub_list = line_list[start_line_number - 1:end_line_number]
        sub_string = '\n'.join(sub_list)
        return sub_string

    def get_xml_dict_from_byte_data(self, byte_data):
        data_string = byte_data.decode('UTF-8')
        cfg_temp = {'data_string': data_string, 'line': {'key_word': '<XML>', 'transform': {'scale': 1, 'shift': 1}}}
        line_numbers_start_xml = self.get_line_numbers_containing_keyword(cfg=cfg_temp)
        cfg_temp = {'data_string': data_string, 'line': {'key_word': '</XML>', 'transform': {'scale': 1, 'shift': -1}}}
        line_numbers_end_xml = self.get_line_numbers_containing_keyword(cfg=cfg_temp)
        cfg_temp = {
            'data_string': data_string,
            'start_line_number': line_numbers_start_xml,
            'end_line_number': line_numbers_end_xml
        }

        data_sub_strings = self.get_substrings_by_line_number_array(cfg=cfg_temp)
        dict_obj_arrays = []
        for sub_string_index in range(0, len(data_sub_strings)):
            sub_string = data_sub_strings[sub_string_index]
            dict_obj = xmltodict.parse(sub_string)
            dict_obj_arrays.append(dict_obj)

        return dict_obj_arrays

    def get_all_lines_containing_all_key_words(self, cfg):
        data_byte = cfg.get('data_byte', None)
        if data_byte is not None:
            data_string = data_byte.decode('UTF-8')
        else:
            data_string = cfg.get('data_string', None)

        key_words = cfg['line'].get('key_words', None)
        line_array_rows = data_string.split('\n')
        line_numbers = self.get_all_array_row_containing_all_keywords(line_array_rows, key_words)
        selected_lines = [line_array_rows[line_number - 1] for line_number in line_numbers]

        return selected_lines

    def get_all_lines_containing_any_key_words(self, cfg):
        data_byte = cfg.get('data_byte', None)
        if data_byte is not None:
            data_string = data_byte.decode('UTF-8')
        else:
            data_string = cfg.get('data_string', None)

        key_words = cfg['line'].get('key_words', None)
        line_array_rows = data_string.split('\n')
        line_numbers = self.get_all_array_rows_containing_any_keyword(line_array_rows, key_words)
        selected_lines = [line_array_rows[line_number - 1] for line_number in line_numbers]

        return selected_lines

    def get_all_array_rows_containing_any_keyword(self, array, key_words):
        selected_rows = []
        for rownum in range(0, len(array)):
            if any(keyword in array[rownum] for keyword in key_words):
                selected_rows.append(rownum + 1)

        return selected_rows

    def get_first_array_row_containing_any_keyword(self, array, key_words):
        for rownum in range(0, len(array)):
            if any(keyword in array[rownum] for keyword in key_words):
                return rownum + 1
                break

    def get_first_array_row_containing_all_keywords(self, array, key_words):
        for rownum in range(0, len(array)):
            if all(keyword in array[rownum] for keyword in key_words):
                return rownum + 1
                break

    def get_all_array_row_containing_all_keywords(self, array, key_words):
        selected_rows = []
        for rownum in range(0, len(array)):
            if all(keyword in array[rownum] for keyword in key_words):
                selected_rows.append(rownum + 1)

        return selected_rows

    def get_intermediate_word_based_on_pattern(self, cfg):
        string = cfg.get('string', '')
        pattern = cfg.get('pattern', None)
        substring_raw = re.search(pattern, string)
        substring = substring_raw.group(1) if substring_raw is not None else None
        return substring


class ReadURLData():

    def __init__(self, cfg):
        headers = cfg.get('headers', headers_default)
        self.http_connection = urllib3.PoolManager(1, headers=headers)

    def get_response_data(self, cfg):
        try:
            response = self.http_connection.request('GET', cfg['url'])
            data = response.data
        except:
            data = None
            logging.info("Failed to parse xml from response (%s)" % traceback.format_exc())

        return data

    def get_xml_data_as_dict(self, cfg):
        import xmltodict
        response_data = self.get_response_data(cfg)
        try:
            data = xmltodict.parse(response_data)
        except:
            data = None
            logging.info("Failed to parse xml from response (%s)" % traceback.format_exc())

        return data


class AttributeDict(dict):

    def __init__(self, *args, **kwargs):
        super(AttributeDict, self).__init__(*args, **kwargs)
        self.__dict__ = self


class RegEx:

    def __init__(self):
        default_string = 'CUSIP No./n/n/n 2543682'
        pattern = r'CUSIP No.'
        replace_with = ''
        self.default_cfg = {'default_string': default_string, 'pattern': pattern, 'replace_with': replace_with}

    def replace_in_string(self, cfg=None):
        if cfg is None:
            cfg = self.default_cfg

        data_byte = cfg.get('data_byte', None)
        if data_byte is not None:
            data_string = data_byte.decode('UTF-8')
        else:
            data_string = cfg.get('data_string', None)

        pattern = cfg.get('pattern')
        if not isinstance(pattern, list):
            pattern = [pattern]

        replace_with = cfg.get('replace_with')
        if not isinstance(replace_with, list):
            replace_with = [replace_with] * len(pattern)

        for item_index in range(0, len(pattern)):
            data_string = re.sub(pattern[item_index], replace_with[item_index], data_string)

        return data_string

    def get_without_html_tags(self, cfg):
        if cfg is None:
            cfg = self.default_cfg

        data_byte = cfg.get('data_byte', None)
        if data_byte is not None:
            data_string = data_byte.decode('UTF-8')
        else:
            data_string = cfg.get('data_string', None)

        features = cfg.get('features', 'lxml')

        soup = BeautifulSoup(data_string, features=features)
        text_string_without_html = soup.get_text()

        return text_string_without_html


def get_initials_from_name(fullname):
    xs = (fullname)
    name_list = xs.split()

    initials = ""

    for name in name_list:  # go through each name
        initials += name[0].upper()  # append the initial

    return initials


def getClosestIntegerInList(list_data, close_value):
    import numpy as np
    list_data = np.asarray(list_data)
    idx = (np.abs(list_data - close_value)).argmin()
    return list_data[idx], idx


def transform_df_datetime_to_str(df, date_format='%Y-%m-%d %H:%M:%S'):
    df = df.copy()
    if len(df) > 0:
        df_columns = list(df.columns)
        for column in df_columns:
            if isinstance(df[column].iloc[0], datetime.datetime) or isinstance(df[column].iloc[0], datetime.date):
                df[column] = [
                    item.strftime(date_format) if item is not None else item for item in df[column].to_list()
                ]

    return df


def transform_df_None_to_NULL(df):
    df = df.copy()

    # default_value = "NULL"
    # df = df.apply(lambda x: x if x is not None else default_value)
    df_columns = list(df.columns)
    for column in df_columns:
        for row_num in range(0, len(df)):
            if df[column].iloc[row_num] is None:
                df[column].iloc[row_num] = "NULL"

    return df
