class ETL_components():
    def __init__(self, cfg):
        self.cfg= cfg

    def from_xlsx_to_yaml_file(self):
        from common.data import ReadData, SaveData

        read_data = ReadData()
        save_data = SaveData()
        df_array = []

        for file_index in range(0, len(self.cfg['files']['from_xlsx'])):
            df_array.append(read_data.from_xlsx(self.cfg, file_index))

            for replace_index in range(0, len(self.cfg['files']['from_xlsx'][file_index]['replace'])):
                column = self.cfg['files']['from_xlsx'][file_index]['replace'][replace_index]['column']
                to_replace = self.cfg['files']['from_xlsx'][file_index]['replace'][replace_index]['to_replace']
                replace_with = self.cfg['files']['from_xlsx'][file_index]['replace'][replace_index]['replace_with']
                df_array[file_index][column] = df_array[file_index][column].replace(to_replace=to_replace,
                                                                                    value=replace_with, regex=True)

            df_label = self.cfg['transform']['df_row_to_array']['df_label']
            # TODO Refactor so all of these can be set from the configuration file
            self.cfg[df_label] = []
            for row_index in range(0, len(df_array[file_index])):
                row_items = {}
                row_items.update(
                    {'Name': 'K:\\0182\\Rev6\\Extreme\\' + df_array[file_index].loc[row_index, 'SaveData']})
                row_items.update({'Label': self.cfg['files']['from_xlsx'][file_index]['label'] + '_' +
                                           df_array[file_index].loc[row_index, '// Wave Name'] + '_'
                                           + df_array[file_index].loc[row_index, 'Select Current']})
                row_items.update({'ObjectName': 'SLWR'})

                self.cfg[df_label].append(row_items)

            save_data.saveDataYaml(self.cfg['Files'], self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                   self.cfg['files']['from_xlsx'][file_index]['label'], False)
            print("File saved successfully: {0}".format(
                self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                self.cfg['files']['from_xlsx'][file_index]['label']))

    def write_repeat_pattern_files(self):
        no_of_output_files = len(self.cfg['output_files'])
        for file_index in range(0, no_of_output_files):
            self.file_index = file_index
            if self.cfg['output_files'][self.file_index].__contains__('repeat_patterns'):
                self.write_repeat_pattern_file()

    def write_repeat_pattern_file(self):
        from common.data import SaveData
        save_data = SaveData()

        field_data_array = self.cfg['output_files'][self.file_index]['repeat_patterns']
        all_line_field_lists = self.get_all_line_field_data(field_data_array)

        output_file_single = []
        output_file_single = output_file_single + self.get_header()
        file_name_extension = self.cfg['output_files'][self.file_index]['settings']['file_name']['extension']

        for repeat_pattern_block_index in range(0, self.number_of_repeat_pattern_blocks):
            repeat_pattern_block_file_contents = self.get_repeat_pattern_block_file_contents(all_line_field_lists, repeat_pattern_block_index)
            output_file_single = output_file_single + repeat_pattern_block_file_contents
            output_file_multiple = []
            header_content = self.get_header()
            if header_content is not None:
                output_file_multiple = output_file_multiple + header_content
            output_file_multiple = output_file_multiple + repeat_pattern_block_file_contents
            footer_content = self.get_footer()
            if footer_content is not None:
                output_file_multiple = output_file_multiple + footer_content
            if self.cfg['output_files'][self.file_index]['settings']['multiple_file']:
                file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name_for_overwrite']
                save_data.save_ascii_file_from_array(output_file_multiple, file_name, extension = '.' + file_name_extension)

        footer_content = self.get_footer()
        if footer_content is not None:
            output_file_single = output_file_single + footer_content
        if self.cfg['output_files'][self.file_index]['settings']['single_file']:
            file_name_suffix = self.cfg['output_files'][self.file_index]['settings']['file_name']['suffix']
            file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name_for_overwrite'] + '_' + file_name_suffix
            save_data.save_ascii_file_from_array(output_file_single, file_name, extension = '.' + file_name_extension)
            print("Successfully saved {0}{1}{2}" .format(file_name, '.', file_name_extension))

    def get_repeat_pattern_block_file_contents(self, all_line_field_lists, repeat_pattern_block_index):
        repeat_pattern_block_file_contents = []
        repeat_pattern_block = self.cfg['output_files'][self.file_index]['repeat_patterns']
        for repeat_pattern_index in range(0, len(repeat_pattern_block)):
            line_text = repeat_pattern_block[repeat_pattern_index]['text']
            if type(line_text) is list:
                line_text = self.convert_text_list_to_combined_text(line_text)
            line_fields = repeat_pattern_block[repeat_pattern_index]['fields']
            if line_fields == None:
                repeat_pattern_block_file_contents.append(line_text)
            else:
                number_of_fields = len(repeat_pattern_block[repeat_pattern_index]['fields'])
                # TODO Paarametrize the number of fields so the application code is simple for all field counts.
                # line_tuple = ()
                # for line_field_index in range(0, number_of_fields):
                #     line_tuple + all_line_field_lists[line_index][repeat_pattern_index][line_field_index]
                if number_of_fields == 1:
                    line_field_index = 0
                    line_field1_value = all_line_field_lists[repeat_pattern_index][line_field_index][
                        repeat_pattern_block_index]
                    repeat_pattern_block_file_contents.append(line_text.format(line_field1_value))
                elif number_of_fields == 2:
                    line_field_index = 0
                    line_field1_value = all_line_field_lists[repeat_pattern_index][line_field_index][
                        repeat_pattern_block_index]
                    line_field2_value = all_line_field_lists[repeat_pattern_index][line_field_index + 1][
                        repeat_pattern_block_index]
                    repeat_pattern_block_file_contents.append(line_text.format(line_field1_value, line_field2_value))
                elif number_of_fields == 3:
                    line_field_index = 0
                    line_field1_value = all_line_field_lists[repeat_pattern_index][line_field_index][
                        repeat_pattern_block_index]
                    line_field2_value = all_line_field_lists[repeat_pattern_index][line_field_index + 1][
                        repeat_pattern_block_index]
                    line_field3_value = all_line_field_lists[repeat_pattern_index][line_field_index + 2][
                        repeat_pattern_block_index]
                    repeat_pattern_block_file_contents.append(line_text.format(line_field1_value, line_field2_value, line_field3_value))

        return repeat_pattern_block_file_contents

    def convert_text_list_to_combined_text(self, line_text):
        line_text_combined = ""
        for item in line_text:
            line_text_combined = line_text_combined + item
        line_text = line_text_combined
        return line_text

    def get_all_line_field_data(self, field_data_array):
        from common.data import ReadData
        read_data = ReadData()
        all_line_field_lists = []
        self.number_of_repeat_pattern_blocks = None
        self.number_of_patterns_per_block = len(field_data_array)
        for line_index in range(0, self.number_of_patterns_per_block):
            line_text = field_data_array[line_index]['text']
            line_fields = field_data_array[line_index]['fields']
            line_field_lists = []
            if line_fields != None:
                self.check_line_field_count(line_index, line_text)
                for field_index in range(0, len(line_fields)):
                    cfg_temp = {'files': line_fields[field_index]}
                    if line_fields[field_index].__contains__('from_xlsx'):
                        df = read_data.from_xlsx(cfg_temp)
                        df = read_data.df_filter_by_column_values(cfg_temp, df)
                        column_required = cfg_temp['files']['from_xlsx'][0]['column']
                        field_list = df[column_required].to_list()
                        if line_fields[field_index]['from_xlsx'][0].__contains__('scale'):
                            scale = line_fields[field_index]['from_xlsx'][0]['scale']
                            field_list = [x*scale  for x in field_list]
                    elif line_fields[field_index].__contains__('from_range'):
                        start_range=line_fields[field_index]['from_range'][0]
                        end_range=line_fields[field_index]['from_range'][1] + 1
                        field_list = list(range(start_range, end_range))

                    line_field_lists.append(field_list)
                    if self.number_of_repeat_pattern_blocks ==None:
                        self.number_of_repeat_pattern_blocks = len(field_list)
                    else:
                        self.number_of_repeat_pattern_blocks = min(self.number_of_repeat_pattern_blocks, len(field_list))

            all_line_field_lists.append(line_field_lists)
        return all_line_field_lists

    def check_line_field_count(self, line_index, line_text):
        number_of_fields = len(self.cfg['output_files'][self.file_index]['repeat_patterns'][line_index]['fields'])
        if self.if_line_field_pass(line_text, number_of_fields):
            print("Field count matches")
        else:
            print("Field count mismatch")

    def if_line_field_pass(self, text, number_of_fields):
        number_of_fields_in_text = text.count('{')
        if number_of_fields_in_text == number_of_fields:
            return True
        else:
            return False

    def get_header(self):
        if self.cfg['output_files'][self.file_index].__contains__('header'):
            content = self.cfg['output_files'][self.file_index]['header']
            content_array = self.get_content_array(content)

            return content_array

    def get_footer(self):
        if self.cfg['output_files'][self.file_index].__contains__('footer'):
            content = self.cfg['output_files'][self.file_index]['footer']
            content_array = self.get_content_array(content)

            return content_array

    def get_content_array(self, content):
        content_array = []
        for content_line_index in range(0, len(content)):
            if content[content_line_index].__contains__('text'):
                line_text = content[content_line_index]['text']
                if type(line_text) is list:
                    line_text = self.convert_text_list_to_combined_text(line_text)
                if line_text is not None:
                    content_array.append(content[content_line_index]['text'])
            if content[content_line_index].__contains__('from_file'):
                cfg_temp = content[content_line_index]['from_file']
                content_array_from_file = self.get_content_array_from_file(cfg_temp)
                content_array = content_array + content_array_from_file

        return content_array

    def get_content_array_from_file(self, cfg_temp):
        from common.data import ReadData
        read_data = ReadData()
        content_array_from_file = read_data.from_ascii_file_get_lines_as_string_arrays(cfg_temp)

        return content_array_from_file

    def write_replace_files(self):
        import os
        self.file_index = 0
        self.get_ascii_file_content_as_array()
        self.model_state_information = self.get_model_state_information()
        if self.cfg['output_files'][self.file_index]['settings']['multiple_file']:
            cfg_field_lists = [self.cfg['output_files'][self.file_index]['settings']['file_name']['suffix']]
            file_name_suffix_fields = self.generic_get_all_line_field_data(cfg_field_lists)
            file_name_extension = self.cfg['output_files'][self.file_index]['settings']['file_name']['extension']
            if cfg_field_lists[0]['fields'] != None:
                for field_index in range(0, len(file_name_suffix_fields[0][0])):
                    file_name_suffix = self.cfg['output_files'][self.file_index]['settings']['file_name']['suffix'][
                        'name']
                    if type(file_name_suffix) is list:
                        from common.ETL_components import ETL_components
                        etl_components = ETL_components(cfg=None)
                        file_name_suffix = etl_components.convert_text_list_to_combined_text(file_name_suffix)
                    file_name_suffix = file_name_suffix .format(file_name_suffix_fields[0][0][field_index])
                    if self.cfg['Analysis']['custom_folder'] is not None:
                        file_name = os.path.join(self.cfg['Analysis']['custom_folder'] , file_name_suffix)
                    else:
                        file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis'][
                            'file_name_for_overwrite'] + '_' + file_name_suffix

                    self.field_data_array = self.cfg['output_files'][self.file_index]['replace']
                    # Physically update the current profile id here.
                    # TODO perform this operation from .yml in long run
                    self.field_data_array[2]['fields'][0]['from_xlsx'][0]['filter'][2]['value'] = file_name_suffix_fields[0][0][field_index]
                    self.field_data_array[2]['fields'][1]['from_xlsx'][0]['filter'][2]['value'] = file_name_suffix_fields[0][0][field_index]

                    all_line_field_lists = self.generic_get_all_line_field_data(self.field_data_array)
                    all_line_field_lists = self.transform_shear7_data(all_line_field_lists)
                    result_file_array = self.replace_file_contents(all_line_field_lists)

                    self.write_replace_file(result_file_array, file_name, file_name_extension)

    def write_replace_file(self, result_file_array, file_name, file_name_extension):
        from common.data import SaveData
        save_data = SaveData()
        save_data.save_ascii_file_from_array(result_file_array, file_name, extension = '.' + file_name_extension)
        print("Successfully saved {0}{1}{2}" .format(file_name, '.', file_name_extension))

    def transform_shear7_data(self, all_line_field_lists):
        import numpy as np
        for line_index in range(0, len(self.field_data_array)):
            if self.field_data_array[line_index]['fields'] != None:
                for line_field_index in range(0, len(self.field_data_array[line_index]['fields'])):
                    if self.field_data_array[line_index]['fields'][line_field_index].__contains__('from_xlsx'):
                        if self.field_data_array[line_index]['fields'][line_field_index]['from_xlsx'][0].__contains__('transform'):
                            if self.field_data_array[line_index]['fields'][line_field_index]['from_xlsx'][0]['transform']:
                                water_depth = self.model_state_information['water_depth']
                                depth_below_msl = self.model_state_information['depth_below_msl']
                                shear7_x_by_L = self.model_state_information['shear7_x_by_L']
                                raw_depth = list(all_line_field_lists[line_index][line_field_index])
                                raw_current = list(all_line_field_lists[line_index][line_field_index+1])
                                modified_depth = []
                                modified_current = []
                                for item_index in range(0, len(raw_depth)):
                                    if raw_depth[item_index] < water_depth:
                                        modified_depth.append(raw_depth[item_index])
                                        modified_current.append(raw_current[item_index])
                                modified_depth.append(water_depth - 0.5)
                                modified_current.append(modified_current[-1])
                                modified_depth.reverse()
                                modified_current.reverse()

                                modified_current, modified_depth = self.implement_current_shielding_effects(
                                    modified_current, modified_depth)

                                transformed_data_depth = [np.interp(item, depth_below_msl, shear7_x_by_L )  for item in modified_depth ]
                                all_line_field_lists[line_index][line_field_index] = transformed_data_depth
                                all_line_field_lists[line_index][line_field_index+1] = modified_current
                                all_line_field_lists[line_index-1].append([len(modified_current)])


                    if self.field_data_array[line_index]['fields'][line_field_index].__contains__('from_ascii'):
                        if self.field_data_array[line_index]['fields'][line_field_index]['from_ascii'].__contains__('transform'):
                            all_line_field_lists[line_index][line_field_index][0] = all_line_field_lists[line_index][line_field_index][0] - 1

        return all_line_field_lists

    def implement_current_shielding_effects(self, modified_current, modified_depth):
        if self.model_state_information.__contains__('shielding'):
            shielding_info = self.model_state_information['shielding']
            for shielding_index in range(0, len(shielding_info)):

                if shielding_info[0][0] ==0:
                    end_depth_index_for_logic = len(modified_depth) + 2
                else:
                    end_depth_index_for_logic = len(modified_depth) + 4

                for depth_index in range(0, end_depth_index_for_logic):
                    depth = modified_depth[depth_index]
                    if depth >= shielding_info[0][0] and depth <= shielding_info[0][1]:
                        temporary_current = modified_current[depth_index]
                        modified_current[depth_index] = 0
                        if depth_index > 0 and (depth_index < end_depth_index_for_logic -1):
                            if modified_depth[depth_index - 1] > shielding_info[0][1]:
                                modified_depth.insert(depth_index, shielding_info[0][1])
                                modified_current.insert(depth_index, temporary_current)
                                modified_depth.insert(depth_index + 1, shielding_info[0][1] - 0.1)
                                modified_current.insert(depth_index + 1, modified_current[depth_index] * \
                                                                    self.model_state_information['shielding_factor'])
                            if modified_depth[depth_index + 1] < shielding_info[0][0]:
                                modified_depth.insert(depth_index + 1, shielding_info[0][0])
                                modified_current.insert(depth_index + 1, modified_current[depth_index + 1]* \
                                                        self.model_state_information['shielding_factor'])
                                modified_depth.insert(depth_index + 2, shielding_info[0][0] + 0.1)
                                modified_current.insert(depth_index + 2, modified_current[depth_index + 2])
                                break

        return modified_current, modified_depth

    def replace_file_contents(self, all_line_field_lists):
        replace_line_set_array = self.get_line_numbers_to_replace()
        result_file_array = self.replace_lines(self.base_file_content_array, replace_line_set_array, all_line_field_lists)
        return result_file_array

    def get_line_numbers_to_replace(self):
        replace_block = self.cfg['output_files'][self.file_index]['replace']
        replace_line_set_array = []
        for replace_line_set_index in range(0, len(replace_block)):
            start_line_contain_key_text = replace_block[replace_line_set_index]['lines']['from']['containing_text']
            start_line_number = self.get_array_index_containing_key_text(self.base_file_content_array, start_line_contain_key_text)
            end_line_number= None
            if replace_block[replace_line_set_index]['lines'].__contains__('to') and replace_block[replace_line_set_index]['lines']['to'] != None:
                end_line_contain_key_text = replace_block[replace_line_set_index]['lines']['to']['containing_text']
                end_line_number = self.get_array_index_containing_key_text(self.base_file_content_array, end_line_contain_key_text)
            replace_line_set_array.append({'start_line_number': start_line_number, 'end_line_number': end_line_number})

        return replace_line_set_array

    def get_array_index_containing_key_text(self, line_array, key_text):
        for item_index in range(0, len(line_array)):
            if key_text in line_array[item_index]:
                return item_index
                break

    def replace_lines(self, line_array, replace_line_set_array, all_line_field_lists):
        result_array = []
        replace_line_set_array_index = 0
        replace_block = self.cfg['output_files'][self.file_index]['replace']
        for line_array_index in range(0, len(line_array)):
            if replace_line_set_array[replace_line_set_array_index]['start_line_number'] == line_array_index:
                line_field_lists =  all_line_field_lists[replace_line_set_array_index]
                if not line_field_lists:
                    result_array.append(replace_block[replace_line_set_array_index]['template_line'])
                else:
                    for value_index in range(0, len(line_field_lists[0])):
                        if len(line_field_lists) == 1:
                            value = all_line_field_lists[replace_line_set_array_index][0][value_index]
                            result_array.append(replace_block[replace_line_set_array_index]['template_line'] .format(value))
                        elif len(line_field_lists) == 2:
                            value_1 = line_field_lists[0][value_index]
                            value_2 = line_field_lists[1][value_index]
                            result_array.append(replace_block[replace_line_set_array_index]['template_line'] .format(value_1, value_2))
                if replace_line_set_array_index < len(replace_line_set_array)-1:
                    replace_line_set_array_index = replace_line_set_array_index + 1
            else:
                result_array.append(line_array[line_array_index])

        return result_array

    def get_ascii_file_content_as_array(self):
        cfg_temp = {'io': self.cfg['output_files'][self.file_index]['settings']['basefile']}
        self.base_file_content_array =  self.get_content_array_from_file(cfg_temp)

    def get_model_state_information(self):
        import ast

        from common.data import ReadData
        read_data = ReadData()

        cfg_temp = self.cfg['output_files'][self.file_index]['settings']['model_state_information']['from_ascii']

        start_line = cfg_temp['line']['from']['number']
        if cfg_temp['line'].__contains__('to'):
            end_line = cfg_temp['line']['to']['number']
        else:
            end_line = None
        cfg_temp.update({'start_line': start_line, 'end_line': end_line})


        line_text = read_data.from_ascii_file_get_lines_as_string_arrays(cfg_temp)
        if cfg_temp['filter']['data_type'] == 'dict':
            result = ast.literal_eval(line_text)
        else:
            result = line_text

        return result

    def generic_get_all_line_field_data(self, field_data_array):
        from common.data import ReadData
        read_data = ReadData()
        all_line_field_lists = []
        self.number_of_repeat_pattern_blocks = None
        self.number_of_patterns_per_block = len(field_data_array)
        for line_index in range(0, self.number_of_patterns_per_block):
            line_fields = field_data_array[line_index]['fields']
            line_field_lists = []
            if line_fields != None:
                for field_index in range(0, len(line_fields)):
                    cfg_temp = {'files': line_fields[field_index]}
                    if line_fields[field_index].__contains__('from_xlsx'):
                        df = read_data.from_xlsx(cfg_temp)
                        df = read_data.df_filter_by_column_values(cfg_temp, df)
                        column_required = cfg_temp['files']['from_xlsx'][0]['column']
                        field_list = df[column_required].to_list()
                        if line_fields[field_index]['from_xlsx'][0].__contains__('scale'):
                            scale = line_fields[field_index]['from_xlsx'][0]['scale']
                            field_list = [x * scale for x in field_list]
                    elif line_fields[field_index].__contains__('from_range'):
                        start_range = line_fields[field_index]['from_range'][0]
                        end_range = line_fields[field_index]['from_range'][1]
                        field_list = list(range(start_range, end_range))
                    elif line_fields[field_index].__contains__('from_ascii'):
                        start_line = line_fields[field_index]['from_ascii']['line']['from']['number']
                        if line_fields[field_index]['from_ascii']['line'].__contains__('to'):
                            end_line = line_fields[field_index]['from_ascii']['line']['to']['number']
                        else:
                            end_line = None
                        cfg_temp = line_fields[field_index]['from_ascii']
                        cfg_temp.update({'start_line': start_line, 'end_line': end_line})
                        value = read_data.from_ascii_file_get_value(cfg_temp)
                        field_list = [value]

                    line_field_lists.append(field_list)

            all_line_field_lists.append(line_field_lists)
        return all_line_field_lists

    def get_sum_df_from_df_array(self, df_array):
        import pandas as pd
        sum_df = pd.DataFrame(columns=df_array[0].columns)
        for data_frame_index in range(0, len(df_array)):
            sum_df.loc[len(sum_df)] = df_array[data_frame_index].sum(axis=0)

        return sum_df

    def get_sum_df_from_df_array_test(self):
        import numpy as np
        import pandas as pd

        # import unittest

        df_array = []
        df = pd.DataFrame(np.array([[1, 2],
                         [3, 4]]), columns = ['A', 'B'])
        df_array.append(df.copy())

        df = pd.DataFrame(np.array([[10, 20],
                         [30, 40]]), columns = ['A', 'B'])
        df_array.append(df.copy())

        sum_df = self.get_sum_df_from_df_array(df_array)
        # unittest.TestCase.assertEqual(len(sum_df), len(df_array))
        assert len(sum_df) == len(df_array)

    def extract_data(self):
        from common.data import ReadData
        read_data  = ReadData()
        extract_data = self.cfg.default['extract']
        self.df_array = []
        for data_category_index in range(0, len(extract_data)):
            if extract_data[data_category_index].__contains__('from_xlsx'):
                for df_index in range(0, len(extract_data[data_category_index]['from_xlsx'])):
                    df = read_data.from_xlsx({'files': extract_data[data_category_index]}, df_index)
                    self.df_array.append(df)

    def transform_viv_current_reformat_1(self):
        import pandas as pd
        transformed_df_columns = ['location_id', 'loading_type', 'Type', 'current_profile_id', 'depth', 'current_speed', 'direction', 'return_period', 'probability', 'Label']
        self.transformed_df_array = []
        for df_index in range(0, len(self.df_array)):
            depths = [self.df_array[df_index].columns[col_index] for col_index in range(3, len(self.df_array[df_index].columns))]
            df_columns = self.df_array[df_index].columns[0:3].to_list() + ['depth_' + str(depth_index-2) for depth_index in range(3, len(self.df_array[df_index].columns))]
            self.df_array[df_index].columns = df_columns
            transformed_df = pd.DataFrame(columns=transformed_df_columns)
            for row_index in range(0, len(self.df_array[df_index])):
                for depth_index in range(0, len(depths)):
                    location_id = self.cfg.default['transform']['viv_current_reformat_1']['location_id']
                    loading_type = self.cfg.default['transform']['viv_current_reformat_1']['loading_type']
                    Type = self.cfg.default['transform']['viv_current_reformat_1']['Type']
                    current_profile_id = self.df_array[df_index].iloc[row_index]['No.']
                    depth = depths[depth_index] * \
                            self.cfg.default['transform']['viv_current_reformat_1']['scale']['depth']
                    current_speed = self.df_array[df_index].iloc[row_index]['depth_' + str(depth_index+1)] * \
                                    self.cfg.default['transform']['viv_current_reformat_1']['scale']['current_speed']
                    direction = self.cfg.default['transform']['viv_current_reformat_1']['direction']
                    return_period = self.cfg.default['transform']['viv_current_reformat_1']['return_period']
                    probability = self.df_array[df_index].iloc[row_index]['%']
                    Label = self.cfg.default['extract'][0]['from_xlsx'][df_index]['label']
                    transformed_df.loc[len(transformed_df)] = [location_id, loading_type, Type, current_profile_id, depth,
                                                               current_speed, direction, return_period, probability, Label]

            self.transformed_df_array.append(transformed_df)

    def save(self):
        from common.data import SaveData
        save_data = SaveData()
        cfg_temp = {'FileName': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '.xlsx',
                    'SheetNames': [df_data['label'] for df_data in self.cfg.default['extract'][0]['from_xlsx']]}
        save_data.DataFrameArray_To_xlsx_openpyxl(self.transformed_df_array, cfg_temp)

if __name__ == '__main__':
    etl_components = ETL_components(cfg=None)
    etl_components.get_sum_df_from_df_array_test()