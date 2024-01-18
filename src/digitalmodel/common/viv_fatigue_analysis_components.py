import os
import statistics
import logging
import pandas as pd
import numpy as np

from assetutilities.common.data import FromString
from assetutilities.common.data import ReadData
from assetutilities.common.data import SaveData
from assetutilities.common.visualizations import Visualization


class VIVFatigueAnalysisComponents():

    def __init__(self, cfg):
        self.cfg = cfg
        self.get_model_state_information()

    def get_model_state_information(self):
        if self.cfg['default'].__contains__('model_state_information'):
            if self.cfg['default']['model_state_information']['flag']:
                if self.cfg['default']['model_state_information'].__contains__('from_csv'):
                    file_name = self.cfg['default']['model_state_information']['from_csv']['io']
                    attribute = self.cfg['default']['model_state_information']['from_csv']['label']
                setattr(self, attribute, pd.read_csv(file_name))

    def shear7_viv_fatigue_analysis(self):
        self.read_shear7_viv_plt_files()
        self.get_viv_modes_excited()
        self.get_total_viv_fatigue()
        self.tabulate_viv_results()
        self.plot_viv_tables()
        self.tabulate_overall_viv_results()

    def get_viv_modes_excited(self):
        save_data = SaveData()

        SheetNames = []
        self.viv_modes_excited_df_array = []
        columns = ['io', 'dominant_mode', 'min_mode', 'max_mode']
        self.viv_modes_excited_summary = pd.DataFrame(columns=columns)
        start_line_keyword_text = 'mode no. frequency'
        end_line_keyword_text = 'No. of potentially excited modes'
        cfg_temp_start_line = {'io': None, 'line': {'key_words': [start_line_keyword_text], 'transform': {'scale': 1, 'shift': 3}}}
        cfg_temp_end_line = {'io': None, 'line': {'key_words': [end_line_keyword_text], 'transform': {'scale': 1, 'shift': -2}}}
        for file_index in range(0, len(self.cfg['Shear7_sets'])):
            SheetNames.append(str(file_index))
            io = self.cfg['Shear7_sets'][file_index]['io'][0:len(self.cfg['Shear7_sets'][file_index]['io']) - 3] + 'out'
            print("Reading file {}" .format(io))
            cfg_temp_start_line.update({'io': io})
            cfg_temp_end_line.update({'io': io})

            file_df = pd.DataFrame()
            try:
                start_line = read_data.from_ascii_file_get_line_number_containing_keywords(cfg_temp_start_line)
                end_line = read_data.from_ascii_file_get_line_number_containing_keywords(cfg_temp_end_line)
                cfg_temp = {'io': io, 'start_line': start_line, 'end_line': end_line,
                            'DataFrame': True, 'delimiter': 'space',
                            'columns': ['Mode Number', 'Frequency', 'Modal Force', 'Modal Damping', 'Modal Power',
                                        'Power Ratio', 'Power Ratio to exponent']}
                file_df = read_data.from_ascii_file_get_structured_data_delimited_white_space(cfg_temp)
                dominant_mode = file_df[file_df['Power Ratio'] == 1]['Mode Number'].iloc[0]
                summary_row = [io, dominant_mode , file_df['Mode Number'].min(), file_df['Mode Number'].max()]
                self.viv_modes_excited_summary.loc[len(self.viv_modes_excited_summary)] = summary_row
            except:
                print("No excited modes obtained for file {}" .format(io))

            self.viv_modes_excited_df_array.append(file_df)

            cfg_temp = {'FileName': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                'viv_modes_excited.xlsx',
                    'SheetNames': SheetNames,
                    "thin_border": True}
            save_data.DataFrameArray_To_xlsx_openpyxl(self.viv_modes_excited_df_array, cfg_temp)
            file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis'][
                'file_name'] + '_' + 'viv_modes_excited_summary.csv'
            self.viv_modes_excited_summary.to_csv(file_name)

    def read_shear7_viv_plt_files(self):
        read_data = ReadData()
        self.viv_plt_df_array = []
        for file_index in range(0, len(self.cfg['Shear7_sets'])):
            cfg_temp = self.cfg['Shear7_sets'][file_index]
            print("Reading file {}" .format(cfg_temp['io']))
            cfg_temp.update({'delimiter': 'space',
                             'columns': ['x/L', 'RMS Displacement', 'unknown', 'RMS Accleration', 'RMS Stress',
                                         'Damage (1/year)', 'Cf'], 'DataFrame': True})
            if os.stat(cfg_temp['io']).st_size == 0:
                file_df = pd.DataFrame()
            else:
                file_df = read_data.from_ascii_file_get_structured_data_delimited_white_space(cfg_temp)
                file_df = self.add_riser_length_to_shear7_result(file_df)
            self.viv_plt_df_array.append(file_df)

    def get_total_viv_fatigue(self):
        self.total_viv_fatigue = pd.DataFrame()
        self.total_viv_fatigue['x/L'] = self.viv_plt_df_array[-1]['x/L']
        self.total_viv_fatigue['total_damage'] = self.viv_plt_df_array[-1]['x/L'] * 0
        for file_index in range(0, len(self.viv_plt_df_array)):
            if not self.viv_plt_df_array[file_index].empty:
                probability_of_occurrence = self.cfg['Shear7_sets'][file_index]['probability']/100
                self.viv_plt_df_array[file_index]['current_profile_damage_per_year'] = self.viv_plt_df_array[file_index][
                                                      'Damage (1/year)'] * probability_of_occurrence
                self.total_viv_fatigue['total_damage'] = self.total_viv_fatigue[
                                                             'total_damage'] + self.viv_plt_df_array[file_index]['current_profile_damage_per_year']

        self.total_viv_fatigue = self.add_riser_length_to_shear7_result(self.total_viv_fatigue)

    def tabulate_viv_results(self):
        save_data = SaveData()

        if self.cfg.__contains__('viv_tables'):
            self.viv_tables = []
            SheetNames = []
            columns = ['label', 'file_index', 'probability', 'duration', 'max_damage_rate', 'event_damage', 'event_damage_with_fos', 'max_damage_per_year', 'total_damage', 'percent_damage']
            for table_index in range(0, len(self.cfg.viv_tables['length'])):
                SheetNames.append(self.cfg.viv_tables['length'][table_index]['label'])
                df = pd.DataFrame(columns=columns)
                for file_index in range(0, len(self.viv_plt_df_array)):
                    label = self.cfg['Shear7_sets'][file_index]['label']
                    probability = self.cfg['Shear7_sets'][file_index]['probability'] / 100
                    if self.cfg['Shear7_sets'][file_index].__contains__('duration'):
                        duration = self.cfg['Shear7_sets'][file_index]['duration']
                    else:
                        duration = 20
                    if not self.viv_plt_df_array[file_index].empty:
                        max_damage_rate = self.viv_plt_df_array[file_index]['Damage (1/year)'][
                            (self.viv_plt_df_array[file_index]['elevation_above_mudline'] >
                             self.cfg.viv_tables['length'][table_index]['start']) & (
                                    self.viv_plt_df_array[file_index]['elevation_above_mudline'] <
                                    self.cfg.viv_tables['length'][table_index][
                                        'end'])].max()
                        max_damage_per_year = self.viv_plt_df_array[file_index]['current_profile_damage_per_year'][
                            (self.viv_plt_df_array[file_index]['elevation_above_mudline'] >
                             self.cfg.viv_tables['length'][table_index]['start']) & (
                                    self.viv_plt_df_array[file_index]['elevation_above_mudline'] <
                                    self.cfg.viv_tables['length'][table_index][
                                        'end'])].max()
                        total_damage = self.total_viv_fatigue['total_damage'][
                            (self.total_viv_fatigue['elevation_above_mudline'] >
                             self.cfg.viv_tables['length'][table_index]['start']) & (
                                    self.total_viv_fatigue['elevation_above_mudline'] <
                                    self.cfg.viv_tables['length'][table_index][
                                        'end'])].max()
                        percent_damage = max_damage_per_year/total_damage * 100
                        event_damage = max_damage_rate * duration
                        event_damage_with_fos = event_damage*self.cfg.default['Analysis']['viv_fatigue']['factor_of_safety']
                        row_array = [label, file_index, probability, duration, max_damage_rate, event_damage, event_damage_with_fos, max_damage_per_year, total_damage, percent_damage]
                    else:
                        row_array = [label, file_index, probability, duration, 0, 0, 0, 0, 0]
                    df.loc[len(df)] = row_array
                self.viv_tables.append(df)

            cfg_temp = {'FileName': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                'viv_summary_individual.xlsx',
                    'SheetNames': SheetNames,
                    "thin_border": True}

            save_data.DataFrameArray_To_xlsx_openpyxl(self.viv_tables, cfg_temp)

    def tabulate_overall_viv_results(self):
        save_data = SaveData()

        if self.cfg.__contains__('viv_tables'):
            self.viv_tables = []
            SheetNames = ['overall']
            columns = ['label', 'total_damage']
            df = pd.DataFrame(columns=columns)
            for table_index in range(0, len(self.cfg.viv_tables['length'])):
                label = self.cfg.viv_tables['length'][table_index]['label']
                if not self.total_viv_fatigue.empty:
                    total_damage = self.total_viv_fatigue['total_damage'][
                        (self.total_viv_fatigue['elevation_above_mudline'] >
                         self.cfg.viv_tables['length'][table_index]['start']) & (
                                self.total_viv_fatigue['elevation_above_mudline'] <
                                self.cfg.viv_tables['length'][table_index][
                                    'end'])].max()
                    row_array = [label, total_damage]
                else:
                    row_array = [label, 0]
                df.loc[len(df)] = row_array

            self.viv_tables.append(df)

            cfg_temp = {'FileName': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                'viv_summary_combined.xlsx',
                    'SheetNames': SheetNames,
                    "thin_border": True}

            save_data.DataFrameArray_To_xlsx_openpyxl(self.viv_tables, cfg_temp)
            cfg_temp = {'FileName': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                'total_viv_fatigue.xlsx', "SheetNames": ['total_viv_fatigue']}
            save_data.DataFrameArray_To_xlsx_openpyxl([self.total_viv_fatigue], cfg_temp)

    def plot_viv_tables(self):
        if self.cfg.__contains__('viv_tables') and self.cfg.viv_tables.__contains__('plot'):
            for table_index in range(0, len(self.cfg.viv_tables['length'])):
                viz = Visualization()
                plt_settings = self.cfg.viv_tables['plot'][0]
                plt_settings.update(
                    {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  plt_settings['file_suffix'] + '_' + self.cfg.viv_tables['length'][table_index]['label'] + '.png'})
                viz.from_df_columns(self.viv_tables[table_index], plt_settings)
                viz.add_title_and_axis_labels()
                viz.add_x_y_scale_formats()
                viz.add_reference_lines_and_spans()
                viz.add_legend()
                print("Saving {0} plot for event damage  ..".format(plt_settings['file_suffix']))
                viz.save_and_close()

            # % damage contribution plot
            for table_index in range(0, len(self.cfg.viv_tables['length'])):
                viz = Visualization()
                plt_settings = self.cfg.viv_tables['plot'][1]
                plt_settings.update(
                    {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  plt_settings['file_suffix'] + '_' + self.cfg.viv_tables['length'][table_index]['label'] + '.png'})
                viz.from_df_columns(self.viv_tables[table_index], plt_settings)
                viz.add_title_and_axis_labels()
                print("Saving {0} plot for percentage damage  ..".format(plt_settings['file_suffix']))
                viz.save_and_close()

            # % damage contribution plot
            for table_index in range(0, len(self.cfg.viv_tables['length'])):
                viz = Visualization()
                plt_settings = self.cfg.viv_tables['plot'][1]
                plt_settings.update(
                    {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                                  plt_settings['file_suffix'] + '_top_15_' + self.cfg.viv_tables['length'][table_index]['label'] + '.png'})
                df = self.viv_tables[table_index].sort_values(by=['percent_damage'], ascending=False).copy()
                if len(df) > 20:
                    df['file_index'] = df['file_index'] + 1
                    df['file_index'] = df['file_index'].apply(str)
                    viz.from_df_columns(df.head(15), plt_settings)
                    viz.add_title_and_axis_labels()
                    print("Saving {0} plot for percentage damage  ..".format(plt_settings['file_suffix']))
                    viz.save_and_close()

    def add_riser_length_to_shear7_result(self, df):
        x = list(df['x/L'])
        if hasattr(self,'riser_stack_up_properties'):
            logging.info("Utilizing riser stack up properties for Shear7 X calculations")
            xp = self.riser_stack_up_properties.shear7_bottom_x_by_L.tolist()
            fp = self.riser_stack_up_properties.component_bottom_elevation_above_mudline.tolist()
            result = np.interp(x, xp, fp)
        else:
            logging.info("Utilizing model_state_information from configuration file for Shear7 X calculations")
            model_length = self.cfg.model_state_information['from_dict']['model_length']
            model_start = self.cfg.model_state_information['from_dict']['model_start']
            result = [(item * model_length + model_start) for item in x]
        df['elevation_above_mudline'] = result

        return df

    def save_viv_fatigue_life_visualizations(self):
        viz = Visualization()
        for plot_index in range(0, len(self.cfg.viv_plots['combined'])):
            plt_settings = self.cfg.viv_plots['combined'][plot_index]
            plt_settings.update(
                {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' + plt_settings['file_suffix'] + '.png'})
            viz.from_df_columns(self.total_viv_fatigue, plt_settings)
            viz.add_title_and_axis_labels()
            viz.add_text_fields()
            viz.save_and_close()

        for plot_index in range(0, len(self.cfg.viv_plots['individual'])):
            plt_settings = self.cfg.viv_plots['individual'][plot_index]
            plt_settings.update({'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis'][
                'file_name'] + '_' + plt_settings['file_suffix'] + '.png'})
            if self.cfg.Shear7_sets[0].__contains__('label'):
                plt_settings.update({'array_label': [[item['label']] for item in self.cfg.Shear7_sets]})
            else:
                plt_settings.update({'array_label': [[str(item)] for item in range(0, len(self.cfg.Shear7_sets))]})
            viz.from_df_array(self.viv_plt_df_array, plt_settings)
            viz.add_title_and_axis_labels()
            if len(self.viv_plt_df_array) <= 10:
                viz.add_legend()
            viz.add_text_fields()
            viz.save_and_close()

    def get_wave_fatigue_result(self):
        self.read_orcaflex_wave_fatigue()
        self.add_riser_length_axis_to_orcaflex_result()


    def read_orcaflex_wave_fatigue(self):
        read_data = ReadData()
        cfg_temp = self.cfg['WaveFatigue_Sets'][0]
        self.total_wave_fatigue = read_data.xlsx_to_df_by_keyword_search(cfg_temp)

    def add_riser_length_axis_to_orcaflex_result(self):
        x = list(self.total_wave_fatigue['Arc Length'])
        xp = self.riser_stack_up_properties.component_bottom_elevation_from_end_A.tolist()
        fp = self.riser_stack_up_properties.component_bottom_elevation_above_mudline.tolist()
        result = np.interp(x, xp, fp)
        self.total_wave_fatigue['elevation_above_mudline'] = result

    def get_combined_fatigue(self):
        self.combined_fatigue = pd.DataFrame()
        self.combined_fatigue['elevation_above_mudline'] = self.total_viv_fatigue['elevation_above_mudline']
        self.combined_fatigue['total_viv_fatigue_damage'] = self.total_viv_fatigue['total_damage']
        self.combined_fatigue['total_wave_fatigue_damage'] = self.get_wave_fatigue_converted_to_viv_divisions()
        self.combined_fatigue['total_combined_fatigue_damage'] = self.total_viv_fatigue['total_damage'] + self.combined_fatigue['total_wave_fatigue_damage']

    def get_viv_histograms(self):
        self.viv_histogram_df_array = []
        for file_index in range(0, len(self.viv_plt_df_array)):
            histogram_file = self.get_yearly_histogram_object_from_viv_run(file_index)
            self.viv_histogram_df_array.append(histogram_file)

    def get_viv_cummulative_histograms(self):
        self.probability_array = []
        self.simulation_duration_array = []
        self.shear7_bins = list(
            range(self.cfg['histogram']['shear7']['range'][0], self.cfg['histogram']['shear7']['range'][1],
                  self.cfg['histogram']['shear7']['bins']))
        self.viv_cummulative_histograms = pd.DataFrame(self.shear7_bins, columns=['bins'])
        for label_index in range(0, len(self.viv_histogram_df_array[0].columns)-1):
            label = self.histogram_labels[label_index]
            self.viv_cummulative_histograms[label] = pd.Series([0] * len(self.shear7_bins))

        self.viv_file_label_array = []
        for file_index in range(0, len(self.cfg['Shear7_sets'])):
            file_label = 'cp_{:02d}' .format(file_index)
            self.viv_file_label_array.append(file_label)
            self.probability_array.append(self.cfg['Shear7_sets'][file_index]['probability'])
            for label_index in range(0, len(self.viv_histogram_df_array[0].columns)-1):
                label = self.histogram_labels[label_index]
                self.viv_cummulative_histograms[label] = self.viv_cummulative_histograms[label] + self.viv_histogram_df_array[file_index][label] * \
                                                         self.probability_array[file_index] / 100

        self.viv_file_label_array.append('cummulative_histograms')
        self.viv_histogram_df_array.append(self.viv_cummulative_histograms.copy())

        self.qa_histograms()
        self.viv_file_label_array.append('histograms_qa')
        self.viv_histogram_df_array.append(self.sum_df)

    def qa_histograms(self):
        from common.ETL_components import ETL_components
        etl_components = ETL_components(cfg=None)
        self.sum_df = etl_components.get_sum_df_from_df_array(self.viv_histogram_df_array)

    def save_viv_histograms(self):
        from common.data import SaveData
        save_data = SaveData()
        cfg_temp = {
            "FileName": self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_viv_histograms.xlsx',
            "SheetNames": self.viv_file_label_array,
            "thin_border": True}
        save_data.DataFrameArray_To_xlsx_openpyxl(self.viv_histogram_df_array, cfg_temp)


    def get_yearly_histogram_object_from_viv_run(self, file_index):
        self.shear7_bins = list(
            range(self.cfg['histogram']['shear7']['range'][0], self.cfg['histogram']['shear7']['range'][1],
                  self.cfg['histogram']['shear7']['bins']))
        histogram_file = pd.DataFrame(self.shear7_bins, columns=['bins'])
        try:
            natural_frequency = self.get_viv_fundamental_natural_frequency(file_index)
        except:
            natural_frequency = 0.00001
        natural_time_period = 1 / natural_frequency
        self.histogram_labels = []
        for line_item_index in range(0, len(self.riser_stack_up_properties)):
            if self.riser_stack_up_properties.iloc[line_item_index]['stack_length'] > 0:
                component = self.riser_stack_up_properties.iloc[line_item_index]['component']
                shear7_x_by_L = self.riser_stack_up_properties.iloc[line_item_index][
                    'shear7_x_by_L_at_max_damage']
                RadialPos = 'Outer'
                component_average_elevation_above_mudline = statistics.mean([
                    self.riser_stack_up_properties.iloc[
                        line_item_index][
                        'component_bottom_elevation_above_mudline'],
                    self.riser_stack_up_properties.iloc[
                        line_item_index][
                        'component_top_elevation_above_mudline']])
                label = component + ', ' + RadialPos + 'Dia, max_dam, @avg elev {:.1f}'.format(
                    component_average_elevation_above_mudline)

                self.histogram_labels.append(label)
                histogram_object = self.get_histogram_object_at_shear7_x_by_L_from_viv_run(file_index, shear7_x_by_L)
                transformed_histogram_object = list(map(lambda x: (x*(365.25*24*3600)/natural_time_period), histogram_object))
                histogram_file[label] = pd.Series(transformed_histogram_object, index=histogram_file.index)

        return histogram_file

    def get_histogram_object_at_shear7_x_by_L_from_viv_run(self, file_index, shear7_x_by_L):
        x = shear7_x_by_L
        if not self.viv_plt_df_array[file_index].empty:
            xp =self.viv_plt_df_array[file_index]['x/L'].tolist()
            fp = self.viv_plt_df_array[file_index]['RMS Stress'].tolist()
            rms_stress = np.interp(x, xp, fp)/1000
            stress_half_cycle = 2**(0.5)*rms_stress

            histogram_object = []
            for i in range(0, len(self.shear7_bins)):
                if (stress_half_cycle > self.shear7_bins[i]) and (stress_half_cycle < self.shear7_bins[i+1]):
                    histogram_object.append(1)
                else:
                    histogram_object.append(0)
        else:
            histogram_object = []
            for i in range(0, len(self.shear7_bins)):
                histogram_object.append(0)

        return histogram_object

    def get_viv_fundamental_natural_frequency(self, file_index):
        read_data = ReadData()
        cfg_temp = self.cfg['histogram']['shear7']['fundamental_natural_frequency']['from_ascii'].copy()
        io = self.cfg['Shear7_sets'][file_index]['io'][0:len(self.cfg['Shear7_sets'][file_index]['io'])-3] + 'out'
        cfg_temp.update({'io': io})
        # key_words = self.cfg['histogram']['shear7']['fundamental_natural_frequency']['from_ascii']['line']['key_words']
        # cfg_temp.update({'io': io, 'key_words': key_words})
        keyword_line_number = read_data.from_ascii_file_get_line_number_containing_keywords(cfg_temp)
        cfg_temp.update({'start_line': keyword_line_number, 'end_line': None})
        natural_frequency_text = read_data.from_ascii_file_get_value(cfg_temp)

        # 2nd stage delimiter operation
        from_string = FromString()
        cfg_temp = {'text': natural_frequency_text, 'delimiter': '(', 'column': 1, 'data_type': 'float'}
        natural_frequency = from_string.get_value_by_delimiter(cfg_temp)

        return natural_frequency

    def get_wave_cummulative_histograms(self):
        read_data = ReadData()
        cfg_temp = {'files': {'from_xlsx': [self.cfg['histogram']['wave']]}}
        self.wave_cummulative_histograms = read_data.from_xlsx(cfg_temp)

    def get_combined_histograms(self):
        self.combined_histograms = pd.DataFrame(columns = self.wave_cummulative_histograms.columns)
        self.combined_histograms['bins'] = self.wave_cummulative_histograms['bins']
        for label_index in range(0, len(self.wave_cummulative_histograms.columns)):
            label = self.wave_cummulative_histograms.columns[label_index]
            self.combined_histograms[label] = self.wave_cummulative_histograms[label] + self.viv_cummulative_histograms[label]

    def save_combined_histograms(self):
        from common.data import SaveData
        save_data = SaveData()
        df_array = [ self.combined_histograms, self.wave_cummulative_histograms, self.viv_cummulative_histograms]
        cfg_temp = {
            "FileName": self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_combined_histograms.xlsx',
            "SheetNames": ['combined_fatigue', 'wave_fatigue', 'viv fatigue'],
            "thin_border": True}
        save_data.DataFrameArray_To_xlsx_openpyxl(df_array, cfg_temp)

    def get_wave_fatigue_converted_to_viv_divisions(self):
        x = self.combined_fatigue.elevation_above_mudline.tolist()
        xp = self.total_wave_fatigue.elevation_above_mudline.tolist()
        xp.reverse()
        fp = list(self.total_wave_fatigue['Overall Damage'])
        fp.reverse()
        result = np.interp(x, xp, fp)

        return result

    def save_results_and_visualizations(self):
        self.add_fatigue_damage_results_to_df()
        self.save_fatigue_summary_result()
        self.save_visualizations()

    def save_fatigue_summary_result(self):
        save_data = SaveData()
        basefile_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name']
        self.combined_fatigue.to_csv(basefile_name + '_combined_fatigue.csv')
        self.riser_stack_up_properties.to_csv(basefile_name + '_damage_results.csv')
        columns_for_tabular_results = ['component','component_bottom_elevation_above_mudline', 'stack_length', 'StressOD', 'StressID', 'max_damage', 'elevation_above_mudline_at_max_damage', 'total_viv_max_damage', 'total_wave_max_damage']
        column_headers_for_tabular_results = ['Component','Bottom Elev above Mudline', 'Lenth', 'Stress OD', 'Stress ID', 'Max Damage', 'Max Damage Elev', 'VIV at Max Damage', 'Wave at Max Damage']
        tabular_results_df = self.riser_stack_up_properties[self.riser_stack_up_properties['stack_length']>0][columns_for_tabular_results].copy()
        tabular_decimal_array = pd.Series([0, 2, 1, 4, 4, 5, 2, 5, 5], index=tabular_results_df.columns.values)
        tabular_results_df = tabular_results_df.round(tabular_decimal_array)
        for row_index in range(0, len(tabular_results_df)):
            tabular_results_df.max_damage.iloc[row_index] = np.format_float_scientific(
                tabular_results_df.max_damage.iloc[row_index], unique=False, precision=2)
            tabular_results_df.total_viv_max_damage.iloc[row_index] = np.format_float_scientific(
                tabular_results_df.total_viv_max_damage.iloc[row_index], unique=False, precision=2)
            tabular_results_df.total_wave_max_damage.iloc[row_index] = np.format_float_scientific(
                tabular_results_df.total_wave_max_damage.iloc[row_index], unique=False, precision=2)

        tabular_results_df.columns = column_headers_for_tabular_results

        save_data.df_to_table_as_image(tabular_results_df, cfg={'file_name': basefile_name + '_damage_result_summary'})
        save_data.df_to_table_as_docx(tabular_results_df, cfg={'file_name': basefile_name + '_damage_result_summary'})
        tabular_results_df.to_csv(basefile_name + '_damage_result_summary.csv')

    def add_fatigue_damage_results_to_df(self):
        for line_item_index in range(0, len(self.riser_stack_up_properties)):
            if self.riser_stack_up_properties.iloc[line_item_index]['stack_length'] > 0:
                component_start = self.riser_stack_up_properties.iloc[line_item_index]['component_bottom_elevation_above_mudline']
                component_end = self.riser_stack_up_properties.iloc[line_item_index]['component_top_elevation_above_mudline']
                if line_item_index-1 >= 0:
                    if self.riser_stack_up_properties.iloc[line_item_index-1]['component'] == 'Upper FJ':
                        component_end = component_end - 1.0
                if line_item_index+1 < len(self.riser_stack_up_properties):
                    if self.riser_stack_up_properties.iloc[line_item_index+1]['component'] == 'Lower FJ':
                        component_start = component_start + 1.0



                damage_along_component = self.combined_fatigue['total_combined_fatigue_damage'][
                    (self.combined_fatigue['elevation_above_mudline'] > component_start) & (
                                self.combined_fatigue['elevation_above_mudline'] < component_end)]
                maximum_damage = damage_along_component.max()
                maximum_damage_index = damage_along_component.idxmax()

                self.riser_stack_up_properties.loc[line_item_index, 'max_damage'] = maximum_damage
                self.riser_stack_up_properties.loc[line_item_index, 'total_viv_max_damage'] = self.combined_fatigue.iloc[maximum_damage_index]['total_viv_fatigue_damage']
                self.riser_stack_up_properties.loc[line_item_index, 'total_wave_max_damage'] = self.combined_fatigue.iloc[maximum_damage_index]['total_wave_fatigue_damage']
                elevation_above_mudline_at_max_damage = self.combined_fatigue.iloc[maximum_damage_index]['elevation_above_mudline']
                self.riser_stack_up_properties.loc[line_item_index, 'elevation_above_mudline_at_max_damage'] = elevation_above_mudline_at_max_damage
                self.riser_stack_up_properties.loc[line_item_index, 'shear7_x_by_L_at_max_damage'] = self.get_shear7_x_by_L_from_elevation_above_mudline(elevation_above_mudline_at_max_damage)
                self.riser_stack_up_properties.loc[line_item_index, 'elevation_from_end_A_at_max_damage'] = self.get_elevation_from_end_A_from_elevation_above_mudline(elevation_above_mudline_at_max_damage)
            else:
                self.riser_stack_up_properties.loc[line_item_index, 'max_damage'] = None
                self.riser_stack_up_properties.loc[line_item_index, 'elevation_above_mudline_at_max_damage'] = None
                self.riser_stack_up_properties.loc[line_item_index, 'shear7_x_by_L_at_max_damage'] = None
                self.riser_stack_up_properties.loc[line_item_index, 'elevation_from_end_A_at_max_damage'] = None

    def get_shear7_x_by_L_from_elevation_above_mudline(self, x):
        xp = self.riser_stack_up_properties.component_bottom_elevation_above_mudline.tolist()
        xp.reverse()
        fp = self.riser_stack_up_properties.shear7_bottom_x_by_L.tolist()
        fp.reverse()
        result = np.interp(x, xp, fp)

        return result

    def get_elevation_from_end_A_from_elevation_above_mudline(self, x):
        xp = self.riser_stack_up_properties.component_bottom_elevation_above_mudline.tolist()
        xp.reverse()
        fp = self.riser_stack_up_properties.component_bottom_elevation_from_end_A.tolist()
        fp.reverse()
        result = np.interp(x, xp, fp)

        return result

    def save_visualizations(self):
        viz = Visualization()
        plt_settings = self.cfg['plot_settings'][0]
        plt_settings.update(
            {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '.png'})
        viz.from_df_columns(self.combined_fatigue, plt_settings)
        viz.add_title_and_axis_labels()
        viz.add_legend()
        # viz.plt.grid()
        viz.save_and_close()

    def orcaflex_wave_fatigue_analysis(self):
        cfg = self.cfg

        viz_data = Visualization()
        read_data = ReadData()
        save_data = SaveData()

        if cfg['default']['plot_fatigue_life']:
            print("Preparing wave fatigue life plots")
            dfArray = []
            dfSummary = pd.DataFrame(columns=['FileName'] + cfg['FatigueLifeReadingSets'][0]['Label'])
            for fileIndex in range(0, len(cfg['FatigueLifeReadingSets'])):
                customdata = {"FileName": cfg['FatigueLifeReadingSets'][fileIndex]['io'],
                              "SheetName": cfg['FatigueLifeReadingSets'][fileIndex]['sheet_name'],
                              "KeyWords": cfg['FatigueLifeReadingSets'][fileIndex]['KeyWords'],
                              "RowsToSkip": cfg['FatigueLifeReadingSets'][fileIndex]['RowsToSkip'],
                              "RowsToRead": cfg['FatigueLifeReadingSets'][fileIndex]['RowsToRead'],
                              "Columns": cfg['FatigueLifeReadingSets'][fileIndex]['Columns'],
                              "FactorOfSafety": cfg['FatigueLifeReadingSets'][fileIndex]['FactorOfSafety']
                              }

                df = read_data.superseded_xlsx_to_df_by_keyword_search(customdata)
                df['Life (years)'] = df['Life (years)'] / customdata['FactorOfSafety']

                #  For plot
                customdata = {"PltSupTitle": cfg['FatigueLifePlotSettings']['PltSupTitle'],
                              "PltTitle": cfg['FatigueLifePlotSettings']['PltTitle'],
                              "PltXLabel": cfg['FatigueLifePlotSettings']['PltXLabel'],
                              "PltYLabel": cfg['FatigueLifePlotSettings']['PltYLabel'],
                              'Label': cfg['FatigueLifePlotSettings']['PltLabel'],
                              'Axhline': [cfg['FatigueLifePlotSettings']['LifeLimit']],
                              'PltColumns': ['Arc Length', 'Life (years)'],
                              # 'Text': 'MAWP Reduced, Flaw dimension o: s={0} and c={1},  Min. Measured WT ={2}' .format(LMLSummaryDF.iloc[0]['s'], LMLSummaryDF.iloc[0]['c'], LMLSummaryDF.iloc[0]['tmm']),
                              'TextFields': [{"x": 2000, "y": 40, "Text": "Minimum Fatigue Life"}],
                              'FileName': cfg['Analysis']['result_folder'] + os.path.basename(
                                  cfg['FatigueLifeReadingSets'][fileIndex]['io']).replace(".xlsx", ""),
                              'XLimRanges': cfg['FatigueLifePlotSettings']['XLimRanges'],
                              'YLimRanges': cfg['FatigueLifePlotSettings']['YLimRanges']
                              }
                viz_data.plotCustomFatigue(df, customdata)

                dfArray.append(df)

                rowDF = []
                rowDF.append(customdata['FileName'])
                UniqueSNCurves = df['S-N Curve'].unique()
                for SNCurveIndex in range(0, len(UniqueSNCurves)):
                    rowDF.append(df[df['S-N Curve'] == UniqueSNCurves[SNCurveIndex]][customdata['PltColumns'][1]].min())

                dfSummary.loc[len(dfSummary)] = rowDF

            #  For plot
            customdata = {"PltSupTitle": cfg['FatigueLifePlotSettings']['PltSupTitle'],
                          "PltTitle": cfg['FatigueLifePlotSettings']['PltTitle'],
                          "PltXLabel": cfg['FatigueLifePlotSettings']['PltXLabel'],
                          "PltYLabel": cfg['FatigueLifePlotSettings']['PltYLabel'],
                          'Label': cfg['FatigueLifePlotSettings']['PltLabel'],
                          'Axhline': [cfg['FatigueLifePlotSettings']['LifeLimit']],
                          'PltColumns': ['Arc Length', 'Life (years)'],
                          # 'Text': 'MAWP Reduced, Flaw dimension o: s={0} and c={1},  Min. Measured WT ={2}' .format(LMLSummaryDF.iloc[0]['s'], LMLSummaryDF.iloc[0]['c'], LMLSummaryDF.iloc[0]['tmm']),
                          'TextFields': [{"x": 500, "y": 40, "Text": "Minimum Fatigue Life"}],
                          'FileName': cfg['Analysis']['result_folder'] + 'Life_' + cfg['Analysis']['file_name'],
                          'XLimRanges': cfg['FatigueLifePlotSettings']['XLimRanges'],
                          'YLimRanges': cfg['FatigueLifePlotSettings']['YLimRanges'],
                          'LineStyles': cfg['FatigueLifePlotSettings']['LineStyles'],
                          'Colors': cfg['FatigueLifePlotSettings']['Colors']
                          }
            viz_data.plotCustomFatigueComparison(dfArray, customdata, cfg)
            save_data.saveDataFrame(dfSummary, cfg['Analysis']['result_folder'] + cfg['Analysis']['file_name'])

        if cfg['default']['plot_damage_distribution']:
            damage_by_direction_array = []
            damage_by_direction_summed_df = pd.DataFrame()
            for fileIndex in range(0, len(cfg['DamageContribution']['ReadingSets'])):
                customdata = {"FileName": cfg['DamageContribution']['ReadingSets'][fileIndex]['io'],
                              "SheetName": cfg['DamageContribution']['ReadingSets'][fileIndex]['sheet_name'],
                              "KeyWords": cfg['DamageContribution']['ReadingSets'][fileIndex]['KeyWords'],
                              "RowsToSkip": cfg['DamageContribution']['ReadingSets'][fileIndex]['RowsToSkip'],
                              "RowsToRead": cfg['DamageContribution']['ReadingSets'][fileIndex]['RowsToRead'],
                              "Columns": cfg['DamageContribution']['ReadingSets'][fileIndex]['Columns']
                              }

                df = read_data.superseded_xlsx_to_df_by_keyword_search(customdata)
                print(df)
                print("Check dataframe for any format errors")
                max_series = df.loc[len(df) - 1, df.columns[3:19]]
                max_column = max_series[max_series == max_series.max()].index[0]
                sum_of_damage = df.ix[len(df) - 1, max_column]
                df_by_direction = pd.DataFrame()
                for set_index in range(0, len(
                        cfg['DamageContribution']['ReadingSets'][fileIndex]['direction_sets']['sets'])):
                    start_row = \
                    cfg['DamageContribution']['ReadingSets'][fileIndex]['direction_sets']['sets'][set_index][
                        'start_row']
                    end_row = cfg['DamageContribution']['ReadingSets'][fileIndex]['direction_sets']['sets'][set_index][
                        'end_row']
                    column = cfg['DamageContribution']['ReadingSets'][fileIndex]['direction_sets']['columns'][set_index]
                    df_by_direction[column] = df.ix[start_row:end_row, max_column].values / sum_of_damage * 100

                damage_by_direction_array.append(df_by_direction)
                damage_by_direction_sum_array_column = cfg['DamageContribution']['ReadingSets'][fileIndex]['label']
                damage_by_direction_summed_df[damage_by_direction_sum_array_column] = df_by_direction.sum().values
                damage_by_direction_summed_df.index = damage_by_direction_array[0].sum().index
                plt_settings = cfg['DamageContribution']['ReadingSets'][fileIndex]['viz']
                plt_settings.update({'file_name': cfg['Analysis']['result_folder'] + cfg['Analysis'][
                    'file_name'] + '_damage_by_direction_' + '{0}'.format(fileIndex) + '.png'})
                viz_data.from_df_get_plot(df_by_direction, plt_settings)

        plt_settings = cfg['DamageContribution']['viz']
        plt_settings.update({'file_name': cfg['Analysis']['result_folder'] + cfg['Analysis'][
            'file_name'] + '_damage_by_direction' + '.png'})
        viz_data.from_df_get_plot(damage_by_direction_summed_df, plt_settings)


