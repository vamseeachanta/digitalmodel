import logging


class VisualizationComponents():
    # https://plot.ly/python/v3/fft-filters/
    # http://scipy-lectures.org/intro/scipy/auto_examples/plot_fftpack.html
    # https://dsp.stackexchange.com/questions/724/low-pass-filter-and-fft-for-beginners-with-python

    def __init__(self, cfg):
        self.cfg = cfg

    def get_raw_data(self):
        import logging
        if self.cfg.default['input_data']['source'] == 'db':
            self.get_environments()
            for env_index in range(0, len(self.environments)):
                # for env_index in range(0, 1):
                self.env = self.environments[env_index]
                db_properties = self.cfg.default['db'][self.env]
                self.set_up_db_connection(db_properties)
                try:
                    self.get_input_data()
                except Exception as e:
                    print("Error encountered: {}".format(e))
                    logging.info(str(e))
                    print("No connection to environment {}".format(self.env))
        else:
            import sys
            print("No data source specified")
            sys.exit()

    def get_input_data(self):
        cfg_input = self.cfg.default['input_data'].copy()
        if cfg_input['source'] == 'db':
            self.dbe.get_input_data_from_db(cfg_input)
        else:
            print("No input data source defined")

    def get_environments(self):
        self.environments = list(self.cfg.default['db'].keys())

    def set_up_db_connection(self, db_properties):
        from common.database import Database
        self.dbe = Database(db_properties)
        try:
            self.dbe.enable_connection_and_cursor()
            return True
        except Exception as e:
            print("Error as {}".format(e))
            print("No connection for environment: {}".format(db_properties))
            return False

    def save_raw_data(self):
        if self.cfg.default['input_data'].__contains__('save') and self.cfg.default['input_data']['save']['flag']:
            from common.data import SaveData
            save_data = SaveData()
            cfg_input = self.cfg.default['input_data'].copy()
            sheet_array = []
            df_array = []
            for set_index in range(0, len(cfg_input['sets'])):
                set_info = cfg_input['sets'][set_index]
                sheet_array.append(set_info['label'])
                df = getattr(self.dbe, 'input_data_' + set_info['label'])
                if cfg_input['save']['to_csv']:
                    file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis'][
                        'file_name'] + '_' + sheet_array[set_index] + '.csv'
                    df.to_csv(file_name, index=False)
                df_array.append(df)

            if cfg_input['save']['to_xlsx']:
                cfg_temp = {
                    'SheetNames': sheet_array,
                    'FileName': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '.xlsx'
                }
                save_data.DataFrameArray_To_xlsx_openpyxl(df_array, cfg_temp)

    def prepare_visualizations(self, app_object=None):
        if self.cfg.__contains__('plot_settings'):
            self.prepare_single()
        if self.cfg.__contains__('plot_multiple'):
            self.prepare_multiple(app_object)

    def prepare_single(self):
        from common.visualizations import Visualization
        viz_data = Visualization()

        for plt_index in range(0, len(self.cfg['plot_settings'])):
            plt_settings = self.cfg['plot_settings'][plt_index]
            df = self.prepare_plot_input_data(plt_index, plt_settings)

            plt_settings.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                    plt_settings['file_name_extension'] + '.png'
            })

            for data_index in range(0, len(plt_settings['data'])):
                plt_settings_temp = plt_settings['data'][data_index].copy()
                viz_data.from_df_columns(df, plt_settings_temp)

            viz_data.add_title_and_axis_labels()
            viz_data.add_legend()
            viz_data.add_x_y_lim_formats()
            viz_data.add_reference_lines_and_spans()
            viz_data.save_and_close()

    def prepare_plot_input_data(self, data_set_cfg, app_object=None):
        if data_set_cfg['df'] == 'input_data_table_statistics':
            df = self.dbe.prepare_input_statistics(data_set_cfg)
        elif app_object is None:
            df = getattr(self.dbe, data_set_cfg['df'])
        else:
            if 'output' in data_set_cfg['df']:
                df = getattr(app_object, data_set_cfg['df'])
            elif 'input_data_' in data_set_cfg['df']:
                if hasattr(app_object, 'dbe'):
                    df = getattr(app_object.dbe, data_set_cfg['df'])
                elif hasattr(app_object, data_set_cfg['df']):
                    df = getattr(app_object, data_set_cfg['df'])
            else:
                import pandas as pd
                print("Data frame with label: {} can not be found in class object".format(data_set_cfg['df']))
                df = pd.DataFrame()

        df = self.get_filtered_df(data_set_cfg, df)
        df = self.get_scaled_df(data_set_cfg, df)

        return df

    def get_filtered_df(self, data_set_cfg, df):
        from common.data import ReadData
        read_data = ReadData()
        df = df.copy()
        if data_set_cfg.__contains__('filter'):
            df = read_data.df_filter_by_column_values(data_set_cfg.copy(), df)
        return df.copy()

    def get_scaled_df(self, data_set_cfg, df):
        df = df.copy()
        if data_set_cfg.__contains__('scale'):
            for column_index in range(0, len(data_set_cfg['scale']['columns'])):
                column_name = data_set_cfg['scale']['columns'][column_index]
                df[column_name] = df[column_name] * data_set_cfg['scale']['factors'][column_index]
        return df

    def prepare_multiple(self, app_object):
        from common.visualizations import Visualization
        self.viz_data = Visualization()

        for mult_plt_index in range(0, len(self.cfg['plot_multiple'])):
            self.prepare_a_multiple_plot(mult_plt_index, app_object)

    def prepare_a_multiple_plot(self, mult_plt_index, app_object=None):
        cfg_mult = self.cfg['plot_multiple'][mult_plt_index].copy()
        if cfg_mult.__contains__('nrows'):
            nrows = cfg_mult['nrows']
        else:
            nrows = len(cfg_mult['sets'])
        if cfg_mult.__contains__('ncols'):
            ncols = cfg_mult['ncols']
        else:
            ncols = 1

        if cfg_mult['file_name_extension'] is not None:
            cfg_mult.update({
                'file_name':
                    self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '_' +
                    cfg_mult['file_name_extension'] + '.png'
            })
        else:
            cfg_mult.update(
                {'file_name': self.cfg['Analysis']['result_folder'] + self.cfg['Analysis']['file_name'] + '.png'})
        self.viz_data.multiple_plots(cfg_mult)
        for plt_index in range(0, nrows * ncols):
            plt_settings = cfg_mult['sets'][plt_index]
            self.viz_data.autoselect_current_subplot(plt_index)
            if not plt_settings.__contains__('data'):
                self.plotting_for_no_data_object(plt_settings, app_object)
            else:
                for data_set_index in range(0, len(plt_settings['data'])):
                    data_set_cfg = plt_settings['data'][data_set_index]
                    self.plot_a_data_set(app_object, data_set_cfg)

            self.viz_data.add_title_and_axis_labels(plt_settings)
            self.viz_data.add_x_y_lim_formats()
            self.viz_data.resolution_adjust()
            self.viz_data.add_reference_lines_and_spans()
        self.viz_data.save_and_close()

    def plot_a_data_set(self, app_object, data_set_cfg):
        try:
            df = self.prepare_plot_input_data(data_set_cfg, app_object)
            if len(df) > 0:
                if data_set_cfg['x'].__contains__('fft_freq'):
                    if data_set_cfg.__contains__('color'):
                        color = data_set_cfg['color']
                    else:
                        color = None
                    cfg_text = self.add_fft_peaks_notes(df, color)
                    data_set_cfg.update(cfg_text)
                self.viz_data.plot_subplot(df, data_set_cfg)
                self.viz_data.add_legend()
                self.viz_data.add_text_fields()
        except Exception as e:
            print("See Error below. Can not process {}. ".format(str(data_set_cfg)))
            print("Error: {}".format(e))

    def plotting_for_no_data_object(self, plt_settings, app_object):
        if plt_settings['df_array']['flag']:
            df_array_label = plt_settings['df_array']['variable']
            df_array_dict = getattr(app_object, df_array_label)
            df_labels = list(df_array_dict.keys())
            for data_set_index in range(0, len(df_labels)):
                data_set_cfg = plt_settings['df_array'].copy()
                label = df_labels[data_set_index]
                data_set_cfg.update({'label': [label]})
                app_object.output_df_temp_from_df_array = df_array_dict[label]
                self.plot_a_data_set(app_object, data_set_cfg)
        else:
            import sys
            print("Could not find data object. So exiting application without plotting")
            sys.exit()

    def add_fft_peaks_notes(self, df, color=None):
        notes_array = []
        notes_df = df[df['peak_flag'] == True].copy()
        notes_df.reset_index(inplace=True, drop=True)
        for notes_index in range(0, len(notes_df)):
            x = notes_df['fft_freq'].iloc[notes_index]
            y = notes_df['power'].iloc[notes_index]
            notes_element = {'x': x, 'y': y, 'text': ' {0} s, {1} Hz'.format(round(1 / x, 2), round(x, 2))}
            notes_element.update({'color': color})
            notes_array.append(notes_element)

        cfg_text = {'text_fields': notes_array}
        return cfg_text

    def run_example(self):
        pass
        # TBA
