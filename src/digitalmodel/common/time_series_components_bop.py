import logging


class TimeSeriesComponents():
    # https://plot.ly/python/v3/fft-filters/
    # http://scipy-lectures.org/intro/scipy/auto_examples/plot_fftpack.html
    # https://dsp.stackexchange.com/questions/724/low-pass-filter-and-fft-for-beginners-with-python

    def __init__(self, cfg):
        self.cfg = cfg

    def get_raw_data(self):
        if self.cfg.default['data_source'] == 'db':
            self.get_environments()
            for env_index in range(0, len(self.environments)):
                # for env_index in range(0, 1):
                self.env = self.environments[env_index]
                db_properties = self.cfg.default['db'][self.env]
                try:
                    self.set_up_db_connection(db_properties)
                except Exception as e:
                    print("No connection to environment {0}, error {1}".format(self.env, e))
                try:
                    self.get_input_data()
                except Exception as e:
                    print("Error getting input data in environment {0}, error {1}".format(self.env, e))
        else:
            import sys
            print("No data source specified")
            sys.exit()

    def perform_db_analysis(self):
        try:
            cfg_analysis = self.cfg.default['analysis']['db'].copy()
            db_analysis_result = self.dbe.perform_analysis(cfg_analysis=cfg_analysis)
            self.write_db_analysis_results(db_analysis_result)
        except Exception as e:
            print("Error Performing cfg analysis in environment {0}, error {1}".format(self.env, e))

    def write_db_analysis_results(self, db_analysis_result):
        from common.data import SaveData
        save_data = SaveData()

        if db_analysis_result.__contains__('db_table_df'):
            file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis'][
                'file_name'] + '_db_tables.csv'
            db_table_df = db_analysis_result['db_table_df']
            db_table_df.to_csv(file_name)

        if db_analysis_result.__contains__('column_df_array_tables') and db_analysis_result.__contains__(
                'table_column_df_array'):
            file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis'][
                'file_name'] + '_db_columns.xlsx'
            column_df_array_tables = db_analysis_result['column_df_array_tables']
            table_column_df_array = db_analysis_result['table_column_df_array']
            temp_cfg_data = {'SheetNames': column_df_array_tables, 'FileName': file_name}
            save_data.DataFrameArray_To_xlsx_openpyxl(table_column_df_array, temp_cfg_data)

        if db_analysis_result.__contains__('extreme_events_df_array'):
            extreme_events_df_array = db_analysis_result['extreme_events_df_array']
            for df_index in range(0, len(extreme_events_df_array)):
                df = extreme_events_df_array[df_index]
                file_name = self.cfg['Analysis']['result_folder'] + self.cfg['Analysis'][
                    'file_name'] + '_extreme_events_{}.csv'.format(df_index)
                # TODO save to db
                df.to_csv(file_name)

    def get_input_data(self):
        if self.cfg.default.__contains__('input_data'):
            cfg_input = self.cfg.default['input_data'].copy()
            if cfg_input['source'] == 'db':
                self.dbe.get_input_data(cfg_input)
            else:
                print("No input data source defined")
        else:
            print("No input data in configuration")

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

    def perform_fft_analysis(self):
        '''
        fft or spectral analysis
        '''
        for set_index in range(0, len(self.cfg.default['input_data']['sets'])):
            set_info = self.cfg.default['input_data']['sets'][set_index]
            df = getattr(self.dbe, 'input_data_' + set_info['label'])
            self.fft_analysis_for_df(df, set_info)

    def fft_analysis_for_df(self, df, set_info):
        import pandas as pd
        columns = df.columns
        fft_filtered_signal_df = pd.DataFrame(columns=columns)

        for col_index in range(0, len(columns)):
            if col_index == 0:
                fft_filtered_signal_df[columns[col_index]] = df[columns[col_index]]
            else:
                signal = df[columns[col_index]]
                fft_df, filtered_signal = self.fft_analysis(signal)
                average_fft_df = self.fft_window_analysis(signal)
                fft_filtered_signal_df[columns[col_index]] = filtered_signal
                setattr(self, 'output_rms_signal' + set_info['label'] + '_' + columns[col_index], signal.std())
                print('output_rms_signal' + set_info['label'] + '_' + columns[col_index] + ':' + str(signal.std()))
                setattr(self, 'output_rms_filtered_signal' + set_info['label'] + '_' + columns[col_index],
                        filtered_signal.std())
                print(self, 'output_rms_filtered_signal' + set_info['label'] + '_' + columns[col_index] + ':' + str(filtered_signal.std()))
                setattr(self, 'output_fft_' + set_info['label'] + '_' + columns[col_index], fft_df)
                setattr(self, 'output_average_fft_' + set_info['label'] + '_' + columns[col_index], average_fft_df)
        setattr(self, 'output_fft_filtered_signal_' + set_info['label'], fft_filtered_signal_df)

    def fft_window_analysis(self, signal, time_window=None):
        import math
        import pandas as pd
        cfg_fft = self.cfg['default']['analysis']['fft'].copy()
        average_fft_df = pd.DataFrame()
        peak_indices = []
        if time_window is None and cfg_fft.__contains__('window'):
            time_window = cfg_fft['window']['size']
            if len(signal) > 1.5 * time_window:
                number_of_windows = math.floor(len(signal) / time_window)
                for window_index in range(0, number_of_windows):
                    window_fft, filtered_window_fft = self.fft_analysis(
                        signal[window_index * time_window: (window_index + 1) * time_window])
                    if window_index == 0:
                        sum_window_fft = window_fft.copy()
                    else:
                        sum_window_fft['power'] = sum_window_fft['power'] + window_fft['power']

                average_fft_df = sum_window_fft.copy()
                average_fft_df['power'] = average_fft_df['power'] / (number_of_windows + 1)

                if cfg_fft['window']['moving_average']['flag']:
                    rolling_window_size = cfg_fft['window']['moving_average']['window_size']
                    average_fft_df['power'] = average_fft_df['power'].rolling(window=rolling_window_size).mean()

                average_fft_df.fillna(value=0, inplace=True)
                if cfg_fft.__contains__('peaks') and cfg_fft['peaks']['flag']:
                    cfg_peaks = cfg_fft['peaks'].copy()
                    x_list = average_fft_df['fft_freq'].to_list()
                    y_list = average_fft_df['power'].to_list()
                    peak_indices = self.identify_signal_peaks(x_list, y_list, cfg_peaks)

                average_fft_df['peak_flag'] = False
                for peak_index in peak_indices:
                    average_fft_df['peak_flag'].iloc[peak_index] = True

            else:
                import sys
                print("No FFT Analysis performed. Check data")
                sys.exit()

        else:
            print("Error: FFT Average analysis time_window is not available")

        return average_fft_df

    def identify_signal_peaks(self, x_list, y_list, cfg_peaks):
        import scipy.signal
        import numpy as np
        import math
        peak_indices = []
        if cfg_peaks['solver'] in ['peaks_cwt']:
            indexes = scipy.signal.find_peaks_cwt(y_list, np.arange(1, 4), max_distances=np.arange(1, 4) * 2)
            indexes = np.array(indexes) - 1
            peak_indices = indexes
        elif cfg_peaks['solver'] in ['argrelextrema']:
            indexes = scipy.signal.argrelextrema(np.array(y_list), comparator=np.greater, order=2)
            peak_indices = indexes[0]
        elif cfg_peaks['solver'] in ['find_peaks', None]:
            min_height = max(y_list) * cfg_peaks['min_height_percentage'] / 100
            min_distance = math.floor(len(x_list) * cfg_peaks['min_distance_index_percentage'] / 100)
            indexes, _ = scipy.signal.find_peaks(y_list, height=min_height, distance=min_distance)
            peak_indices = indexes

        return peak_indices

    def fft_analysis(self, signal, time_step=None):
        import numpy as np
        import scipy.fftpack
        import pandas as pd

        cfg_fft = self.cfg['default']['analysis']['fft'].copy()
        if time_step is None:
            time_step = cfg_fft['time_step']

        sig_fft = scipy.fftpack.fft(signal)
        power = np.abs(sig_fft) ** 2
        if type(signal) is list:
            fft_freq = scipy.fftpack.fftfreq(len(signal), d=time_step)
        else:
            fft_freq = scipy.fftpack.fftfreq(signal.size, d=time_step)

        filtered_sig_fft = sig_fft.copy()
        if cfg_fft['filter'].__contains__('band_pass') and cfg_fft['filter']['band_pass']['flag']:
            min_freq = cfg_fft['filter']['band_pass']['frequency_minimum']
            max_freq = cfg_fft['filter']['band_pass']['frequency_maximum']
            index_selected = (fft_freq > min_freq) & (fft_freq < max_freq)
            filtered_sig_fft[(np.abs(fft_freq) <= min_freq) | (np.abs(fft_freq) >= max_freq)] = 0
        else:
            index_selected = None

        columns = ['power', 'fft_freq']
        fft_df = pd.DataFrame(columns=columns)
        if index_selected is None:
            fft_df['power'] = power
            fft_df['fft_freq'] = fft_freq
        else:
            fft_df['power'] = power[index_selected]
            fft_df['fft_freq'] = fft_freq[index_selected]

        fft_df.fillna(value=0, inplace=True)

        filtered_signal = np.real(scipy.fftpack.ifft(filtered_sig_fft))

        return fft_df, filtered_signal

    def simple_moving_average(self):
        for set_index in range(0, len(self.cfg.default['input_data']['sets'])):
            set_info = self.cfg.default['input_data']['sets'][set_index]
            df = getattr(self.dbe, 'input_data_' + set_info['label'])
            setattr(self, 'output_sma_' + set_info['label'], self.get_sma_for_df(df.copy()))

    def get_sma_for_df(self, df):
        '''
        Simple moving average for a dataframe
        First column is assumed to be always time/index and is not processed
        '''
        import pandas as pd
        cfg_moving_avg = self.cfg['default']['analysis']['moving_average'].copy()
        columns = df.columns
        moving_avg_df = pd.DataFrame(columns=columns)
        if cfg_moving_avg['settings'].__contains__('by_data_points'):
            window_size = cfg_moving_avg['settings']['by_data_points']['window_size']
            for col_index in range(0, len(columns)):
                if col_index == 0:
                    moving_avg_df[columns[col_index]] = df[columns[col_index]]
                else:
                    moving_avg_df[columns[col_index]] = df[columns[col_index]].rolling(window=window_size).mean()

        return moving_avg_df

    def filter_by_value(self):
        '''
        Value filtering for a dataframe
        Accepts range and
        '''
        for set_index in range(0, len(self.cfg.default['input_data']['sets'])):
            set_info = self.cfg.default['input_data']['sets'][set_index]
            df = getattr(self.dbe, 'input_data_' + set_info['label'])
            df_statistics = getattr(self, 'output_stats_' + set_info['label'])
            df_filter_by_value = self.filter_by_value_for_df(df.copy(), df_statistics)
            setattr(self, 'output_f_by_v_' + set_info['label'], df_filter_by_value)

    def filter_by_value_for_df(self, df, df_statistics):
        import numpy as np
        cfg_filter = self.cfg['default']['analysis']['filter'].copy()
        columns = df.columns
        if cfg_filter.__contains__('range'):
            if '-sigma' in cfg_filter['range'][0]:
                for col_index in range(0, len(columns)):
                    if col_index != 0:
                        floor = cfg_filter['sigma_range'][0] * \
                                df_statistics[df_statistics['statistic'] == 'stdev'][columns[col_index]].values[0]
                        df[columns[col_index]][df[columns[col_index]] < floor] = np.nan
            if 'sigma' in cfg_filter['range'][0]:
                for col_index in range(0, len(columns)):
                    if col_index != 0:
                        ceiling = cfg_filter['sigma_range'][1] * \
                                  df_statistics[df_statistics['statistic'] == 'stdev'][columns[col_index]].values[0]
                        df[columns[col_index]][df[columns[col_index]] > ceiling] = np.nan

        return df

    def perform_statistics(self):
        for set_index in range(0, len(self.cfg.default['input_data']['sets'])):
            set_info = self.cfg.default['input_data']['sets'][set_index]
            df = getattr(self.dbe, 'input_data_' + set_info['label'])
            setattr(self, 'output_stats_' + set_info['label'], self.get_statistics_for_df(df.copy()))

    def get_statistics_for_df(self, df):
        import pandas as pd
        cfg_statistics = self.cfg['default']['analysis']['statistics'].copy()
        columns = df.columns
        statistics_df = pd.DataFrame(columns=['statistic'] + columns.to_list())
        statistics_array = ['minimum'] + df[columns.to_list()].min().to_list()
        statistics_df.loc[len(statistics_df)] = statistics_array
        statistics_array = ['maximum'] + df[columns.to_list()].max().to_list()
        statistics_df.loc[len(statistics_df)] = statistics_array
        statistics_array = ['mean', None] + df[columns.to_list()].mean().to_list()
        statistics_df.loc[len(statistics_df)] = statistics_array
        statistics_array = ['stdev', None] + df[columns.to_list()].std().to_list()
        statistics_df.loc[len(statistics_df)] = statistics_array

        return statistics_df

    def prepare_visualizations(self):
        from common.visualization_components import VisualizationComponents
        vc = VisualizationComponents(self.cfg)
        vc.prepare_visualizations(self)

    def perform_custom_calculation_1(self):
        '''
        Disys resultant acceleration from orthogonal components
        '''
        cfg_cus_calc = self.cfg['default']['analysis']['custom_calculation_1'].copy()
        for data_set_index in range(0, len(cfg_cus_calc['sets'])):
            data_set_cfg = cfg_cus_calc['sets'][data_set_index]
            if data_set_cfg['df'] == 'input_data_table_statistics':
                df = self.dbe.prepare_input_statistics(data_set_cfg)
                df['resultant'] = (df[data_set_cfg['y'][0]] * df[data_set_cfg['y'][0]] + df[data_set_cfg['y'][1]] * df[
                    data_set_cfg['y'][1]])**0.5
                setattr(self, 'output_data_' + data_set_cfg['label'], df)

    def perform_custom_calculation_2(self):
        '''
        Disys displacement from angle and also resultant displacement from orthogonal components.
        '''
        import math
        cfg_cus_calc = self.cfg['default']['analysis']['custom_calculation_2'].copy()
        for data_set_index in range(0, len(cfg_cus_calc['sets'])):
            data_set_cfg = cfg_cus_calc['sets'][data_set_index]
            if data_set_cfg['df'] == 'input_data_table_statistics':
                df = self.dbe.prepare_input_statistics(data_set_cfg)
                df['forward_displacement'] = data_set_cfg['arc_length'] * df[data_set_cfg['y'][0]].apply(math.tan)
                df['starboard_displacement'] =data_set_cfg['arc_length'] * df[data_set_cfg['y'][1]].apply(math.tan)
                df['resultant'] = (df['forward_displacement'] ** 2 + df['starboard_displacement'] ** 2) ** 0.5
                setattr(self, 'output_data_' + data_set_cfg['label'], df)
            else:
                df = getattr(self.dbe, data_set_cfg['df'])
                df['forward_displacement'] = data_set_cfg['arc_length'] * df[data_set_cfg['y'][0]].apply(math.tan)
                df['starboard_displacement'] =data_set_cfg['arc_length'] * df[data_set_cfg['y'][1]].apply(math.tan)
                df['resultant'] = (df['forward_displacement'] ** 2 + df['starboard_displacement'] ** 2) ** 0.5
                setattr(self, 'output_data_' + data_set_cfg['label'], df)


    def perform_integration(self):
        '''
        fft or spectral analysis
        '''
        for set_index in range(0, len(self.cfg.default['input_data']['sets'])):
            data_set_info = self.cfg.default['input_data']['sets'][set_index]
            df = getattr(self.dbe, 'input_data_' + data_set_info['label'])
            self.integration_for_df(df, data_set_info)

    def integration_for_df(self, df, data_set_info):
        cfg_integration = self.cfg['default']['analysis']['integration'].copy()
        from scipy import integrate
        columns = df.columns
        for column_index in range(1, len(columns)):
            dx = df[columns[0]].diff().min().microseconds/1000000
            column_name =columns[column_index]
            y = df[column_name].to_list()
            if cfg_integration['solver'] in ['trapz']:
                df['single_integration_' + column_name] = integrate.cumtrapz(y, dx=dx, initial=0)
                if cfg_integration.__contains__('double'):
                    y = df['single_integration_' + column_name].to_list()
                    df['double_integration_' + column_name] = integrate.cumtrapz(y, dx=dx, initial=0)

        setattr(self, 'output_integration_' + data_set_info['label'], df)

    def get_sample_time_series(self):
        import numpy as np
        from matplotlib import pyplot as plt
        # Seed the random number generator
        np.random.seed(1234)
        time_step = 0.02
        peak_period = 5.
        time_vector = np.arange(0, 20, time_step)
        sig = (np.sin(2 * np.pi / peak_period * time_vector)
               + 0.5 * np.random.randn(time_vector.size))

        plt.figure(figsize=(6, 5))
        plt.plot(time_vector, sig, label='Original signal')

        return time_vector, sig, time_step, peak_period

    def run_example(self):
        import numpy as np
        from scipy import fftpack
        from matplotlib import pyplot as plt

        time_vector, signal, time_step, peak_period = self.get_sample_time_series()
        power, sample_freq, sig_fft = self.fft_analysis(signal, time_step)

        # Find the peak frequency: we can focus on only the positive frequencies
        pos_mask = np.where(sample_freq > 0)
        freqs = sample_freq[pos_mask]
        peak_freq = freqs[power[pos_mask].argmax()]

        # Check that it does indeed correspond to the frequency that we generate
        # the signal with
        np.allclose(peak_freq, 1. / peak_period)

        # An inner plot to show the peak frequency
        axes = plt.axes([0.55, 0.3, 0.3, 0.5])
        plt.title('Peak frequency')
        plt.plot(freqs[:8], power[:8])
        plt.setp(axes, yticks=[])

        # scipy.signal.find_peaks_cwt can also be used for more advanced
        # peak detection
        high_freq_fft = sig_fft.copy()
        high_freq_fft[np.abs(sample_freq) > peak_freq] = 0
        filtered_sig = fftpack.ifft(high_freq_fft)

        # Filtering the high frequencies
        plt.figure(figsize=(6, 5))
        plt.plot(time_vector, signal, label='Original signal')
        plt.plot(time_vector, filtered_sig, linewidth=3, label='Filtered signal')
        plt.xlabel('Time [s]')
        plt.ylabel('Amplitude')

        plt.legend(loc='best')

        # Note This is actually a bad way of creating a filter: such brutal cut-off in frequency space does not control distorsion on the signal.
        # Filters should be created using the scipy filter design code

        plt.show()
