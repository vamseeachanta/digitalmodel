"""
DEPRECATED: This module is being replaced by digitalmodel.signal_processing.signal_analysis

Please migrate to the new signal analysis module:
- RainflowCounter for rainflow counting
- SpectralAnalyzer for FFT/spectral analysis
- FatigueDamageCalculator for fatigue calculations

See migration guide: docs/migration/signal_analysis_migration.md
"""

# Standard library imports
import array
import logging
import math
import os
import warnings

# Issue deprecation warning
warnings.warn(
    "TimeSeriesComponents is deprecated. Use digitalmodel.signal_processing.signal_analysis instead",
    DeprecationWarning,
    stacklevel=2
)

# Third party imports
import numpy as np
import pandas as pd
import rainflow
import scipy.fftpack
from assetutilities.common.data import PandasChainedAssignent
from assetutilities.common.data_management import DataManagement
from assetutilities.common.utilities import is_file_valid_func
from assetutilities.common.visualization.visualization_templates_matplotlib import (
    VisualizationTemplates,
)
from assetutilities.engine import engine as au_engine
from colorama import Fore, Style
from colorama import init as colorama_init

colorama_init()
dm = DataManagement()
viz_templates = VisualizationTemplates()


class TimeSeriesComponents:
    # https://plot.ly/python/v3/fft-filters/
    # http://scipy-lectures.org/intro/scipy/auto_examples/plot_fftpack.html
    # https://dsp.stackexchange.com/questions/724/low-pass-filter-and-fft-for-beginners-with-python

    def __init__(self, cfg=None):
        self.cfg = cfg

    def window_averaged_fft(self, cfg):
        """
        Window average fft
        """
        for group_cfg in cfg["data"]["groups"]:
            if cfg["data"]["type"] == "csv":
                analysis_root_folder = cfg["Analysis"]["analysis_root_folder"]
                file_is_valid, valid_file = is_file_valid_func(
                    group_cfg["file_name"], analysis_root_folder
                )
                if not file_is_valid:
                    logging.error(
                        FileNotFoundError(
                            f'Invalid file name/path: {group_cfg["file_name"]}'
                        )
                    )
                    logging.error(
                        f'Please check the file name/path in the input file: {group_cfg["file_name"]}'
                    )
                    logging.error(
                        f"Program {Fore.RED}continues to run ...{Style.RESET_ALL}"
                    )

                else:
                    df = pd.read_csv(valid_file)
                    df = dm.get_filtered_df(group_cfg, df)
                    df = dm.get_transformed_df(group_cfg, df)

                    signal = df[group_cfg["columns"]["signal"]].to_list()
                    time = df[group_cfg["columns"]["time"]].to_list()
                    time_step = time[1] - time[0]
                    average_fft_df = self.fft_window_analysis(cfg, signal, time_step)
                    average_fft_df = self.filter_spectrum(cfg, average_fft_df)
                    signal_dict = {"time_trace": signal, "fft": average_fft_df}
                    filtered_signal_dict = None
                    label = group_cfg["label"]
                    cfg = self.save_fft_results(
                        cfg, signal_dict, filtered_signal_dict, label
                    )

    def sample_window_averaged_fft(self, cfg):
        """
        Sample Window average fft
        """
        time_vector, signal, time_step, peak_period = self.get_sample_time_series(cfg)
        average_fft_df = self.fft_window_analysis(cfg, signal, time_step)
        signal_dict = {"time_trace": signal, "fft": average_fft_df}
        filtered_signal_dict = None
        cfg = self.save_fft_results(cfg, signal_dict, filtered_signal_dict)
        self.plot_fft_results(cfg)

        return signal_dict, filtered_signal_dict

    def sample_fft(self, cfg):
        """
        Sample analysis for time series
        """
        time_vector, signal, time_step, peak_period = self.get_sample_time_series(cfg)
        signal_dict, filtered_signal_dict = self.run_simple_fft_example(
            cfg, signal, time_step
        )

        cfg = self.save_fft_results(cfg, signal_dict, filtered_signal_dict)
        self.plot_fft_results(cfg)

        return signal_dict, filtered_signal_dict

    def save_fft_results(self, cfg, signal_dict, filtered_signal_dict, label=None):

        signal_fft = signal_dict["fft"]
        if label is not None:
            file_name = (
                cfg["Analysis"]["file_name_for_overwrite"]
                + label
                + "_signal_fft"
                + ".csv"
            )
        else:
            file_name = cfg["Analysis"]["file_name_for_overwrite"] + "_signal_fft.csv"
        file_name = os.path.join(cfg["Analysis"]["result_folder"], file_name)
        signal_fft.to_csv(file_name, index=False)
        signal_fft_file_name = file_name

        if filtered_signal_dict is not None:
            filtered_signal_fft = filtered_signal_dict["fft"]
            if label is not None:
                file_name = (
                    cfg["Analysis"]["file_name_for_overwrite"]
                    + label
                    + "_filtered_signal_fft"
                    + ".csv"
                )
            else:
                file_name = (
                    cfg["Analysis"]["file_name_for_overwrite"]
                    + "_filtered_signal_fft.csv"
                )
            file_name = os.path.join(cfg["Analysis"]["result_folder"], file_name)
            filtered_signal_fft.to_csv(file_name, index=False)
            filtered_signal_fft_file_name = file_name
        else:
            filtered_signal_fft_file_name = None

        dict = {
            cfg["basename"]: {
                "csv": {
                    "signal_fft": signal_fft_file_name,
                    "filtered_signal_fft": filtered_signal_fft_file_name,
                }
            }
        }
        cfg.update(dict)

        return cfg

    def plot_fft_results(self, cfg):

        plot_yml = viz_templates.get_xy_line_csv(cfg["Analysis"].copy())
        file_name = cfg[cfg["basename"]]["csv"]["signal_fft"]
        csv_groups = [{"file_name": file_name, "label": ""}]

        plot_yml["data"]["groups"] = csv_groups
        columns = {"x": ["fft_freq"], "y": ["power"]}
        plot_yml["master_settings"]["groups"]["columns"] = columns

        settings = {
            "file_name": cfg["Analysis"]["file_name_for_overwrite"] + "_signal_fft",
            "title": "Frequency Analysis",
            "xlabel": "Frequency [Hz]",
            "ylabel": "Power",
        }
        plot_yml["settings"].update(settings)
        au_engine(inputfile=None, cfg=plot_yml, config_flag=False)

    def perform_fft_analysis(self):
        """
        fft or spectral analysis
        """
        for set_index in range(0, len(self.cfg.default["input_data"]["sets"])):
            set_info = self.cfg.default["input_data"]["sets"][set_index]
            df = getattr(self.dbe, "input_data_" + set_info["label"])
            self.fft_analysis_for_df(df, set_info)

    def fft_analysis_for_df(self, df, set_info):
        # Third party imports
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
                setattr(
                    self,
                    "output_rms_signal" + set_info["label"] + "_" + columns[col_index],
                    signal.std(),
                )
                print(
                    "output_rms_signal"
                    + set_info["label"]
                    + "_"
                    + columns[col_index]
                    + ":"
                    + str(signal.std())
                )
                setattr(
                    self,
                    "output_rms_filtered_signal"
                    + set_info["label"]
                    + "_"
                    + columns[col_index],
                    filtered_signal.std(),
                )
                print(
                    self,
                    "output_rms_filtered_signal"
                    + set_info["label"]
                    + "_"
                    + columns[col_index]
                    + ":"
                    + str(filtered_signal.std()),
                )
                setattr(
                    self,
                    "output_fft_" + set_info["label"] + "_" + columns[col_index],
                    fft_df,
                )
                setattr(
                    self,
                    "output_average_fft_"
                    + set_info["label"]
                    + "_"
                    + columns[col_index],
                    average_fft_df,
                )
        setattr(
            self,
            "output_fft_filtered_signal_" + set_info["label"],
            fft_filtered_signal_df,
        )

    def filter_spectrum(self, cfg, average_fft_df):
        cfg_fft = cfg["fft"].copy()
        if cfg_fft.__contains__("filter") and cfg_fft["filter"]["flag"]:
            min_frequency = cfg_fft["filter"].get("min_frequency", None)
            max_frequency = cfg_fft["filter"].get("max_frequency", None)

        fft_freq = average_fft_df["fft_freq"]
        index_selected = (fft_freq > min_frequency) & (fft_freq < max_frequency)
        average_fft_df = average_fft_df[index_selected].copy()

        return average_fft_df

    def fft_window_analysis(self, cfg, signal, time_step, time_window=None):
        if cfg is not None:
            cfg_fft = cfg["fft"].copy()
        else:
            cfg_fft = self.cfg["fft"].copy()

        average_fft_df = pd.DataFrame()
        peak_indices = []
        if time_window is None and cfg_fft.__contains__("window"):
            time_window = cfg_fft["window"]["size"]

        if len(signal) < 1.5 * time_window:
            raise Exception("No FFT Analysis performed. Check data")

        number_of_windows = math.floor(len(signal) / time_window)
        for window_index in range(0, number_of_windows):
            windowed_signal = signal[
                window_index * time_window : (window_index + 1) * time_window
            ]
            window_fft_dict, filtered_window_fft_dict = self.fft_analysis(
                cfg, windowed_signal, time_step
            )
            window_fft = window_fft_dict["fft"]
            if window_index == 0:
                sum_window_fft = window_fft.copy()
            else:
                sum_window_fft["power"] = sum_window_fft["power"] + window_fft["power"]
                sum_window_fft["fft"] = sum_window_fft["fft"] + window_fft["fft"]

        average_fft_df = sum_window_fft.copy()
        average_fft_df["power"] = average_fft_df["power"] / (number_of_windows + 1)
        average_fft_df["fft"] = average_fft_df["fft"] / (number_of_windows + 1)

        if cfg_fft["window"]["moving_average"]["flag"]:
            rolling_window_size = cfg_fft["window"]["moving_average"]["window_size"]
            average_fft_df["power"] = (
                average_fft_df["power"].rolling(window=rolling_window_size).mean()
            )
            average_fft_df["fft"] = (
                average_fft_df["fft"].rolling(window=rolling_window_size).mean()
            )

        average_fft_df.fillna(value=0, inplace=True)
        if cfg_fft.__contains__("peaks") and cfg_fft["peaks"]["flag"]:
            cfg_peaks = cfg_fft["peaks"].copy()
            frequency = average_fft_df["fft_freq"].to_list()
            power = average_fft_df["power"].to_list()
            peak_indices = self.identify_signal_peaks(frequency, power, cfg_peaks)

        average_fft_df["peak_flag"] = False
        # Third party imports
        with PandasChainedAssignent():
            for peak_index in peak_indices:
                average_fft_df.loc[peak_index, "peak_flag"] = True

        return average_fft_df

    def identify_signal_peaks(self, x_list, y_list, cfg_peaks):
        # Standard library imports
        import math

        # Third party imports
        import numpy as np
        import scipy.signal

        peak_indices = []
        if cfg_peaks["solver"] in ["peaks_cwt"]:
            indexes = scipy.signal.find_peaks_cwt(
                y_list, np.arange(1, 4), max_distances=np.arange(1, 4) * 2
            )
            indexes = np.array(indexes) - 1
            peak_indices = indexes
        elif cfg_peaks["solver"] in ["argrelextrema"]:
            indexes = scipy.signal.argrelextrema(
                np.array(y_list), comparator=np.greater, order=2
            )
            peak_indices = indexes[0]
        elif cfg_peaks["solver"] in ["find_peaks", None]:
            min_height = max(y_list) * cfg_peaks["min_height_percentage"] / 100
            min_distance = math.floor(
                len(x_list) * cfg_peaks["min_distance_index_percentage"] / 100
            )
            indexes, _ = scipy.signal.find_peaks(
                y_list, height=min_height, distance=min_distance
            )
            peak_indices = indexes

        return peak_indices

    def fft_analysis(self, cfg, signal, time_step=None):
        # Third party imports

        if cfg is None and self.cfg is None:
            raise ("Error: FFT Analysis configuration not available")
        elif cfg is None:
            cfg = self.cfg.copy()

        cfg_fft = cfg["fft"].copy()
        if time_step is None:
            time_step = cfg_fft["time_step"]

        sig_fft = scipy.fftpack.fft(signal)
        power = np.abs(sig_fft) ** 2
        if type(signal) is list:
            fft_freq = scipy.fftpack.fftfreq(len(signal), d=time_step)
        else:
            fft_freq = scipy.fftpack.fftfreq(signal.size, d=time_step)

        columns = ["fft", "power", "fft_freq"]
        with PandasChainedAssignent():
            fft_df = pd.DataFrame(columns=columns)
        fft_df["fft"] = sig_fft
        fft_df["power"] = power
        fft_df["fft_freq"] = fft_freq

        signal_dict = {"time_trace": signal, "fft": fft_df}
        filtered_signal_dict = self.get_filtered_signal(cfg_fft, signal_dict)

        signal_dict["fft"] = fft_df[fft_df["fft_freq"] >= 0].copy()

        return signal_dict, filtered_signal_dict

    def get_filtered_signal(self, cfg_fft, signal_dict):

        fft_df = signal_dict["fft"]
        sig_fft = fft_df["fft"]
        power = fft_df["power"]
        fft_freq = fft_df["fft_freq"]

        columns = ["fft", "power", "fft_freq"]
        with PandasChainedAssignent():
            filtered_fft_df = pd.DataFrame(columns=columns)

        filtered_sig_fft = sig_fft.copy()
        if (
            cfg_fft["filter"].__contains__("band_pass")
            and cfg_fft["filter"]["band_pass"]["flag"]
        ):
            min_freq = cfg_fft["filter"]["band_pass"]["frequency_minimum"]
            max_freq = cfg_fft["filter"]["band_pass"]["frequency_maximum"]
            index_selected = (fft_freq > min_freq) & (fft_freq < max_freq)
            filtered_sig_fft[
                (np.abs(fft_freq) <= min_freq) | (np.abs(fft_freq) >= max_freq)
            ] = 0
            filtered_fft_df["fft"] = sig_fft[index_selected]
            filtered_fft_df["power"] = power[index_selected]
            filtered_fft_df["fft_freq"] = fft_freq[index_selected]

            filtered_fft_df.fillna(value=0, inplace=True)
            fft_df_pos_freq = filtered_fft_df[filtered_fft_df["fft_freq"] >= 0].copy()
            try:
                filtered_signal = np.real(scipy.fftpack.ifft(filtered_sig_fft))
            except:
                filtered_signal = None
            filtered_signal_dict = {
                "time_trace": filtered_signal,
                "fft": fft_df_pos_freq,
            }

        else:
            filtered_signal_dict = signal_dict

        return filtered_signal_dict

    def get_RAO(self, signal, excitation, cfg_rao=None):
        # Third party imports
        import numpy as np
        import pandas as pd

        signal_fft_df = self.fft_window_analysis(signal)
        excitation_fft_df = self.fft_window_analysis(excitation)

        columns = ["complex", "amplitude", "phase", "frequency"]
        RAO_raw = pd.DataFrame(columns=columns)
        RAO_raw["complex"] = signal_fft_df["fft"] / excitation_fft_df["fft"]
        RAO_raw["Re"] = 0
        RAO_raw["Im"] = 0
        for row_index in range(0, len(RAO_raw)):
            # Third party imports
            from common.data import PandasChainedAssignent

            with PandasChainedAssignent():
                RAO_raw.loc[row_index, "Re"] = RAO_raw.loc[row_index, "complex"].real
                RAO_raw.loc[row_index, "Im"] = RAO_raw.loc[row_index, "complex"].imag

        RAO_raw["amplitude"] = np.absolute(RAO_raw["complex"])
        if cfg_rao is not None and cfg_rao.__contains__("phase"):
            deg_flag = cfg_rao["phase"].get("deg", False)
            RAO_raw["phase"] = np.angle(RAO_raw["complex"], deg=deg_flag)
        else:
            RAO_raw["phase"] = np.angle(RAO_raw["complex"])

        RAO_raw["frequency"] = signal_fft_df["fft_freq"]

        RAO_filtered = RAO_raw.copy()
        if cfg_rao is not None and cfg_rao.__contains__("filter"):
            max_power = excitation_fft_df["power"].max()
            min_power = max_power * cfg_rao["filter"]["amplitude"]["min_ratio"]

            # Third party imports
            from common.data import PandasChainedAssignent

            with PandasChainedAssignent():
                RAO_filtered.loc[
                    excitation_fft_df["power"] < min_power, "complex"
                ] = complex(0, 0)
                RAO_filtered.loc[
                    excitation_fft_df["power"] < min_power, "amplitude"
                ] = 0
                RAO_filtered.loc[excitation_fft_df["power"] < min_power, "phase"] = 0
                RAO_filtered.loc[excitation_fft_df["power"] < min_power, "Re"] = 0
                RAO_filtered.loc[excitation_fft_df["power"] < min_power, "Im"] = 0

        return RAO_raw, RAO_filtered

    def simple_moving_average(self):
        for set_index in range(0, len(self.cfg.default["input_data"]["sets"])):
            set_info = self.cfg.default["input_data"]["sets"][set_index]
            df = getattr(self.dbe, "input_data_" + set_info["label"])
            setattr(
                self, "output_sma_" + set_info["label"], self.get_sma_for_df(df.copy())
            )

    def get_sma_for_df(self, df):
        """
        Simple moving average for a dataframe
        First column is assumed to be always time/index and is not processed
        """
        # Third party imports
        import pandas as pd

        cfg_moving_avg = self.cfg["default"]["analysis"]["moving_average"].copy()
        columns = df.columns
        moving_avg_df = pd.DataFrame(columns=columns)
        if cfg_moving_avg["settings"].__contains__("by_data_points"):
            window_size = cfg_moving_avg["settings"]["by_data_points"]["window_size"]
            for col_index in range(0, len(columns)):
                if col_index == 0:
                    moving_avg_df[columns[col_index]] = df[columns[col_index]]
                else:
                    moving_avg_df[columns[col_index]] = (
                        df[columns[col_index]].rolling(window=window_size).mean()
                    )

        return moving_avg_df

    def filter_by_value(self):
        """
        Value filtering for a dataframe
        Accepts range and
        """
        for set_index in range(0, len(self.cfg.default["input_data"]["sets"])):
            set_info = self.cfg.default["input_data"]["sets"][set_index]
            df = getattr(self.dbe, "input_data_" + set_info["label"])
            df_statistics = getattr(self, "output_stats_" + set_info["label"])
            df_filter_by_value = self.filter_by_value_for_df(df.copy(), df_statistics)
            setattr(self, "output_f_by_v_" + set_info["label"], df_filter_by_value)

    def filter_by_value_for_df(self, df, df_statistics):
        # Third party imports
        import numpy as np

        cfg_filter = self.cfg["default"]["analysis"]["filter"].copy()
        columns = df.columns
        if cfg_filter.__contains__("range"):
            if "-sigma" in cfg_filter["range"][0]:
                for col_index in range(0, len(columns)):
                    if col_index != 0:
                        floor = (
                            cfg_filter["sigma_range"][0]
                            * df_statistics[df_statistics["statistic"] == "stdev"][
                                columns[col_index]
                            ].values[0]
                        )
                        df[columns[col_index]][df[columns[col_index]] < floor] = np.nan
            if "sigma" in cfg_filter["range"][0]:
                for col_index in range(0, len(columns)):
                    if col_index != 0:
                        ceiling = (
                            cfg_filter["sigma_range"][1]
                            * df_statistics[df_statistics["statistic"] == "stdev"][
                                columns[col_index]
                            ].values[0]
                        )
                        df[columns[col_index]][
                            df[columns[col_index]] > ceiling
                        ] = np.nan

        return df

    def perform_statistics(self):
        for set_index in range(0, len(self.cfg.default["input_data"]["sets"])):
            set_info = self.cfg.default["input_data"]["sets"][set_index]
            df = getattr(self.dbe, "input_data_" + set_info["label"])
            setattr(
                self,
                "output_stats_" + set_info["label"],
                self.get_statistics_for_df(df.copy()),
            )

    def get_statistics_for_df(self, df):
        # Third party imports
        import pandas as pd

        cfg_statistics = self.cfg["default"]["analysis"]["statistics"].copy()
        columns = df.columns
        statistics_df = pd.DataFrame(columns=["statistic"] + columns.to_list())
        statistics_array = ["minimum"] + df[columns.to_list()].min().to_list()
        statistics_df.loc[len(statistics_df)] = statistics_array
        statistics_array = ["maximum"] + df[columns.to_list()].max().to_list()
        statistics_df.loc[len(statistics_df)] = statistics_array
        statistics_array = ["mean", None] + df[columns.to_list()].mean().to_list()
        statistics_df.loc[len(statistics_df)] = statistics_array
        statistics_array = ["stdev", None] + df[columns.to_list()].std().to_list()
        statistics_df.loc[len(statistics_df)] = statistics_array

        return statistics_df

    def prepare_visualizations(self):
        # Third party imports
        from common.visualization_components import VisualizationComponents

        vc = VisualizationComponents(self.cfg)
        vc.prepare_visualizations(self)

    def get_sample_time_series(self, ts_cfg=None, plt_flag=False):
        # Third party imports
        import numpy as np
        from matplotlib import pyplot as plt

        if ts_cfg is None:
            ts_cfg = {
                "seed": 1234,
                "time_step": 0.02,
                "peak_period": 5,
                "time_range": [0, 20],
            }
            random_seed = 1234
            time_step = 0.02
            peak_period = 5
            time_range = [0, 20]
        else:
            random_seed = ts_cfg.get("seed", 1234)
            time_step = ts_cfg.get("time_step", 0.02)
            peak_period = ts_cfg.get("peak_period", 5)
            time_range = ts_cfg.get("time_range", [0, 20])

        np.random.seed(random_seed)
        time_vector = np.arange(time_range[0], time_range[1], time_step)
        signal = np.sin(2 * np.pi / peak_period * time_vector) + 0.5 * np.random.randn(
            time_vector.size
        )

        if plt_flag:
            plt.figure(figsize=(6, 5))
            plt.plot(time_vector, signal, label="Original signal")

        return time_vector, signal, time_step, peak_period

    def run_simple_fft_example(self, cfg, signal, time_step, plt_flag=False):
        signal_dict, filtered_signal_dict = self.fft_analysis(cfg, signal, time_step)

        return signal_dict, filtered_signal_dict

    # def run_window_fft_example(self, signal, plt_flag=False):
    #     average_fft_df = self.fft_window_analysis(signal)
    #
    #     return average_fft_df

    def count_cycles(self, time_series: array) -> pd.DataFrame:
        """
        https://pypi.org/project/rainflow/
        ASTM E1049-85 rainflow cycle counting algorythm for fatigue analysis
        """
        logging.debug("Rainflow count from time series ... BEGIN")

        df_columns = ["range", "mean", "count", "i_start", "i_end"]
        df = pd.DataFrame(rainflow.extract_cycles(time_series), columns=df_columns)
        dict = df.to_dict()

        logging.debug("Rainflow count from time series ... END")

        return df, dict

    def perform_custom_calculation_1(self):
        """
        Disys resultant acceleration from orthogonal components
        """
        cfg_cus_calc = self.cfg["default"]["analysis"]["custom_calculation_1"].copy()
        for data_set_index in range(0, len(cfg_cus_calc["sets"])):
            data_set_cfg = cfg_cus_calc["sets"][data_set_index]
            if data_set_cfg["df"] == "input_data_table_statistics":
                df = self.dbe.prepare_input_statistics(data_set_cfg)
                df["resultant"] = (
                    df[data_set_cfg["y"][0]] * df[data_set_cfg["y"][0]]
                    + df[data_set_cfg["y"][1]] * df[data_set_cfg["y"][1]]
                ) ** 0.5
                setattr(self, "output_data_" + data_set_cfg["label"], df)

    def perform_custom_calculation_2(self):
        """
        Disys displacement from angle and also resultant displacement from orthogonal components.
        """
        # Standard library imports
        import math

        cfg_cus_calc = self.cfg["default"]["analysis"]["custom_calculation_2"].copy()
        for data_set_index in range(0, len(cfg_cus_calc["sets"])):
            data_set_cfg = cfg_cus_calc["sets"][data_set_index]
            if data_set_cfg["df"] == "input_data_table_statistics":
                df = self.dbe.prepare_input_statistics(data_set_cfg)
                df["forward_displacement"] = data_set_cfg["arc_length"] * df[
                    data_set_cfg["y"][0]
                ].apply(math.tan)
                df["starboard_displacement"] = data_set_cfg["arc_length"] * df[
                    data_set_cfg["y"][1]
                ].apply(math.tan)
                df["resultant"] = (
                    df["forward_displacement"] ** 2 + df["starboard_displacement"] ** 2
                ) ** 0.5
                setattr(self, "output_data_" + data_set_cfg["label"], df)
            else:
                df = getattr(self.dbe, data_set_cfg["df"])
                df["forward_displacement"] = data_set_cfg["arc_length"] * df[
                    data_set_cfg["y"][0]
                ].apply(math.tan)
                df["starboard_displacement"] = data_set_cfg["arc_length"] * df[
                    data_set_cfg["y"][1]
                ].apply(math.tan)
                df["resultant"] = (
                    df["forward_displacement"] ** 2 + df["starboard_displacement"] ** 2
                ) ** 0.5
                setattr(self, "output_data_" + data_set_cfg["label"], df)

    def perform_integration(self):
        """
        fft or spectral analysis
        """
        for set_index in range(0, len(self.cfg.default["input_data"]["sets"])):
            data_set_info = self.cfg.default["input_data"]["sets"][set_index]
            df = getattr(self.dbe, "input_data_" + data_set_info["label"])
            self.integration_for_df(df, data_set_info)

    def integration_for_df(self, df, data_set_info):
        cfg_integration = self.cfg["default"]["analysis"]["integration"].copy()
        # Third party imports
        from scipy import integrate

        columns = df.columns
        for column_index in range(1, len(columns)):
            dx = df[columns[0]].diff().min().microseconds / 1000000
            column_name = columns[column_index]
            y = df[column_name].to_list()
            if cfg_integration["solver"] in ["trapz"]:
                df["single_integration_" + column_name] = integrate.cumtrapz(
                    y, dx=dx, initial=0
                )
                if cfg_integration.__contains__("double"):
                    y = df["single_integration_" + column_name].to_list()
                    df["double_integration_" + column_name] = integrate.cumtrapz(
                        y, dx=dx, initial=0
                    )

        setattr(self, "output_integration_" + data_set_info["label"], df)
