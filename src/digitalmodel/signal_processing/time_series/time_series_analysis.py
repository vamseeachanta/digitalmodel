# Standard library imports
import copy
import logging

# Third party imports
from assetutilities.common.update_deep import update_deep_dictionary

# Reader imports
from digitalmodel.signal_processing.signal_analysis.adapters import TimeSeriesComponentsAdapter

tca = TimeSeriesComponentsAdapter()


class TimeSeriesAnalysis:
    def __init__(self):
        pass

    def router(self, cfg: dict) -> dict:
        logging.debug("Time series analysis ... BEGIN")

        cfg = self.get_cfg_with_master_data(cfg)

        if cfg["analysis"]["basic"]["sample_fft"]:
            sig_fft, filtered_signal = tca.sample_fft(cfg)

        if cfg["analysis"]["basic"]["sample_window_averaged_fft"]:
            sig_fft, filtered_signal = tca.sample_window_averaged_fft(cfg)

        if cfg["analysis"]["basic"]["window_averaged_fft"]:
            tca.window_averaged_fft(cfg)

        if cfg["analysis"]["basic"]["rainflow"]:
            rainflow_df, rainflow_dict = tca.count_cycles(
                time_series
            )

        logging.debug("Time series analysis ... END")

        return cfg

    def get_cfg_with_master_data(self, cfg):
        if "master_settings" in cfg:
            master_settings = cfg["master_settings"].copy()
            data_settings = cfg["data"]

            for group_index in range(0, len(data_settings["groups"])):
                group = data_settings["groups"][group_index].copy()
                group = update_deep_dictionary(master_settings["groups"], group)
                data_settings["groups"][group_index] = copy.deepcopy(group)

        return cfg
