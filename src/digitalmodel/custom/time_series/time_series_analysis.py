# Standard library imports
import logging

# Third party imports
# Reader imports
from digitalmodel.custom.time_series.time_series_components import TimeSeriesComponents

tca = TimeSeriesComponents()

class TimeSeriesAnalysis():

    def __init__(self):
        pass
    
    def router(self, cfg: dict) -> dict:
        logging.debug("Time series analysis ... BEGIN")

        if cfg["analysis"]['basic']["sample"]:
            sig_fft, filtered_signal = tca.sample_analysis(cfg)
        
        if cfg["analysis"]['basic']["rainflow"]:
            rainflow_df, rainflow_dict = tca.get_rainflow_count_from_time_series(time_series)
        
        logging.debug("Time series analysis ... END")

        return cfg
    
