import logging
import array
import pandas as pd
import rainflow


class TimeSeriesAnalysis():

    def __init__(self):
        pass
    
    def get_rainflow_count_from_time_series(self, time_series: array) -> pd.DataFrame:
        '''
        https://pypi.org/project/rainflow/
        ASTM E1049-85 rainflow cycle counting algorythm for fatigue analysis
        '''
        logging.debug("Rainflow count from time series ... BEGIN")

        df_columns = ['range', 'mean', 'count', 'i_start', 'i_end']
        df = pd.DataFrame(rainflow.extract_cycles(time_series), columns = df_columns)
        dict = df.to_dict()

        logging.debug("Rainflow count from time series ... END")
    
        return df, dict
