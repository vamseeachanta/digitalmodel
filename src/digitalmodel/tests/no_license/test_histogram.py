import pytest
import deepdiff
import os
import sys
import numpy as np


from digitalmodel.common.basic_statistics import BasicStatistics

bs = BasicStatistics()


def run_histogram(input_file, expected_result={}):
    df = tsa.get_rainflow_count_from_time_series(input_file)
    df.head()

    dataset = [1, 1, 2, 2, 2, 2, 3]
    cfg_hist = {"bins": 5, "range": (0, 5)}
    orcaflex_rainflow_hist = bs.get_histograms(
        orcaflex_rainflow_half_cycles_result, cfg_hist
    )

    expected_result = {
        "result": orcaflex_rainflow_half_cycles_result,
        "histogram": orcaflex_rainflow_hist,
    }
    run_rainflow_for_timeseries(timeseries_data, expected_result)


test_rainflow_for_timeseries()
