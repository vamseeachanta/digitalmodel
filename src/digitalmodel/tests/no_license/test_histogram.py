import pytest
import deepdiff
import os
import sys
import numpy as np


from digitalmodel.common.basic_statistics import BasicStatistics

bs = BasicStatistics()


def run_histogram_1(dataset, expected_result={}):
    cfg_hist = {"bins": 5, "bin_range": (0, 5)}
    df = bs.get_histograms(dataset, cfg_hist)

    result = {"histogram": list(df["frequency"])}

    hist_diff = deepdiff.DeepDiff(result, expected_result, ignore_order=True)
    assert hist_diff == {}

    return df


def run_histogram_2(dataset, expected_result={}):
    cfg_hist = {"bins": 5}
    df = bs.get_histograms(dataset, cfg_hist)

    result = {"histogram": list(df["frequency"])}

    hist_diff = deepdiff.DeepDiff(result, expected_result, ignore_order=True)
    assert hist_diff == {}

    return df


def test_histogram():
    dataset = [1, 1, 2, 2, 2, 2, 3]

    expected_result = {
        "histogram": [0, 2, 4, 1, 0],
    }

    run_histogram_1(dataset, expected_result)
    run_histogram_2(dataset, expected_result)


test_histogram()
