import pytest
import deepdiff
import os
import sys
import numpy as np


from digitalmodel.common.basic_statistics import BasicStatistics

bs = BasicStatistics()


def run_histogram(dataset, expected_result={}):
    cfg_hist = {"bins": 5, "range": (0, 5)}
    df = bs.get_histograms(dataset, cfg_hist)

    return df

def test_histogram():
    dataset = [1, 1, 2, 2, 2, 2, 3]

    expected_result = {
        "histogram": [],
    }

    run_histogram(dataset, expected_result)


test_histogram()
