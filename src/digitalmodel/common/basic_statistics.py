import logging
import array
import numpy as np
import pandas as pd
import rainflow


class BasicStatistics:
    def __init__(self):
        pass

    def get_histograms(self, dataset: array, cfg) -> pd.DataFrame:
        bins = cfg["bins"]

        histogram = np.histogram(dataset, bins=10, range=(0, 2000))

        frequency = list(histogram[0])
        bin_edges = list(histogram[1])
        start_value = bin_edges[:-1]
        end_value = bin_edges[1:]
        labels = [str(bin_edges[i]) + "-" + str(bin_edges[i + 1]) for i in range(len(bin_edges) - 1)]
        df = pd.DataFrame(
            {
                "frequency": frequency,
                "start_value": start_value,
                "end_value": end_value,
                "labels": labels,
            }
        )
        return df

    deg get_bins_from_range(self, dataset: array, cfg) -> pd.DataFrame:
        bins = cfg["bins"]

        histogram = np.histogram(dataset, bins=10, range=(0, 2000))

        frequency = list(histogram[0])
        bins = list(histogram[1])
        start_value = bins[:-1]
        end_value = bins[1:]
        labels = [str(bins[i]) + "-" + str(bins[i + 1]) for i in range(len(bins) - 1)]
        df = pd.DataFrame(
            {
                "frequency": frequency,
                "start_value": start_value,
                "end_value": end_value,
                "labels": labels,
            }
        )
        return df
    