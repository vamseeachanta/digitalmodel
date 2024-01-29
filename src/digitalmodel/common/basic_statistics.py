import logging
import array
import numpy as np
import pandas as pd
import math


class BasicStatistics:
    def __init__(self):
        pass

    def get_histograms(self, dataset: array, cfg) -> pd.DataFrame:
        bins = cfg["bins"]
        bin_range = cfg.get("bin_range", None)
        if bin_range == None:
            bin_range = self.get_bins_from_dataset(dataset, cfg)

        histogram = np.histogram(dataset, bins=bins, range=bin_range)

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

    def get_bins_from_dataset(self, dataset: array, cfg) -> array:
# TODO works for integers only. Need to accommodate floats as well
        bins = cfg["bins"]
        min_value = min(dataset)
        max_value = max(dataset)
        bin_size = math.ceil((max_value - min_value + 1)/5)
        
        existing_bins = list(range(min_value, max_value + 1, bin_size))
        
        no_of_bins_to_add = bins - len(existing_bins)
        
        if no_of_bins_to_add > 0:
            no_of_bins_to_add_left = no_of_bins_to_add / 2 
            if min_value > 0 and min_value - no_of_bins_to_add_left*bin_size < 0:
                min_value = 0
            else:
                min_value = min_value - no_of_bins_to_add_left*bin_size
            
            no_of_bins_to_add_right = no_of_bins_to_add - no_of_bins_to_add_left
            max_value = max_value + no_of_bins_to_add_right*bin_size

        
        bin_range = (min_value, max_value+1)

        return bin_range