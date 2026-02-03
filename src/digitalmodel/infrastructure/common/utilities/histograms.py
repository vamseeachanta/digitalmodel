import pandas as pd


class Histogram:

    def __init__(self):
        pass

    def get_histograms_for_array(self, data_array, data_bins, unit=None):
        import numpy as np
        hist, bin_edges = np.histogram(data_array, bins=data_bins)
        histogram_df = self.get_histogram_df(hist, bin_edges, unit)

        histogram_dict = {'histogram_df': histogram_df, 'hist': hist, 'bin_edges': bin_edges}
        return histogram_dict

    def get_histogram_df(self, hist, bin_edges, unit):
        columns = ['bin_range', 'count']
        histogram_df = pd.DataFrame(columns=columns)
        bin_range_array = []
        for bin_indx in range(0, len(bin_edges) - 1):
            if unit is not None:
                bin_range = str(bin_edges[bin_indx]) + ' - ' + str(bin_edges[bin_indx + 1]) + ' ' + unit
            else:
                bin_range = str(bin_edges[bin_indx]) + ' - ' + str(bin_edges[bin_indx + 1])
            bin_range_array.append(bin_range)
        histogram_df['bin_range'] = bin_range_array
        histogram_df['bin_count'] = hist

        return histogram_df


if __name__ == '__main__':
    hist = Histogram()
    data_array = [1, 1, 2, 2, 2, 2, 3]
    data_bins = list(range(5))
    histogram_dict = hist.get_histograms_for_array(data_array, data_bins)
    print(histogram_dict)
