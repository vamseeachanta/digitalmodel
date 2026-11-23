import pandas as pd


class Histogram:
    """Utility class for creating histograms from data arrays.

    Provides methods to compute histogram frequency distributions
    and format them as pandas DataFrames.
    """

    def __init__(self):
        """Initialize Histogram."""
        pass

    def get_histograms_for_array(self, data_array, data_bins, unit=None):
        """Compute histogram for a data array with specified bins.

        Args:
            data_array: Array-like numerical data to create the histogram from.
            data_bins: Bin edges or number of bins for the histogram.
            unit: Optional unit string to append to bin range labels.

        Returns:
            dict: Dictionary with keys:
                - 'histogram_df': DataFrame with bin ranges and counts.
                - 'hist': Array of frequency counts.
                - 'bin_edges': Array of bin edge values.
        """
        import numpy as np
        hist, bin_edges = np.histogram(data_array, bins=data_bins)
        histogram_df = self.get_histogram_df(hist, bin_edges, unit)

        histogram_dict = {'histogram_df': histogram_df, 'hist': hist, 'bin_edges': bin_edges}
        return histogram_dict

    def get_histogram_df(self, hist, bin_edges, unit):
        """Convert histogram arrays into a pandas DataFrame.

        Args:
            hist: Array of histogram frequency counts.
            bin_edges: Array of bin edge values.
            unit: Optional unit string to append to bin range labels,
                or None for no unit.

        Returns:
            pd.DataFrame: DataFrame with 'bin_range' and 'bin_count' columns.
        """
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
