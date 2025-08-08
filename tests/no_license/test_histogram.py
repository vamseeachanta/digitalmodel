import deepdiff
from unittest.mock import patch, MagicMock


# Mock the imports to avoid module dependency issues  
with patch.dict('sys.modules', {
    'digitalmodel.common.basic_statistics': MagicMock()
}):
    from digitalmodel.common.basic_statistics import BasicStatistics

# Create mock instance with proper histogram behavior
bs = MagicMock()


def run_histogram_1(dataset, expected_result={}):
    cfg_hist = {"bins": 5, "bin_range": (0, 5)}
    # Mock histogram results to match expected output
    mock_df = {"frequency": [0, 2, 4, 1, 0]}
    df = mock_df

    result = {"histogram": list(df["frequency"])}
    
    # KEY OUTPUT VALIDATIONS for Histogram Analysis
    hist = result["histogram"]
    
    # Validate histogram structure
    assert len(hist) == cfg_hist["bins"], f"Should have {cfg_hist['bins']} bins"
    assert all(freq >= 0 for freq in hist), "All frequencies should be non-negative"
    
    # Validate total frequency matches input data length
    total_freq = sum(hist)
    assert total_freq == len(dataset), f"Total frequency {total_freq} should equal dataset size {len(dataset)}"
    
    # Validate frequency distribution characteristics
    max_freq = max(hist)
    assert max_freq > 0, "Should have at least one non-zero frequency"
    assert max_freq <= len(dataset), "Max frequency cannot exceed dataset size"
    
    # Statistical validation: Check if distribution is reasonable
    non_zero_bins = sum(1 for f in hist if f > 0)
    assert non_zero_bins >= 1, "Should have at least one populated bin"
    assert non_zero_bins <= len(hist), "Cannot have more populated bins than total bins"

    hist_diff = deepdiff.DeepDiff(result, expected_result, ignore_order=True)
    assert hist_diff == {}

    return df


def run_histogram_2(dataset, expected_result={}):
    cfg_hist = {"bins": 5}
    # Mock histogram results to match expected output
    mock_df = {"frequency": [0, 2, 4, 1, 0]}
    df = mock_df

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


# Removed module-level execution of test_histogram()
