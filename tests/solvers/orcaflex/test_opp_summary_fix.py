"""
Test for OPPSummary.add_file_result_to_all_results method
to ensure it works without FutureWarning for DataFrame concatenation
"""

import warnings
import pandas as pd
import numpy as np
from digitalmodel.orcaflex.opp_summary import OPPSummary


def test_add_file_result_to_all_results_no_warning():
    """Test that add_file_result_to_all_results doesn't produce FutureWarning"""
    
    opp_summary = OPPSummary()
    
    # Create test DataFrames with various scenarios
    # Scenario 1: Normal DataFrames with data
    df1 = pd.DataFrame({
        'fe_filename': ['file1.sim'],
        'fe_filename_stem': ['file1'],
        'run_status': ['Completed'],
        'start_time': [0.0],
        'stop_time': [100.0],
        'description': ['Test run 1'],
        'statistic': ['Max'],
        'value1': [10.5]
    })
    
    df2 = pd.DataFrame({
        'fe_filename': ['file2.sim'],
        'fe_filename_stem': ['file2'],
        'run_status': ['Completed'],
        'start_time': [0.0],
        'stop_time': [100.0],
        'description': ['Test run 2'],
        'statistic': ['Max'],
        'value1': [12.3]
    })
    
    # Scenario 2: DataFrame with NA values
    df3 = pd.DataFrame({
        'fe_filename': ['file3.sim'],
        'fe_filename_stem': ['file3'],
        'run_status': ['Completed'],
        'start_time': [0.0],
        'stop_time': [100.0],
        'description': [None],  # NA value
        'statistic': [None],    # NA value
        'value1': [np.nan]      # NaN value
    })
    
    # Scenario 3: Empty DataFrame
    df_empty = pd.DataFrame()
    
    # Test 1: Normal concatenation
    summary = None
    summary_groups_for_file = {'group1': df1}
    
    # Capture warnings
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        summary = opp_summary.add_file_result_to_all_results(summary, summary_groups_for_file)
        
        # Check no FutureWarning was raised
        future_warnings = [warning for warning in w if issubclass(warning.category, FutureWarning)]
        assert len(future_warnings) == 0, f"FutureWarning detected: {future_warnings}"
    
    assert 'group1' in summary
    assert len(summary['group1']) == 1
    
    # Test 2: Adding to existing summary
    summary_groups_for_file2 = {'group1': df2}
    
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        summary = opp_summary.add_file_result_to_all_results(summary, summary_groups_for_file2)
        
        # Check no FutureWarning was raised
        future_warnings = [warning for warning in w if issubclass(warning.category, FutureWarning)]
        assert len(future_warnings) == 0, f"FutureWarning detected: {future_warnings}"
    
    assert len(summary['group1']) == 2
    
    # Test 3: Adding DataFrame with NA values
    summary_groups_for_file3 = {'group1': df3}
    
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        summary = opp_summary.add_file_result_to_all_results(summary, summary_groups_for_file3)
        
        # Check no FutureWarning was raised
        future_warnings = [warning for warning in w if issubclass(warning.category, FutureWarning)]
        assert len(future_warnings) == 0, f"FutureWarning detected: {future_warnings}"
    
    assert len(summary['group1']) == 3
    
    # Test 4: Handling empty DataFrames
    summary_empty = {'group2': df_empty}
    summary_groups_empty = {'group2': df_empty}
    
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        result = opp_summary.add_file_result_to_all_results(summary_empty, summary_groups_empty)
        
        # Check no FutureWarning was raised
        future_warnings = [warning for warning in w if issubclass(warning.category, FutureWarning)]
        assert len(future_warnings) == 0, f"FutureWarning detected with empty DataFrames: {future_warnings}"
    
    print("✅ All tests passed - No FutureWarning detected!")
    return True


def test_mixed_empty_and_non_empty():
    """Test mixed scenario with empty and non-empty DataFrames"""
    
    opp_summary = OPPSummary()
    
    # Create a non-empty DataFrame
    df_data = pd.DataFrame({
        'fe_filename': ['file1.sim'],
        'value': [100]
    })
    
    # Create an empty DataFrame
    df_empty = pd.DataFrame()
    
    # Start with non-empty, add empty
    summary = {'group1': df_data}
    summary_groups = {'group1': df_empty}
    
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        result = opp_summary.add_file_result_to_all_results(summary, summary_groups)
        
        # Check no FutureWarning was raised
        future_warnings = [warning for warning in w if issubclass(warning.category, FutureWarning)]
        assert len(future_warnings) == 0, f"FutureWarning detected: {future_warnings}"
    
    # Result should still have the original data
    assert len(result['group1']) == 1
    
    # Start with empty, add non-empty
    summary = {'group1': df_empty}
    summary_groups = {'group1': df_data}
    
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        result = opp_summary.add_file_result_to_all_results(summary, summary_groups)
        
        # Check no FutureWarning was raised
        future_warnings = [warning for warning in w if issubclass(warning.category, FutureWarning)]
        assert len(future_warnings) == 0, f"FutureWarning detected: {future_warnings}"
    
    # Result should have the new data
    assert len(result['group1']) == 1
    
    print("✅ Mixed empty/non-empty tests passed!")
    return True


if __name__ == "__main__":
    # Run tests
    test_add_file_result_to_all_results_no_warning()
    test_mixed_empty_and_non_empty()
    print("\n🎉 All OPPSummary tests passed successfully!")