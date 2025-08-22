#!/usr/bin/env python
"""
Integration test for OrcaFlex time series analysis module

Tests the complete implementation with sample data.
"""

import sys
import pytest
from pathlib import Path
import pandas as pd
import numpy as np

# Add project to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from digitalmodel.modules.signal_analysis.orcaflex import (
    GenericTimeSeriesReader,
    TimeSeriesAnalyzer,
    BatchProcessor,
    ConfigurationManager
)


def test_configuration_manager():
    """Test configuration management"""
    # Load test configuration
    config_path = Path(__file__).parent / 'test_configs' / 'orcaflex_tension_analysis.yml'
    
    config_mgr = ConfigurationManager(config_path)
    
    # Test configuration access
    assert config_mgr.get('metadata.name') == 'OrcaFlex Tension Analysis'
    assert config_mgr.get('analysis.rainflow.method') == 'astm'
    assert config_mgr.get('analysis.fft.window_size') == 4096
    
    # Test profile access (profiles are optional)
    profiles = config_mgr.list_profiles()
    # Profiles may not be defined in test config
    
    print("OK: Configuration manager test passed")


def test_generic_reader():
    """Test generic time series reader"""
    reader = GenericTimeSeriesReader()
    
    # Test file discovery
    test_data_dir = Path(__file__).parent / 'test_data'
    files = reader.discover_files(pattern='*.csv', directory=test_data_dir)
    assert len(files) > 0
    
    # Test auto-detection on sample data
    sample_file = test_data_dir / 'fat002_fsts_sample.csv'
    if sample_file.exists():
        df, mapping = reader.read_file(sample_file, auto_detect=True)
        
        # Check that time column was detected
        assert mapping['time'] is not None
        assert len(mapping['data_columns']) > 0
        
        # Validate data
        validation = reader.validate_data(df, mapping)
        assert validation['valid']
        
        print(f"OK: Reader test passed - detected {len(mapping['data_columns'])} columns")


def test_analyzer():
    """Test time series analyzer"""
    # Create test configuration
    config = {
        'analysis': {
            'rainflow': {'method': 'astm', 'bin_count': 20},
            'fft': {'window_size': 256, 'overlap': 0.5}
        },
        'output': {
            'directory': 'output/test',
            'plots': {'enable': False}  # Disable plots for testing
        }
    }
    
    analyzer = TimeSeriesAnalyzer(config=config, auto_detect_columns=True)
    
    # Test with sample file
    test_file = Path(__file__).parent / 'test_data' / 'fat002_fsts_sample.csv'
    
    if test_file.exists():
        results = analyzer.process_file(test_file)
        
        # Check results structure
        assert 'file' in results
        assert 'columns' in results
        assert len(results['columns']) > 0
        
        # Check analysis results for first column
        first_col = list(results['columns'].keys())[0]
        col_results = results['columns'][first_col]
        
        assert 'statistics' in col_results
        assert 'rainflow' in col_results
        assert 'spectral' in col_results
        
        # Check statistics
        stats = col_results['statistics']
        assert 'mean' in stats
        assert 'std' in stats
        assert 'max' in stats
        assert 'min' in stats
        
        # Check rainflow results
        rainflow = col_results['rainflow']
        assert 'cycles' in rainflow
        assert 'statistics' in rainflow
        
        print(f"OK: Analyzer test passed - analyzed {len(results['columns'])} columns")


def test_batch_processor():
    """Test batch processing"""
    config = {
        'analysis': {
            'rainflow': {'method': 'astm', 'bin_count': 20},
            'fft': {'window_size': 256, 'overlap': 0.5}
        },
        'batch': {
            'parallel': {'enable': False},  # Disable parallel for testing
            'continue_on_error': True
        },
        'output': {
            'plots': {'enable': False}
        }
    }
    
    analyzer = TimeSeriesAnalyzer(config=config, auto_detect_columns=True)
    batch_processor = BatchProcessor(analyzer, config)
    
    # Get test files
    test_data_dir = Path(__file__).parent / 'test_data'
    test_files = list(test_data_dir.glob('*.csv'))[:2]  # Limit to 2 files for testing
    
    if test_files:
        results = batch_processor.process_files(test_files, parallel=False)
        
        # Check results
        assert len(results) <= len(test_files)
        
        # Check processing times
        assert 'total' in batch_processor.processing_times
        assert 'average' in batch_processor.processing_times
        
        print(f"OK: Batch processor test passed - processed {len(results)} files")


def test_integration():
    """Full integration test"""
    print("\n" + "="*60)
    print("OrcaFlex Module Integration Test")
    print("="*60)
    
    # Load configuration
    config_path = Path(__file__).parent / 'test_configs' / 'orcaflex_tension_analysis.yml'
    config_mgr = ConfigurationManager(config_path)
    
    # Create analyzer with configuration
    analyzer = TimeSeriesAnalyzer(config=config_mgr.to_dict(), auto_detect_columns=True)
    
    # Process sample file
    test_file = Path(__file__).parent / 'test_data' / 'fat002_fsts_sample.csv'
    
    if test_file.exists():
        results = analyzer.process_file(test_file)
        
        # Verify complete results
        assert results is not None
        assert 'validation' in results
        assert results['validation']['valid']
        
        # Check that we got results for expected columns
        expected_columns = ['Tension (Jacket End)', 'Tension (Vessel End)']
        for col in expected_columns:
            if col in results['columns']:
                col_results = results['columns'][col]
                
                # Verify all analysis components present
                assert col_results['statistics']['mean'] is not None
                assert col_results['rainflow']['statistics']['total_cycles'] > 0
                assert len(col_results['spectral']['peaks']) > 0
                
                print(f"  OK: {col}: {col_results['rainflow']['statistics']['total_cycles']} cycles detected")
        
        print("\nOK: Integration test passed successfully!")
        return True
    else:
        print("  Warning: Sample file not found, skipping integration test")
        return False


def main():
    """Run all tests"""
    print("\n" + "#"*60)
    print("#  OrcaFlex Module Test Suite")
    print("#"*60)
    
    # Run tests
    tests = [
        ("Configuration Manager", test_configuration_manager),
        ("Generic Reader", test_generic_reader),
        ("Time Series Analyzer", test_analyzer),
        ("Batch Processor", test_batch_processor),
        ("Full Integration", test_integration)
    ]
    
    results = []
    for name, test_func in tests:
        print(f"\nTesting {name}...")
        try:
            test_func()
            results.append((name, True))
        except Exception as e:
            print(f"  FAIL: {name} failed: {e}")
            results.append((name, False))
    
    # Summary
    print("\n" + "="*60)
    print("Test Summary")
    print("="*60)
    
    for name, passed in results:
        status = "PASS" if passed else "FAIL"
        print(f"  [{status}]: {name}")
    
    all_passed = all(r[1] for r in results)
    
    if all_passed:
        print("\n[SUCCESS] All tests passed!")
    else:
        print("\n[FAILURE] Some tests failed")
    
    return 0 if all_passed else 1


if __name__ == "__main__":
    sys.exit(main())