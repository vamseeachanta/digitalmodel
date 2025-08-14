"""
Automated Real Data Test for OrcaFlex Browser Backend
Tests with actual OrcaFlex CSV files without user input
"""

import sys
import os
sys.path.append(os.path.dirname(__file__))

from backend.file_processor import FileProcessor, TensionAnalyzer, MetadataExtractor, VerificationLogger
from pathlib import Path
import json
from datetime import datetime
import argparse


def test_real_data_auto(base_path=None, pattern=None, max_files=None):
    """Test with real OrcaFlex data automatically"""
    
    print("=" * 70)
    print("OrcaFlex Browser Backend - AUTOMATED REAL DATA TEST")
    print("=" * 70)
    
    # Use provided path or try common paths
    if not base_path:
        possible_paths = [
            "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr",
            "D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr",
            "sample_data"
        ]
        for path in possible_paths:
            if Path(path).exists():
                base_path = path
                print(f"Found data directory: {base_path}")
                break
        else:
            print("No valid data path found. Using sample data.")
            base_path = "sample_data"
    
    print(f"\nBase path: {base_path}")
    
    # Check if path exists
    if not Path(base_path).exists():
        print(f"[ERROR] Path does not exist: {base_path}")
        return None
    
    # Initialize components
    file_processor = FileProcessor(base_path)
    tension_analyzer = TensionAnalyzer()
    metadata_extractor = MetadataExtractor()
    verification_logger = VerificationLogger("verification_real_data")
    
    # 1. Search for files
    print("\n" + "-" * 50)
    print("1. FILE SEARCH")
    print("-" * 50)
    
    if not pattern:
        pattern = "dm_*_strut_dyn.csv"
    
    print(f"Searching for pattern: {pattern}")
    files = file_processor.search_files(pattern)
    
    if not files:
        print(f"No files found with pattern '{pattern}'")
        # Try alternative patterns
        alt_patterns = ["*strut*.csv", "*Strut*.csv", "*.csv"]
        for alt in alt_patterns[:2]:  # Limit to avoid too many files
            files = file_processor.search_files(alt)
            if files:
                print(f"Found {len(files)} files with pattern '{alt}'")
                pattern = alt
                break
    
    if not files:
        print("[ERROR] No CSV files found")
        return None
    
    print(f"Found {len(files)} files")
    
    # Limit files if specified
    if max_files:
        files = files[:max_files]
        print(f"Processing first {len(files)} files")
    
    # Display first few files
    for i, f in enumerate(files[:5], 1):
        size_kb = f.stat().st_size / 1024 if f.exists() else 0
        print(f"  [{i}] {f.name} ({size_kb:.1f} KB)")
    if len(files) > 5:
        print(f"  ... and {len(files)-5} more")
    
    # 2. Read files in parallel
    print("\n" + "-" * 50)
    print("2. PARALLEL DATA LOADING")
    print("-" * 50)
    
    start_time = datetime.now()
    print(f"Loading {len(files)} CSV files...")
    
    try:
        dataframes = file_processor.read_multiple_csvs_parallel(files, max_workers=4)
        elapsed = (datetime.now() - start_time).total_seconds()
        
        print(f"Loaded {len(dataframes)} dataframes in {elapsed:.2f} seconds")
        
        # Check for eff_tension column
        has_eff_tension = sum(1 for df in dataframes.values() if 'eff_tension' in df.columns)
        print(f"Files with eff_tension column: {has_eff_tension}/{len(dataframes)}")
        
        if has_eff_tension == 0:
            print("\n[WARNING] No files contain 'eff_tension' column!")
            print("Available columns in first file:")
            if dataframes:
                first_df = list(dataframes.values())[0]
                print(f"  {', '.join(first_df.columns[:10])}")
                if len(first_df.columns) > 10:
                    print(f"  ... and {len(first_df.columns)-10} more columns")
        
    except Exception as e:
        print(f"[ERROR] Failed to read files: {e}")
        return None
    
    # 3. Analyze tensions
    print("\n" + "-" * 50)
    print("3. TENSION ANALYSIS")
    print("-" * 50)
    
    try:
        results = tension_analyzer.analyze_tensions(dataframes)
        
        print(f"Analyzed {results['summary']['total_struts_analyzed']} struts")
        
        if results['summary']['absolute_max_tension'] is not None:
            print(f"Absolute MIN tension: {results['summary']['absolute_min_tension']:.2f}")
            print(f"Absolute MAX tension: {results['summary']['absolute_max_tension']:.2f}")
            
            # Show top tensions
            print("\nTop 3 Maximum Tensions:")
            strut_maxes = [(name, data['max_tension']) 
                          for name, data in results['strut_analysis'].items()]
            strut_maxes.sort(key=lambda x: x[1], reverse=True)
            for i, (strut, max_val) in enumerate(strut_maxes[:3], 1):
                print(f"  {i}. {strut}: {max_val:.2f}")
        
        # Display critical case
        if results['absolute_maximum']:
            abs_max = results['absolute_maximum']
            print("\n" + "=" * 60)
            print("CRITICAL CASE - ABSOLUTE MAXIMUM")
            print("=" * 60)
            print(f"Value: {abs_max['value']:.2f}")
            print(f"Strut: {abs_max['strut']}")
            if abs_max.get('fe_filename'):
                print(f"FE File: {Path(abs_max['fe_filename']).name}")
            if abs_max.get('fe_filename_stem'):
                print(f"FE Stem: {abs_max['fe_filename_stem']}")
            print("=" * 60)
        
    except Exception as e:
        print(f"[ERROR] Analysis failed: {e}")
        return None
    
    # 4. Extract metadata
    print("\n" + "-" * 50)
    print("4. METADATA EXTRACTION")
    print("-" * 50)
    
    metadata_results = {}
    
    if results.get('absolute_maximum'):
        fe_filename = results['absolute_maximum'].get('fe_filename', '')
        if fe_filename:
            metadata = metadata_extractor.extract_from_fe_filename(fe_filename)
            
            print("Metadata from critical file:")
            print(f"  Loading: {metadata['loading_condition'] or 'N/A'}")
            print(f"  LNG: {metadata['lng_loading'] or 'N/A'}")
            print(f"  Tide: {metadata['tide_level'] or 'N/A'}")
            print(f"  Environment: {metadata['environment_type'] or 'N/A'}")
            print(f"  Direction: {metadata['direction'] or 'N/A'}")
            
            metadata_results['critical_file'] = metadata
    
    # 5. Save verification
    print("\n" + "-" * 50)
    print("5. VERIFICATION SAVE")
    print("-" * 50)
    
    complete_results = {
        'test_type': 'automated_real_data',
        'timestamp': datetime.now().isoformat(),
        'configuration': {
            'base_path': base_path,
            'pattern': pattern,
            'files_processed': len(files)
        },
        'summary': {
            'files_found': len(files),
            'dataframes_loaded': len(dataframes),
            'struts_analyzed': len(results['strut_analysis']),
            'max_tension': results['summary']['absolute_max_tension'],
            'min_tension': results['summary']['absolute_min_tension']
        },
        'tension_analysis': results,
        'metadata_extraction': metadata_results
    }
    
    verification_path = verification_logger.log_verification(complete_results)
    print(f"Saved: {verification_path}")
    
    # 6. Summary
    print("\n" + "=" * 70)
    print("TEST SUMMARY")
    print("=" * 70)
    print(f"Path: {base_path}")
    print(f"Files: {len(files)}")
    print(f"DataFrames: {len(dataframes)}")
    print(f"Struts: {len(results['strut_analysis'])}")
    if results['summary']['absolute_max_tension'] is not None:
        print(f"Max Tension: {results['summary']['absolute_max_tension']:.2f}")
        print(f"Critical Strut: {results['absolute_maximum']['strut']}")
    print(f"Verification: {Path(verification_path).name}")
    print("=" * 70)
    
    return complete_results


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Test OrcaFlex Backend with Real Data")
    parser.add_argument('--path', help='Path to CSV files')
    parser.add_argument('--pattern', default='dm_*_strut_dyn.csv', help='File pattern')
    parser.add_argument('--max-files', type=int, help='Maximum files to process')
    parser.add_argument('--show-json', action='store_true', help='Display JSON output')
    
    args = parser.parse_args()
    
    results = test_real_data_auto(args.path, args.pattern, args.max_files)
    
    if results and args.show_json:
        print("\n" + "=" * 70)
        print("JSON OUTPUT")
        print("=" * 70)
        print(json.dumps(results['summary'], indent=2))