"""
Test OrcaFlex Browser Backend with Real Data
Interactive test for actual OrcaFlex CSV files
"""

import sys
import os
sys.path.append(os.path.dirname(__file__))

from backend.file_processor import FileProcessor, TensionAnalyzer, MetadataExtractor, VerificationLogger
from pathlib import Path
import json
from datetime import datetime
import pandas as pd


def test_real_data():
    """Test with real OrcaFlex data"""
    
    print("=" * 70)
    print("OrcaFlex Browser Backend - REAL DATA TEST")
    print("=" * 70)
    
    # Get the real path from user
    print("\nEnter the path to your OrcaFlex CSV files:")
    print("Example: D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr")
    base_path = input("Path: ").strip()
    
    if not base_path:
        # Use default path
        base_path = "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr"
        print(f"Using default path: {base_path}")
    
    # Check if path exists
    if not Path(base_path).exists():
        print(f"\n[ERROR] Path does not exist: {base_path}")
        alt_paths = [
            "D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr",
            "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr",
            "sample_data"  # Fallback to sample data
        ]
        for alt in alt_paths:
            if Path(alt).exists():
                print(f"Found alternative path: {alt}")
                base_path = alt
                break
        else:
            print("No valid path found. Please check the path and try again.")
            return
    
    print(f"\nUsing path: {base_path}")
    
    # Initialize components
    file_processor = FileProcessor(base_path)
    tension_analyzer = TensionAnalyzer()
    metadata_extractor = MetadataExtractor()
    verification_logger = VerificationLogger("verification_real_data")
    
    # 1. Search for files
    print("\n" + "=" * 50)
    print("1. SEARCHING FOR FILES")
    print("=" * 50)
    
    pattern = input("Enter file pattern (default: dm_*_strut_dyn.csv): ").strip()
    if not pattern:
        pattern = "dm_*_strut_dyn.csv"
    
    files = file_processor.search_files(pattern)
    print(f"\nFound {len(files)} files matching pattern '{pattern}':")
    
    if not files:
        print("[WARNING] No files found!")
        print("\nTrying alternative patterns...")
        alt_patterns = [
            "*strut*.csv",
            "*Strut*.csv",
            "dm_*.csv",
            "*.csv"
        ]
        for alt_pattern in alt_patterns:
            files = file_processor.search_files(alt_pattern)
            if files:
                print(f"Found {len(files)} files with pattern '{alt_pattern}'")
                if len(files) > 0:
                    print("First 5 files:")
                    for i, f in enumerate(files[:5], 1):
                        print(f"  [{i}] {f.name}")
                    use_this = input(f"\nUse these files? (y/n): ").strip().lower()
                    if use_this == 'y':
                        break
        else:
            print("No CSV files found in the directory.")
            return
    else:
        # Display files found
        display_limit = 10
        for i, f in enumerate(files[:display_limit], 1):
            size_kb = f.stat().st_size / 1024 if f.exists() else 0
            print(f"  [{i:2d}] {f.name} ({size_kb:.1f} KB)")
        
        if len(files) > display_limit:
            print(f"  ... and {len(files) - display_limit} more files")
    
    # Ask how many files to process
    print(f"\nHow many files to process? (1-{len(files)}, default: all)")
    num_files = input("Number: ").strip()
    if num_files.isdigit():
        num_files = min(int(num_files), len(files))
        files = files[:num_files]
    
    print(f"\nProcessing {len(files)} files...")
    
    # 2. Read files in parallel
    print("\n" + "=" * 50)
    print("2. READING CSV FILES IN PARALLEL")
    print("=" * 50)
    
    max_workers = input("Number of parallel workers (default: 4): ").strip()
    max_workers = int(max_workers) if max_workers.isdigit() else 4
    
    print(f"Reading {len(files)} files with {max_workers} workers...")
    start_time = datetime.now()
    
    try:
        dataframes = file_processor.read_multiple_csvs_parallel(files, max_workers)
        elapsed = (datetime.now() - start_time).total_seconds()
        print(f"Completed in {elapsed:.2f} seconds")
        print(f"Successfully loaded {len(dataframes)} dataframes")
        
        # Display dataframe info
        for name, df in list(dataframes.items())[:5]:
            print(f"  {name}: {len(df)} rows, {len(df.columns)} columns")
            if 'eff_tension' in df.columns:
                print(f"    - eff_tension range: {df['eff_tension'].min():.2f} to {df['eff_tension'].max():.2f}")
        
        if len(dataframes) > 5:
            print(f"  ... and {len(dataframes) - 5} more dataframes")
            
    except Exception as e:
        print(f"[ERROR] Failed to read files: {e}")
        return
    
    # 3. Analyze tensions
    print("\n" + "=" * 50)
    print("3. ANALYZING TENSIONS")
    print("=" * 50)
    
    print("Analyzing effective tensions across all struts...")
    
    try:
        results = tension_analyzer.analyze_tensions(dataframes)
        
        # Display summary
        print(f"\nAnalysis Summary:")
        print(f"  Total struts analyzed: {results['summary']['total_struts_analyzed']}")
        print(f"  Overall min tension: {results['summary']['absolute_min_tension']:.2f}")
        print(f"  Overall max tension: {results['summary']['absolute_max_tension']:.2f}")
        
        # Display top 5 maximum tensions
        print("\nTop 5 Maximum Tensions by Strut:")
        strut_maxes = [(name, data['max_tension']) for name, data in results['strut_analysis'].items()]
        strut_maxes.sort(key=lambda x: x[1], reverse=True)
        for i, (strut, max_val) in enumerate(strut_maxes[:5], 1):
            print(f"  {i}. {strut}: {max_val:.2f}")
        
        # Display absolute maximum details
        if results['absolute_maximum']:
            abs_max = results['absolute_maximum']
            print("\n" + "=" * 60)
            print("ABSOLUTE MAXIMUM TENSION DETAILS")
            print("=" * 60)
            print(f"  Value: {abs_max['value']:.2f}")
            print(f"  Strut: {abs_max['strut']}")
            print(f"  FE Filename: {abs_max.get('fe_filename', 'N/A')}")
            print(f"  FE Filename Stem: {abs_max.get('fe_filename_stem', 'N/A')}")
            
            # Show additional row data if available
            row_data = abs_max.get('row_data', {})
            if 'time' in row_data:
                print(f"  Time: {row_data['time']}")
            print("=" * 60)
            
    except Exception as e:
        print(f"[ERROR] Failed to analyze tensions: {e}")
        import traceback
        traceback.print_exc()
        return
    
    # 4. Extract metadata
    print("\n" + "=" * 50)
    print("4. EXTRACTING METADATA")
    print("=" * 50)
    
    metadata_results = {}
    
    if results.get('absolute_maximum') and results['absolute_maximum'].get('fe_filename'):
        fe_filename = results['absolute_maximum']['fe_filename']
        print(f"\nExtracting metadata from critical file:")
        print(f"  {fe_filename}")
        
        metadata = metadata_extractor.extract_from_fe_filename(fe_filename)
        
        print(f"\nExtracted Metadata:")
        print(f"  Loading Condition: {metadata['loading_condition'] or 'Not detected'}")
        print(f"  LNG Loading: {metadata['lng_loading'] or 'Not detected'}")
        print(f"  Tide Level: {metadata['tide_level'] or 'Not detected'}")
        print(f"  Environment Type: {metadata['environment_type'] or 'Not detected'}")
        print(f"  Direction: {metadata['direction'] or 'Not detected'}")
        
        if metadata['parsed_components']:
            print(f"  Parsed Components: {metadata['parsed_components']}")
        
        metadata_results['critical_file'] = metadata
    else:
        print("[WARNING] No fe_filename found in absolute maximum data")
        print("Attempting to extract metadata from source files...")
        
        # Try to extract from first few files
        for name, df in list(dataframes.items())[:3]:
            if 'source_file' in df.columns:
                filename = df['source_file'].iloc[0]
                metadata = metadata_extractor.extract_metadata(filename)
                print(f"\n  {filename}:")
                if metadata['lng_loading']:
                    print(f"    LNG: {metadata['lng_loading']}")
                if metadata['tide_level']:
                    print(f"    Tide: {metadata['tide_level']}")
    
    # 5. User validation
    print("\n" + "=" * 50)
    print("5. USER VALIDATION")
    print("=" * 50)
    
    feedback = {
        'timestamp': datetime.now().isoformat(),
        'validation': {},
        'corrections': {},
        'comments': ''
    }
    
    # Ask for validation
    print("\nPlease validate the results:")
    
    tension_correct = input("Are the tension values correct? (y/n): ").strip().lower()
    feedback['validation']['tensions_correct'] = tension_correct == 'y'
    
    if tension_correct != 'y':
        correction = input("What correction is needed?: ").strip()
        feedback['corrections']['tensions'] = correction
    
    metadata_correct = input("Is the metadata extraction correct? (y/n): ").strip().lower()
    feedback['validation']['metadata_correct'] = metadata_correct == 'y'
    
    if metadata_correct != 'y':
        correction = input("What metadata correction is needed?: ").strip()
        feedback['corrections']['metadata'] = correction
    
    comments = input("Any additional comments? (press Enter to skip): ").strip()
    if comments:
        feedback['comments'] = comments
    
    # 6. Save verification
    print("\n" + "=" * 50)
    print("6. SAVING VERIFICATION RESULTS")
    print("=" * 50)
    
    # Compile complete results
    complete_results = {
        'test_type': 'real_data',
        'timestamp': datetime.now().isoformat(),
        'configuration': {
            'base_path': base_path,
            'pattern': pattern,
            'files_processed': len(files),
            'parallel_workers': max_workers
        },
        'file_search': {
            'files_found': len(files),
            'file_list': [str(f) for f in files[:20]]  # Limit to 20 for JSON
        },
        'data_loading': {
            'dataframes_loaded': len(dataframes),
            'total_rows': sum(len(df) for df in dataframes.values())
        },
        'tension_analysis': results,
        'metadata_extraction': metadata_results
    }
    
    verification_path = verification_logger.log_verification(complete_results, feedback)
    print(f"Verification saved to: {verification_path}")
    
    # 7. Final summary
    print("\n" + "=" * 70)
    print("TEST COMPLETE - SUMMARY")
    print("=" * 70)
    print(f"[OK] Files Found: {len(files)}")
    print(f"[OK] DataFrames Loaded: {len(dataframes)}")
    print(f"[OK] Struts Analyzed: {len(results['strut_analysis'])}")
    print(f"[OK] Absolute Max Tension: {results['summary']['absolute_max_tension']:.2f}")
    print(f"[OK] Critical Strut: {results['absolute_maximum']['strut'] if results['absolute_maximum'] else 'N/A'}")
    print(f"[OK] Verification Saved: {Path(verification_path).name}")
    print("=" * 70)
    
    # Ask to display JSON
    show_json = input("\nDisplay verification JSON? (y/n): ").strip().lower()
    if show_json == 'y':
        with open(verification_path, 'r') as f:
            print("\n" + json.dumps(json.load(f), indent=2))
    
    print("\nTest completed successfully!")


if __name__ == "__main__":
    try:
        test_real_data()
    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
    except Exception as e:
        print(f"\n[ERROR] Unexpected error: {e}")
        import traceback
        traceback.print_exc()