"""
Enhanced Test for OrcaFlex Browser Backend
Handles both summary and time-series CSV formats
"""

import sys
import os
sys.path.append(os.path.dirname(__file__))

from backend.file_processor import FileProcessor, MetadataExtractor, VerificationLogger
from backend.orcaflex_analyzer import OrcaFlexAnalyzer
from pathlib import Path
import json
from datetime import datetime
import pandas as pd


def test_enhanced():
    """Test with enhanced analyzer that handles multiple formats"""
    
    print("=" * 70)
    print("OrcaFlex Browser Backend - ENHANCED TEST")
    print("=" * 70)
    
    # Try to find real data
    possible_paths = [
        "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr",
        "D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr",
        "sample_data"
    ]
    
    base_path = None
    for path in possible_paths:
        if Path(path).exists():
            base_path = path
            print(f"Found data directory: {base_path}")
            break
    
    if not base_path:
        print("No data directory found!")
        return
    
    # Initialize components
    file_processor = FileProcessor(base_path)
    analyzer = OrcaFlexAnalyzer()
    metadata_extractor = MetadataExtractor()
    verification_logger = VerificationLogger("verification_enhanced")
    
    # 1. Search for files
    print("\n" + "-" * 50)
    print("1. FILE DISCOVERY")
    print("-" * 50)
    
    # Try multiple patterns
    patterns = ["dm_*_strut_dyn.csv", "*strut*.csv", "*.csv"]
    files = []
    used_pattern = None
    
    for pattern in patterns:
        files = file_processor.search_files(pattern)
        if files:
            used_pattern = pattern
            print(f"Found {len(files)} files with pattern: {pattern}")
            break
    
    if not files:
        print("No CSV files found!")
        return
    
    # Limit to first 10 files for testing
    files = files[:10]
    print(f"Processing {len(files)} files:")
    for i, f in enumerate(files[:5], 1):
        print(f"  [{i}] {f.name}")
    if len(files) > 5:
        print(f"  ... and {len(files)-5} more")
    
    # 2. Load data
    print("\n" + "-" * 50)
    print("2. DATA LOADING")
    print("-" * 50)
    
    print("Loading CSV files...")
    dataframes = file_processor.read_multiple_csvs_parallel(files, max_workers=4)
    print(f"Loaded {len(dataframes)} dataframes")
    
    # Show sample of columns from first dataframe
    if dataframes:
        first_df = list(dataframes.values())[0]
        print(f"\nSample columns from first file:")
        cols = first_df.columns.tolist()
        
        # Group columns by type
        strut_cols = [c for c in cols if 'Strut' in c]
        fe_cols = [c for c in cols if 'fe_' in c]
        other_cols = [c for c in cols if c not in strut_cols and c not in fe_cols]
        
        if strut_cols:
            print(f"  Strut columns ({len(strut_cols)}):")
            for col in strut_cols[:5]:
                print(f"    - {col}")
            if len(strut_cols) > 5:
                print(f"    ... and {len(strut_cols)-5} more")
        
        if fe_cols:
            print(f"  FE columns: {', '.join(fe_cols)}")
        
        if other_cols:
            print(f"  Other columns: {', '.join(other_cols[:5])}")
    
    # 3. Analyze with enhanced analyzer
    print("\n" + "-" * 50)
    print("3. ENHANCED ANALYSIS")
    print("-" * 50)
    
    print("Analyzing data with format detection...")
    results = analyzer.analyze_multiple_dataframes(dataframes)
    
    print(f"\nAnalysis Results:")
    print(f"  Files analyzed: {results['summary']['total_files_analyzed']}")
    print(f"  Struts found: {results['summary']['total_struts_found']}")
    print(f"  Formats detected: {', '.join(results['summary']['formats_detected'])}")
    
    if results['summary']['absolute_max_tension'] is not None:
        print(f"  Absolute MIN: {results['summary']['absolute_min_tension']:.2f}")
        print(f"  Absolute MAX: {results['summary']['absolute_max_tension']:.2f}")
    
    # Display strut analysis
    if results['strut_analysis']:
        print("\nStrut-by-Strut Analysis:")
        
        # Create and display summary table
        summary_df = analyzer.get_strut_summary_table(results)
        if not summary_df.empty:
            print(summary_df.to_string(index=False))
    
    # Display absolute maximum details
    if results['absolute_maximum']:
        abs_max = results['absolute_maximum']
        print("\n" + "=" * 60)
        print("ABSOLUTE MAXIMUM TENSION")
        print("=" * 60)
        print(f"Value: {abs_max['value']:.2f}")
        print(f"Strut: {abs_max['strut']}")
        print(f"Source Files: {', '.join(abs_max['source_files'][:2])}")
        
        if 'fe_filename' in abs_max:
            print(f"FE Filename: {Path(abs_max['fe_filename']).name}")
            
            # Extract metadata
            metadata = metadata_extractor.extract_from_fe_filename(abs_max['fe_filename'])
            print("\nExtracted Metadata:")
            print(f"  Loading: {metadata['loading_condition'] or 'N/A'}")
            print(f"  LNG: {metadata['lng_loading'] or 'N/A'}")
            print(f"  Tide: {metadata['tide_level'] or 'N/A'}")
            print(f"  Environment: {metadata['environment_type'] or 'N/A'}")
            print(f"  Direction: {metadata['direction'] or 'N/A'}")
        print("=" * 60)
    
    # 4. Save verification
    print("\n" + "-" * 50)
    print("4. VERIFICATION")
    print("-" * 50)
    
    verification_data = {
        'test_type': 'enhanced',
        'timestamp': datetime.now().isoformat(),
        'configuration': {
            'base_path': base_path,
            'pattern': used_pattern,
            'files_processed': len(files)
        },
        'analysis_results': results,
        'metadata': {
            'critical_file': metadata if results.get('absolute_maximum') else None
        }
    }
    
    verification_path = verification_logger.log_verification(verification_data)
    print(f"Saved: {verification_path}")
    
    # 5. Summary
    print("\n" + "=" * 70)
    print("TEST COMPLETE")
    print("=" * 70)
    print(f"[OK] Files processed: {len(files)}")
    print(f"[OK] Struts analyzed: {results['summary']['total_struts_found']}")
    print(f"[OK] Formats handled: {', '.join(results['summary']['formats_detected'])}")
    
    if results['absolute_maximum']:
        print(f"[OK] Critical strut: {results['absolute_maximum']['strut']}")
        print(f"[OK] Maximum tension: {results['absolute_maximum']['value']:.2f}")
    
    print(f"[OK] Verification saved: {Path(verification_path).name}")
    print("=" * 70)
    
    return results


if __name__ == "__main__":
    try:
        results = test_enhanced()
        
        # Option to show JSON
        if results and input("\nShow JSON summary? (y/n): ").strip().lower() == 'y':
            print("\nJSON Summary:")
            print(json.dumps(results['summary'], indent=2))
            
    except KeyboardInterrupt:
        print("\n\nTest interrupted")
    except Exception as e:
        print(f"\nError: {e}")
        import traceback
        traceback.print_exc()