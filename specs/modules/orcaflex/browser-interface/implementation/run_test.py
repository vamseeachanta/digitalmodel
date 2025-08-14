"""
Quick test runner for OrcaFlex Browser Backend
"""

import sys
import os
sys.path.append(os.path.dirname(__file__))

from backend.file_processor import FileProcessor, TensionAnalyzer, MetadataExtractor, VerificationLogger
from pathlib import Path
import json
from datetime import datetime


def run_quick_test():
    """Run a quick test of the backend functionality"""
    
    print("=" * 60)
    print("OrcaFlex Browser Backend - Quick Test")
    print("=" * 60)
    
    # Test with sample path (update this to your actual path)
    base_path = input("Enter base path (or press Enter for default): ").strip()
    if not base_path:
        base_path = "D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr"
    
    print(f"\nUsing base path: {base_path}")
    
    # Initialize components
    file_processor = FileProcessor(base_path)
    tension_analyzer = TensionAnalyzer()
    metadata_extractor = MetadataExtractor()
    verification_logger = VerificationLogger("verification")
    
    # 1. Search for files
    print("\n1. Searching for files...")
    files = file_processor.search_files("dm_*_strut_dyn.csv")
    print(f"   Found {len(files)} files")
    if files:
        for i, f in enumerate(files[:5]):  # Show first 5
            print(f"   [{i+1}] {f.name}")
        if len(files) > 5:
            print(f"   ... and {len(files)-5} more")
    
    if not files:
        print("   No files found! Check the path and pattern.")
        return
    
    # 2. Read files in parallel
    print("\n2. Reading CSV files in parallel...")
    dataframes = file_processor.read_multiple_csvs_parallel(files[:10], max_workers=4)  # Limit to 10 for testing
    print(f"   Loaded {len(dataframes)} dataframes")
    
    # 3. Analyze tensions
    print("\n3. Analyzing tensions...")
    results = tension_analyzer.analyze_tensions(dataframes)
    
    if results['absolute_maximum']:
        abs_max = results['absolute_maximum']
        print(f"\n   ABSOLUTE MAXIMUM TENSION:")
        print(f"   Value: {abs_max['value']:.2f}")
        print(f"   Strut: {abs_max['strut']}")
        print(f"   FE Filename: {abs_max.get('fe_filename', 'N/A')}")
        print(f"   FE Filename Stem: {abs_max.get('fe_filename_stem', 'N/A')}")
    
    # 4. Extract metadata
    print("\n4. Extracting metadata...")
    if results.get('absolute_maximum') and results['absolute_maximum'].get('fe_filename'):
        fe_filename = results['absolute_maximum']['fe_filename']
        metadata = metadata_extractor.extract_from_fe_filename(fe_filename)
        
        print(f"\n   Metadata from critical file:")
        print(f"   Loading Condition: {metadata['loading_condition']}")
        print(f"   LNG Loading: {metadata['lng_loading']}")
        print(f"   Tide Level: {metadata['tide_level']}")
        print(f"   Environment Type: {metadata['environment_type']}")
        print(f"   Direction: {metadata['direction']}")
    
    # 5. Save verification
    print("\n5. Saving verification results...")
    verification_path = verification_logger.log_verification(results)
    print(f"   Saved to: {verification_path}")
    
    print("\n" + "=" * 60)
    print("Test Complete!")
    print("=" * 60)
    
    # Ask for feedback
    feedback = input("\nAny feedback or corrections? (Enter to skip): ").strip()
    if feedback:
        # Save feedback
        feedback_data = {
            'timestamp': datetime.now().isoformat(),
            'user_feedback': feedback
        }
        verification_logger.log_verification(results, feedback_data)
        print("Feedback saved!")


if __name__ == "__main__":
    try:
        run_quick_test()
    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
    except Exception as e:
        print(f"\nError: {e}")
        import traceback
        traceback.print_exc()