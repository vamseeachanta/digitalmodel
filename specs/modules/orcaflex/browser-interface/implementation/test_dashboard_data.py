"""
Test script to verify dashboard data loading
"""

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__)))

from backend.file_processor import FileProcessor
from backend.orcaflex_analyzer import OrcaFlexAnalyzer
from pathlib import Path
import pandas as pd

def test_dashboard_data():
    """Test what data the dashboard is seeing"""
    
    base_path = "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr"
    
    print("Testing Dashboard Data Loading")
    print("=" * 60)
    
    # Initialize
    file_processor = FileProcessor(base_path)
    analyzer = OrcaFlexAnalyzer()
    
    # Search files
    files = file_processor.search_files("dm_*_strut_dyn.csv")
    print(f"Found {len(files)} files:")
    for f in files:
        print(f"  - {f.name}")
    
    # Load files
    print("\nLoading files...")
    dataframes = file_processor.read_multiple_csvs_parallel(files)
    print(f"Loaded {len(dataframes)} dataframes")
    
    # Check what's in the dataframes
    for name, df in dataframes.items():
        print(f"\n{name}:")
        print(f"  Shape: {df.shape}")
        print(f"  Columns sample: {list(df.columns[:5])}")
        
        # Check for strut columns
        strut_cols = [col for col in df.columns if 'Strut' in col and 'max' in col]
        if strut_cols:
            print(f"  Strut columns found: {len(strut_cols)}")
            # Check actual values
            for col in strut_cols[:3]:
                max_val = df[col].max() if not df[col].isna().all() else 0
                print(f"    {col}: max = {max_val:.2f}")
    
    # Analyze
    print("\nAnalyzing...")
    results = analyzer.analyze_multiple_dataframes(dataframes)
    
    print("\nAnalysis Results:")
    print(f"  Struts found: {results['summary']['total_struts_found']}")
    print(f"  Absolute MAX: {results['summary']['absolute_max_tension']}")
    print(f"  Absolute MIN: {results['summary']['absolute_min_tension']}")
    
    if results.get('absolute_maximum'):
        abs_max = results['absolute_maximum']
        print(f"\nCritical Case:")
        print(f"  Strut: {abs_max['strut']}")
        print(f"  Value: {abs_max['value']}")
        print(f"  Source: {abs_max.get('source_files', [])}")
    
    # Check for the real maximum
    print("\n" + "=" * 60)
    print("SEARCHING FOR REAL MAXIMUM (8265.55):")
    print("=" * 60)
    
    for file in files:
        df = pd.read_csv(file)
        print(f"\n{file.name}:")
        
        # Check all max columns
        for col in df.columns:
            if 'Strut' in col and 'max' in col:
                max_val = df[col].max() if not df[col].isna().all() else 0
                if max_val > 8000:  # Looking for our known maximum
                    print(f"  *** FOUND: {col} = {max_val:.2f}")
                    # Find the row
                    max_row = df[df[col] == max_val]
                    if not max_row.empty and 'fe_filename' in df.columns:
                        fe_file = max_row['fe_filename'].iloc[0]
                        print(f"      FE File: {Path(fe_file).name}")

if __name__ == "__main__":
    test_dashboard_data()