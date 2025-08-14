"""
Verify Critical Loading Condition - Absolute Maximum Tension
Focus on identifying the exact file and conditions for maximum tension
"""

import sys
import os
sys.path.append(os.path.dirname(__file__))

from backend.file_processor import FileProcessor, MetadataExtractor
from backend.orcaflex_analyzer import OrcaFlexAnalyzer
from pathlib import Path
import pandas as pd
from datetime import datetime


def verify_critical_case():
    """Verify the critical loading condition with absolute maximum tension"""
    
    print("=" * 70)
    print("CRITICAL LOADING CONDITION VERIFICATION")
    print("=" * 70)
    
    # Use the real data path
    base_path = "D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv\\03c_100yr"
    
    if not Path(base_path).exists():
        print(f"Error: Path not found - {base_path}")
        return
    
    print(f"\nData Directory: {base_path}")
    
    # Initialize components
    file_processor = FileProcessor(base_path)
    analyzer = OrcaFlexAnalyzer()
    metadata_extractor = MetadataExtractor()
    
    # Search for summary files
    print("\n" + "-" * 50)
    print("SEARCHING FOR SUMMARY FILES")
    print("-" * 50)
    
    files = file_processor.search_files("dm_*_strut_dyn.csv")
    print(f"Found {len(files)} summary CSV files:")
    for i, f in enumerate(files, 1):
        print(f"  {i}. {f.name}")
    
    # Load all files
    print("\n" + "-" * 50)
    print("LOADING AND ANALYZING FILES")
    print("-" * 50)
    
    dataframes = file_processor.read_multiple_csvs_parallel(files)
    print(f"Loaded {len(dataframes)} dataframes")
    
    # Analyze to find absolute maximum
    results = analyzer.analyze_multiple_dataframes(dataframes)
    
    # Display detailed strut analysis
    print("\n" + "-" * 50)
    print("STRUT-BY-STRUT MAXIMUM TENSIONS")
    print("-" * 50)
    
    strut_maxes = []
    for strut_name, data in results['strut_analysis'].items():
        strut_maxes.append({
            'strut': strut_name,
            'max_tension': data.get('max_tension', 0),
            'min_tension': data.get('min_tension', 0),
            'sources': ', '.join(data.get('source_files', []))
        })
    
    # Sort by max tension
    strut_maxes.sort(key=lambda x: x['max_tension'], reverse=True)
    
    for item in strut_maxes:
        print(f"{item['strut']:8s}: Max={item['max_tension']:10.2f}, Min={item['min_tension']:10.2f}")
    
    # Focus on absolute maximum
    if results['absolute_maximum']:
        abs_max = results['absolute_maximum']
        
        print("\n" + "=" * 70)
        print("ABSOLUTE MAXIMUM TENSION - CRITICAL CASE")
        print("=" * 70)
        
        print(f"\n1. TENSION VALUE:")
        print(f"   Maximum: {abs_max['value']:.2f} (units)")
        print(f"   Strut: {abs_max['strut'].upper()}")
        
        print(f"\n2. SOURCE FILES:")
        for source in abs_max.get('source_files', []):
            print(f"   - {source}")
        
        # Find which specific file contains this maximum
        print(f"\n3. SPECIFIC FILE ANALYSIS:")
        
        # Check each file's data to find exact source
        for name, analysis in results['file_analyses'].items():
            strut_data = analysis.get('strut_data', {})
            critical_strut = abs_max['strut']
            
            if critical_strut in strut_data:
                max_val = strut_data[critical_strut].get('max_tension', 0)
                if abs(max_val - abs_max['value']) < 0.01:  # Found the exact file
                    print(f"   Found in dataframe: {name}")
                    
                    # Get the original filename
                    for original_file in files:
                        if name in str(original_file):
                            print(f"   Original CSV file: {original_file.name}")
                            
                            # Parse the CSV filename for conditions
                            metadata = metadata_extractor.extract_metadata(original_file.name)
                            print(f"\n4. LOADING CONDITIONS FROM CSV FILENAME:")
                            print(f"   LNG Loading: {metadata.get('lng_loading', 'Not detected')}")
                            print(f"   Tide Level: {metadata.get('tide_level', 'Not detected')}")
                            break
        
        # Check FE filename
        if 'fe_filename' in abs_max:
            print(f"\n5. FE SIMULATION FILE:")
            print(f"   FE Filename: {abs_max['fe_filename']}")
            
            # Extract metadata from FE filename
            fe_metadata = metadata_extractor.extract_from_fe_filename(abs_max['fe_filename'])
            
            print(f"\n6. CONDITIONS FROM FE FILENAME:")
            print(f"   Loading Condition: {fe_metadata.get('loading_condition', 'Not detected')}")
            print(f"   LNG Loading: {fe_metadata.get('lng_loading', 'Not detected')}")
            print(f"   Tide Level: {fe_metadata.get('tide_level', 'Not detected')}")
            print(f"   Environment Type: {fe_metadata.get('environment_type', 'Not detected')}")
            print(f"   Direction: {fe_metadata.get('direction', 'Not detected')}")
            
            if fe_metadata.get('parsed_components'):
                print(f"\n7. PARSED COMPONENTS:")
                for key, value in fe_metadata['parsed_components'].items():
                    print(f"   {key}: {value}")
        
        # Now let's check the actual data to confirm
        print(f"\n8. DATA VERIFICATION:")
        
        # Load the specific files to double-check
        for file in files:
            df = pd.read_csv(file)
            
            # Check all strut columns for the maximum value
            for col in df.columns:
                if 'strut' in col.lower() and 'max' in col.lower():
                    max_in_col = df[col].max() if not df[col].isna().all() else 0
                    if abs(max_in_col - abs_max['value']) < 0.01:
                        print(f"\n   CONFIRMED in file: {file.name}")
                        print(f"   Column: {col}")
                        print(f"   Value: {max_in_col:.2f}")
                        
                        # Check if there's an associated FE filename in this row
                        if 'fe_filename' in df.columns:
                            # Find the row with this max value
                            max_row_mask = df[col] == max_in_col
                            if max_row_mask.any():
                                fe_files = df.loc[max_row_mask, 'fe_filename'].values
                                if len(fe_files) > 0:
                                    print(f"   Associated FE file: {fe_files[0]}")
                        
                        # Show the filename components
                        print(f"\n   CSV Filename breakdown:")
                        parts = file.name.replace('.csv', '').split('_')
                        for i, part in enumerate(parts):
                            print(f"     [{i}] {part}")
                        
                        break
        
        print("\n" + "=" * 70)
        print("CRITICAL LOADING CONDITION SUMMARY")
        print("=" * 70)
        print(f"Maximum Tension: {abs_max['value']:.2f}")
        print(f"Critical Strut: {abs_max['strut'].upper()}")
        
        if 'fe_filename' in abs_max:
            print(f"FE Simulation: {abs_max['fe_filename']}")
            
            # Final summary of conditions
            if fe_metadata:
                conditions = []
                if fe_metadata.get('lng_loading'):
                    conditions.append(fe_metadata['lng_loading'])
                if fe_metadata.get('tide_level'):
                    conditions.append(fe_metadata['tide_level'])
                if fe_metadata.get('environment_type'):
                    conditions.append(fe_metadata['environment_type'])
                if fe_metadata.get('direction'):
                    conditions.append(fe_metadata['direction'])
                
                if conditions:
                    print(f"Conditions: {' | '.join(conditions)}")
        
        print("=" * 70)
    
    else:
        print("\n[ERROR] No absolute maximum found in analysis")
    
    return results


if __name__ == "__main__":
    try:
        results = verify_critical_case()
    except Exception as e:
        print(f"\nError: {e}")
        import traceback
        traceback.print_exc()