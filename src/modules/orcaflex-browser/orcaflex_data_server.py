#!/usr/bin/env python3
"""
OrcaFlex Data Server - Backend for reading actual CSV files
Enhanced with Excel collation file support for dynamic UI configuration
"""

import os
import json
import glob
import pandas as pd
from flask import Flask, jsonify, request, send_from_directory
from flask_cors import CORS
from pathlib import Path
import re
from excel_reader import ExcelCollationReader, get_excel_config, DEFAULT_CONFIG
from concurrent.futures import ProcessPoolExecutor, as_completed
import multiprocessing
import time

app = Flask(__name__)
CORS(app)  # Enable CORS for all routes

# Base directories for OrcaFlex data
PARENT_DIR = r"D:\1522\ctr7\orcaflex\rev_a08"  # Parent directory with .dat files
BASE_DIR = r"D:\1522\ctr7\orcaflex\rev_a08\output\csv"  # CSV output directory

# Initialize Excel reader
excel_reader = ExcelCollationReader(PARENT_DIR)

@app.route('/')
def index():
    """Serve the HTML dashboard"""
    return send_from_directory('.', 'orcaflex-data-browser-v6-clean.html')

@app.route('/v5')
def index_v5():
    """Serve the previous version of the dashboard"""
    return send_from_directory('.', 'orcaflex-data-browser-v5-enhanced.html')

@app.route('/debug')
def debug():
    """Serve the debug dashboard"""
    return send_from_directory('.', 'debug-dashboard.html')

@app.route('/api/subfolders')
def get_subfolders():
    """Get list of actual subfolders in the base directory"""
    try:
        if not os.path.exists(BASE_DIR):
            return jsonify({'error': f'Base directory not found: {BASE_DIR}'}), 404
        
        # Get all subdirectories
        subfolders = []
        for item in os.listdir(BASE_DIR):
            item_path = os.path.join(BASE_DIR, item)
            if os.path.isdir(item_path):
                subfolders.append(item)
        
        subfolders.sort()
        
        # Get base names from parent directory (looking for .dat or .yml files)
        base_names = set()
        if os.path.exists(PARENT_DIR):
            for file in os.listdir(PARENT_DIR):
                if file.endswith(('.dat', '.yml', '.yaml')):
                    # Remove extension and add to base names
                    base_name = os.path.splitext(file)[0]
                    base_names.add(base_name)
        
        return jsonify({
            'base_dir': BASE_DIR,
            'parent_dir': PARENT_DIR,
            'subfolders': subfolders,
            'base_names': sorted(list(base_names))
        })
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/files/<subfolder>')
def get_files(subfolder):
    """Get list of CSV files in a subfolder and extract available parameters"""
    try:
        folder_path = os.path.join(BASE_DIR, subfolder)
        if not os.path.exists(folder_path):
            return jsonify({'error': f'Subfolder not found: {folder_path}'}), 404
        
        # Get all CSV files
        csv_files = glob.glob(os.path.join(folder_path, "*.csv"))
        
        # Extract unique parameters from filenames
        fst_configs = set()
        tide_levels = set()
        env_types = set()
        headings = set()
        base_names = set()
        
        file_info = []
        for csv_file in csv_files:
            filename = os.path.basename(csv_file)
            
            # Try to parse filename patterns
            # Example: FST1_E_FST2_E_HWL_colinear_000deg.csv
            parts = filename.replace('.csv', '').split('_')
            
            file_data = {
                'filename': filename,
                'path': csv_file,
                'size': os.path.getsize(csv_file)
            }
            
            # Extract FST configuration (FST1_E_FST2_E pattern)
            if 'FST1' in filename and 'FST2' in filename:
                fst_match = re.search(r'(FST1_[EF])_(FST2_[EF])', filename)
                if fst_match:
                    fst_config = fst_match.group(0)
                    fst_configs.add(fst_config)
                    file_data['fst_config'] = fst_config
            
            # Extract tide level (HWL, MWL, LWL)
            for tide in ['HWL', 'MWL', 'LWL']:
                if tide in filename:
                    tide_levels.add(tide)
                    file_data['tide_level'] = tide
                    break
            
            # Extract environment type (colinear, non-colinear)
            if 'colinear' in filename.lower():
                if 'non' in filename.lower():
                    env_types.add('non-colinear')
                    file_data['env_type'] = 'non-colinear'
                else:
                    env_types.add('colinear')
                    file_data['env_type'] = 'colinear'
            
            # Extract heading (000deg, 045deg, etc.)
            heading_match = re.search(r'(\d{3})deg', filename)
            if heading_match:
                heading = heading_match.group(0)
                headings.add(heading)
                file_data['heading'] = heading
            
            # Extract base name - match with parent directory .dat files
            # Try to match with known base names from parent dir
            possible_base = filename.split('_')[0] if '_' in filename else filename.replace('.csv', '')
            
            # Check if this matches any .dat file in parent directory
            parent_files = []
            if os.path.exists(PARENT_DIR):
                parent_files = [os.path.splitext(f)[0] for f in os.listdir(PARENT_DIR) 
                               if f.endswith(('.dat', '.yml', '.yaml'))]
            
            # Find best matching base name
            base_name = possible_base
            for parent_base in parent_files:
                if filename.startswith(parent_base):
                    base_name = parent_base
                    break
            
            base_names.add(base_name)
            file_data['base_name'] = base_name
            
            file_info.append(file_data)
        
        return jsonify({
            'folder': folder_path,
            'files': file_info,
            'parameters': {
                'fst_configs': sorted(list(fst_configs)),
                'tide_levels': sorted(list(tide_levels)),
                'env_types': sorted(list(env_types)),
                'headings': sorted(list(headings)),
                'base_names': sorted(list(base_names))
            }
        })
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/load_csv')
def load_csv():
    """Load actual CSV data from a file"""
    try:
        subfolder = request.args.get('subfolder')
        fst_config = request.args.get('fst_config')
        tide_level = request.args.get('tide_level')
        env_type = request.args.get('env_type')
        heading = request.args.get('heading')
        base_name = request.args.get('base_name', '')
        
        if not all([subfolder]):
            return jsonify({'error': 'Missing required parameters'}), 400
        
        folder_path = os.path.join(BASE_DIR, subfolder)
        
        # Find matching CSV file
        csv_files = glob.glob(os.path.join(folder_path, "*.csv"))
        
        matching_file = None
        for csv_file in csv_files:
            filename = os.path.basename(csv_file)
            
            # Check if file matches all criteria
            matches = True
            if base_name and not filename.startswith(base_name):
                matches = False
            if fst_config and fst_config not in filename:
                matches = False
            if tide_level and tide_level not in filename:
                matches = False
            if env_type:
                if env_type == 'non-colinear' and 'non' not in filename.lower():
                    matches = False
                elif env_type == 'colinear' and ('non' in filename.lower() or 'colinear' not in filename.lower()):
                    matches = False
            if heading and heading not in filename:
                matches = False
            
            if matches:
                matching_file = csv_file
                break
        
        if not matching_file:
            # Try to find any CSV file in the folder as fallback
            if csv_files:
                matching_file = csv_files[0]
                print(f"Warning: No exact match found, using {matching_file}")
            else:
                return jsonify({'error': 'No matching CSV file found'}), 404
        
        # Read CSV file
        df = pd.read_csv(matching_file)
        
        # Prepare data for frontend
        data = {
            'filename': os.path.basename(matching_file),
            'columns': df.columns.tolist(),
            'time': None,
            'lines': {},
            'struts': {},
            'jacket': {},
            'raw_data': {}
        }
        
        # Identify time column
        time_cols = [col for col in df.columns if 'time' in col.lower()]
        if time_cols:
            data['time'] = df[time_cols[0]].tolist()
        elif df.columns[0].lower() in ['t', 'time', 'seconds']:
            data['time'] = df.iloc[:, 0].tolist()
        else:
            # Assume first column is time
            data['time'] = df.iloc[:, 0].tolist()
        
        # Extract mooring line data
        line_cols = [col for col in df.columns if 'line' in col.lower() or 'mooring' in col.lower()]
        for col in line_cols:
            data['lines'][col] = df[col].tolist()
        
        # Extract strut data
        strut_cols = [col for col in df.columns if 'strut' in col.lower()]
        for col in strut_cols:
            data['struts'][col] = df[col].tolist()
        
        # Extract jacket data
        jacket_cols = [col for col in df.columns if 'jacket' in col.lower()]
        for col in jacket_cols:
            data['jacket'][col] = df[col].tolist()
        
        # If no specific categorization found, include all numeric columns as raw data
        if not (line_cols or strut_cols or jacket_cols):
            for col in df.columns:
                if df[col].dtype in ['float64', 'int64']:
                    data['raw_data'][col] = df[col].tolist()
        
        # Add metadata
        data['metadata'] = {
            'rows': len(df),
            'duration': data['time'][-1] if data['time'] else 0,
            'dt': data['time'][1] - data['time'][0] if len(data['time']) > 1 else 0.1
        }
        
        return jsonify(data)
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/api/data')
def get_data():
    """Load data based on vessel type and configuration"""
    try:
        subfolder = request.args.get('subfolder')
        vessel_type = request.args.get('vesselType', 'FST')
        tide = request.args.get('tide', 'hwl')
        heading = request.args.get('heading', '0')
        env_type = request.args.get('envType', 'colinear')
        
        if not subfolder:
            return jsonify({'error': 'Missing subfolder parameter'}), 400
        
        folder_path = os.path.join(BASE_DIR, subfolder)
        if not os.path.exists(folder_path):
            return jsonify({'error': f'Folder not found: {subfolder}'}), 404
        
        # Build file pattern based on vessel type
        pattern_parts = []
        
        if vessel_type == 'FST':
            fst1 = request.args.get('fst1', '15')
            fst2 = request.args.get('fst2', '15')
            mooring = request.args.get('mooring', 'intact')
            
            # Map percentage to E/F notation
            fst1_code = 'E' if fst1 == '15' else 'F'
            fst2_code = 'E' if fst2 == '15' else 'F'
            
            pattern_parts.extend([f'FST1_{fst1_code}', f'FST2_{fst2_code}'])
            
        elif vessel_type == 'LNGC':
            capacity = request.args.get('capacity', '125000')
            loading = request.args.get('loading', 'ballast')
            berthing = request.args.get('berthing', 'port')
            
            # Add LNGC-specific pattern parts
            pattern_parts.append('LNGC')
            
        # Find matching CSV file - prioritize time series over summary files
        csv_files = glob.glob(os.path.join(folder_path, "*.csv"))
        
        # Separate time series from summary files
        time_series_files = [f for f in csv_files if not os.path.basename(f).startswith('dm_')]
        summary_files = [f for f in csv_files if os.path.basename(f).startswith('dm_')]
        
        # Prefer time series files
        search_files = time_series_files if time_series_files else summary_files
        
        # Try to find best match
        matching_file = None
        best_score = 0
        
        for csv_file in search_files:
            filename = os.path.basename(csv_file).upper()
            score = 0
            
            # Score based on pattern matches
            for part in pattern_parts:
                if part.upper() in filename:
                    score += 1
            
            # Check tide
            if tide.upper() in filename:
                score += 1
            
            # Check heading
            if f'HDG{heading.zfill(3)}' in filename or f'H{heading.zfill(3)}' in filename:
                score += 1
            
            # Check environment type
            if env_type == 'colinear' and 'NON' not in filename:
                score += 0.5
            elif env_type == 'non-colinear' and 'NON' in filename:
                score += 1
            
            if score > best_score:
                best_score = score
                matching_file = csv_file
        
        if not matching_file and csv_files:
            # Fallback to first CSV file
            matching_file = csv_files[0]
            print(f"Warning: No good match found, using {os.path.basename(matching_file)}")
        
        if not matching_file:
            return jsonify({'error': 'No CSV files found in folder'}), 404
        
        # Read CSV file
        df = pd.read_csv(matching_file)
        filename = os.path.basename(matching_file)
        
        # Determine if this is a summary file or time series
        is_summary = filename.startswith('dm_')
        
        # Determine data category from filename
        filename_lower = filename.lower()
        file_category = None
        if 'jacket' in filename_lower:
            file_category = 'jacket'
        elif 'strut' in filename_lower:
            file_category = 'strut'
        elif 'mooring' in filename_lower or 'line' in filename_lower:
            file_category = 'mooring'
        elif 'fst1' in filename_lower:
            file_category = 'fst1'
        elif 'fst2' in filename_lower:
            file_category = 'fst2'
        
        # Prepare response data
        data = {
            'filename': filename,
            'columns': df.columns.tolist(),
            'data_type': 'summary' if is_summary else 'time_series',
            'time': None,
            'lines': {},
            'struts': {},
            'jacket': {},
            'summary_data': {},
            'metadata': {
                'rows': len(df),
                'vessel_type': vessel_type,
                'parameters': request.args.to_dict(),
                'is_summary': is_summary
            }
        }
        
        if is_summary:
            # Handle summary data - extract all numeric columns
            # For summary files, we'll send all the data organized by category
            data['summary_data'] = {}
            
            # Convert DataFrame to dictionary for easier access in frontend
            for col in df.columns:
                col_lower = col.lower()
                
                # Skip non-numeric columns
                if df[col].dtype in ['object', 'string']:
                    continue
                    
                # Categorize columns
                if 'fst1' in col_lower:
                    if 'fst1' not in data['summary_data']:
                        data['summary_data']['fst1'] = {}
                    data['summary_data']['fst1'][col] = df[col].tolist()
                elif 'fst2' in col_lower:
                    if 'fst2' not in data['summary_data']:
                        data['summary_data']['fst2'] = {}
                    data['summary_data']['fst2'][col] = df[col].tolist()
                elif 'mooring' in col_lower or 'line' in col_lower:
                    if 'mooring' not in data['summary_data']:
                        data['summary_data']['mooring'] = {}
                    data['summary_data']['mooring'][col] = df[col].tolist()
                elif 'strut' in col_lower:
                    if 'struts' not in data['summary_data']:
                        data['summary_data']['struts'] = {}
                    data['summary_data']['struts'][col] = df[col].tolist()
                elif 'jacket' in col_lower or 'jkt' in col_lower:
                    if 'jacket' not in data['summary_data']:
                        data['summary_data']['jacket'] = {}
                    data['summary_data']['jacket'][col] = df[col].tolist()
                elif 'lngc' in col_lower:
                    if 'lngc' not in data['summary_data']:
                        data['summary_data']['lngc'] = {}
                    data['summary_data']['lngc'][col] = df[col].tolist()
                else:
                    if 'other' not in data['summary_data']:
                        data['summary_data']['other'] = {}
                    data['summary_data']['other'][col] = df[col].tolist()
            
            # For summary data, create synthetic time axis if needed
            data['time'] = list(range(len(df)))
            data['metadata']['duration'] = len(df)
            data['metadata']['summary_categories'] = list(data['summary_data'].keys())
            
        else:
            # Handle time series data
            # Find time column
            time_cols = [col for col in df.columns if 'time' in col.lower()]
            if time_cols:
                data['time'] = df[time_cols[0]].tolist()
            elif df.columns[0].lower() in ['t', 'time', 'seconds']:
                data['time'] = df.iloc[:, 0].tolist()
            else:
                data['time'] = df.iloc[:, 0].tolist()
            
            # If we know the file category from the filename, put all data in that category
            if file_category:
                # Skip time column and put all other columns in the appropriate category
                data_cols = [col for col in df.columns if col.lower() not in ['time', 't', 'seconds']]
                
                if file_category == 'jacket':
                    for col in data_cols:
                        data['jacket'][col] = df[col].tolist()
                elif file_category == 'strut':
                    for col in data_cols:
                        data['struts'][col] = df[col].tolist()
                elif file_category == 'mooring':
                    for col in data_cols:
                        data['lines'][col] = df[col].tolist()
                elif file_category in ['fst1', 'fst2']:
                    # For FST files, categorize by data type
                    for col in data_cols:
                        col_lower = col.lower()
                        if 'force' in col_lower or 'fx' in col_lower or 'fy' in col_lower or 'fz' in col_lower:
                            if 'forces' not in data:
                                data['forces'] = {}
                            data['forces'][col] = df[col].tolist()
                        elif 'displace' in col_lower or 'velocity' in col_lower or 'position' in col_lower:
                            if 'motion' not in data:
                                data['motion'] = {}
                            data['motion'][col] = df[col].tolist()
                        else:
                            if 'other' not in data:
                                data['other'] = {}
                            data['other'][col] = df[col].tolist()
            else:
                # Original logic for files without clear category in filename
                # Extract mooring line data
                line_cols = [col for col in df.columns if 'line' in col.lower() or 'mooring' in col.lower()]
                for col in line_cols:
                    data['lines'][col] = df[col].tolist()
                
                # Extract strut data  
                strut_cols = [col for col in df.columns if 'strut' in col.lower()]
                for col in strut_cols:
                    data['struts'][col] = df[col].tolist()
                
                # Extract jacket data
                jacket_cols = [col for col in df.columns if 'jacket' in col.lower()]
                for col in jacket_cols:
                    data['jacket'][col] = df[col].tolist()
                
                # Extract force data
                force_cols = [col for col in df.columns if 'force' in col.lower() or 'fx' in col.lower() or 'fy' in col.lower() or 'fz' in col.lower()]
                if force_cols:
                    data['forces'] = {}
                    for col in force_cols:
                        data['forces'][col] = df[col].tolist()
                
                # Extract displacement/motion data
                motion_cols = [col for col in df.columns if 'displace' in col.lower() or 'velocity' in col.lower() or 'position' in col.lower()]
                if motion_cols:
                    data['motion'] = {}
                    for col in motion_cols:
                        data['motion'][col] = df[col].tolist()
                
                # If we still don't have categorized data, put all numeric columns in 'other'
                if not any([data['lines'], data['struts'], data['jacket'], data.get('forces'), data.get('motion')]):
                    data['other'] = {}
                    for col in df.columns:
                        if col not in ['time', time_cols[0] if time_cols else ''] and df[col].dtype in ['float64', 'int64']:
                            data['other'][col] = df[col].tolist()
            
            # Add metadata about data duration
            if data['time']:
                data['metadata']['duration'] = data['time'][-1] if data['time'] else 0
                data['metadata']['dt'] = data['time'][1] - data['time'][0] if len(data['time']) > 1 else 0.1
        
        return jsonify(data)
        
    except Exception as e:
        print(f"Error in /api/data: {e}")
        import traceback
        traceback.print_exc()
        return jsonify({'error': str(e)}), 500

@app.route('/api/test')
def test():
    """Test endpoint to verify server is running"""
    return jsonify({'status': 'Server is running', 'base_dir': BASE_DIR})

@app.route('/api/excel_config')
def get_excel_configuration():
    """Get complete Excel-based configuration for UI"""
    try:
        config = excel_reader.get_all_configurations()
        return jsonify(config)
    except Exception as e:
        print(f"Error reading Excel config: {e}")
        return jsonify(DEFAULT_CONFIG)

@app.route('/api/vessel_types')
def get_vessel_types():
    """Get available vessel types from Excel files"""
    try:
        vessel_types = excel_reader.get_vessel_types()
        if not vessel_types:
            # Fallback to default vessel types
            vessel_types = DEFAULT_CONFIG['vessel_types']
        return jsonify(vessel_types)
    except Exception as e:
        print(f"Error getting vessel types: {e}")
        return jsonify(DEFAULT_CONFIG['vessel_types'])

@app.route('/api/vessel_config/<vessel_type>')
def get_vessel_configuration(vessel_type):
    """Get configuration for specific vessel type"""
    try:
        config = excel_reader.get_configuration_for_vessel(vessel_type.upper())
        if not config:
            # Return appropriate default based on vessel type
            if vessel_type.upper() == 'FST':
                config = DEFAULT_CONFIG.get('fst', {})
            elif vessel_type.upper() == 'LNGC':
                config = DEFAULT_CONFIG.get('lngc', {})
            else:
                config = {}
        return jsonify(config)
    except Exception as e:
        print(f"Error getting vessel config: {e}")
        return jsonify({})

@app.route('/api/refresh_config', methods=['POST'])
def refresh_configuration():
    """Refresh Excel configuration cache"""
    try:
        excel_reader.refresh_cache()
        return jsonify({'status': 'success', 'message': 'Configuration refreshed'})
    except Exception as e:
        print(f"Error refreshing config: {e}")
        return jsonify({'status': 'error', 'message': str(e)}), 500

@app.route('/api/file_patterns/<vessel_type>')
def get_file_patterns(vessel_type):
    """Get file patterns for specific vessel type"""
    try:
        config = excel_reader.get_configuration_for_vessel(vessel_type.upper())
        patterns = config.get('file_patterns', [])
        return jsonify({'vessel_type': vessel_type, 'patterns': patterns})
    except Exception as e:
        print(f"Error getting file patterns: {e}")
        return jsonify({'vessel_type': vessel_type, 'patterns': []})

def process_single_strut_file(file_path):
    """Process a single strut file to find max force - used for parallel processing"""
    try:
        df = pd.read_csv(file_path)
        filename = os.path.basename(file_path)
        
        # Find force columns
        force_cols = []
        for col in df.columns:
            col_lower = col.lower()
            if 'force' in col_lower or 'fx' in col_lower or 'fy' in col_lower or 'fz' in col_lower:
                if df[col].dtype in ['float64', 'int64']:
                    force_cols.append(col)
        
        max_force = 0
        max_col = None
        
        # Get max absolute force across all force columns
        for col in force_cols:
            col_max = df[col].abs().max()
            if col_max > max_force:
                max_force = col_max
                max_col = col
        
        if max_force > 0:
            # Extract configuration from filename
            parts = filename.upper().replace('.CSV', '').split('_')
            config = {
                'filename': filename,
                'basename': filename.split('_')[0] if '_' in filename else filename.replace('.csv', ''),
                'max_force': float(max_force),
                'force_column': max_col,
                'file_path': file_path
            }
            
            # Extract FST configuration
            if 'FST1' in filename.upper():
                fst1_match = re.search(r'FST1_([EF])', filename.upper())
                fst2_match = re.search(r'FST2_([EF])', filename.upper())
                if fst1_match:
                    config['fst1'] = '15' if fst1_match.group(1) == 'E' else '95'
                if fst2_match:
                    config['fst2'] = '15' if fst2_match.group(1) == 'E' else '95'
            
            # Extract tide level
            for tide in ['HWL', 'MWL', 'LWL']:
                if tide in filename.upper():
                    config['tide'] = tide.lower()
                    break
            
            # Extract heading
            heading_match = re.search(r'(\d{3})deg', filename, re.IGNORECASE)
            if heading_match:
                config['heading'] = heading_match.group(1).lstrip('0') or '0'
            
            # Extract environment type
            if 'non' in filename.lower() and 'colinear' in filename.lower():
                config['envType'] = 'non-colinear'
            elif 'colinear' in filename.lower():
                config['envType'] = 'colinear'
            
            # Detect vessel type from filename
            if 'FST' in filename.upper():
                config['vesselType'] = 'FST'
            elif 'LNGC' in filename.upper():
                config['vesselType'] = 'LNGC'
            
            return config
        
        return None
        
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return None

@app.route('/api/max_strut_force')
def get_max_strut_force_config():
    """Find the configuration with maximum absolute strut force using parallel processing"""
    try:
        start_time = time.time()
        
        subfolder = request.args.get('subfolder')
        if not subfolder:
            return jsonify({'error': 'Missing subfolder parameter'}), 400
        
        folder_path = os.path.join(BASE_DIR, subfolder)
        if not os.path.exists(folder_path):
            return jsonify({'error': f'Folder not found: {subfolder}'}), 404
        
        # Find all strut force files
        csv_files = glob.glob(os.path.join(folder_path, "*.csv"))
        strut_files = [f for f in csv_files if 'strut' in os.path.basename(f).lower()]
        
        if not strut_files:
            # No strut files, find any jacket files as fallback
            strut_files = [f for f in csv_files if 'jacket' in os.path.basename(f).lower()]
        
        if not strut_files:
            return jsonify({'error': 'No strut or jacket force files found in folder'}), 404
        
        print(f"Processing {len(strut_files)} files in parallel...")
        
        # Use parallel processing to scan all files simultaneously
        max_workers = min(multiprocessing.cpu_count(), len(strut_files))
        max_config = None
        max_force = 0
        
        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            # Submit all files for processing
            future_to_file = {executor.submit(process_single_strut_file, file_path): file_path 
                             for file_path in strut_files}
            
            # Collect results as they complete
            for future in as_completed(future_to_file):
                file_path = future_to_file[future]
                try:
                    result = future.result()
                    if result and result['max_force'] > max_force:
                        max_force = result['max_force']
                        max_config = result
                except Exception as e:
                    print(f"Error processing {file_path}: {e}")
        
        if max_config:
            # Now find all related files with the same configuration
            related_files = []
            base_pattern = max_config.get('basename', '')
            
            # Build pattern from extracted config
            pattern_parts = []
            if 'fst1' in max_config and 'fst2' in max_config:
                fst1_code = 'E' if max_config['fst1'] == '15' else 'F'
                fst2_code = 'E' if max_config['fst2'] == '15' else 'F'
                pattern_parts.extend([f'FST1_{fst1_code}', f'FST2_{fst2_code}'])
            
            # Find all files matching this configuration
            for csv_file in csv_files:
                filename = os.path.basename(csv_file)
                matches = True
                
                for part in pattern_parts:
                    if part not in filename.upper():
                        matches = False
                        break
                
                if 'tide' in max_config and max_config['tide'].upper() not in filename.upper():
                    matches = False
                
                if 'heading' in max_config:
                    heading_pattern = f"{max_config['heading'].zfill(3)}deg"
                    if heading_pattern not in filename.lower():
                        matches = False
                
                if matches:
                    related_files.append({
                        'filename': filename,
                        'path': csv_file,
                        'category': 'jacket' if 'jacket' in filename.lower() else
                                   'strut' if 'strut' in filename.lower() else
                                   'mooring' if 'mooring' in filename.lower() or 'line' in filename.lower() else
                                   'fst' if 'fst' in filename.lower() else 'other'
                    })
            
            max_config['related_files'] = related_files
            max_config['total_files'] = len(related_files)
            
            # Add processing time
            processing_time = time.time() - start_time
            max_config['processing_time'] = f"{processing_time:.2f} seconds"
            print(f"Parallel processing completed in {processing_time:.2f} seconds")
            
            return jsonify(max_config)
        else:
            return jsonify({'error': 'No force data found in strut files'}), 404
            
    except Exception as e:
        print(f"Error in /api/max_strut_force: {e}")
        import traceback
        traceback.print_exc()
        return jsonify({'error': str(e)}), 500

if __name__ == '__main__':
    print(f"Starting OrcaFlex Data Server...")
    print(f"Base directory: {BASE_DIR}")
    print(f"Server running at http://localhost:5000")
    print(f"Dashboard available at http://localhost:5000/")
    app.run(debug=True, port=5000)