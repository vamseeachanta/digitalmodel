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
    return send_from_directory('.', 'orcaflex-data-browser-v5-enhanced.html')

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

if __name__ == '__main__':
    print(f"Starting OrcaFlex Data Server...")
    print(f"Base directory: {BASE_DIR}")
    print(f"Server running at http://localhost:5000")
    print(f"Dashboard available at http://localhost:5000/")
    app.run(debug=True, port=5000)