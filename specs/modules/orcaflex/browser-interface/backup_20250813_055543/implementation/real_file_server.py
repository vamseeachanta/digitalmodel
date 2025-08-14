"""
Real OrcaFlex File Server - Actually reads and serves real CSV files
This server connects to the actual OrcaFlex directory and provides a web API
"""

from flask import Flask, jsonify, request, send_from_directory, Response
from flask_cors import CORS
import pandas as pd
import numpy as np
from pathlib import Path
import json
import os
import re
from datetime import datetime

app = Flask(__name__)
CORS(app)

# ACTUAL OrcaFlex data path - CHANGE THIS TO YOUR REAL PATH
ORCAFLEX_BASE_PATH = Path("D:/1522/ctr7/orcaflex/rev_a08")
OUTPUT_PATH = ORCAFLEX_BASE_PATH / "output" / "csv"

# Global cache for file scanning
file_cache = {
    'files': [],
    'last_scan': None,
    'max_config': None
}

def scan_real_files():
    """Scan for actual OrcaFlex CSV files"""
    global file_cache
    
    files = []
    search_paths = [
        OUTPUT_PATH,
        OUTPUT_PATH / "combined",
        OUTPUT_PATH / "individual",
        ORCAFLEX_BASE_PATH / "output",
        Path("sample_orcaflex_data/output/csv")  # Fallback to sample data
    ]
    
    for path in search_paths:
        if path.exists():
            csv_files = list(path.glob("*.csv"))
            for f in csv_files:
                files.append({
                    'path': str(f),
                    'name': f.name,
                    'size': f.stat().st_size,
                    'modified': datetime.fromtimestamp(f.stat().st_mtime).isoformat(),
                    'category': categorize_file(f.name)
                })
    
    file_cache['files'] = files
    file_cache['last_scan'] = datetime.now().isoformat()
    
    return files

def categorize_file(filename):
    """Categorize file based on name"""
    filename_lower = filename.lower()
    if filename_lower.startswith('dm'):
        return 'summary'
    elif 'strut' in filename_lower:
        return 'strut'
    elif 'jacket' in filename_lower:
        return 'jacket'
    elif 'mooring' in filename_lower or 'line' in filename_lower:
        return 'mooring'
    elif 'fst' in filename_lower:
        return 'fst'
    return 'other'

def find_max_force_config():
    """Find configuration with maximum forces from real files"""
    summary_files = [f for f in file_cache['files'] if f['category'] == 'summary']
    
    if not summary_files:
        # Return default if no summary files
        return {
            'vessel_type': 'fsts',
            'loading_condition': 'l095',
            'tide_level': 'hwl',
            'return_period': '0100yr',
            'wave_direction': '000deg',
            'analysis_type': '03c',
            'max_force': 0
        }
    
    max_force = 0
    max_config = None
    
    for file_info in summary_files[:5]:  # Check first 5 summary files
        try:
            df = pd.read_csv(file_info['path'], nrows=100)
            
            # Find force columns
            force_cols = [col for col in df.columns if 'max' in col.lower()]
            
            for col in force_cols:
                try:
                    col_max = pd.to_numeric(df[col], errors='coerce').max()
                    if not np.isnan(col_max) and col_max > max_force:
                        max_force = col_max
                        # Parse config from filename
                        max_config = parse_filename_config(file_info['name'])
                        max_config['max_force'] = float(col_max)
                        max_config['source_file'] = file_info['name']
                except:
                    pass
        except:
            pass
    
    if max_config:
        file_cache['max_config'] = max_config
        return max_config
    
    return None

def parse_filename_config(filename):
    """Parse configuration from filename"""
    config = {
        'vessel_type': 'fsts',
        'loading_condition': 'l015',
        'tide_level': 'hwl',
        'return_period': '0100yr',
        'wave_direction': '000deg',
        'analysis_type': '03c'
    }
    
    filename_lower = filename.lower()
    
    # Extract parameters using regex
    for vessel in ['fsts', 'flng', 'lngc']:
        if vessel in filename_lower:
            config['vessel_type'] = vessel
    
    match = re.search(r'(l\d{3})', filename_lower)
    if match:
        config['loading_condition'] = match.group(1)
    
    for tide in ['hwl', 'mwl', 'lwl']:
        if tide in filename_lower:
            config['tide_level'] = tide
    
    match = re.search(r'(\d{4}yr)', filename_lower)
    if match:
        config['return_period'] = match.group(1)
    
    return config

@app.route('/')
def index():
    """Serve main page"""
    return '''
    <!DOCTYPE html>
    <html>
    <head>
        <title>OrcaFlex Browser - Real Files</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 20px; }
            .container { max-width: 1200px; margin: 0 auto; }
            .status { padding: 10px; margin: 10px 0; border-radius: 5px; }
            .success { background: #d4edda; color: #155724; }
            .error { background: #f8d7da; color: #721c24; }
            .info { background: #d1ecf1; color: #0c5460; }
            .file-list { max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; }
            .file-item { padding: 5px; border-bottom: 1px solid #eee; }
            button { padding: 10px 20px; margin: 5px; cursor: pointer; }
            .config-display { background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; }
            pre { background: #f4f4f4; padding: 10px; border-radius: 3px; overflow-x: auto; }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>OrcaFlex Browser Interface - Real File System</h1>
            
            <div id="status" class="status info">Initializing...</div>
            
            <div>
                <button onclick="scanFiles()">Scan Files</button>
                <button onclick="findMax()">Find Max Configuration</button>
                <button onclick="searchFiles()">Search Pattern</button>
                <button onclick="createSample()">Create Sample Data</button>
            </div>
            
            <div id="config" class="config-display"></div>
            
            <h2>Files Found: <span id="fileCount">0</span></h2>
            <div id="files" class="file-list"></div>
            
            <div id="results"></div>
        </div>
        
        <script>
            async function scanFiles() {
                const status = document.getElementById('status');
                status.className = 'status info';
                status.textContent = 'Scanning files...';
                
                try {
                    const response = await fetch('/api/scan');
                    const data = await response.json();
                    
                    status.className = 'status success';
                    status.textContent = `Found ${data.count} files at ${data.path}`;
                    
                    document.getElementById('fileCount').textContent = data.count;
                    
                    const fileList = document.getElementById('files');
                    fileList.innerHTML = data.files.slice(0, 50).map(f => 
                        `<div class="file-item">
                            <strong>${f.name}</strong> 
                            <span style="color: #666;">[${f.category}]</span>
                            <span style="float: right;">${(f.size/1024).toFixed(1)} KB</span>
                        </div>`
                    ).join('');
                } catch (error) {
                    status.className = 'status error';
                    status.textContent = 'Error: ' + error.message;
                }
            }
            
            async function findMax() {
                const status = document.getElementById('status');
                status.className = 'status info';
                status.textContent = 'Finding maximum force configuration...';
                
                try {
                    const response = await fetch('/api/find-max');
                    const data = await response.json();
                    
                    if (data.config) {
                        status.className = 'status success';
                        status.textContent = `Max force: ${data.config.max_force?.toFixed(2) || 'N/A'}`;
                        
                        document.getElementById('config').innerHTML = 
                            '<h3>Maximum Force Configuration</h3>' +
                            '<pre>' + JSON.stringify(data.config, null, 2) + '</pre>';
                    } else {
                        status.className = 'status info';
                        status.textContent = 'No maximum configuration found';
                    }
                } catch (error) {
                    status.className = 'status error';
                    status.textContent = 'Error: ' + error.message;
                }
            }
            
            async function searchFiles() {
                const pattern = prompt('Enter search pattern (e.g., fsts_l095_hwl):');
                if (!pattern) return;
                
                const status = document.getElementById('status');
                status.className = 'status info';
                status.textContent = 'Searching...';
                
                try {
                    const response = await fetch('/api/search', {
                        method: 'POST',
                        headers: {'Content-Type': 'application/json'},
                        body: JSON.stringify({pattern})
                    });
                    const data = await response.json();
                    
                    status.className = 'status success';
                    status.textContent = `Found ${data.count} matching files`;
                    
                    const results = document.getElementById('results');
                    results.innerHTML = '<h3>Search Results</h3>' +
                        '<div class="file-list">' +
                        data.files.map(f => 
                            `<div class="file-item">${f.name}</div>`
                        ).join('') +
                        '</div>';
                } catch (error) {
                    status.className = 'status error';
                    status.textContent = 'Error: ' + error.message;
                }
            }
            
            async function createSample() {
                if (!confirm('Create sample OrcaFlex data files?')) return;
                
                const status = document.getElementById('status');
                status.className = 'status info';
                status.textContent = 'Creating sample data...';
                
                try {
                    const response = await fetch('/api/create-sample', {method: 'POST'});
                    const data = await response.json();
                    
                    status.className = 'status success';
                    status.textContent = data.message;
                    
                    // Rescan files
                    setTimeout(scanFiles, 1000);
                } catch (error) {
                    status.className = 'status error';
                    status.textContent = 'Error: ' + error.message;
                }
            }
            
            // Initial scan
            window.onload = scanFiles;
        </script>
    </body>
    </html>
    '''

@app.route('/api/scan')
def api_scan_files():
    """API endpoint to scan files"""
    files = scan_real_files()
    
    # Check if path exists
    path_exists = OUTPUT_PATH.exists() or Path("sample_orcaflex_data").exists()
    
    return jsonify({
        'count': len(files),
        'files': files,
        'path': str(OUTPUT_PATH if OUTPUT_PATH.exists() else "sample_orcaflex_data"),
        'path_exists': path_exists,
        'categories': {
            cat: len([f for f in files if f['category'] == cat])
            for cat in ['summary', 'strut', 'jacket', 'mooring', 'fst', 'other']
        }
    })

@app.route('/api/find-max')
def api_find_max():
    """API endpoint to find maximum configuration"""
    if not file_cache['files']:
        scan_real_files()
    
    config = find_max_force_config()
    return jsonify({'config': config})

@app.route('/api/search', methods=['POST'])
def api_search():
    """API endpoint to search files"""
    data = request.json
    pattern = data.get('pattern', '').lower()
    
    if not file_cache['files']:
        scan_real_files()
    
    matching = [f for f in file_cache['files'] if pattern in f['name'].lower()]
    
    return jsonify({
        'pattern': pattern,
        'count': len(matching),
        'files': matching[:100]  # Limit to 100 results
    })

@app.route('/api/create-sample', methods=['POST'])
def api_create_sample():
    """Create sample OrcaFlex data"""
    sample_dir = Path("sample_orcaflex_data/output/csv")
    sample_dir.mkdir(parents=True, exist_ok=True)
    
    # Create sample files
    files_created = []
    
    # Summary file
    summary_data = {
        'fe_filename': ['fsts_03c_0100yr_l095_hwl.sim'],
        'Strut1_Body_eff_tension_max': [1500.0],
        'Strut2_Body_eff_tension_max': [1450.0],
        'Jacket1_force_max': [2000.0]
    }
    df = pd.DataFrame(summary_data)
    summary_file = sample_dir / "dm_fsts_03c_0100yr_strut_dyn.csv"
    df.to_csv(summary_file, index=False)
    files_created.append(summary_file.name)
    
    # Time series files
    for component in ['Strut1', 'Strut2', 'Jacket1', 'Mooring1']:
        time = np.linspace(0, 3600, 361)
        force = 1000 + 100 * np.sin(2 * np.pi * time / 300)
        
        ts_data = {'Time': time, 'Force': force}
        df = pd.DataFrame(ts_data)
        
        filename = f"fsts_03c_0100yr_l095_hwl_{component}.csv"
        df.to_csv(sample_dir / filename, index=False)
        files_created.append(filename)
    
    return jsonify({
        'success': True,
        'message': f'Created {len(files_created)} sample files in {sample_dir}',
        'files': files_created
    })

@app.route('/api/read-file/<path:filename>')
def api_read_file(filename):
    """Read actual CSV file data"""
    file_path = None
    
    # Search for file
    for f in file_cache['files']:
        if f['name'] == filename:
            file_path = Path(f['path'])
            break
    
    if not file_path or not file_path.exists():
        return jsonify({'error': 'File not found'}), 404
    
    try:
        df = pd.read_csv(file_path, nrows=100)  # Read first 100 rows
        return jsonify({
            'filename': filename,
            'columns': list(df.columns),
            'shape': df.shape,
            'data': df.head(20).to_dict('records'),
            'stats': {
                col: {
                    'min': float(df[col].min()) if pd.api.types.is_numeric_dtype(df[col]) else None,
                    'max': float(df[col].max()) if pd.api.types.is_numeric_dtype(df[col]) else None,
                    'mean': float(df[col].mean()) if pd.api.types.is_numeric_dtype(df[col]) else None
                }
                for col in df.columns if pd.api.types.is_numeric_dtype(df[col])
            }
        })
    except Exception as e:
        return jsonify({'error': str(e)}), 500

if __name__ == '__main__':
    print("\n" + "="*60)
    print("ORCAFLEX BROWSER - REAL FILE SERVER")
    print("="*60)
    print(f"\n[DIR] OrcaFlex Path: {ORCAFLEX_BASE_PATH}")
    print(f"[DIR] Output Path: {OUTPUT_PATH}")
    print(f"[DIR] Path Exists: {OUTPUT_PATH.exists()}")
    
    if not OUTPUT_PATH.exists():
        print("\n[WARNING] OrcaFlex path not found!")
        print("   Will use sample data if created.")
    
    print("\n[WEB] Server starting at: http://localhost:5001")
    print("\n[API] Endpoints:")
    print("   GET  /                - Web interface")
    print("   GET  /api/scan        - Scan for files")
    print("   GET  /api/find-max    - Find max configuration")
    print("   POST /api/search      - Search files")
    print("   POST /api/create-sample - Create sample data")
    print("\nPress Ctrl+C to stop")
    print("="*60 + "\n")
    
    app.run(debug=True, port=5001)