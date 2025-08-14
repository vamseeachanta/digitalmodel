"""
Step-by-Step OrcaFlex Browser Interface
Allows users to select folders and test each feature step by step
"""

from flask import Flask, jsonify, request, render_template_string
from flask_cors import CORS
import pandas as pd
import numpy as np
from pathlib import Path
import json
import os
import re
from datetime import datetime
import glob

app = Flask(__name__)
CORS(app)

# Global state
current_state = {
    'selected_folder': None,
    'files_found': [],
    'max_config': None,
    'current_step': 1
}

# HTML Template with step-by-step interface
HTML_TEMPLATE = '''
<!DOCTYPE html>
<html>
<head>
    <title>OrcaFlex Browser - Step by Step</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { 
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }
        .container { 
            max-width: 1000px; 
            margin: 0 auto;
            background: white;
            border-radius: 20px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            overflow: hidden;
        }
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            text-align: center;
        }
        .header h1 {
            font-size: 28px;
            margin-bottom: 10px;
        }
        .header p {
            opacity: 0.9;
            font-size: 14px;
        }
        .steps-container {
            display: flex;
            justify-content: center;
            padding: 30px;
            background: #f8f9fa;
            border-bottom: 1px solid #dee2e6;
        }
        .step {
            display: flex;
            align-items: center;
            margin: 0 15px;
        }
        .step-circle {
            width: 40px;
            height: 40px;
            border-radius: 50%;
            background: #e9ecef;
            color: #6c757d;
            display: flex;
            align-items: center;
            justify-content: center;
            font-weight: bold;
            margin-right: 10px;
            transition: all 0.3s;
        }
        .step.active .step-circle {
            background: #667eea;
            color: white;
            transform: scale(1.1);
        }
        .step.completed .step-circle {
            background: #28a745;
            color: white;
        }
        .step-label {
            font-size: 14px;
            color: #6c757d;
        }
        .step.active .step-label {
            color: #667eea;
            font-weight: 600;
        }
        .content {
            padding: 40px;
        }
        .step-content {
            display: none;
            animation: fadeIn 0.5s;
        }
        .step-content.active {
            display: block;
        }
        @keyframes fadeIn {
            from { opacity: 0; transform: translateY(10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        .action-card {
            background: #f8f9fa;
            border-radius: 10px;
            padding: 25px;
            margin-bottom: 20px;
            border: 2px solid #e9ecef;
        }
        .action-card h3 {
            color: #495057;
            margin-bottom: 15px;
            font-size: 20px;
        }
        .action-card p {
            color: #6c757d;
            margin-bottom: 20px;
            line-height: 1.6;
        }
        .folder-input {
            display: flex;
            gap: 10px;
            margin-bottom: 20px;
        }
        .folder-input input {
            flex: 1;
            padding: 12px;
            border: 2px solid #dee2e6;
            border-radius: 8px;
            font-size: 14px;
        }
        .folder-input input:focus {
            outline: none;
            border-color: #667eea;
        }
        .btn {
            padding: 12px 24px;
            border: none;
            border-radius: 8px;
            font-size: 14px;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.3s;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }
        .btn-primary {
            background: #667eea;
            color: white;
        }
        .btn-primary:hover {
            background: #5a67d8;
            transform: translateY(-2px);
            box-shadow: 0 5px 15px rgba(102, 126, 234, 0.4);
        }
        .btn-secondary {
            background: #6c757d;
            color: white;
        }
        .btn-success {
            background: #28a745;
            color: white;
        }
        .btn-info {
            background: #17a2b8;
            color: white;
        }
        .results {
            background: white;
            border: 2px solid #e9ecef;
            border-radius: 10px;
            padding: 20px;
            margin-top: 20px;
            max-height: 400px;
            overflow-y: auto;
        }
        .results h4 {
            color: #495057;
            margin-bottom: 15px;
            font-size: 16px;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }
        .file-item {
            padding: 10px;
            background: #f8f9fa;
            margin-bottom: 8px;
            border-radius: 6px;
            display: flex;
            justify-content: space-between;
            align-items: center;
            transition: all 0.2s;
        }
        .file-item:hover {
            background: #e9ecef;
            transform: translateX(5px);
        }
        .file-name {
            font-weight: 500;
            color: #495057;
        }
        .file-category {
            background: #667eea;
            color: white;
            padding: 3px 10px;
            border-radius: 12px;
            font-size: 11px;
            text-transform: uppercase;
        }
        .status {
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 20px;
            display: flex;
            align-items: center;
            gap: 10px;
        }
        .status.success {
            background: #d4edda;
            color: #155724;
            border: 1px solid #c3e6cb;
        }
        .status.error {
            background: #f8d7da;
            color: #721c24;
            border: 1px solid #f5c6cb;
        }
        .status.info {
            background: #d1ecf1;
            color: #0c5460;
            border: 1px solid #bee5eb;
        }
        .status-icon {
            font-size: 20px;
        }
        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
            gap: 15px;
            margin-top: 20px;
        }
        .stat-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 10px;
            text-align: center;
        }
        .stat-value {
            font-size: 32px;
            font-weight: bold;
            margin-bottom: 5px;
        }
        .stat-label {
            font-size: 12px;
            opacity: 0.9;
            text-transform: uppercase;
        }
        .config-display {
            background: #f8f9fa;
            border-radius: 8px;
            padding: 15px;
            font-family: 'Courier New', monospace;
            font-size: 13px;
            white-space: pre-wrap;
            border: 2px solid #dee2e6;
        }
        .navigation {
            display: flex;
            justify-content: space-between;
            margin-top: 30px;
            padding-top: 20px;
            border-top: 1px solid #dee2e6;
        }
        .sample-paths {
            background: #fff3cd;
            border: 1px solid #ffeaa7;
            border-radius: 8px;
            padding: 15px;
            margin-bottom: 20px;
        }
        .sample-paths h4 {
            color: #856404;
            margin-bottom: 10px;
            font-size: 14px;
        }
        .sample-paths ul {
            list-style: none;
            padding: 0;
        }
        .sample-paths li {
            padding: 5px 0;
            color: #856404;
            font-family: 'Courier New', monospace;
            font-size: 12px;
        }
        .spinner {
            border: 3px solid #f3f3f3;
            border-top: 3px solid #667eea;
            border-radius: 50%;
            width: 40px;
            height: 40px;
            animation: spin 1s linear infinite;
            margin: 20px auto;
        }
        @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🚀 OrcaFlex Browser Interface</h1>
            <p>Step-by-step guide to browse and analyze OrcaFlex files</p>
        </div>
        
        <div class="steps-container">
            <div class="step" id="step1">
                <div class="step-circle">1</div>
                <div class="step-label">Select Folder</div>
            </div>
            <div class="step" id="step2">
                <div class="step-circle">2</div>
                <div class="step-label">Scan Files</div>
            </div>
            <div class="step" id="step3">
                <div class="step-circle">3</div>
                <div class="step-label">Find Maximum</div>
            </div>
            <div class="step" id="step4">
                <div class="step-circle">4</div>
                <div class="step-label">Search & Filter</div>
            </div>
        </div>
        
        <div class="content">
            <!-- Step 1: Select Folder -->
            <div class="step-content" id="content1">
                <div class="action-card">
                    <h3>📁 Step 1: Select OrcaFlex Data Folder</h3>
                    <p>Enter the path to your OrcaFlex output folder containing CSV files. This is typically located in your project's output/csv directory.</p>
                    
                    <div class="sample-paths">
                        <h4>💡 Example paths to try:</h4>
                        <ul>
                            <li>D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv</li>
                            <li>C:\\OrcaFlex\\Projects\\MyProject\\output\\csv</li>
                            <li>sample_orcaflex_data\\output\\csv (for demo)</li>
                        </ul>
                    </div>
                    
                    <div class="folder-input">
                        <input type="text" id="folderPath" placeholder="Enter folder path (e.g., D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv)" value="D:\\1522\\ctr7\\orcaflex\\rev_a08\\output\\csv">
                        <button class="btn btn-primary" onclick="selectFolder()">Select Folder</button>
                    </div>
                    
                    <button class="btn btn-info" onclick="createSampleData()">Create Sample Data</button>
                    
                    <div id="folderStatus"></div>
                </div>
            </div>
            
            <!-- Step 2: Scan Files -->
            <div class="step-content" id="content2">
                <div class="action-card">
                    <h3>🔍 Step 2: Scan for OrcaFlex Files</h3>
                    <p>Now let's scan the selected folder for OrcaFlex CSV files. The system will categorize them automatically.</p>
                    
                    <button class="btn btn-primary" onclick="scanFiles()">Start Scanning</button>
                    
                    <div id="scanStatus"></div>
                    
                    <div id="scanResults" class="results" style="display:none;">
                        <h4>Files Found</h4>
                        <div id="filesList"></div>
                    </div>
                    
                    <div class="stats-grid" id="fileStats" style="display:none;">
                        <div class="stat-card">
                            <div class="stat-value" id="totalFiles">0</div>
                            <div class="stat-label">Total Files</div>
                        </div>
                        <div class="stat-card">
                            <div class="stat-value" id="summaryFiles">0</div>
                            <div class="stat-label">Summary Files</div>
                        </div>
                        <div class="stat-card">
                            <div class="stat-value" id="strutFiles">0</div>
                            <div class="stat-label">Strut Files</div>
                        </div>
                        <div class="stat-card">
                            <div class="stat-value" id="jacketFiles">0</div>
                            <div class="stat-label">Jacket Files</div>
                        </div>
                    </div>
                </div>
            </div>
            
            <!-- Step 3: Find Maximum -->
            <div class="step-content" id="content3">
                <div class="action-card">
                    <h3>⚡ Step 3: Find Maximum Force Configuration</h3>
                    <p>Analyze summary files to find the configuration with maximum strut forces. This helps identify critical loading conditions.</p>
                    
                    <button class="btn btn-primary" onclick="findMaxConfig()">Find Maximum</button>
                    
                    <div id="maxStatus"></div>
                    
                    <div id="maxResults" style="display:none;">
                        <h4 style="margin-top: 20px;">Maximum Configuration Found:</h4>
                        <div class="config-display" id="maxConfig"></div>
                    </div>
                </div>
            </div>
            
            <!-- Step 4: Search & Filter -->
            <div class="step-content" id="content4">
                <div class="action-card">
                    <h3>🎯 Step 4: Search and Filter Files</h3>
                    <p>Search for specific files using patterns. Try searching for vessel types, loading conditions, or components.</p>
                    
                    <div class="folder-input">
                        <input type="text" id="searchPattern" placeholder="Enter search pattern (e.g., fsts, l095, strut)">
                        <button class="btn btn-primary" onclick="searchFiles()">Search</button>
                    </div>
                    
                    <div style="margin-top: 10px;">
                        <button class="btn btn-secondary" onclick="searchFiles('strut')">Search Struts</button>
                        <button class="btn btn-secondary" onclick="searchFiles('jacket')">Search Jackets</button>
                        <button class="btn btn-secondary" onclick="searchFiles('mooring')">Search Moorings</button>
                        <button class="btn btn-secondary" onclick="searchFiles('summary')">Search Summaries</button>
                    </div>
                    
                    <div id="searchStatus"></div>
                    
                    <div id="searchResults" class="results" style="display:none;">
                        <h4>Search Results</h4>
                        <div id="searchList"></div>
                    </div>
                </div>
            </div>
            
            <div class="navigation">
                <button class="btn btn-secondary" onclick="previousStep()" id="prevBtn" style="display:none;">← Previous</button>
                <button class="btn btn-success" onclick="nextStep()" id="nextBtn" style="display:none;">Next →</button>
            </div>
        </div>
    </div>
    
    <script>
        let currentStep = 1;
        let folderSelected = false;
        let filesScanned = false;
        let maxFound = false;
        
        function updateSteps() {
            // Update step indicators
            for (let i = 1; i <= 4; i++) {
                const step = document.getElementById('step' + i);
                const content = document.getElementById('content' + i);
                
                step.classList.remove('active', 'completed');
                content.classList.remove('active');
                
                if (i < currentStep) {
                    step.classList.add('completed');
                } else if (i === currentStep) {
                    step.classList.add('active');
                    content.classList.add('active');
                }
            }
            
            // Update navigation buttons
            document.getElementById('prevBtn').style.display = currentStep > 1 ? 'block' : 'none';
            document.getElementById('nextBtn').style.display = 
                (currentStep < 4 && canProceed()) ? 'block' : 'none';
        }
        
        function canProceed() {
            switch(currentStep) {
                case 1: return folderSelected;
                case 2: return filesScanned;
                case 3: return maxFound;
                default: return false;
            }
        }
        
        function nextStep() {
            if (currentStep < 4 && canProceed()) {
                currentStep++;
                updateSteps();
            }
        }
        
        function previousStep() {
            if (currentStep > 1) {
                currentStep--;
                updateSteps();
            }
        }
        
        async function selectFolder() {
            const path = document.getElementById('folderPath').value;
            if (!path) {
                alert('Please enter a folder path');
                return;
            }
            
            const status = document.getElementById('folderStatus');
            status.className = 'status info';
            status.innerHTML = '<span class="status-icon">⏳</span> Checking folder...';
            
            try {
                const response = await fetch('/api/select-folder', {
                    method: 'POST',
                    headers: {'Content-Type': 'application/json'},
                    body: JSON.stringify({path})
                });
                const data = await response.json();
                
                if (data.success) {
                    status.className = 'status success';
                    status.innerHTML = `<span class="status-icon">✅</span> Folder selected: ${data.path}`;
                    folderSelected = true;
                    document.getElementById('nextBtn').style.display = 'block';
                } else {
                    status.className = 'status error';
                    status.innerHTML = `<span class="status-icon">❌</span> ${data.message}`;
                }
            } catch (error) {
                status.className = 'status error';
                status.innerHTML = '<span class="status-icon">❌</span> Error: ' + error.message;
            }
        }
        
        async function createSampleData() {
            const status = document.getElementById('folderStatus');
            status.className = 'status info';
            status.innerHTML = '<span class="status-icon">⏳</span> Creating sample data...';
            
            try {
                const response = await fetch('/api/create-sample', {method: 'POST'});
                const data = await response.json();
                
                if (data.success) {
                    status.className = 'status success';
                    status.innerHTML = `<span class="status-icon">✅</span> ${data.message}`;
                    document.getElementById('folderPath').value = data.path;
                    folderSelected = true;
                    document.getElementById('nextBtn').style.display = 'block';
                }
            } catch (error) {
                status.className = 'status error';
                status.innerHTML = '<span class="status-icon">❌</span> Error: ' + error.message;
            }
        }
        
        async function scanFiles() {
            const status = document.getElementById('scanStatus');
            status.className = 'status info';
            status.innerHTML = '<span class="status-icon">⏳</span> Scanning for files...';
            
            try {
                const response = await fetch('/api/scan');
                const data = await response.json();
                
                if (data.success) {
                    status.className = 'status success';
                    status.innerHTML = `<span class="status-icon">✅</span> Found ${data.count} files`;
                    
                    // Display files
                    const filesList = document.getElementById('filesList');
                    filesList.innerHTML = data.files.slice(0, 20).map(f => 
                        `<div class="file-item">
                            <span class="file-name">${f.name}</span>
                            <span class="file-category">${f.category}</span>
                        </div>`
                    ).join('');
                    
                    document.getElementById('scanResults').style.display = 'block';
                    
                    // Update stats
                    document.getElementById('totalFiles').textContent = data.count;
                    document.getElementById('summaryFiles').textContent = data.categories.summary || 0;
                    document.getElementById('strutFiles').textContent = data.categories.strut || 0;
                    document.getElementById('jacketFiles').textContent = data.categories.jacket || 0;
                    document.getElementById('fileStats').style.display = 'grid';
                    
                    filesScanned = true;
                    document.getElementById('nextBtn').style.display = 'block';
                }
            } catch (error) {
                status.className = 'status error';
                status.innerHTML = '<span class="status-icon">❌</span> Error: ' + error.message;
            }
        }
        
        async function findMaxConfig() {
            const status = document.getElementById('maxStatus');
            status.className = 'status info';
            status.innerHTML = '<span class="status-icon">⏳</span> Analyzing forces...';
            
            try {
                const response = await fetch('/api/find-max');
                const data = await response.json();
                
                if (data.success) {
                    status.className = 'status success';
                    status.innerHTML = `<span class="status-icon">✅</span> Maximum configuration found!`;
                    
                    document.getElementById('maxConfig').textContent = JSON.stringify(data.config, null, 2);
                    document.getElementById('maxResults').style.display = 'block';
                    
                    maxFound = true;
                    document.getElementById('nextBtn').style.display = 'block';
                }
            } catch (error) {
                status.className = 'status error';
                status.innerHTML = '<span class="status-icon">❌</span> Error: ' + error.message;
            }
        }
        
        async function searchFiles(pattern) {
            pattern = pattern || document.getElementById('searchPattern').value;
            if (!pattern) {
                alert('Please enter a search pattern');
                return;
            }
            
            const status = document.getElementById('searchStatus');
            status.className = 'status info';
            status.innerHTML = '<span class="status-icon">⏳</span> Searching...';
            
            try {
                const response = await fetch('/api/search', {
                    method: 'POST',
                    headers: {'Content-Type': 'application/json'},
                    body: JSON.stringify({pattern})
                });
                const data = await response.json();
                
                status.className = 'status success';
                status.innerHTML = `<span class="status-icon">✅</span> Found ${data.count} matching files`;
                
                const searchList = document.getElementById('searchList');
                searchList.innerHTML = data.files.map(f => 
                    `<div class="file-item">
                        <span class="file-name">${f.name}</span>
                        <span class="file-category">${f.category}</span>
                    </div>`
                ).join('');
                
                document.getElementById('searchResults').style.display = 'block';
            } catch (error) {
                status.className = 'status error';
                status.innerHTML = '<span class="status-icon">❌</span> Error: ' + error.message;
            }
        }
        
        // Initialize
        updateSteps();
    </script>
</body>
</html>
'''

@app.route('/')
def index():
    """Serve the step-by-step interface"""
    return render_template_string(HTML_TEMPLATE)

@app.route('/api/select-folder', methods=['POST'])
def select_folder():
    """Select and validate folder"""
    data = request.json
    folder_path = data.get('path', '').strip()
    
    # Convert to Path object
    folder = Path(folder_path)
    
    # Check if folder exists
    if folder.exists() and folder.is_dir():
        current_state['selected_folder'] = folder
        return jsonify({
            'success': True,
            'path': str(folder),
            'exists': True
        })
    else:
        # Try to create if it's a sample path
        if 'sample' in folder_path.lower():
            folder.mkdir(parents=True, exist_ok=True)
            current_state['selected_folder'] = folder
            return jsonify({
                'success': True,
                'path': str(folder),
                'exists': True,
                'message': 'Folder created'
            })
        
        return jsonify({
            'success': False,
            'message': f'Folder not found: {folder_path}'
        })

@app.route('/api/scan')
def scan_files():
    """Scan selected folder for CSV files"""
    if not current_state['selected_folder']:
        return jsonify({'success': False, 'message': 'No folder selected'})
    
    folder = current_state['selected_folder']
    csv_files = list(folder.glob('*.csv'))
    
    files_data = []
    categories = {'summary': 0, 'strut': 0, 'jacket': 0, 'mooring': 0, 'fst': 0, 'other': 0}
    
    for file_path in csv_files:
        category = categorize_file(file_path.name)
        categories[category] += 1
        
        files_data.append({
            'name': file_path.name,
            'path': str(file_path),
            'size': file_path.stat().st_size,
            'category': category,
            'modified': datetime.fromtimestamp(file_path.stat().st_mtime).isoformat()
        })
    
    current_state['files_found'] = files_data
    
    return jsonify({
        'success': True,
        'count': len(files_data),
        'files': files_data,
        'categories': categories
    })

@app.route('/api/find-max')
def find_max():
    """Find maximum force configuration"""
    if not current_state['files_found']:
        return jsonify({'success': False, 'message': 'No files scanned yet'})
    
    summary_files = [f for f in current_state['files_found'] if f['category'] == 'summary']
    
    if not summary_files:
        # Return default configuration
        config = {
            'vessel_type': 'fsts',
            'loading_condition': 'l095',
            'tide_level': 'hwl',
            'return_period': '0100yr',
            'max_force': 'N/A',
            'message': 'No summary files found, using default'
        }
        current_state['max_config'] = config
        return jsonify({'success': True, 'config': config})
    
    # Read first summary file to find max
    max_force = 0
    max_config = {}
    
    for file_info in summary_files[:1]:  # Just check first file for demo
        try:
            df = pd.read_csv(file_info['path'], nrows=10)
            
            # Look for max columns
            for col in df.columns:
                if 'max' in col.lower():
                    try:
                        val = pd.to_numeric(df[col], errors='coerce').max()
                        if not pd.isna(val) and val > max_force:
                            max_force = val
                            max_config = parse_config_from_filename(file_info['name'])
                            max_config['max_force'] = float(val)
                            max_config['source_file'] = file_info['name']
                    except:
                        pass
        except Exception as e:
            print(f"Error reading {file_info['name']}: {e}")
    
    if not max_config:
        max_config = {
            'vessel_type': 'fsts',
            'loading_condition': 'l095',
            'tide_level': 'hwl',
            'return_period': '0100yr',
            'max_force': 0,
            'message': 'Could not determine max from files'
        }
    
    current_state['max_config'] = max_config
    return jsonify({'success': True, 'config': max_config})

@app.route('/api/search', methods=['POST'])
def search_files():
    """Search files by pattern"""
    if not current_state['files_found']:
        return jsonify({'success': False, 'message': 'No files scanned yet'})
    
    data = request.json
    pattern = data.get('pattern', '').lower()
    
    # Special case for category search
    if pattern in ['summary', 'strut', 'jacket', 'mooring', 'fst']:
        matching = [f for f in current_state['files_found'] if f['category'] == pattern]
    else:
        matching = [f for f in current_state['files_found'] if pattern in f['name'].lower()]
    
    return jsonify({
        'success': True,
        'pattern': pattern,
        'count': len(matching),
        'files': matching[:50]  # Limit to 50 results
    })

@app.route('/api/create-sample', methods=['POST'])
def create_sample():
    """Create sample OrcaFlex data"""
    sample_dir = Path('sample_orcaflex_data/output/csv')
    sample_dir.mkdir(parents=True, exist_ok=True)
    
    # Create sample files
    files_created = []
    
    # Create summary file
    summary_data = {
        'fe_filename': ['fsts_03c_0100yr_l095_hwl.sim', 'fsts_03c_0100yr_l050_mwl.sim'],
        'Strut1_Body_eff_tension_max': [1500.0, 1200.0],
        'Strut2_Body_eff_tension_max': [1450.0, 1150.0],
        'Jacket1_force_max': [2000.0, 1800.0]
    }
    df = pd.DataFrame(summary_data)
    summary_file = sample_dir / 'dm_fsts_03c_0100yr_strut_dyn.csv'
    df.to_csv(summary_file, index=False)
    files_created.append(summary_file.name)
    
    # Create time series files
    components = ['Strut1', 'Strut2', 'Jacket1', 'Jacket2', 'Mooring1', 'Mooring2']
    configs = [
        ('fsts_03c_0100yr_l095_hwl', 1500),
        ('fsts_03c_0100yr_l050_mwl', 1200),
        ('fsts_03c_0100yr_l015_lwl', 900)
    ]
    
    for config_name, base_force in configs:
        for component in components[:3]:  # Create 3 components per config
            time = np.linspace(0, 3600, 100)
            force = base_force + 100 * np.sin(2 * np.pi * time / 300)
            
            ts_data = {
                'Time': time,
                'Force': force,
                'Moment_X': force * 0.5
            }
            df = pd.DataFrame(ts_data)
            
            filename = f'{config_name}_{component}.csv'
            df.to_csv(sample_dir / filename, index=False)
            files_created.append(filename)
    
    current_state['selected_folder'] = sample_dir
    
    return jsonify({
        'success': True,
        'message': f'Created {len(files_created)} sample files',
        'path': str(sample_dir),
        'files': files_created
    })

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

def parse_config_from_filename(filename):
    """Parse configuration from filename"""
    config = {
        'vessel_type': 'fsts',
        'loading_condition': 'l015',
        'tide_level': 'hwl',
        'return_period': '0100yr'
    }
    
    filename_lower = filename.lower()
    
    # Extract parameters
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

if __name__ == '__main__':
    print("\n" + "="*60)
    print("ORCAFLEX BROWSER - STEP BY STEP INTERFACE")
    print("="*60)
    print("\n[INFO] Starting server with step-by-step interface")
    print("[WEB] Open your browser to: http://localhost:5002")
    print("\n[FEATURES]")
    print("  1. Select any folder containing OrcaFlex CSV files")
    print("  2. Scan and categorize files automatically")
    print("  3. Find maximum force configuration")
    print("  4. Search files by pattern")
    print("\nPress Ctrl+C to stop")
    print("="*60 + "\n")
    
    app.run(debug=True, port=5002)