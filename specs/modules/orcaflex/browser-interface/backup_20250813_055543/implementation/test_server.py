"""
Simple test server for OrcaFlex Browser Interface
Run this to test the backend services via HTTP
"""

from flask import Flask, jsonify, request, send_from_directory
from flask_cors import CORS
import sys
import os

# Add backend to path
sys.path.append(os.path.join(os.path.dirname(__file__), 'src', 'backend'))

# Import our services
from parameter_service import ParameterService, ParameterConfig
from file_search_engine import FileSearchEngine
from config_manager import ConfigurationManager
from validation_service import ConfigurationValidator
from max_force_finder import MaxForceFinder, ModeController

app = Flask(__name__)
CORS(app)  # Enable CORS for all routes

# Initialize services
param_service = ParameterService()
search_engine = FileSearchEngine()
config_manager = ConfigurationManager()
validator = ConfigurationValidator()
max_finder = MaxForceFinder()
mode_controller = ModeController(max_finder)

@app.route('/')
def index():
    """Serve the test dashboard"""
    return send_from_directory('.', 'test-dashboard.html')

@app.route('/api/status')
def status():
    """Check if server is running"""
    return jsonify({
        'status': 'running',
        'services': {
            'parameter_service': 'active',
            'search_engine': 'active',
            'config_manager': 'active',
            'validator': 'active',
            'max_finder': 'active'
        }
    })

@app.route('/api/parameters/options')
def get_parameter_options():
    """Get available parameter options"""
    return jsonify(param_service.get_available_options())

@app.route('/api/parameters/validate', methods=['POST'])
def validate_parameters():
    """Validate parameter configuration"""
    data = request.json
    config = ParameterConfig(**data)
    is_valid, errors = config.validate()
    return jsonify({
        'is_valid': is_valid,
        'errors': errors
    })

@app.route('/api/parameters/generate-pattern', methods=['POST'])
def generate_pattern():
    """Generate file pattern from parameters"""
    data = request.json
    config = ParameterConfig(**data)
    pattern = param_service.generate_pattern(config)
    return jsonify({
        'pattern': pattern,
        'config': config.to_dict()
    })

@app.route('/api/search/files', methods=['POST'])
def search_files():
    """Search for files matching pattern"""
    data = request.json
    pattern = data.get('pattern', '')
    
    # Perform search
    result = search_engine.search_files(pattern)
    
    # Convert to JSON-serializable format
    return jsonify({
        'pattern': result.pattern,
        'total_files': result.total_files,
        'available': result.available,
        'search_time': result.search_time,
        'files_by_category': {
            cat.value: len(files) 
            for cat, files in result.files_by_category.items()
        }
    })

@app.route('/api/search/max-configuration')
def get_max_configuration():
    """Get maximum force configuration"""
    max_config = mode_controller.get_current_configuration()
    return jsonify(max_config)

@app.route('/api/configs/save', methods=['POST'])
def save_configuration():
    """Save configuration"""
    data = request.json
    config = config_manager.save_configuration(
        name=data['name'],
        parameters=data['parameters'],
        user_id=data.get('user_id', 'test_user'),
        description=data.get('description')
    )
    return jsonify(config.to_dict())

@app.route('/api/configs/list')
def list_configurations():
    """List saved configurations"""
    user_id = request.args.get('user_id', 'test_user')
    configs = config_manager.list_configurations(user_id=user_id)
    return jsonify([c.to_dict() for c in configs])

@app.route('/api/test/create-mock-data', methods=['POST'])
def create_mock_data():
    """Create mock data files for testing"""
    import pandas as pd
    from pathlib import Path
    
    # Create test directory
    test_dir = Path("test_data/output/csv")
    test_dir.mkdir(parents=True, exist_ok=True)
    
    # Create mock summary file
    summary_data = {
        'fe_filename': ['fsts_03c_0100yr_l095_hwl.sim'],
        'Strut1_Body_eff_tension_max': [1500.0],
        'Strut2_Body_eff_tension_max': [1450.0],
        'Jacket1_force_max': [2000.0]
    }
    df = pd.DataFrame(summary_data)
    df.to_csv(test_dir / "dm_fsts_03c_0100yr_l095_hwl_strut_dyn.csv", index=False)
    
    # Create mock time series files
    files_created = []
    for component in ['Strut1', 'Strut2', 'Jacket1', 'Mooring1']:
        filename = f"fsts_l095_hwl_ncl_000deg_{component}.csv"
        ts_data = {
            'Time': range(0, 100),
            'Force': [1000 + i*10 for i in range(100)]
        }
        df = pd.DataFrame(ts_data)
        df.to_csv(test_dir / filename, index=False)
        files_created.append(filename)
    
    # Update search engine base path to use test data
    search_engine.base_path = Path("test_data")
    search_engine.refresh_index()
    
    return jsonify({
        'status': 'success',
        'message': f'Created {len(files_created) + 1} mock files',
        'files': files_created,
        'directory': str(test_dir)
    })

if __name__ == '__main__':
    print("\n" + "="*60)
    print("OrcaFlex Browser Interface - Test Server")
    print("="*60)
    print("\nStarting server...")
    print("\n📌 Access the interface at:")
    print("   http://localhost:5000")
    print("\n📌 API endpoints available at:")
    print("   http://localhost:5000/api/status")
    print("   http://localhost:5000/api/parameters/options")
    print("   http://localhost:5000/api/search/files")
    print("\n📌 Create mock data:")
    print("   POST http://localhost:5000/api/test/create-mock-data")
    print("\nPress Ctrl+C to stop the server")
    print("="*60 + "\n")
    
    app.run(debug=True, port=5000)