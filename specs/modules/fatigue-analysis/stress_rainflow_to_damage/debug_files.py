import yaml
from pathlib import Path

# Load sample config
with open('input/damage_analysis_config_sample.yml', 'r') as f:
    config = yaml.safe_load(f)

# Get base folder
base_folder = Path(config['input_data']['stress_rainflow']['data_folder'])
print(f"Base folder: {base_folder}")
print(f"Exists: {base_folder.exists()}")

# Get selection criteria
selection = config['input_data']['processing']['file_selection']
print(f"\nSelection criteria:")
print(f"  all_files: {selection['all_files']}")
print(f"  configs: {selection.get('configs', [])}")
print(f"  fc_numbers: {selection.get('fc_numbers', [])}")
print(f"  strut_numbers: {selection.get('strut_numbers', [])}")
print(f"  location_ids: {selection.get('location_ids', [])}")

# List all CSV files
all_files = list(base_folder.glob('*_stress_rainflow.csv'))
print(f"\nTotal files found: {len(all_files)}")

# Check first 5 files
print("\nFirst 5 files:")
for file in all_files[:5]:
    filename = file.stem
    parts = filename.split('_')
    print(f"  {filename}")
    print(f"    Parts: {parts}")
    
    # Find components
    location_id = parts[-2]
    for i, part in enumerate(parts):
        if part.startswith('FC'):
            fc_num = int(part[2:])
            config_name = '_'.join(parts[:i])
            print(f"    Config: {config_name}, FC: {fc_num}, Location: {location_id}")
            break

# Test matching logic
print("\n\nTesting matches for sample config:")
matched = []
for file in all_files:
    filename = file.stem
    parts = filename.split('_')
    
    location_id = parts[-2]
    strut_num = None
    fc_num = None
    config_name = None
    
    for i, part in enumerate(parts):
        if part.startswith('FC'):
            fc_num = int(part[2:])
            config_name = '_'.join(parts[:i])
        if part.startswith('Strut'):
            strut_num = int(part[5:])
    
    # Check match
    match = True
    
    # Check config
    if selection.get('configs') and config_name not in selection['configs']:
        match = False
        
    # Check FC number
    if selection.get('fc_numbers') and fc_num not in selection['fc_numbers']:
        match = False
        
    # Check strut number  
    if selection.get('strut_numbers') and strut_num not in selection['strut_numbers']:
        match = False
        
    # Check location
    if selection.get('location_ids') and location_id not in selection['location_ids']:
        match = False
    
    if match:
        matched.append(filename)
        if len(matched) <= 10:
            print(f"  MATCH: {filename}")