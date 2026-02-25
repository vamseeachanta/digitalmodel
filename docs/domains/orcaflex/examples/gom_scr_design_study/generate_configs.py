#!/usr/bin/env python3
"""
ABOUTME: Configuration generator for Gulf of Mexico SCR design study - creates 27 YAML
         configuration files for parametric study with varying riser, depth, and environment.
"""

from pathlib import Path
import yaml

print("="*80)
print("GULF OF MEXICO SCR DESIGN STUDY - Configuration Generator")
print("="*80)

# Output directory
output_dir = Path(__file__).parent / "configs"
output_dir.mkdir(exist_ok=True)

# Study parameters
study_params = {
    'risers': [
        {'id': 'SCR_10inch_X65', 'name': '10in'},
        {'id': 'SCR_12inch_X65', 'name': '12in'},
        {'id': 'SCR_10inch_X70', 'name': 'x70'},
    ],
    'depths': [
        {'value': 1000, 'name': '1000m'},
        {'value': 1200, 'name': '1200m'},
        {'value': 1400, 'name': '1400m'},
    ],
    'environments': [
        {'id': 'GoM_1yr', 'name': '1yr'},
        {'id': 'GoM_10yr', 'name': '10yr'},
        {'id': 'GoM_100yr', 'name': '100yr'},
    ]
}

print(f"\nStudy Parameters:")
print(f"  Risers: {len(study_params['risers'])} options")
print(f"  Water Depths: {len(study_params['depths'])} conditions")
print(f"  Environments: {len(study_params['environments'])} conditions")
print(f"  Total Models: {len(study_params['risers']) * len(study_params['depths']) * len(study_params['environments'])}")

# Generate configurations
configs_generated = []
model_count = 0

for riser in study_params['risers']:
    for depth in study_params['depths']:
        for env in study_params['environments']:
            model_count += 1

            # Create model name
            model_name = f"model_{riser['name']}_{depth['name']}_{env['name']}"

            # Create configuration
            config = {
                'model': {
                    'type': 'scr_catenary',
                    'name': f"GoM_SCR_{model_name}",
                    'description': f"SCR design study: {riser['id']}, {depth['value']}m depth, {env['id']}"
                },
                'vessel': {
                    'lookup': 'FPSO_P50',
                    'position': {'x': 0, 'y': 0, 'z': 0}
                },
                'riser': {
                    'lookup': riser['id'],
                    'length': depth['value'] + 200,  # Riser length = depth + 200m
                    'segments': int((depth['value'] + 200) / 10)
                },
                'environment': {
                    'lookup': env['id'],
                    'water_depth': depth['value']
                },
                'analysis': {
                    'type': 'static'
                }
            }

            # Save configuration
            config_file = output_dir / f"{model_name}.yml"
            with open(config_file, 'w') as f:
                yaml.dump(config, f, default_flow_style=False, sort_keys=False)

            configs_generated.append({
                'file': config_file.name,
                'riser': riser['id'],
                'depth': depth['value'],
                'environment': env['id']
            })

            print(f"  [{model_count:02d}/27] Generated: {config_file.name}")

print(f"\n{'='*80}")
print(f"CONFIGURATION GENERATION COMPLETE")
print(f"{'='*80}")

print(f"\nSummary:")
print(f"  Total configurations: {len(configs_generated)}")
print(f"  Output directory: {output_dir}")

print(f"\nConfigurations by Riser:")
for riser in study_params['risers']:
    count = sum(1 for c in configs_generated if c['riser'] == riser['id'])
    print(f"  {riser['id']}: {count} configs")

print(f"\nConfigurations by Depth:")
for depth in study_params['depths']:
    count = sum(1 for c in configs_generated if c['depth'] == depth['value'])
    print(f"  {depth['value']}m: {count} configs")

print(f"\nConfigurations by Environment:")
for env in study_params['environments']:
    count = sum(1 for c in configs_generated if c['environment'] == env['id'])
    print(f"  {env['id']}: {count} configs")

print(f"\nNext Steps:")
print(f"  1. Run: python design_study.py")
print(f"  2. This will generate 27 OrcaFlex models")
print(f"  3. Models will be saved to models/ directory")

# Save study matrix for reference
matrix_file = output_dir.parent / "study_matrix.yml"
study_matrix = {
    'study_name': 'Gulf of Mexico SCR Design Study',
    'total_models': len(configs_generated),
    'parameters': study_params,
    'configurations': configs_generated
}

with open(matrix_file, 'w') as f:
    yaml.dump(study_matrix, f, default_flow_style=False, sort_keys=False)

print(f"\nStudy matrix saved to: {matrix_file}")
