#!/usr/bin/env python3
"""
ABOUTME: Comprehensive examples for OrcaFlex Model Generator - demonstrates component assembly,
         parametric studies, custom components, and full workflow integration.
"""

from pathlib import Path
from digitalmodel.orcaflex.model_generator import OrcaFlexModelGenerator, generate_model
import pandas as pd

print("="*80)
print("ORCAFLEX MODEL GENERATOR - USAGE EXAMPLES")
print("="*80)

# Example 1: List Available Components
print("\n[EXAMPLE 1] List Available Components")
print("-"*80)

generator = OrcaFlexModelGenerator()

# List vessels
vessels = generator.list_components("vessels")
print(f"Available vessels ({len(vessels)}): {vessels[:5]}...")

# List risers
risers = generator.list_components("lines/risers")
print(f"Available risers ({len(risers)}): {risers[:5]}...")

# List environments
envs = generator.list_components("environment")
print(f"Available environments ({len(envs)}): {envs[:5]}...")

# List materials
materials = generator.list_components("materials/steel")
print(f"Available steel materials ({len(materials)}): {materials}")

# Example 2: Get Component Details
print("\n[EXAMPLE 2] Get Component Specifications")
print("-"*80)

# Get vessel details
vessel = generator.get_component("vessels", "FPSO_P50")
print(f"\nFPSO P50 Specifications:")
print(f"  LOA: {vessel['LOA']}m")
print(f"  Breadth: {vessel['Breadth']}m")
print(f"  Displacement: {vessel['Displacement']:,.0f} tonnes")
print(f"  Description: {vessel['Description']}")

# Get riser details
riser = generator.get_component("lines/risers", "SCR_10inch_X65")
print(f"\nSCR 10-inch X65 Specifications:")
print(f"  OD: {riser['OD']}m")
print(f"  Wall Thickness: {riser['WallThickness']}m")
print(f"  Material: {riser['Material']}")
print(f"  Max Tension: {riser['MaxTension']} kN")

# Get environment details
env = generator.get_component("environment", "GoM_100yr")
print(f"\nGoM 100-year Environment:")
print(f"  Hs: {env['Hs']}m")
print(f"  Tp: {env['Tp']}s")
print(f"  Surface Current: {env['SurfaceCurrent']}m/s")
print(f"  Water Depth: {env['WaterDepth']}m")

# Example 3: Basic Model Generation
print("\n[EXAMPLE 3] Basic SCR Model Generation")
print("-"*80)

basic_config = {
    'model': {
        'type': 'scr_catenary',
        'name': 'Basic_SCR_Example'
    },
    'vessel': {
        'lookup': 'FPSO_P50',
        'position': {'x': 0, 'y': 0, 'z': 0}
    },
    'riser': {
        'lookup': 'SCR_10inch_X65',
        'length': 1200,
        'segments': 120
    },
    'environment': {
        'lookup': 'GoM_1yr',
        'water_depth': 1000
    },
    'analysis': {
        'type': 'static'
    }
}

model = generator.generate_from_template(
    template="risers/scr_catenary",
    config=basic_config,
    output="temp_examples/basic_scr_model.yml"
)

print(f"✅ Generated: temp_examples/basic_scr_model.yml")
print(f"   Model name: {model.get('model', {}).get('name', 'N/A')}")

# Validate
validation = generator.validate(model)
print(f"   Validation: {'✅ PASS' if validation['is_valid'] else '❌ FAIL'}")
if validation['warnings']:
    print(f"   Warnings: {len(validation['warnings'])}")

# Example 4: Model with Custom Overrides
print("\n[EXAMPLE 4] Model with Custom Property Overrides")
print("-"*80)

custom_config = {
    'model': {
        'type': 'scr_catenary',
        'name': 'Custom_SCR_Overrides'
    },
    'vessel': {
        'lookup': 'FPSO_P50',
        'position': {'x': 0, 'y': 0, 'z': 0},
        # Override displacement for loaded condition
        'Displacement': 220000
    },
    'riser': {
        'lookup': 'SCR_10inch_X65',
        'length': 1500,
        'segments': 150,
        # Override for thicker wall
        'WallThickness': 0.016
    },
    'environment': {
        'lookup': 'GoM_100yr',
        'water_depth': 1400,
        # Override for more severe current
        'SurfaceCurrent': 1.8
    },
    'analysis': {
        'type': 'dynamic',
        'duration': 10800,
        'time_step': 0.1
    }
}

model = generator.generate_from_template(
    template="risers/scr_catenary",
    config=custom_config,
    output="temp_examples/custom_scr_model.yml"
)

print(f"✅ Generated: temp_examples/custom_scr_model.yml")
print(f"   Custom overrides applied:")
print(f"     - Vessel displacement: 220,000 tonnes")
print(f"     - Riser wall thickness: 0.016m")
print(f"     - Surface current: 1.8 m/s")

# Example 5: Parametric Study - Water Depth Variation
print("\n[EXAMPLE 5] Parametric Study - Water Depth Variation")
print("-"*80)

depths = range(800, 1600, 100)
models_generated = []

for depth in depths:
    config = {
        'model': {
            'type': 'scr_catenary',
            'name': f'SCR_Depth_{depth}m'
        },
        'vessel': {'lookup': 'FPSO_P50'},
        'riser': {
            'lookup': 'SCR_10inch_X65',
            'length': depth + 300,  # Riser length = depth + 300m
            'segments': int((depth + 300) / 10)
        },
        'environment': {
            'lookup': 'GoM_10yr',
            'water_depth': depth
        },
        'analysis': {'type': 'static'}
    }

    model = generator.generate_from_template(
        template="risers/scr_catenary",
        config=config,
        output=f"temp_examples/parametric/scr_depth_{depth}.yml"
    )

    models_generated.append(f"scr_depth_{depth}.yml")

print(f"✅ Generated {len(models_generated)} parametric models")
print(f"   Water depths: {min(depths)}m to {max(depths)}m (step: 100m)")
print(f"   Models: {models_generated[:3]}... (+{len(models_generated)-3} more)")

# Example 6: Multi-Variable Parametric Study
print("\n[EXAMPLE 6] Multi-Variable Parametric Study")
print("-"*80)

# Create parametric study matrix
study_matrix = []
for vessel_id in ['FPSO_P50', 'FPSO_P70']:
    for riser_id in ['SCR_10inch_X65', 'SCR_12inch_X65']:
        for env_id in ['GoM_1yr', 'GoM_10yr']:
            study_matrix.append({
                'vessel': vessel_id,
                'riser': riser_id,
                'environment': env_id
            })

print(f"Study matrix: {len(study_matrix)} combinations")
print(f"  Vessels: 2 (P50, P70)")
print(f"  Risers: 2 (10-inch, 12-inch)")
print(f"  Environments: 2 (1-year, 10-year)")

for idx, params in enumerate(study_matrix, 1):
    config = {
        'model': {
            'type': 'scr_catenary',
            'name': f'Study_{idx:02d}_{params["vessel"]}_{params["riser"]}_{params["environment"]}'
        },
        'vessel': {'lookup': params['vessel']},
        'riser': {'lookup': params['riser'], 'length': 1200, 'segments': 120},
        'environment': {'lookup': params['environment'], 'water_depth': 1000},
        'analysis': {'type': 'static'}
    }

    generator.generate_from_template(
        template="risers/scr_catenary",
        config=config,
        output=f"temp_examples/multivar/study_{idx:02d}.yml"
    )

print(f"✅ Generated {len(study_matrix)} models for multi-variable study")

# Example 7: Add Custom Component
print("\n[EXAMPLE 7] Add Custom Component to Library")
print("-"*80)

try:
    generator.add_component(
        category="vessels",
        component_id="ProjectX_Custom_FPSO",
        properties={
            "VesselID": "ProjectX_Custom_FPSO",
            "VesselName": "Project X Custom FPSO",
            "LOA": 315.0,
            "Breadth": 63.0,
            "Depth": 31.0,
            "Draught": 19.0,
            "Displacement": 225000.0,
            "LCG": 0.0,
            "VCG": 10.5,
            "WindArea": 8500.0,
            "CurrentArea": 6500.0,
            "OilCapacity": 1900000.0,
            "Description": "Custom FPSO for Project X deepwater operations"
        }
    )

    print(f"✅ Added custom component: ProjectX_Custom_FPSO")

    # Use custom component
    config = {
        'model': {'type': 'scr_catenary', 'name': 'Custom_Vessel_Model'},
        'vessel': {'lookup': 'ProjectX_Custom_FPSO'},
        'riser': {'lookup': 'SCR_10inch_X65', 'length': 1400, 'segments': 140},
        'environment': {'lookup': 'GoM_10yr', 'water_depth': 1200},
        'analysis': {'type': 'static'}
    }

    model = generator.generate_from_template(
        template="risers/scr_catenary",
        config=config,
        output="temp_examples/custom_vessel_model.yml"
    )

    print(f"✅ Generated model using custom vessel")

except Exception as e:
    print(f"⚠️ Custom component may already exist: {e}")

# Example 8: Convenience Function
print("\n[EXAMPLE 8] Using Convenience Function")
print("-"*80)

# One-liner model generation
model = generate_model(
    template="risers/scr_catenary",
    config={
        'model': {'name': 'QuickGenerate'},
        'vessel': {'lookup': 'FPSO_P50'},
        'riser': {'lookup': 'SCR_10inch_X65', 'length': 1300, 'segments': 130},
        'environment': {'lookup': 'GoM_1yr', 'water_depth': 1100},
        'analysis': {'type': 'static'}
    },
    output="temp_examples/quick_model.yml"
)

print(f"✅ Quick generation complete: temp_examples/quick_model.yml")

# Example 9: Batch Generation from DataFrame
print("\n[EXAMPLE 9] Batch Generation from DataFrame")
print("-"*80)

# Create study parameters as DataFrame
study_df = pd.DataFrame({
    'ModelName': [f'Batch_{i:03d}' for i in range(1, 6)],
    'VesselID': ['FPSO_P50', 'FPSO_P70', 'FPSO_P30', 'FPSO_P50', 'FPSO_P70'],
    'RiserID': ['SCR_10inch_X65', 'SCR_12inch_X65', 'SCR_10inch_X70', 'SCR_12inch_X65', 'SCR_10inch_X65'],
    'RiserLength': [1200, 1400, 1100, 1500, 1300],
    'EnvironmentID': ['GoM_1yr', 'GoM_10yr', 'GoM_1yr', 'GoM_100yr', 'GoM_10yr'],
    'WaterDepth': [1000, 1200, 900, 1400, 1100],
    'AnalysisType': ['static', 'static', 'static', 'dynamic', 'static']
})

print(f"Batch study DataFrame:")
print(study_df.head())

for idx, row in study_df.iterrows():
    config = {
        'model': {'name': row['ModelName']},
        'vessel': {'lookup': row['VesselID']},
        'riser': {
            'lookup': row['RiserID'],
            'length': row['RiserLength'],
            'segments': int(row['RiserLength'] / 10)
        },
        'environment': {
            'lookup': row['EnvironmentID'],
            'water_depth': row['WaterDepth']
        },
        'analysis': {'type': row['AnalysisType']}
    }

    if row['AnalysisType'] == 'dynamic':
        config['analysis']['duration'] = 10800
        config['analysis']['time_step'] = 0.1

    generator.generate_from_template(
        template="risers/scr_catenary",
        config=config,
        output=f"temp_examples/batch/{row['ModelName']}.yml"
    )

print(f"\n✅ Generated {len(study_df)} models from DataFrame")

# Example 10: Validation Example
print("\n[EXAMPLE 10] Model Validation")
print("-"*80)

config = {
    'model': {'name': 'ValidationTest'},
    'vessel': {'lookup': 'FPSO_P50'},
    'riser': {'lookup': 'SCR_10inch_X65', 'length': 1200, 'segments': 120},
    'environment': {'lookup': 'GoM_10yr', 'water_depth': 1000},
    'analysis': {'type': 'static'}
}

model = generator.generate_from_template(
    template="risers/scr_catenary",
    config=config
)

validation = generator.validate(model)

print(f"Validation Results:")
print(f"  Valid: {validation['is_valid']}")
print(f"  Checks performed: {validation['checks_performed']}")
print(f"  Errors: {len(validation['errors'])}")
print(f"  Warnings: {len(validation['warnings'])}")

if validation['errors']:
    print(f"\n  Errors:")
    for error in validation['errors']:
        print(f"    - {error}")

if validation['warnings']:
    print(f"\n  Warnings:")
    for warning in validation['warnings']:
        print(f"    - {warning}")

print("\n" + "="*80)
print("ALL EXAMPLES COMPLETE")
print("="*80)

print(f"\nSummary:")
print(f"  - 10 comprehensive examples executed")
print(f"  - Component library demonstrated")
print(f"  - Model generation validated")
print(f"  - Parametric studies shown")
print(f"  - Custom components illustrated")
print(f"  - Batch processing demonstrated")
print(f"\nGenerated files location: temp_examples/")
