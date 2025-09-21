#!/usr/bin/env python3
"""
Generate intermediate outputs for Step 5 - Scaling Calculation Test
This creates visual and data outputs to better understand the scaling process
"""

import sys
from pathlib import Path
import pandas as pd
import numpy as np
import json

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent.parent.parent.parent))

from src.digitalmodel.modules.fatigue_analysis.strut_foundation_processor import (
    LoadScaler,
    ProductionDataHandler,
    FatigueCondition
)

def generate_step5_intermediate_outputs():
    """Generate intermediate outputs for Step 5 verification"""
    
    base_path = Path(__file__).parent
    sample_data_path = base_path / "sample_data"
    output_dir = base_path / "output_step5_intermediate"
    output_dir.mkdir(exist_ok=True)
    
    print("="*60)
    print("GENERATING STEP 5 INTERMEDIATE OUTPUTS")
    print("="*60)
    
    # Initialize handlers
    data_handler = ProductionDataHandler(sample_data_path)
    processor = LoadScaler(data_handler)
    
    # Test conditions from Step 5
    test_conditions = [
        FatigueCondition(id=1, wind_speed=15, wind_dir=0, hs=0.75, tp=4.0, wave_dir=0, occurrence=1.0),
        FatigueCondition(id=2, wind_speed=10, wind_dir=0, hs=0.5, tp=2.7, wave_dir=0, occurrence=1.0),  # Reference
        FatigueCondition(id=3, wind_speed=5, wind_dir=0, hs=0.25, tp=2.0, wave_dir=0, occurrence=1.0),   # Low
        FatigueCondition(id=4, wind_speed=20, wind_dir=0, hs=1.0, tp=5.0, wave_dir=0, occurrence=1.0),   # High
    ]
    
    # Process each condition and save intermediate data
    scaling_summary = []
    
    for condition in test_conditions:
        print(f"\nProcessing FC{condition.id:03d}: Wind={condition.wind_speed}m/s, Hs={condition.hs}m")
        
        # Calculate scaling factors
        wind_scale = (condition.wind_speed / 10) ** 2
        wave_scale = condition.hs / 0.5
        
        # Process for one configuration and strut
        config_name = 'fsts_l015'
        strut_num = 1
        
        # Load raw reference data
        time_wind, tension_wind = data_handler.load_strut_data(config_name, 'wind01', strut_num)
        time_wave, tension_wave = data_handler.load_strut_data(config_name, 'wave01', strut_num)
        
        # Apply scaling
        scaled_wind = tension_wind[:1000] * wind_scale
        scaled_wave = tension_wave[:1000] * wave_scale
        combined = scaled_wind + scaled_wave
        
        # Save intermediate components
        components_df = pd.DataFrame({
            'Time': np.arange(len(scaled_wind)) * 0.1,
            'Original_Wind': tension_wind[:1000],
            'Original_Wave': tension_wave[:1000],
            'Scaled_Wind': scaled_wind,
            'Scaled_Wave': scaled_wave,
            'Combined_Tension': combined
        })
        
        # Save to CSV
        output_file = output_dir / f"FC{condition.id:03d}_components.csv"
        components_df.to_csv(output_file, index=False)
        print(f"  Saved: {output_file.name}")
        
        # Collect summary statistics
        scaling_summary.append({
            'FC_ID': f"FC{condition.id:03d}",
            'Wind_Speed': condition.wind_speed,
            'Wave_Hs': condition.hs,
            'Wind_Scale': wind_scale,
            'Wave_Scale': wave_scale,
            'Original_Wind_Min': tension_wind[:1000].min(),
            'Original_Wind_Max': tension_wind[:1000].max(),
            'Original_Wave_Min': tension_wave[:1000].min(),
            'Original_Wave_Max': tension_wave[:1000].max(),
            'Scaled_Wind_Min': scaled_wind.min(),
            'Scaled_Wind_Max': scaled_wind.max(),
            'Scaled_Wave_Min': scaled_wave.min(),
            'Scaled_Wave_Max': scaled_wave.max(),
            'Combined_Min': combined.min(),
            'Combined_Max': combined.max(),
            'Wind_Contribution_%': (scaled_wind.mean() / combined.mean()) * 100,
            'Wave_Contribution_%': (scaled_wave.mean() / combined.mean()) * 100
        })
    
    # Save summary table
    summary_df = pd.DataFrame(scaling_summary)
    summary_file = output_dir / "scaling_summary.csv"
    summary_df.to_csv(summary_file, index=False)
    print(f"\nSaved summary: {summary_file.name}")
    
    # Create comparison table for documentation
    comparison_data = []
    for row in scaling_summary:
        comparison_data.append({
            'Condition': row['FC_ID'],
            'Wind (m/s)': row['Wind_Speed'],
            'Hs (m)': row['Wave_Hs'],
            'Wind Scale': f"{row['Wind_Scale']:.2f}",
            'Wave Scale': f"{row['Wave_Scale']:.2f}",
            'Combined Range (kN)': f"{row['Combined_Min']:.1f} - {row['Combined_Max']:.1f}",
            'Wind %': f"{row['Wind_Contribution_%']:.1f}%",
            'Wave %': f"{row['Wave_Contribution_%']:.1f}%"
        })
    
    comparison_df = pd.DataFrame(comparison_data)
    
    # Save as markdown table
    markdown_file = output_dir / "scaling_comparison.md"
    with open(markdown_file, 'w') as f:
        f.write("# Step 5 Scaling Comparison\n\n")
        f.write("## Test Conditions and Results\n\n")
        f.write(comparison_df.to_markdown(index=False))
        f.write("\n\n## Key Observations\n\n")
        f.write("- Wind scaling follows quadratic relationship: (V/10)²\n")
        f.write("- Wave scaling is linear: Hs/0.5\n")
        f.write("- Wind contribution dominates at higher wind speeds\n")
        f.write("- Combined tension is simple addition of scaled components\n")
        f.write("- All outputs maintain 1000 samples (100 seconds at 10 Hz)\n")
    
    print(f"Saved markdown table: {markdown_file.name}")
    
    # Save metadata as JSON
    metadata = {
        'step': 5,
        'description': 'Scaling Calculation Test',
        'test_conditions': [
            {
                'id': c.id,
                'wind_speed': c.wind_speed,
                'wave_hs': c.hs,
                'wave_tp': c.tp,
                'description': 'Test' if c.id == 1 else 'Reference' if c.id == 2 else 'Low' if c.id == 3 else 'High'
            } for c in test_conditions
        ],
        'outputs_generated': [
            'FC001_components.csv',
            'FC002_components.csv', 
            'FC003_components.csv',
            'FC004_components.csv',
            'scaling_summary.csv',
            'scaling_comparison.md'
        ],
        'formulas': {
            'wind_scaling': '(V_actual / 10)^2',
            'wave_scaling': 'Hs_actual / 0.5',
            'combination': 'scaled_wind + scaled_wave'
        }
    }
    
    metadata_file = output_dir / "step5_metadata.json"
    with open(metadata_file, 'w') as f:
        json.dump(metadata, f, indent=2)
    
    print(f"Saved metadata: {metadata_file.name}")
    
    print("\n" + "="*60)
    print("INTERMEDIATE OUTPUTS GENERATED SUCCESSFULLY")
    print("="*60)
    print(f"\nOutput directory: {output_dir}")
    print("\nFiles created:")
    for file in sorted(output_dir.glob("*")):
        print(f"  - {file.name}")
    
    return output_dir

if __name__ == "__main__":
    generate_step5_intermediate_outputs()