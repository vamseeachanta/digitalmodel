#!/usr/bin/env python3
"""
Generate FC condition mapping table showing metadata and reference files used.
This provides clear traceability between FC conditions and source data files.
"""
import json
import pandas as pd
from pathlib import Path
from datetime import datetime


def generate_fc_mapping_table():
    """Generate comprehensive mapping of FC conditions to reference files and metadata."""
    
    base_path = Path(__file__).parent
    output_dir = base_path / 'output' / 'verification' / 'intermediate'
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Define FC conditions with full metadata
    fc_conditions = [
        {
            'fc_id': 'FC001',
            'description': 'Test Condition',
            'wind_speed_ms': 15,
            'wind_direction_deg': 0,
            'hs_m': 0.75,
            'tp_s': 4.0,
            'wave_direction_deg': 0,
            'current_speed_ms': 0.5,
            'current_direction_deg': 0
        },
        {
            'fc_id': 'FC002',
            'description': 'Reference Baseline',
            'wind_speed_ms': 10,
            'wind_direction_deg': 0,
            'hs_m': 0.5,
            'tp_s': 2.7,
            'wave_direction_deg': 0,
            'current_speed_ms': 0.5,
            'current_direction_deg': 0
        },
        {
            'fc_id': 'FC003',
            'description': 'Low Condition',
            'wind_speed_ms': 5,
            'wind_direction_deg': 0,
            'hs_m': 0.25,
            'tp_s': 2.0,
            'wave_direction_deg': 0,
            'current_speed_ms': 0.5,
            'current_direction_deg': 0
        },
        {
            'fc_id': 'FC004',
            'description': 'High Condition',
            'wind_speed_ms': 20,
            'wind_direction_deg': 0,
            'hs_m': 1.0,
            'tp_s': 5.0,
            'wave_direction_deg': 0,
            'current_speed_ms': 0.5,
            'current_direction_deg': 0
        }
    ]
    
    # Reference conditions metadata
    reference_conditions = {
        'wind01': {
            'type': 'Wind Reference',
            'wind_speed_ms': 10,
            'wind_direction_deg': 0,
            'description': 'Baseline wind condition'
        },
        'wave01': {
            'type': 'Wave Reference',
            'hs_m': 0.5,
            'tp_s': 2.7,
            'wave_direction_deg': 0,
            'description': 'Baseline wave condition'
        }
    }
    
    # Vessel configurations
    vessel_configs = {
        'fsts_l015': 'FSTs Light (15% loaded)',
        'fsts_l095': 'FSTs Full (95% loaded)',
        'fsts_l015_125km3_l100_pb': 'FSTs Light + LNGC Full',
        'fsts_l095_125km3_l000_pb': 'FSTs Full + LNGC Light'
    }
    
    # Create mapping table
    mapping_data = []
    
    for fc in fc_conditions:
        # Calculate scaling factors
        wind_scale = (fc['wind_speed_ms'] / reference_conditions['wind01']['wind_speed_ms']) ** 2
        wave_scale = fc['hs_m'] / reference_conditions['wave01']['hs_m']
        
        # Create row for each vessel configuration
        for config_key, config_desc in vessel_configs.items():
            mapping_data.append({
                'FC ID': fc['fc_id'],
                'FC Description': fc['description'],
                'Vessel Config': config_key,
                'Vessel Description': config_desc,
                'Wind Speed (m/s)': fc['wind_speed_ms'],
                'Wind Dir (deg)': fc['wind_direction_deg'],
                'Hs (m)': fc['hs_m'],
                'Tp (s)': fc['tp_s'],
                'Wave Dir (deg)': fc['wave_direction_deg'],
                'Wind Scale Factor': f"{wind_scale:.2f}",
                'Wave Scale Factor': f"{wave_scale:.2f}",
                'Wind Ref File': f"{config_key}_mwl_wind01_Strut*.csv",
                'Wave Ref File': f"{config_key}_mwl_wave01_Strut*.csv",
                'Wind Ref Speed': f"{reference_conditions['wind01']['wind_speed_ms']} m/s",
                'Wave Ref Hs': f"{reference_conditions['wave01']['hs_m']} m"
            })
    
    # Create DataFrame
    df = pd.DataFrame(mapping_data)
    
    # Save as CSV
    csv_path = output_dir / 'fc_condition_mapping.csv'
    df.to_csv(csv_path, index=False)
    print(f"Saved FC mapping to: {csv_path}")
    
    # Create markdown version with better formatting
    md_content = ["# FC Condition to Reference File Mapping\n"]
    md_content.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
    
    # Reference conditions table
    md_content.append("## Reference Conditions (Baseline)\n\n")
    md_content.append("| Reference | Type | Wind Speed | Hs | Tp | Direction | Description |\n")
    md_content.append("|-----------|------|------------|----|----|-----------|-------------|\n")
    md_content.append(f"| wind01 | Wind | 10 m/s | - | - | 0° | Baseline wind condition |\n")
    md_content.append(f"| wave01 | Wave | - | 0.5 m | 2.7 s | 0° | Baseline wave condition |\n\n")
    
    # FC conditions summary
    md_content.append("## FC Conditions Summary\n\n")
    md_content.append("| FC ID | Description | Wind (m/s) | Hs (m) | Tp (s) | Wind Scale | Wave Scale |\n")
    md_content.append("|-------|-------------|------------|--------|--------|------------|------------|\n")
    
    for fc in fc_conditions:
        wind_scale = (fc['wind_speed_ms'] / 10) ** 2
        wave_scale = fc['hs_m'] / 0.5
        md_content.append(f"| {fc['fc_id']} | {fc['description']} | "
                         f"{fc['wind_speed_ms']} | {fc['hs_m']} | {fc['tp_s']} | "
                         f"{wind_scale:.2f}x | {wave_scale:.2f}x |\n")
    
    md_content.append("\n## Detailed File Mapping\n\n")
    
    # Group by FC condition for cleaner presentation
    for fc in fc_conditions:
        wind_scale = (fc['wind_speed_ms'] / 10) ** 2
        wave_scale = fc['hs_m'] / 0.5
        
        md_content.append(f"### {fc['fc_id']}: {fc['description']}\n\n")
        md_content.append(f"**Environmental Conditions:**\n")
        md_content.append(f"- Wind: {fc['wind_speed_ms']} m/s @ {fc['wind_direction_deg']}°\n")
        md_content.append(f"- Wave: Hs={fc['hs_m']}m, Tp={fc['tp_s']}s @ {fc['wave_direction_deg']}°\n")
        md_content.append(f"- Scaling: Wind={wind_scale:.2f}x, Wave={wave_scale:.2f}x\n\n")
        
        md_content.append("**Reference Files Used:**\n\n")
        md_content.append("| Vessel Config | Wind Reference File | Wave Reference File |\n")
        md_content.append("|---------------|--------------------|--------------------||\n")
        
        for config_key, config_desc in vessel_configs.items():
            md_content.append(f"| {config_key} | {config_key}_mwl_wind01_Strut[1-8].csv | "
                             f"{config_key}_mwl_wave01_Strut[1-8].csv |\n")
        md_content.append("\n")
    
    # Scaling formula reminder
    md_content.append("## Scaling Formulas\n\n")
    md_content.append("- **Wind Scaling**: `(V_actual / V_reference)²` = `(V / 10)²`\n")
    md_content.append("- **Wave Scaling**: `Hs_actual / Hs_reference` = `Hs / 0.5`\n")
    md_content.append("- **Combined**: `Tension = Wind_base * Wind_scale + Wave_base * Wave_scale`\n\n")
    
    # File naming pattern
    md_content.append("## File Naming Pattern\n\n")
    md_content.append("```\n")
    md_content.append("{vessel_config}_mwl_{reference}_Strut{#}.csv\n")
    md_content.append("```\n\n")
    md_content.append("Where:\n")
    md_content.append("- `vessel_config`: fsts_l015, fsts_l095, fsts_l015_125km3_l100_pb, fsts_l095_125km3_l000_pb\n")
    md_content.append("- `reference`: wind01 or wave01\n")
    md_content.append("- `#`: Strut number (1-8)\n\n")
    
    # Verification checklist
    md_content.append("## Verification Checklist\n\n")
    md_content.append("- [ ] All FC conditions have both wind and wave reference files\n")
    md_content.append("- [ ] Reference files exist for all 4 vessel configurations\n")
    md_content.append("- [ ] Each configuration has 8 strut files (Strut1-8)\n")
    md_content.append("- [ ] Scaling factors match the formula calculations\n")
    md_content.append("- [ ] Wind reference uses 10 m/s baseline\n")
    md_content.append("- [ ] Wave reference uses 0.5 m Hs baseline\n")
    
    # Save markdown
    md_path = output_dir / 'fc_condition_mapping.md'
    with open(md_path, 'w') as f:
        f.writelines(md_content)
    print(f"Saved FC mapping markdown to: {md_path}")
    
    # Create JSON version for programmatic access
    json_data = {
        'timestamp': datetime.now().isoformat(),
        'reference_conditions': reference_conditions,
        'fc_conditions': fc_conditions,
        'vessel_configurations': vessel_configs,
        'scaling_formulas': {
            'wind': '(V/10)^2',
            'wave': 'Hs/0.5'
        },
        'file_pattern': '{vessel_config}_mwl_{reference}_Strut{#}.csv'
    }
    
    json_path = output_dir / 'fc_condition_mapping.json'
    with open(json_path, 'w') as f:
        json.dump(json_data, f, indent=2)
    print(f"Saved FC mapping JSON to: {json_path}")
    
    # Display summary
    print("\n" + "="*60)
    print("FC CONDITION MAPPING COMPLETE")
    print("="*60)
    print(f"\nGenerated 3 output files:")
    print(f"  1. fc_condition_mapping.csv - Full tabular data")
    print(f"  2. fc_condition_mapping.md - Formatted documentation")
    print(f"  3. fc_condition_mapping.json - Programmatic access")
    print(f"\nFiles saved to: {output_dir}")
    
    return df


if __name__ == "__main__":
    generate_fc_mapping_table()