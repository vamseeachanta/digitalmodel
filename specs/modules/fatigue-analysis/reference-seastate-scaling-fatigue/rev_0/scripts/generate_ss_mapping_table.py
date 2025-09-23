#!/usr/bin/env python3
"""
Generate Sea State (SS) condition mapping table showing metadata and reference files used.
This provides clear traceability between SS conditions and source data files.
Updated to use new naming convention: REF_WIND01/REF_WAVE01 for references, SS### for field conditions.
"""
import json
import pandas as pd
from pathlib import Path
from datetime import datetime


def generate_ss_mapping_table():
    """Generate comprehensive mapping of SS conditions to reference files and metadata."""
    
    base_path = Path(__file__).parent
    output_dir = base_path / 'output' / 'verification' / 'intermediate'
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Define Sea State conditions with full metadata (formerly FC conditions)
    ss_conditions = [
        {
            'ss_id': 'SS001',
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
            'ss_id': 'SS002',
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
            'ss_id': 'SS003',
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
            'ss_id': 'SS004',
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
    
    # Reference conditions metadata (updated naming)
    reference_conditions = {
        'REF_WIND01': {
            'type': 'Wind Reference',
            'wind_speed_ms': 10,
            'wind_direction_deg': 0,
            'description': 'Baseline wind condition for calibration'
        },
        'REF_WAVE01': {
            'type': 'Wave Reference',
            'hs_m': 0.5,
            'tp_s': 2.7,
            'wave_direction_deg': 0,
            'description': 'Baseline wave condition for calibration'
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
    
    for ss in ss_conditions:
        # Calculate scaling factors
        wind_scale = (ss['wind_speed_ms'] / reference_conditions['REF_WIND01']['wind_speed_ms']) ** 2
        wave_scale = ss['hs_m'] / reference_conditions['REF_WAVE01']['hs_m']
        
        # Create row for each vessel configuration
        for config_key, config_desc in vessel_configs.items():
            mapping_data.append({
                'SS ID': ss['ss_id'],
                'SS Description': ss['description'],
                'Vessel Config': config_key,
                'Vessel Description': config_desc,
                'Wind Speed (m/s)': ss['wind_speed_ms'],
                'Wind Dir (deg)': ss['wind_direction_deg'],
                'Hs (m)': ss['hs_m'],
                'Tp (s)': ss['tp_s'],
                'Wave Dir (deg)': ss['wave_direction_deg'],
                'Wind Scale Factor': f"{wind_scale:.2f}",
                'Wave Scale Factor': f"{wave_scale:.2f}",
                'Wind Ref File': f"{config_key}_mwl_REF_WIND01_Strut*.csv",
                'Wave Ref File': f"{config_key}_mwl_REF_WAVE01_Strut*.csv",
                'Wind Ref Speed': f"{reference_conditions['REF_WIND01']['wind_speed_ms']} m/s",
                'Wave Ref Hs': f"{reference_conditions['REF_WAVE01']['hs_m']} m"
            })
    
    # Create DataFrame
    df = pd.DataFrame(mapping_data)
    
    # Save as CSV
    csv_path = output_dir / 'ss_condition_mapping.csv'
    df.to_csv(csv_path, index=False)
    print(f"Saved SS mapping to: {csv_path}")
    
    # Create markdown version with better formatting
    md_content = ["# Sea State (SS) Condition to Reference File Mapping\n"]
    md_content.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
    
    # Reference conditions table
    md_content.append("## Reference Conditions (Calibration Baselines)\n\n")
    md_content.append("| Reference | Type | Wind Speed | Hs | Tp | Direction | Description |\n")
    md_content.append("|-----------|------|------------|----|----|-----------|-------------|\n")
    md_content.append(f"| REF_WIND01 | Wind | 10 m/s | - | - | 0° | Baseline wind for calibration |\n")
    md_content.append(f"| REF_WAVE01 | Wave | - | 0.5 m | 2.7 s | 0° | Baseline wave for calibration |\n\n")
    
    # Sea State conditions summary
    md_content.append("## Sea State (Field/Project) Conditions Summary\n\n")
    md_content.append("| SS ID | Description | Wind (m/s) | Hs (m) | Tp (s) | Wind Scale | Wave Scale |\n")
    md_content.append("|-------|-------------|------------|--------|--------|------------|------------|\n")
    
    for ss in ss_conditions:
        wind_scale = (ss['wind_speed_ms'] / 10) ** 2
        wave_scale = ss['hs_m'] / 0.5
        md_content.append(f"| {ss['ss_id']} | {ss['description']} | "
                         f"{ss['wind_speed_ms']} | {ss['hs_m']} | {ss['tp_s']} | "
                         f"{wind_scale:.2f}x | {wave_scale:.2f}x |\n")
    
    md_content.append("\n## Detailed File Mapping\n\n")
    
    # Group by SS condition for cleaner presentation
    for ss in ss_conditions:
        wind_scale = (ss['wind_speed_ms'] / 10) ** 2
        wave_scale = ss['hs_m'] / 0.5
        
        md_content.append(f"### {ss['ss_id']}: {ss['description']}\n\n")
        md_content.append(f"**Environmental Conditions:**\n")
        md_content.append(f"- Wind: {ss['wind_speed_ms']} m/s @ {ss['wind_direction_deg']}°\n")
        md_content.append(f"- Wave: Hs={ss['hs_m']}m, Tp={ss['tp_s']}s @ {ss['wave_direction_deg']}°\n")
        md_content.append(f"- Scaling: Wind={wind_scale:.2f}x (relative to REF_WIND01), Wave={wave_scale:.2f}x (relative to REF_WAVE01)\n\n")
        
        md_content.append("**Reference Files Used:**\n\n")
        md_content.append("| Vessel Config | Wind Reference File | Wave Reference File |\n")
        md_content.append("|---------------|--------------------|--------------------||\n")
        
        for config_key, config_desc in vessel_configs.items():
            md_content.append(f"| {config_key} | {config_key}_mwl_REF_WIND01_Strut[1-8].csv | "
                             f"{config_key}_mwl_REF_WAVE01_Strut[1-8].csv |\n")
        md_content.append("\n")
    
    # Scaling formula reminder
    md_content.append("## Scaling Formulas\n\n")
    md_content.append("- **Wind Scaling**: `(V_actual / V_reference)²` = `(V / 10)²` relative to REF_WIND01\n")
    md_content.append("- **Wave Scaling**: `Hs_actual / Hs_reference` = `Hs / 0.5` relative to REF_WAVE01\n")
    md_content.append("- **Combined**: `Tension = Wind_base * Wind_scale + Wave_base * Wave_scale`\n\n")
    
    # File naming pattern
    md_content.append("## File Naming Patterns\n\n")
    md_content.append("### Reference Files (Calibration Data):\n")
    md_content.append("```\n")
    md_content.append("{vessel_config}_mwl_{reference}_Strut{#}.csv\n")
    md_content.append("```\n")
    md_content.append("Example: `fsts_l015_mwl_REF_WIND01_Strut1.csv`\n\n")
    
    md_content.append("### Output Files (Scaled to Sea States):\n")
    md_content.append("```\n")
    md_content.append("{vessel_config}_SS{###}_Strut{#}.csv\n")
    md_content.append("```\n")
    md_content.append("Example: `fsts_l015_SS001_Strut1.csv`\n\n")
    
    md_content.append("Where:\n")
    md_content.append("- `vessel_config`: fsts_l015, fsts_l095, fsts_l015_125km3_l100_pb, fsts_l095_125km3_l000_pb\n")
    md_content.append("- `reference`: REF_WIND01 or REF_WAVE01\n")
    md_content.append("- `SS{###}`: Sea State identifier (SS001, SS002, etc.)\n")
    md_content.append("- `#`: Strut number (1-8)\n\n")
    
    # Clear distinction section
    md_content.append("## Key Distinction\n\n")
    md_content.append("- **REF_*** files = Reference/calibration data (baseline measurements)\n")
    md_content.append("- **SS###** files = Project/field conditions (actual sea states to analyze)\n\n")
    
    # Verification checklist
    md_content.append("## Verification Checklist\n\n")
    md_content.append("- [ ] All SS conditions have both wind and wave reference files\n")
    md_content.append("- [ ] Reference files are clearly marked with REF_ prefix\n")
    md_content.append("- [ ] Each configuration has 8 strut files (Strut1-8)\n")
    md_content.append("- [ ] Scaling factors match the formula calculations\n")
    md_content.append("- [ ] Wind reference uses REF_WIND01 (10 m/s baseline)\n")
    md_content.append("- [ ] Wave reference uses REF_WAVE01 (0.5 m Hs baseline)\n")
    
    # Save markdown
    md_path = output_dir / 'ss_condition_mapping.md'
    with open(md_path, 'w') as f:
        f.writelines(md_content)
    print(f"Saved SS mapping markdown to: {md_path}")
    
    # Create JSON version for programmatic access
    json_data = {
        'timestamp': datetime.now().isoformat(),
        'reference_conditions': reference_conditions,
        'sea_state_conditions': ss_conditions,
        'vessel_configurations': vessel_configs,
        'scaling_formulas': {
            'wind': '(V/10)^2 relative to REF_WIND01',
            'wave': 'Hs/0.5 relative to REF_WAVE01'
        },
        'file_patterns': {
            'reference': '{vessel_config}_mwl_{reference}_Strut{#}.csv',
            'output': '{vessel_config}_SS{###}_Strut{#}.csv'
        },
        'naming_convention': {
            'REF_': 'Reference/calibration data',
            'SS': 'Sea State (project/field conditions)'
        }
    }
    
    json_path = output_dir / 'ss_condition_mapping.json'
    with open(json_path, 'w') as f:
        json.dump(json_data, f, indent=2)
    print(f"Saved SS mapping JSON to: {json_path}")
    
    # Display summary
    print("\n" + "="*60)
    print("SEA STATE CONDITION MAPPING COMPLETE")
    print("="*60)
    print(f"\nGenerated 3 output files:")
    print(f"  1. ss_condition_mapping.csv - Full tabular data")
    print(f"  2. ss_condition_mapping.md - Formatted documentation")
    print(f"  3. ss_condition_mapping.json - Programmatic access")
    print(f"\nFiles saved to: {output_dir}")
    
    return df


if __name__ == "__main__":
    generate_ss_mapping_table()