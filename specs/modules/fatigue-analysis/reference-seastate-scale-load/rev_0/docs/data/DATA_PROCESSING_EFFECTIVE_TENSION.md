# Data Processing - Effective Tension Only

## Scope: Struts 1-8, Effective Tension Only

### Data Extraction Requirements

**From each strut file, extract ONLY:**
- **Column**: `Tension (Jacket End)` or `Tension (Vessel End)`
- **Use Case**: Effective tension for fatigue analysis
- **Ignore**: All other columns (Shear, Fx, Fy, Fz, positions, velocities, angles)

### File Structure Review

Each strut CSV file contains:
```csv
time,Tension (Jacket End),Shear (Jacket End),Fx (Jacket End),Fy (Jacket End),Fz (Jacket End),X (Jacket End),Y (Jacket End),Z (Jacket End),Vx (Jacket End),Vy (Jacket End),Vz (Jacket End),Angle (Jacket End),Tension (Vessel End),Shear (Vessel End),Fx (Vessel End),Fy (Vessel End),Fz (Vessel End),X (Vessel End),Y (Vessel End),Z (Vessel End),Vx (Vessel End),Vy (Vessel End),Vz (Vessel End),Angle (Vessel End)
```

**We will use:**
- ✅ `time` - Time series index
- ✅ `Tension (Vessel End)` - Effective tension at FST vessel end for fatigue analysis
- ❌ `Tension (Jacket End)` - Not used
- ❌ All other columns - Not needed for fatigue calculation

## Effective Tension Definition

### Which Tension to Use?
- **USE THIS**: `Tension (Vessel End)` - Connection point at FST vessel (moving end)
- **DO NOT USE**: `Tension (Jacket End)` - Connection point at jacket (fixed end)
- **Requirement**: Use `Tension (Vessel End)` ONLY for fatigue analysis

### Sign Convention
- **Positive (+)**: Tension (pulling)
- **Negative (-)**: Compression (pushing)
- **Note**: Struts typically remain in tension during operation

## Python Implementation

```python
import pandas as pd
import numpy as np

def extract_effective_tension(filepath, tension_column='Tension (Vessel End)'):
    """
    Extract only the effective tension from a strut file
    
    Args:
        filepath: Path to strut CSV file
        tension_column: Which tension column to use
                       Default: 'Tension (Vessel End)' - FST vessel connection
    
    Returns:
        Dictionary with time and tension arrays
    """
    # Read only required columns for memory efficiency
    df = pd.read_csv(filepath, usecols=['time', tension_column])
    
    return {
        'time': df['time'].values,
        'tension_kN': df[tension_column].values,
        'mean_tension': df[tension_column].mean(),
        'max_tension': df[tension_column].max(),
        'min_tension': df[tension_column].min(),
        'std_tension': df[tension_column].std()
    }

def process_strut_for_fatigue(input_file, output_dir):
    """
    Process a single strut file for fatigue analysis
    
    Args:
        input_file: Path to input strut CSV
        output_dir: Directory for output files
    
    Returns:
        Fatigue analysis results
    """
    # Step 1: Extract effective tension only
    tension_data = extract_effective_tension(input_file)
    
    # Step 2: Apply scaling if needed
    scaled_tension = apply_scaling(tension_data['tension_kN'], scaling_factor)
    
    # Step 3: Rainflow counting on tension
    ranges, counts = rainflow_count(scaled_tension)
    
    # Step 4: Calculate fatigue damage
    damage = calculate_fatigue_damage(ranges, counts)
    
    return {
        'file': input_file,
        'mean_tension_kN': tension_data['mean_tension'],
        'tension_range_kN': tension_data['max_tension'] - tension_data['min_tension'],
        'cycles_found': len(ranges),
        'annual_damage': damage['annual_damage'],
        'fatigue_life_years': damage['fatigue_life']
    }
```

## Workflow Summary

```mermaid
graph LR
    A[Strut CSV File] --> B[Extract Time & Tension]
    B --> C[Apply Scaling]
    C --> D[Effective Tension]
    D --> E[Rainflow Counting]
    E --> F[S-N Curve Damage]
    F --> G[Fatigue Life]
    
    style B fill:#90EE90
    style D fill:#90EE90
```

## Data Flow Example

### Input (Original File)
```csv
time,Tension (Jacket End),Shear (Jacket End),...[other columns]
0.0,-22.001244,85.584198,...
0.1,-23.381189,85.583954,...
0.2,-23.930864,85.583687,...
```

### Extracted Data (What We Use)
```csv
time,effective_tension_kN
0.0,-22.001244
0.1,-23.381189
0.2,-23.930864
```

### After Scaling (Example with factor 1.5)
```csv
time,scaled_tension_kN
0.0,-33.001866
0.1,-35.071784
0.2,-35.896296
```

### Output Files (With Heading)
```
fsts_l015_mwl_wave01_045deg_Strut1_effective_tension.csv
fsts_l015_mwl_wave01_045deg_Strut1_rainflow.csv
fsts_l015_mwl_wave01_045deg_Strut1_damage.csv
```

## Memory Optimization

By reading only the required columns:
- **Original file columns**: 25 columns
- **Columns we read**: 2 columns (time, tension)
- **Memory reduction**: ~92%
- **Processing speed**: ~10x faster

## Complete Processing Pipeline

```python
def process_all_configurations():
    """Process all 4 configurations using only effective tension"""
    
    configurations = [
        'fsts_l015',
        'fsts_l095', 
        'fsts_l015_125km3_l100_pb',
        'fsts_l095_125km3_l000_pb'
    ]
    
    base_path = r"D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\csv"
    
    for config in configurations:
        print(f"\nProcessing {config}...")
        
        # Process only strut files
        for strut_num in range(1, 9):  # Strut1 to Strut8
            for case_type in ['wave', 'wind']:
                num_cases = 18 if case_type == 'wave' else 16
                
                for case_num in range(1, num_cases + 1):
                    # Build filename
                    filename = f"{config}_mwl_{case_type}{case_num:02d}_Strut{strut_num}.csv"
                    filepath = os.path.join(base_path, filename)
                    
                    if os.path.exists(filepath):
                        # Extract only effective tension
                        tension_data = extract_effective_tension(filepath)
                        
                        # Process for fatigue
                        results = process_strut_for_fatigue(filepath, output_dir)
                        
                        print(f"  {filename}: Life = {results['fatigue_life_years']:.1f} years")
```

## Summary

**Key Points:**
1. ✅ Process only Strut1-Strut8 files
2. ✅ Extract only effective tension column
3. ✅ Ignore jacket files completely
4. ✅ Ignore shear and other force components
5. ✅ Use `Tension (Jacket End)` as primary data
6. ✅ Significant memory and speed optimization