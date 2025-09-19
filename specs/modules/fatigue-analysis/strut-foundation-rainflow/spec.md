# Strut Foundation Fatigue Analysis with Rainflow Counting

## Executive Summary

This specification defines a comprehensive fatigue analysis system for strut foundation structures using rainflow counting methodology. The system uses a metadata CSV file (`reference_seastate_timetrace_metadata.csv`) where each row contains metadata about a time trace file for a specific reference seastate. The actual time trace data is stored in separate CSV files. These reference time traces are directly scaled to match target fatigue conditions, followed by rainflow counting and fatigue damage calculation, eliminating the need for repeated time domain simulations.

The system analyzes 4 distinct vessel configurations, each requiring separate fatigue damage calculations:
1. **FSTs Light (L015)** - Both FSTs in light condition (15% loaded)
2. **FSTs Full (L095)** - Both FSTs in full condition (95% loaded)  
3. **FSTs Light + LNGC Full** - Light FSTs with full 125k m³ LNGC (offloading scenario)
4. **FSTs Full + LNGC Light** - Full FSTs with light 125k m³ LNGC (loading scenario)

## Project Overview

### Objective
Develop an automated fatigue analysis module that implements the complete procedure for strut foundation fatigue evaluation using rainflow counting, as defined in the WLNG fatigue methodology document. The system processes pre-computed reference seastate time traces directly, eliminating the need for repeated time domain simulations.

### Scope
- **Reference Seastates**: 34 pre-computed conditions (18 wave @ Hs=0.5m, 16 wind @ 10m/s)
- **Metadata Format**: CSV file with metadata (each row = metadata for one time trace file)
- **Time Trace Data**: Separate CSV files containing actual time series data
- **Fatigue Conditions**: 81 combined wind-wave conditions with scaling factors
- **Direct Load Scaling**: Scale reference time traces to target fatigue conditions
- **Rainflow Processing**: Apply rainflow counting to scaled time traces for 8 mooring struts
- **Annual Weighting**: Weight results by occurrence percentages
- **FEA Integration**: Map loads to stresses using finite element results
- **Fatigue Calculation**: Apply S-N curves and Miner's rule for damage assessment

## Reference Seastate Time Trace Workflow

### Data Input Structure
The system uses a two-tier data structure for managing reference seastate time traces:

#### 1. Metadata File (`reference_seastate_timetrace_metadata.csv`)
```
┌─────────────┬──────────────┬──────────┬─────────────────┬──────────┬────────────┬──────────────┐
│ seastate_id │ seastate_type│ strut_id │ time_trace_file │ duration │ sample_rate│ units        │
├─────────────┼──────────────┼──────────┼─────────────────┼──────────┼────────────┼──────────────┤
│ W01         │ wave         │ S1       │ W01_S1.csv      │ 10800    │ 0.1        │ kN           │
│ W01         │ wave         │ S2       │ W01_S2.csv      │ 10800    │ 0.1        │ kN           │
│ ...         │ ...          │ ...      │ ...             │ ...      │ ...        │ ...          │
│ WD16        │ wind         │ S8       │ WD16_S8.csv     │ 10800    │ 0.1        │ kN           │
└─────────────┴──────────────┴──────────┴─────────────────┴──────────┴────────────┴──────────────┘
Total rows: 272 (34 reference seastates × 8 struts)
```

#### 2. Reference Seastate Definitions (`reference_seastate_definition.csv`)
Defines the environmental conditions for each reference seastate:
- Load Case ID (F001-F068)
- FST loading conditions (Light/Full)
- LNG carrier configurations
- Water levels (HWL/MWL/LWL)
- Wind: Direction and speed (10 m/s for wind reference cases)
- Wave: Direction, Hs (0.5m for wave reference cases), and Tp

#### 3. Time Trace Data Files
Each time trace file referenced in the metadata contains the actual time series data:
```
Example: W01_S1.csv
┌──────┬───────────┐
│ time │ load_value│
├──────┼───────────┤
│ 0.0  │ 125.3     │
│ 0.1  │ 128.7     │
│ 0.2  │ 124.1     │
│ ...  │ ...       │
│10800 │ 122.9     │
└──────┴───────────┘
```

### Processing Pipeline
1. **Load Metadata**: Read `reference_seastate_timetrace_metadata.csv` to identify all time trace files
2. **Load Time Traces**: Read individual CSV files containing actual time series data
3. **Apply Scaling Factors**: For each of 81 fatigue conditions:
   - Identify corresponding reference seastate (wind or wave)
   - Load the appropriate time trace file
   - Apply scaling factor to entire time trace
   - Wind scaling: (target_speed/10)²
   - Wave scaling: target_Hs/0.5
4. **Rainflow Analysis**: Process scaled time traces through rainflow counting
5. **Fatigue Calculation**: Apply S-N curves to resulting load cycles

## Technical Architecture

### Module Structure
```
src/digitalmodel/modules/fatigue_analysis/
├── core/
│   ├── __init__.py
│   ├── rainflow_processor.py     # Rainflow counting implementation
│   ├── load_scaler.py           # Load scaling for fatigue conditions
│   ├── stress_mapper.py         # FEA stress mapping
│   └── fatigue_calculator.py    # S-N curve and damage calculations
├── config/
│   ├── reference_seastate_timetrace_metadata.csv  # Reference seastate metadata
│   ├── reference_seastate_definition.csv # Reference seastate environmental conditions  
│   ├── fatigue_seastates.csv             # 81 fatigue conditions
│   └── sn_curve_parameters.yml           # S-N curve parameters
├── data/
│   ├── metadata/                # Metadata CSV files
│   ├── timetraces/              # Individual time trace CSV files
│   ├── intermediate/            # Processed rainflow data
│   └── output/                  # Final fatigue results
└── cli/
    └── fatigue_analysis_cli.py  # Command-line interface
```

### Configuration Management
- **Reference Seastates**: CSV file (`reference_seastate_timetrace_metadata.csv`) with 68 OrcaFlex simulation cases
- **Fatigue Conditions**: CSV file (`fatigue_seastates.csv`) with 81 combined conditions and occurrence percentages
- **S-N Curve Parameters**: ABS "E" in Air curve parameters (in YAML configuration)
- **FEA Parameters**: Unit load values and stress concentration factors

## Data Flow Architecture

```mermaid
graph TB
    A[Metadata CSV<br/>reference_seastate_timetrace_metadata.csv] --> B[Load Metadata]
    B --> C[Load Time Trace Files]
    C --> D[Time Series Data]
    E[Fatigue Conditions<br/>81 combinations] --> F[Direct Load Scaling]
    D --> F
    F --> G[Scaled Time Traces]
    G --> H[Rainflow Counting]
    H --> I[Load Ranges & Cycles]
    I --> J[Annual Weighting]
    K[FEA Results] --> L[Stress Mapping]
    J --> L
    L --> M[Fatigue Damage Calculation]
    M --> N[Life Estimation]
    N --> O[Design Verification]
```

## Implementation Requirements

### 1. Reference Seastate Time Trace Input Processing
- **Metadata File**: CSV file (`reference_seastate_timetrace_metadata.csv`)
- **Metadata Structure**: Each row contains metadata pointing to a time trace file
- **Time Trace Files**: Separate CSV files containing actual time series data
- **Reference Conditions**: 34 total (18 wave cases @ Hs=0.5m, 16 wind cases @ 10m/s)
- **Strut Data**: 8 mooring struts per seastate (272 total time trace files)
- **Direct Processing**: Time traces are directly scaled without additional simulation

### 2. Metadata and Time Trace Handler
```python
class MetadataHandler:
    def __init__(self, metadata_path='reference_seastate_timetrace_metadata.csv'):
        self.metadata_path = metadata_path
        self.metadata_df = None
        self.time_traces = {}
    
    def load_metadata(self):
        """Load metadata CSV containing references to time trace files"""
        # Columns: [seastate_id, seastate_type, strut_id, time_trace_file, duration, sample_rate, units]
        self.metadata_df = pd.read_csv(self.metadata_path)
        return self.metadata_df
    
    def load_time_trace(self, seastate_id, strut_id):
        """Load actual time trace data from referenced CSV file"""
        # Find the file path from metadata
        row = self.metadata_df[(self.metadata_df['seastate_id'] == seastate_id) & 
                               (self.metadata_df['strut_id'] == strut_id)]
        file_path = row['time_trace_file'].values[0]
        
        # Load time series data from CSV file
        trace_data = pd.read_csv(f'data/timetraces/{file_path}')
        return trace_data['load_value'].values
    
    def get_reference_type(self, seastate_id):
        """Determine if reference seastate is wind or wave type"""
        return self.metadata_df[self.metadata_df['seastate_id'] == seastate_id]['seastate_type'].values[0]
```

### 3. Rainflow Counting Module
```python
class RainflowProcessor:
    def __init__(self, duration=200):  # seconds
        self.duration = duration
    
    def process_scaled_timeseries(self, scaled_trace):
        """Apply rainflow counting to scaled time trace"""
        # Direct rainflow on already-scaled time trace
        return load_ranges, cycle_counts
    
    def process_all_struts(self, scaled_traces):
        """Process all 8 struts for all fatigue conditions"""
        # Process scaled time traces for each strut
        pass
```

### 4. Direct Load Scaling System
```python
class DirectLoadScaler:
    def __init__(self, metadata_handler, fatigue_conditions_path='fatigue_seastates.csv',
                 scaling_factors_output='fatigue_scaling_factors.csv'):
        self.base_wind_speed = 10  # m/s
        self.base_hs = 0.5  # m
        self.metadata_handler = metadata_handler
        self.fatigue_conditions_path = fatigue_conditions_path
        self.scaling_factors_output = scaling_factors_output
        self.fatigue_conditions = pd.read_csv(fatigue_conditions_path)
        self.scaling_factors_df = None
        
        # Calculate and save scaling factors on initialization
        self.calculate_and_save_scaling_factors()
    
    def calculate_and_save_scaling_factors(self):
        """Calculate all scaling factors and save to CSV for verification"""
        scaling_data = []
        
        for idx, row in self.fatigue_conditions.iterrows():
            wind_speed = row['Wind Speed (m/s)']
            hs = row['Hs (m)']
            occurrence = row['Occurrence (%)']
            
            # Calculate scaling factors
            wind_scale_factor = (wind_speed / self.base_wind_speed) ** 2
            wave_scale_factor = hs / self.base_hs
            
            # Create formula strings for documentation
            wind_formula = f"({wind_speed}/{self.base_wind_speed})^2"
            wave_formula = f"{hs}/{self.base_hs}"
            
            scaling_data.append({
                'Row': row['Row'],
                'Wind Speed (m/s)': wind_speed,
                'Hs (m)': hs,
                'Wind Scale Factor': round(wind_scale_factor, 6),
                'Wave Scale Factor': round(wave_scale_factor, 6),
                'Wind Scale Formula': wind_formula,
                'Wave Scale Formula': wave_formula,
                'Occurrence (%)': occurrence,
                'Wind Dir (°)': row['Wind Dir (°)'],
                'Wave Dir (°)': row['Wave Dir (°)']
            })
        
        # Save to DataFrame and CSV
        self.scaling_factors_df = pd.DataFrame(scaling_data)
        self.scaling_factors_df.to_csv(self.scaling_factors_output, index=False)
        
        # Log summary statistics
        self.log_scaling_statistics()
        
        return self.scaling_factors_df
    
    def log_scaling_statistics(self):
        """Log key statistics about scaling factors"""
        print(f"Scaling factors calculated and saved to: {self.scaling_factors_output}")
        print(f"Wind scaling range: {self.scaling_factors_df['Wind Scale Factor'].min():.4f} to "
              f"{self.scaling_factors_df['Wind Scale Factor'].max():.4f}")
        print(f"Wave scaling range: {self.scaling_factors_df['Wave Scale Factor'].min():.4f} to "
              f"{self.scaling_factors_df['Wave Scale Factor'].max():.4f}")
        print(f"Total fatigue conditions: {len(self.scaling_factors_df)}")
        print(f"Total occurrence check: {self.scaling_factors_df['Occurrence (%)'].sum():.2f}%")
    
    def scale_time_trace(self, time_trace, seastate_type, fatigue_row):
        """Scale entire time trace based on target fatigue condition"""
        # Get pre-calculated scaling factor from DataFrame
        row_num = fatigue_row['Row']
        scaling_row = self.scaling_factors_df[self.scaling_factors_df['Row'] == row_num].iloc[0]
        
        # Use appropriate scaling factor based on seastate type
        if seastate_type == 'wind':
            scale_factor = scaling_row['Wind Scale Factor']
        else:  # wave
            scale_factor = scaling_row['Wave Scale Factor']
        
        # Apply scaling to entire time trace
        scaled_trace = time_trace * scale_factor
        return scaled_trace, scale_factor
    
    def select_wind_reference(self, target_dir):
        """Select appropriate wind reference seastate based on direction
        
        Wind reference seastates (WD01-WD16) cover various directions at 10 m/s
        """
        # Map target direction to closest available wind reference
        # This is a simplified mapping - actual implementation would use metadata
        direction_map = {
            0: 'WD01', 45: 'WD02', 90: 'WD03', 135: 'WD04',
            180: 'WD05', 225: 'WD06', 270: 'WD07', 315: 'WD08',
            # Additional references for specific directions
            70: 'WD09', 110: 'WD10', 125: 'WD11', 150: 'WD12',
            200: 'WD13', 290: 'WD14', 335: 'WD15', 160: 'WD16'
        }
        
        # Find closest direction
        closest_dir = min(direction_map.keys(), key=lambda x: abs(x - target_dir))
        return direction_map[closest_dir]
    
    def select_wave_reference(self, target_dir):
        """Select appropriate wave reference seastate based on direction
        
        Wave reference seastates (W01-W18) cover various directions at Hs = 0.5m
        """
        # Map target direction to closest available wave reference
        direction_map = {
            0: 'W01', 45: 'W02', 70: 'W03', 90: 'W04',
            110: 'W05', 125: 'W06', 135: 'W07', 150: 'W08',
            160: 'W09', 180: 'W10', 200: 'W11', 225: 'W12',
            270: 'W13', 290: 'W14', 310: 'W15', 315: 'W16',
            335: 'W17', 350: 'W18'
        }
        
        # Find closest direction
        closest_dir = min(direction_map.keys(), key=lambda x: abs(x - target_dir))
        return direction_map[closest_dir]
    
    def determine_reference_seastate(self, fatigue_row):
        """Determine which reference seastate to use based on fatigue condition"""
        row_num = fatigue_row['Row']
        scaling_row = self.scaling_factors_df[self.scaling_factors_df['Row'] == row_num].iloc[0]
        
        # Compare relative impact of wind vs wave
        wind_impact = scaling_row['Wind Scale Factor']
        wave_impact = scaling_row['Wave Scale Factor']
        
        # Select reference based on dominant scaling factor
        # Also consider direction matching for better representation
        if wind_impact > wave_impact * 1.5:  # Wind dominant (with 1.5x threshold)
            return self.select_wind_reference(scaling_row['Wind Dir (°)'])
        else:
            return self.select_wave_reference(scaling_row['Wave Dir (°)'])
    
    def process_fatigue_condition(self, fatigue_row, strut_id):
        """Process a single fatigue condition for a specific strut by combining wind and wave effects"""
        row_num = fatigue_row['Row']
        scaling_row = self.scaling_factors_df[self.scaling_factors_df['Row'] == row_num].iloc[0]
        
        # Get both wind and wave reference time traces
        wind_ref_id = self.select_wind_reference(scaling_row['Wind Dir (°)'])
        wave_ref_id = self.select_wave_reference(scaling_row['Wave Dir (°)'])
        
        # Load wind reference time trace and scale it
        wind_trace = self.metadata_handler.load_time_trace(wind_ref_id, strut_id)
        wind_scale_factor = scaling_row['Wind Scale Factor']
        scaled_wind_trace = wind_trace * wind_scale_factor
        
        # Load wave reference time trace and scale it
        wave_trace = self.metadata_handler.load_time_trace(wave_ref_id, strut_id)
        wave_scale_factor = scaling_row['Wave Scale Factor']
        scaled_wave_trace = wave_trace * wave_scale_factor
        
        # Combine wind and wave contributions to get effective tension
        effective_tension = scaled_wind_trace + scaled_wave_trace
        
        # Store metadata about scaling for traceability
        metadata = {
            'fatigue_condition': fatigue_row['Row'],
            'wind_reference': wind_ref_id,
            'wave_reference': wave_ref_id,
            'wind_scale_factor': wind_scale_factor,
            'wave_scale_factor': wave_scale_factor,
            'occurrence_pct': fatigue_row['Occurrence (%)'],
            'wind_dir': scaling_row['Wind Dir (°)'],
            'wave_dir': scaling_row['Wave Dir (°)'],
            'wind_speed': scaling_row['Wind Speed (m/s)'],
            'Hs': scaling_row['Hs (m)']
        }
        
        return effective_tension, metadata
    
    def generate_all_scaled_traces(self, output_dir='data/scaled_traces', time_step=0.1):
        """Generate combined effective tension time traces for all 81 fatigue conditions and 8 struts
        
        Args:
            output_dir: Directory to save the combined time traces
            time_step: Time step for the output time traces (seconds)
        
        Returns:
            Dictionary of effective tension time traces and comprehensive metadata
        """
        import os
        import numpy as np
        
        os.makedirs(output_dir, exist_ok=True)
        
        scaled_results = {}
        scaling_log = []
        
        print(f"Generating effective tension time traces for {len(self.fatigue_conditions)} fatigue conditions...")
        
        for idx, row in self.fatigue_conditions.iterrows():
            condition_id = row['Row']
            
            for strut_id in ['S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8']:
                key = f"FC{condition_id:03d}_{strut_id}"
                
                # Process and get combined effective tension with metadata
                effective_tension, metadata = self.process_fatigue_condition(row, strut_id)
                scaled_results[key] = effective_tension
                
                # Prepare comprehensive scaling information for logging
                scaling_log.append({
                    'Fatigue_Condition': f"FC{condition_id:03d}",
                    'Strut_ID': strut_id,
                    'Wind_Speed_mps': metadata['wind_speed'],
                    'Hs_m': metadata['Hs'],
                    'Wind_Dir_deg': metadata['wind_dir'],
                    'Wave_Dir_deg': metadata['wave_dir'],
                    'Wind_Ref_Seastate': metadata['wind_reference'],
                    'Wave_Ref_Seastate': metadata['wave_reference'],
                    'Wind_Scale_Factor': round(metadata['wind_scale_factor'], 4),
                    'Wave_Scale_Factor': round(metadata['wave_scale_factor'], 4),
                    'Occurrence_pct': metadata['occurrence_pct'],
                    'Max_Tension_kN': round(np.max(effective_tension), 2),
                    'Min_Tension_kN': round(np.min(effective_tension), 2),
                    'Mean_Tension_kN': round(np.mean(effective_tension), 2),
                    'Std_Dev_kN': round(np.std(effective_tension), 2)
                })
                
                # Save combined effective tension time trace to CSV file
                output_path = os.path.join(output_dir, f"{key}_effective_tension.csv")
                
                # Create time array based on trace length and time step
                time_array = np.arange(len(effective_tension)) * time_step
                
                # Save with comprehensive headers
                output_df = pd.DataFrame({
                    'time_s': time_array,
                    'effective_tension_kN': effective_tension,
                })
                
                # Add metadata as header comments
                with open(output_path, 'w') as f:
                    f.write(f"# Fatigue Condition: FC{condition_id:03d}\n")
                    f.write(f"# Strut: {strut_id}\n")
                    f.write(f"# Wind Speed: {metadata['wind_speed']} m/s\n")
                    f.write(f"# Significant Wave Height: {metadata['Hs']} m\n")
                    f.write(f"# Wind Scale Factor: {metadata['wind_scale_factor']:.4f}\n")
                    f.write(f"# Wave Scale Factor: {metadata['wave_scale_factor']:.4f}\n")
                    f.write(f"# Annual Occurrence: {metadata['occurrence_pct']}%\n")
                    f.write(f"# Wind Reference: {metadata['wind_reference']}\n")
                    f.write(f"# Wave Reference: {metadata['wave_reference']}\n")
                    output_df.to_csv(f, index=False)
                
                # Progress indicator
                if (idx * 8 + int(strut_id[1]) - 1) % 50 == 0:
                    progress = ((idx * 8 + int(strut_id[1]) - 1) / (len(self.fatigue_conditions) * 8)) * 100
                    print(f"  Progress: {progress:.1f}% - Processing {key}")
        
        # Save comprehensive scaling log for audit trail and verification
        scaling_log_df = pd.DataFrame(scaling_log)
        scaling_log_path = os.path.join(output_dir, 'effective_tension_scaling_log.csv')
        scaling_log_df.to_csv(scaling_log_path, index=False)
        
        # Generate summary statistics
        summary_path = os.path.join(output_dir, 'effective_tension_summary.txt')
        with open(summary_path, 'w') as f:
            f.write("=" * 80 + "\n")
            f.write("EFFECTIVE TENSION GENERATION SUMMARY\n")
            f.write("=" * 80 + "\n\n")
            f.write(f"Total Fatigue Conditions: {len(self.fatigue_conditions)}\n")
            f.write(f"Total Struts: 8\n")
            f.write(f"Total Time Traces Generated: {len(scaled_results)}\n")
            f.write(f"Output Directory: {output_dir}\n\n")
            
            f.write("SCALING FACTOR RANGES:\n")
            f.write(f"  Wind Scale Factors: {scaling_log_df['Wind_Scale_Factor'].min():.4f} to "
                   f"{scaling_log_df['Wind_Scale_Factor'].max():.4f}\n")
            f.write(f"  Wave Scale Factors: {scaling_log_df['Wave_Scale_Factor'].min():.4f} to "
                   f"{scaling_log_df['Wave_Scale_Factor'].max():.4f}\n\n")
            
            f.write("EFFECTIVE TENSION STATISTICS:\n")
            f.write(f"  Maximum Tension: {scaling_log_df['Max_Tension_kN'].max():.2f} kN\n")
            f.write(f"  Minimum Tension: {scaling_log_df['Min_Tension_kN'].min():.2f} kN\n")
            f.write(f"  Average Mean Tension: {scaling_log_df['Mean_Tension_kN'].mean():.2f} kN\n")
            f.write(f"  Average Std Dev: {scaling_log_df['Std_Dev_kN'].mean():.2f} kN\n\n")
            
            f.write(f"Scaling Log: {scaling_log_path}\n")
            f.write(f"Summary generated at: {pd.Timestamp.now()}\n")
        
        print(f"\n✓ Generated {len(scaled_results)} effective tension time traces")
        print(f"✓ Output directory: {output_dir}")
        print(f"✓ Scaling log: {scaling_log_path}")
        print(f"✓ Summary: {summary_path}")
        
        return scaled_results
    
    def update_scaling_factors(self, new_fatigue_conditions_path=None):
        """Dynamically update scaling factors if fatigue conditions change"""
        if new_fatigue_conditions_path:
            self.fatigue_conditions_path = new_fatigue_conditions_path
        
        # Reload fatigue conditions
        self.fatigue_conditions = pd.read_csv(self.fatigue_conditions_path)
        
        # Recalculate and save scaling factors
        self.calculate_and_save_scaling_factors()
        
        print("Scaling factors updated successfully")
```

### 5. Stress Mapping Integration
```python
class StressMapper:
    def __init__(self, unit_load=4000):  # kN
        self.unit_load = unit_load
    
    def calculate_unit_stress(self, fea_results):
        """Calculate stress per unit load from FEA"""
        return fea_results / self.unit_load
    
    def map_loads_to_stress(self, load_ranges, unit_stress, scf=1.0):
        """Convert load ranges to stress ranges"""
        return scf * load_ranges * unit_stress
```

### 6. Fatigue Damage Calculator
```python
class FatigueCalculator:
    def __init__(self):
        # ABS "E" in Air parameters
        self.A = 1.04e12
        self.m = 3
        self.C = 1.48e11
        self.r = 5
        self.threshold = 1e6
    
    def cycles_to_failure(self, stress_range):
        """Calculate cycles to failure using S-N curve"""
        if stress_range <= self.threshold:
            return self.A * (stress_range ** -self.m)
        else:
            return self.C * (stress_range ** -self.r)
    
    def calculate_damage(self, cycle_counts, stress_ranges):
        """Apply Miner's rule for damage accumulation"""
        total_damage = 0
        for n, stress in zip(cycle_counts, stress_ranges):
            N = self.cycles_to_failure(stress)
            total_damage += n / N
        return total_damage
    
    def annual_damage(self, total_damage, analysis_duration=200):
        """Scale damage to annual basis"""
        seconds_per_year = 31_536_000
        return total_damage * (seconds_per_year / analysis_duration)
```

## Configuration Files

### Environmental Conditions (environmental_conditions.yml)
```yaml
wave_conditions:
  - case_id: "W01"
    Hs: 0.5  # m
    Tp: 8.0  # s
    direction: 0  # degrees
  # ... 17 more wave cases

wind_conditions:
  - case_id: "WD01"
    speed: 10.0  # m/s
    direction: 0  # degrees
  # ... 15 more wind cases
```

### Fatigue Conditions (fatigue_seastates.csv)
The 81 fatigue conditions are defined in `fatigue_seastates.csv` with columns:
- `Row`: Condition index (1-81)
- `Wind Speed (m/s)`: Target wind speed for scaling
- `Wind Dir (°)`: Wind direction
- `Hs (m)`: Significant wave height for scaling
- `Tp (s)`: Peak period
- `Wave Dir (°)`: Wave direction
- `Occurrence (%)`: Annual occurrence percentage for weighting
- `Occurrence (hrs)`: Annual hours

Example entries:
```csv
Row,Wind Speed (m/s),Wind Dir (°),Hs (m),Tp (s),Wave Dir (°),Occurrence (%),Occurrence (hrs)
1,5,180,0.15,1.7,150,7.76,680.2
2,3,180,0.09,1,150,6.977,611.6
...
```

### S-N Curve Parameters (sn_curve_parameters.yml)
```yaml
sn_curves:
  abs_e_in_air:
    A: 1.04e12
    m: 3
    C: 1.48e11
    r: 5
    threshold_cycles: 1e6
    
material_properties:
  steel:
    fatigue_design_factor: 2.0
    design_life_years: 25
```

## Command Line Interface

### Primary Commands
```bash
# Complete fatigue analysis workflow with metadata and time traces
uv run python -m digitalmodel.modules.fatigue_analysis \
    --metadata reference_seastate_timetrace_metadata.csv \
    --timetraces-dir data/timetraces \
    --config config.yml

# Individual processing steps
# Step 1: Validate metadata and time trace files
uv run python -m digitalmodel.modules.fatigue_analysis.validate \
    --metadata reference_seastate_timetrace_metadata.csv \
    --timetraces-dir data/timetraces

# Step 2: Scale time traces for fatigue conditions
uv run python -m digitalmodel.modules.fatigue_analysis.scaling \
    --metadata reference_seastate_timetrace_metadata.csv \
    --timetraces-dir data/timetraces \
    --fatigue-conditions fatigue_seastates.csv \
    --output-dir data/scaled_traces

# Step 3: Apply rainflow counting to scaled traces
uv run python -m digitalmodel.modules.fatigue_analysis.rainflow \
    --scaled-dir data/scaled_traces \
    --output-dir data/rainflow_results

# Batch processing
uv run python -m digitalmodel.modules.fatigue_analysis --batch --parallel 4
```

### CLI Parameters
- `--config`: Main configuration file path
- `--metadata`: Metadata CSV file path (`reference_seastate_timetrace_metadata.csv`)
- `--timetraces-dir`: Directory containing individual time trace CSV files
- `--output-directory`: Results output directory
- `--parallel`: Number of parallel workers
- `--struts`: Specific struts to analyze (default: all 8)
- `--conditions`: Specific fatigue conditions to process
- `--verbose`: Detailed progress output
- `--validate`: Run validation checks only
- `--scaled-dir`: Directory containing scaled time traces (for intermediate steps)
- `--output-dir`: Output directory for specific processing steps

## Input/Output Specifications

### Input Requirements
1. **Metadata File**: `reference_seastate_timetrace_metadata.csv`
   - Format: Each row contains metadata for one time trace file
   - Columns: `[seastate_id, seastate_type, strut_id, time_trace_file, duration, sample_rate, units]`
   - Total rows: 272 (34 reference seastates × 8 struts)
   
2. **Time Trace Files**: Individual CSV files in `data/timetraces/` directory
   - Format: Two columns `[time, load_value]`
   - One file per seastate/strut combination
   - File naming convention: `{seastate_id}_{strut_id}.csv`
   - Example: `W01_S1.csv`, `WD16_S8.csv`
   
3. **FEA Results**: Unit stress values per strut location
   - Stress response per unit load (4000 kN)
   - Multiple critical locations per strut
   
4. **Fatigue Conditions File**: `fatigue_seastates.csv`
   - 81 fatigue conditions with wind/wave parameters
   - Annual occurrence percentages for weighting
   - Target values for load scaling
   
5. **Configuration Files**: 
   - S-N curve parameters (ABS "E" in Air)
   - FEA stress concentration factors

### Output Products
1. **Effective Tension Time Traces**: Combined wind+wave scaled loads
   - Format: CSV files with time and effective_tension_kN columns
   - Naming: `FC{condition}_{strut}_effective_tension.csv`
   - Headers: Comprehensive metadata including scaling factors
   - Total files: 648 (81 conditions × 8 struts)
   
2. **Scaling Log**: Complete audit trail for all processing
   - File: `effective_tension_scaling_log.csv`
   - Contains: Scaling factors, reference seastates, statistics
   - Purpose: Verification and quality assurance
   
3. **Rainflow Results**: Load ranges and cycle counts per strut per condition
   - Processed from effective tension traces
   - Format: Range bins with cycle counts
   
4. **Stress Range Data**: Mapped stress ranges with cycle counts
   - Converted from load ranges using FEA unit stresses
   - Includes SCF application
   
5. **Damage Assessment**: Per-strut damage calculations
   - Miner's rule accumulation
   - Weighted by occurrence percentages
   
6. **Fatigue Life Estimates**: Final life estimates per configuration
   - Separate results for each of 4 vessel configurations:
     - FSTs Light (L015) fatigue life
     - FSTs Full (L095) fatigue life
     - FSTs Light + LNGC Full (offloading) fatigue life
     - FSTs Full + LNGC Light (loading) fatigue life
   - Annual damage rates per configuration
   - Life predictions in years
   - Design compliance check for all configurations

## Validation Requirements

### Unit Testing
- Rainflow counting algorithm verification against known results
- Load scaling factor calculations
- S-N curve damage calculations
- Miner's rule implementation

### Integration Testing
- End-to-end workflow with sample data
- Multi-strut processing verification
- Configuration file parsing and validation

### Performance Requirements
- Process 272 rainflow datasets within 5 minutes
- Support parallel processing for multiple struts
- Memory efficient handling of large time series data

## Quality Assurance

### Code Standards
- Follow repository Python coding standards
- Implement comprehensive logging
- Include docstrings for all public methods
- Type hints for function signatures

### Documentation
- User guide with worked examples
- API documentation for all modules
- Configuration file schema documentation
- Troubleshooting guide

## Dependencies

### Core Dependencies
- **NumPy**: Numerical computations
- **Pandas**: Data manipulation and analysis
- **SciPy**: Advanced mathematical functions
- **PyYAML**: Configuration file parsing
- **Click**: Command-line interface framework

### Optional Dependencies
- **Matplotlib**: Results visualization
- **Plotly**: Interactive plotting
- **Jupyter**: Analysis notebooks

## Agent Delegation

This specification requires coordination with multiple specialized agents:

- **Signal Analysis Agent**: Rainflow counting algorithm implementation
- **OrcaFlex Agent**: Time domain simulation data extraction
- **Testing Agent**: Comprehensive test suite development (parallel execution)
- **Documentation Agent**: User guides and API documentation
- **FEA Agent**: Finite element analysis integration and stress mapping

## Dynamic Scaling Factor Management

### Automatic Calculation and Updates
The `DirectLoadScaler` class automatically:
1. **Calculates scaling factors** on initialization for all 81 fatigue conditions
2. **Saves to CSV file** (`fatigue_scaling_factors.csv`) for verification and audit
3. **Logs statistics** including min/max values and total occurrence check
4. **Updates dynamically** when fatigue conditions change via `update_scaling_factors()`

### Scaling Factor Verification Features
- **Pre-calculated values**: All scaling factors computed and stored for transparency
- **Formula documentation**: Each factor includes its calculation formula
- **Direction tracking**: Wind and wave directions preserved for reference selection
- **Audit trail**: Scaling log generated for each processed condition

### Usage Example
```python
# Initialize scaler - automatically calculates and saves scaling factors
scaler = DirectLoadScaler(metadata_handler)

# Scaling factors are now available in:
# - scaler.scaling_factors_df (DataFrame)
# - fatigue_scaling_factors.csv (file)

# If fatigue conditions change, update dynamically
scaler.update_scaling_factors('new_fatigue_conditions.csv')
```

## Execution Example

### Complete Workflow
```python
# Initialize the fatigue analysis system
metadata = MetadataHandler('reference_seastate_timetrace_metadata.csv', 'sample_timetraces/')
scaler = DirectLoadScaler(metadata, 'fatigue_seastates.csv')

# Step 1: Generate all effective tension time traces (wind + wave combined)
print("Generating effective tension time traces...")
effective_traces = scaler.generate_all_scaled_traces(
    output_dir='output/effective_tension_traces',
    time_step=0.1
)

# Step 2: Apply rainflow counting to each effective tension trace
print("Performing rainflow counting...")
rainflow = RainflowCounter()
for trace_id, effective_tension in effective_traces.items():
    ranges, counts = rainflow.count_cycles(effective_tension)
    rainflow.save_results(trace_id, ranges, counts)

# Step 3: Map to stresses and calculate damage
print("Calculating fatigue damage...")
stress_mapper = StressMapper(unit_load=4000)
fatigue_calc = FatigueCalculator()

# Process each vessel configuration separately
configurations = {
    'FSTs_L015': 'fsts_l015',                    # FSTs Light only
    'FSTs_L095': 'fsts_l095',                    # FSTs Full only
    'FSTs_L015_LNGC_Full': 'fsts_l015_lngc_125_full',   # Light FSTs + Full LNGC
    'FSTs_L095_LNGC_Light': 'fsts_l095_lngc_125_light'  # Full FSTs + Light LNGC
}

# Note: Sample data may not contain all configuration files
# Process available configurations only

fatigue_results = {}

for config_name, config_id in configurations.items():
    print(f"\nProcessing {config_name} configuration...")
    config_results = {}
    
    for strut_id in ['S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'S8']:
        total_damage = 0
        
        for condition in range(1, 82):
            # Get rainflow results for this configuration/condition/strut
            trace_id = f"{config_id}_FC{condition:03d}_{strut_id}"
            ranges, counts = rainflow.load_results(trace_id)
            
            # Convert to stresses
            unit_stress = stress_mapper.get_unit_stress(strut_id, config_id)
            stress_ranges = stress_mapper.map_loads_to_stress(ranges, unit_stress, scf=1.2)
            
            # Calculate damage for this condition
            damage = fatigue_calc.calculate_damage(counts, stress_ranges)
            
            # Weight by occurrence percentage
            occurrence_pct = scaler.scaling_factors_df.loc[condition-1, 'Occurrence (%)']
            weighted_damage = damage * (occurrence_pct / 100)
            
            total_damage += weighted_damage
        
        # Calculate fatigue life for this strut/configuration
        annual_damage = fatigue_calc.annual_damage(total_damage, analysis_duration=200)
        fatigue_life = 1 / annual_damage
        config_results[strut_id] = fatigue_life
        print(f"  {config_name} - Strut {strut_id}: Fatigue Life = {fatigue_life:.1f} years")
    
    fatigue_results[config_name] = config_results

# Summary of all configurations
print("\n" + "="*60)
print("FATIGUE LIFE SUMMARY (ALL CONFIGURATIONS)")
print("="*60)
for config_name, results in fatigue_results.items():
    min_life = min(results.values())
    critical_strut = min(results, key=results.get)
    print(f"{config_name:15} - Min Life: {min_life:.1f} years (Strut {critical_strut})")
```

### Output Directory Structure
```
output/
├── fsts_l015/                    # FSTs Light configuration
│   ├── effective_tension/
│   │   ├── FC001_S1_effective_tension.csv
│   │   ├── ...
│   │   └── FC081_S8_effective_tension.csv
│   ├── rainflow_results/
│   │   └── [rainflow counting results]
│   ├── damage_results/
│   │   └── fatigue_life_summary.csv
│   └── config_summary.txt
├── fsts_l095/                    # FSTs Full configuration  
│   ├── effective_tension/
│   ├── rainflow_results/
│   ├── damage_results/
│   └── config_summary.txt
├── fsts_l015_lngc_125_full/     # FSTs Light + LNGC Full
│   ├── effective_tension/
│   ├── rainflow_results/
│   ├── damage_results/
│   └── config_summary.txt
├── fsts_l095_lngc_125_light/    # FSTs Full + LNGC Light
│   ├── effective_tension/
│   ├── rainflow_results/
│   ├── damage_results/
│   └── config_summary.txt
└── summary/
    ├── all_configurations_comparison.csv
    ├── critical_struts_summary.csv
    └── fatigue_analysis_report.pdf
```

## Success Criteria

1. **Functional**: Complete automated fatigue analysis workflow for all 4 configurations
2. **Performance**: Process full dataset (4 configs × 81 conditions × 8 struts) within time limits
3. **Accuracy**: Validation against manual calculations within 1% tolerance
4. **Configuration Coverage**: Separate fatigue life estimates for:
   - FSTs Light (L015) - standalone
   - FSTs Full (L095) - standalone
   - FSTs Light + LNGC Full (offloading)
   - FSTs Full + LNGC Light (loading)
5. **Usability**: Intuitive CLI with clear error messages and progress indicators
6. **Maintainability**: Well-documented, modular code structure
7. **Integration**: Seamless integration with existing OrcaFlex workflows
8. **Traceability**: Complete audit trail of scaling factors and processing decisions
9. **Critical Component**: Identify critical strut for each configuration

## Risk Mitigation

- **Algorithm Accuracy**: Validate rainflow implementation against established standards
- **Performance**: Implement parallel processing and memory optimization
- **Data Quality**: Comprehensive input validation and error handling
- **Scalability**: Modular design to support additional analysis types