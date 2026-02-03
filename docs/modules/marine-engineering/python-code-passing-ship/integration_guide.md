# Integration Guide

This guide provides detailed instructions for integrating the Passing Ship Forces module with OrcaFlex, AQWA, and other marine analysis software.

## Table of Contents

1. [OrcaFlex Integration](#orcaflex-integration)
2. [AQWA Integration](#aqwa-integration)
3. [ANSYS Integration](#ansys-integration)
4. [Custom Integration](#custom-integration)
5. [Data Exchange Formats](#data-exchange-formats)
6. [Workflow Automation](#workflow-automation)

## OrcaFlex Integration

### Overview

OrcaFlex integration enables the application of passing ship forces as external loads on moored vessels within dynamic simulations.

### Method 1: Time History Import

Generate time-varying forces for direct import into OrcaFlex.

#### Step 1: Calculate Forces

```python
from digitalmodel.marine_ops.marine_analysis.python_code_passing_ship import PassingShipCalculator
import numpy as np

# Configure calculation
calculator = PassingShipCalculator('config.yaml')
results = calculator.calculate()

# Define passing event timeline
vessel_speed = 5.0  # m/s
total_distance = 800.0  # m
duration = total_distance / vessel_speed  # 160 seconds
time_step = 0.1  # seconds
```

#### Step 2: Generate Time Series

```python
def generate_orcaflex_time_series(results, vessel_speed, time_step=0.1):
    """Convert spatial forces to time series."""
    stagger = results['stagger_distances']
    surge = results['surge_force']
    sway = results['sway_force']
    yaw = results['yaw_moment']
    
    # Convert stagger distance to time
    time = (stagger + max(abs(stagger))) / vessel_speed
    
    # Interpolate to regular time steps
    time_regular = np.arange(0, max(time), time_step)
    surge_time = np.interp(time_regular, time, surge)
    sway_time = np.interp(time_regular, time, sway)
    yaw_time = np.interp(time_regular, time, yaw)
    
    return time_regular, surge_time, sway_time, yaw_time
```

#### Step 3: Export to OrcaFlex Format

```python
def export_to_orcaflex(time, fx, fy, mz, filename='passing_forces.txt'):
    """Export forces in OrcaFlex time history format."""
    with open(filename, 'w') as f:
        # OrcaFlex header
        f.write("OrcaFlex Time History File\n")
        f.write("Time[s]\tFx[kN]\tFy[kN]\tMz[kN.m]\n")
        
        # Write data
        for t, fx_val, fy_val, mz_val in zip(time, fx, fy, mz):
            f.write(f"{t:.2f}\t{fx_val/1000:.3f}\t{fy_val/1000:.3f}\t{mz_val/1000:.3f}\n")
    
    print(f"Exported to {filename}")

# Generate and export
time, fx, fy, mz = generate_orcaflex_time_series(results, vessel_speed)
export_to_orcaflex(time, fx, fy, mz)
```

#### Step 4: Import in OrcaFlex

1. Open your OrcaFlex model
2. Select the vessel object
3. Go to Data → Applied Loads
4. Add new load type: "Time History"
5. Import the generated file
6. Map columns to Fx, Fy, Mz
7. Set application point (typically COG)

### Method 2: Python API Integration

Direct integration using OrcFxAPI for automated workflows.

```python
import OrcFxAPI
from digitalmodel.marine_ops.marine_analysis.python_code_passing_ship import PassingShipCalculator

class OrcaFlexPassingShipIntegration:
    def __init__(self, orcaflex_model_path):
        self.model = OrcFxAPI.Model(orcaflex_model_path)
        self.vessel = self.model['Vessel1']
        
    def apply_passing_ship_forces(self, config_path):
        """Calculate and apply passing ship forces."""
        # Calculate forces
        calculator = PassingShipCalculator(config_path)
        results = calculator.calculate()
        
        # Convert to time series
        time, fx, fy, mz = generate_orcaflex_time_series(
            results, 
            vessel_speed=5.0
        )
        
        # Create time history load
        load = self.model.CreateObject(OrcFxAPI.otLoadTimeSeries)
        load.Name = "Passing Ship Forces"
        load.AppliedTo = self.vessel.Name
        
        # Set time series data
        load.TimeSeriesX = fx / 1000  # Convert to kN
        load.TimeSeriesY = fy / 1000
        load.TimeSeriesRz = mz / 1000  # kN.m
        load.Times = time
        
        return load
    
    def run_simulation(self):
        """Run dynamic simulation with passing forces."""
        self.model.RunSimulation()
        return self.model.GetResults()

# Usage
integration = OrcaFlexPassingShipIntegration('moored_vessel.sim')
integration.apply_passing_ship_forces('passing_config.yaml')
results = integration.run_simulation()
```

### Method 3: Constraint Force Generation

Generate passing ship forces as vessel constraints for station-keeping analysis.

```python
def generate_constraint_forces(results, positions):
    """Generate forces for OrcaFlex constraint definition."""
    constraints = []
    
    for pos in positions:
        # Interpolate forces at position
        fx = np.interp(pos, results['stagger_distances'], results['surge_force'])
        fy = np.interp(pos, results['stagger_distances'], results['sway_force'])
        mz = np.interp(pos, results['stagger_distances'], results['yaw_moment'])
        
        constraints.append({
            'position': pos,
            'forces': {'Fx': fx, 'Fy': fy, 'Mz': mz},
            'type': 'external_force'
        })
    
    return constraints
```

## AQWA Integration

### Overview

AQWA integration focuses on frequency-domain analysis and response amplitude operators (RAOs).

### Method 1: Force RAO Generation

Generate force RAOs for AQWA frequency domain analysis.

```python
def generate_aqwa_force_raos(config_path, frequencies):
    """Generate force RAOs for AQWA."""
    calculator = PassingShipCalculator(config_path)
    
    force_raos = {
        'surge': [],
        'sway': [],
        'yaw': []
    }
    
    for freq in frequencies:
        # Modify velocity for frequency
        modified_config = config_path
        modified_config['vessel']['passing']['velocity'] = 2 * np.pi * freq
        
        # Calculate forces
        results = calculator.calculate()
        
        # Store peak values as RAO
        force_raos['surge'].append(max(abs(results['surge_force'])))
        force_raos['sway'].append(max(abs(results['sway_force'])))
        force_raos['yaw'].append(max(abs(results['yaw_moment'])))
    
    return frequencies, force_raos
```

### Method 2: AQWA-LINE Input File

Generate AQWA-LINE compatible input files.

```python
def export_to_aqwa_line(results, filename='passing_forces.aqwa'):
    """Export forces for AQWA-LINE analysis."""
    with open(filename, 'w') as f:
        # AQWA header
        f.write("*PASSING SHIP FORCES\n")
        f.write("*GENERATED BY PYTHON MODULE\n")
        f.write("*\n")
        
        # Structure definition
        f.write("STRU 1\n")
        f.write("*\n")
        
        # Force data
        f.write("*EXTERNAL FORCES\n")
        f.write("FORC\n")
        
        for s, fx, fy, mz in zip(
            results['stagger_distances'],
            results['surge_force'],
            results['sway_force'],
            results['yaw_moment']
        ):
            f.write(f"  {s:8.2f} {fx:12.3e} {fy:12.3e} 0.0 0.0 0.0 {mz:12.3e}\n")
        
        f.write("*END\n")
    
    print(f"AQWA input file created: {filename}")
```

### Method 3: AQWA Workbench Integration

Integration with ANSYS AQWA Workbench using ACT scripting.

```python
# AQWA Workbench ACT Script
def create_aqwa_passing_loads():
    """ACT script for AQWA Workbench."""
    import sys
    sys.path.append(r'path\to\python_module')
    
    from digitalmodel.marine_ops.marine_analysis.python_code_passing_ship import (
        PassingShipCalculator
    )
    
    # Get AQWA model reference
    analysis = ExtAPI.DataModel.Project.Model.Analyses[0]
    
    # Calculate forces
    calc = PassingShipCalculator('config.yaml')
    results = calc.calculate()
    
    # Create external force object
    force = analysis.AddExternalForce()
    force.Name = "Passing Ship Forces"
    
    # Apply forces
    force.DefineBy = ForceDefineBy.Components
    force.XComponent.Inputs[0].DiscreteValues = results['surge_force']
    force.YComponent.Inputs[0].DiscreteValues = results['sway_force']
    force.ZMoment.Inputs[0].DiscreteValues = results['yaw_moment']
    
    return force
```

## ANSYS Integration

### Mechanical APDL Integration

Generate APDL commands for ANSYS Mechanical.

```python
def generate_apdl_commands(results):
    """Generate ANSYS APDL commands for passing forces."""
    commands = []
    
    # Header
    commands.append("! Passing Ship Forces")
    commands.append("! Generated by Python Module")
    commands.append("")
    
    # Define load step
    commands.append("*DIM,PASS_FX,TABLE,{},1,1,TIME".format(len(results['surge_force'])))
    commands.append("*DIM,PASS_FY,TABLE,{},1,1,TIME".format(len(results['sway_force'])))
    commands.append("*DIM,PASS_MZ,TABLE,{},1,1,TIME".format(len(results['yaw_moment'])))
    
    # Populate tables
    for i, (t, fx, fy, mz) in enumerate(zip(
        results['time'],
        results['surge_force'],
        results['sway_force'],
        results['yaw_moment']
    ), 1):
        commands.append(f"PASS_FX({i},0,1) = {t}")
        commands.append(f"PASS_FX({i},1,1) = {fx}")
        commands.append(f"PASS_FY({i},0,1) = {t}")
        commands.append(f"PASS_FY({i},1,1) = {fy}")
        commands.append(f"PASS_MZ({i},0,1) = {t}")
        commands.append(f"PASS_MZ({i},1,1) = {mz}")
    
    # Apply loads
    commands.append("")
    commands.append("! Apply to vessel node")
    commands.append("F,VESSEL_NODE,FX,%PASS_FX%")
    commands.append("F,VESSEL_NODE,FY,%PASS_FY%")
    commands.append("F,VESSEL_NODE,MZ,%PASS_MZ%")
    
    return "\n".join(commands)
```

## Custom Integration

### Generic Export Functions

```python
class ForceExporter:
    """Generic force exporter for various formats."""
    
    def __init__(self, results):
        self.results = results
        
    def to_dict(self):
        """Export as dictionary."""
        return {
            'stagger': self.results['stagger_distances'].tolist(),
            'surge': self.results['surge_force'].tolist(),
            'sway': self.results['sway_force'].tolist(),
            'yaw': self.results['yaw_moment'].tolist(),
            'metadata': self.results.get('metadata', {})
        }
    
    def to_dataframe(self):
        """Export as pandas DataFrame."""
        import pandas as pd
        return pd.DataFrame({
            'stagger_m': self.results['stagger_distances'],
            'surge_N': self.results['surge_force'],
            'sway_N': self.results['sway_force'],
            'yaw_Nm': self.results['yaw_moment']
        })
    
    def to_matlab(self, filename='forces.mat'):
        """Export to MATLAB format."""
        from scipy.io import savemat
        savemat(filename, self.to_dict())
        
    def to_excel(self, filename='forces.xlsx'):
        """Export to Excel."""
        df = self.to_dataframe()
        with pd.ExcelWriter(filename) as writer:
            df.to_excel(writer, sheet_name='Forces', index=False)
            
            # Add metadata sheet
            metadata = pd.DataFrame.from_dict(
                self.results.get('metadata', {}), 
                orient='index', 
                columns=['Value']
            )
            metadata.to_excel(writer, sheet_name='Metadata')
```

### Integration with Mooring Analysis Module

```python
from digitalmodel.subsea.mooring_analysis import MooringAnalyzer
from digitalmodel.marine_ops.marine_analysis.python_code_passing_ship import (
    PassingShipCalculator
)

class IntegratedMooringAnalysis:
    """Integrate passing ship forces with mooring analysis."""
    
    def __init__(self, mooring_config, passing_config):
        self.mooring = MooringAnalyzer(mooring_config)
        self.passing = PassingShipCalculator(passing_config)
        
    def analyze_combined_loads(self):
        """Perform combined analysis."""
        # Calculate passing ship forces
        passing_forces = self.passing.calculate()
        
        # Apply to mooring analysis at critical positions
        critical_positions = [0, 50, 100]  # Stagger distances
        
        results = []
        for pos in critical_positions:
            # Interpolate forces
            fx = np.interp(pos, 
                          passing_forces['stagger_distances'],
                          passing_forces['surge_force'])
            fy = np.interp(pos,
                          passing_forces['stagger_distances'],
                          passing_forces['sway_force'])
            
            # Apply to mooring
            self.mooring.add_external_load(fx=fx, fy=fy)
            mooring_result = self.mooring.solve()
            
            results.append({
                'position': pos,
                'passing_forces': {'fx': fx, 'fy': fy},
                'mooring_tensions': mooring_result['tensions'],
                'vessel_offset': mooring_result['offset']
            })
        
        return results
```

## Data Exchange Formats

### Standard CSV Format

```csv
# Passing Ship Forces Export
# Date: 2025-01-05
# Config: standard_tanker.yaml
#
# Columns: stagger_distance[m], surge_force[N], sway_force[N], yaw_moment[N.m]
-400.0, 1234.56, 2345.67, 3456.78
-350.0, 2345.67, 3456.78, 4567.89
...
```

### JSON with Metadata

```json
{
  "metadata": {
    "version": "1.0.0",
    "timestamp": "2025-01-05T12:00:00",
    "configuration": {
      "vessel": {
        "moored": {"length": 180, "beam": 32, "draft": 12},
        "passing": {"length": 200, "beam": 35, "draft": 14, "velocity": 5}
      },
      "environment": {
        "water_depth": 50,
        "separation_distance": 50
      }
    }
  },
  "results": {
    "stagger_distances": [-400, -350, ...],
    "forces": {
      "surge": [1234.56, 2345.67, ...],
      "sway": [2345.67, 3456.78, ...],
      "yaw": [3456.78, 4567.89, ...]
    },
    "units": {
      "distance": "m",
      "force": "N",
      "moment": "N.m"
    }
  }
}
```

### HDF5 for Large Datasets

```python
import h5py

def export_to_hdf5(results, filename='forces.h5'):
    """Export to HDF5 for large datasets."""
    with h5py.File(filename, 'w') as f:
        # Create groups
        forces = f.create_group('forces')
        metadata = f.create_group('metadata')
        
        # Store arrays
        forces.create_dataset('stagger', data=results['stagger_distances'])
        forces.create_dataset('surge', data=results['surge_force'])
        forces.create_dataset('sway', data=results['sway_force'])
        forces.create_dataset('yaw', data=results['yaw_moment'])
        
        # Store metadata
        for key, value in results.get('metadata', {}).items():
            metadata.attrs[key] = value
```

## Workflow Automation

### Batch Processing Pipeline

```python
class PassingShipPipeline:
    """Automated pipeline for batch processing."""
    
    def __init__(self, base_config):
        self.base_config = base_config
        self.results = []
        
    def process_scenarios(self, scenarios):
        """Process multiple scenarios."""
        for scenario in scenarios:
            # Update configuration
            config = self.base_config.copy()
            config.update(scenario)
            
            # Calculate
            calc = PassingShipCalculator(config)
            result = calc.calculate()
            
            # Store with scenario ID
            result['scenario_id'] = scenario.get('id', 'unknown')
            self.results.append(result)
            
        return self.results
    
    def export_all(self, output_dir='results/'):
        """Export all results."""
        import os
        os.makedirs(output_dir, exist_ok=True)
        
        for result in self.results:
            scenario_id = result['scenario_id']
            
            # Export to multiple formats
            base_path = os.path.join(output_dir, scenario_id)
            
            # JSON
            with open(f'{base_path}.json', 'w') as f:
                json.dump(result, f, indent=2, default=str)
            
            # CSV
            df = pd.DataFrame({
                'stagger': result['stagger_distances'],
                'surge': result['surge_force'],
                'sway': result['sway_force'],
                'yaw': result['yaw_moment']
            })
            df.to_csv(f'{base_path}.csv', index=False)
            
            # Plot
            self.plot_scenario(result, f'{base_path}.png')
    
    def generate_report(self):
        """Generate summary report."""
        report = []
        report.append("# Passing Ship Forces Analysis Report")
        report.append(f"Generated: {datetime.now()}")
        report.append("")
        
        for result in self.results:
            report.append(f"## Scenario: {result['scenario_id']}")
            report.append(f"- Max Surge: {max(abs(result['surge_force'])):.2f} N")
            report.append(f"- Max Sway: {max(abs(result['sway_force'])):.2f} N")
            report.append(f"- Max Yaw: {max(abs(result['yaw_moment'])):.2f} N.m")
            report.append("")
        
        return "\n".join(report)
```

### Automated OrcaFlex Batch Run

```python
def run_orcaflex_batch(model_template, passing_configs, output_dir):
    """Run batch OrcaFlex simulations with different passing scenarios."""
    import OrcFxAPI
    import os
    
    results = []
    
    for config_path in passing_configs:
        # Calculate passing forces
        calc = PassingShipCalculator(config_path)
        forces = calc.calculate()
        
        # Load OrcaFlex model
        model = OrcFxAPI.Model(model_template)
        
        # Apply forces
        apply_passing_forces_to_model(model, forces)
        
        # Run simulation
        model.RunSimulation()
        
        # Extract results
        result = extract_orcaflex_results(model)
        result['config'] = config_path
        results.append(result)
        
        # Save model
        config_name = os.path.basename(config_path).replace('.yaml', '')
        model.SaveAs(os.path.join(output_dir, f'{config_name}.sim'))
    
    return results
```

## Best Practices

### 1. Unit Consistency

Always verify unit consistency between modules:

```python
# Unit conversion utilities
def forces_to_orcaflex(forces_N):
    """Convert forces from N to kN for OrcaFlex."""
    return forces_N / 1000

def forces_to_aqwa(forces_N):
    """Convert forces for AQWA (already in N)."""
    return forces_N

def moments_to_orcaflex(moments_Nm):
    """Convert moments from N.m to kN.m for OrcaFlex."""
    return moments_Nm / 1000
```

### 2. Validation

Always validate integration results:

```python
def validate_integration(original_results, integrated_results):
    """Validate that forces are correctly integrated."""
    # Check magnitude preservation
    original_max = max(abs(original_results['surge_force']))
    integrated_max = max(abs(integrated_results['surge']))
    
    relative_error = abs(original_max - integrated_max) / original_max
    
    if relative_error > 0.01:  # 1% tolerance
        raise ValueError(f"Integration error: {relative_error:.2%}")
    
    return True
```

### 3. Error Handling

Implement robust error handling:

```python
try:
    # Calculate forces
    results = calculator.calculate()
    
    # Export to OrcaFlex
    export_to_orcaflex(results)
    
except CalculationError as e:
    logger.error(f"Calculation failed: {e}")
    # Use fallback or cached results
    
except IOError as e:
    logger.error(f"Export failed: {e}")
    # Retry or use alternative format
```

## Troubleshooting Integration Issues

### Common Issues and Solutions

| Issue | Cause | Solution |
|-------|-------|----------|
| Forces appear scaled incorrectly | Unit mismatch | Check unit conversion functions |
| Time series misaligned | Time step mismatch | Ensure consistent time steps |
| Forces not applied in OrcaFlex | Wrong reference frame | Check coordinate system alignment |
| AQWA import fails | Format error | Validate file format against template |
| Zero forces in simulation | Integration point error | Verify force application point |

### Debug Checklist

- [ ] Units consistent across all systems
- [ ] Coordinate systems aligned
- [ ] Time steps match simulation requirements
- [ ] Force application points correct
- [ ] File formats validated
- [ ] Integration results verified