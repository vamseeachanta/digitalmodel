# Sample Time Trace Data Documentation

## Overview
This directory contains sample time trace data for the Strut Foundation Fatigue Analysis system. The data represents reference seastate time traces that are used as input for fatigue analysis calculations.

## Data Source
- **Original Location**: `D:\1522\ctr9\fatigue_wsp_method\07c_fatigue\`
- **Total Available Files**: 
  - 36 wave cases (18 for L015, 18 for L095)
  - 32 wind cases (16 for L015, 16 for L095)
  - Each case has 8 strut files and 4 jacket files

## Sample Data Structure

### Reference Seastate Configuration: `reference_seastate_timetraces.csv`
Contains the original fatigue load case definitions with OrcaFlex simulation parameters:
- Complete list of 68 fatigue cases (36 wave + 32 wind)
- Simulation file paths and parameters
- Environmental conditions (wind speed, wave height, directions)
- FST vessel headings and load conditions

### Sample Metadata File: `sample_timetraces_metadata.csv`
Contains metadata for the sample time trace files with the following columns:
- `seastate_id`: Unique identifier for the seastate (e.g., W01, WD01)
- `seastate_type`: Type of seastate (wave or wind)
- `strut_id`: Strut identifier (S1-S8)
- `time_trace_file`: Path to the actual time trace CSV file
- `duration`: Duration of time trace in seconds (10800s = 3 hours)
- `sample_rate`: Sampling interval in seconds (0.1s)
- `units`: Load units (kN)
- `description`: Human-readable description

### Time Trace Files: `sample_timetraces/`
Each CSV file contains time series data with the following columns:
- `time`: Time in seconds
- `Tension (Jacket End)`: Tension force at jacket end (kN)
- `Shear (Jacket End)`: Shear force at jacket end  
- `Fx (Jacket End)`: X-direction force at jacket end
- `Fy (Jacket End)`: Y-direction force at jacket end
- `Fz (Jacket End)`: Z-direction force at jacket end
- `X (Jacket End)`: X position at jacket end
- `Y (Jacket End)`: Y position at jacket end
- `Z (Jacket End)`: Z position at jacket end
- `Vx (Jacket End)`: X velocity at jacket end
- `Vy (Jacket End)`: Y velocity at jacket end
- `Vz (Jacket End)`: Z velocity at jacket end
- `Angle (Jacket End)`: Angle at jacket end
- Similar columns for `(Vessel End)`

## File Naming Convention
- **Wave Cases**: `W{case_number}_{strut_id}.csv`
  - Example: `W01_S1.csv` = Wave case 01, Strut 1
- **Wind Cases**: `WD{case_number}_{strut_id}.csv`
  - Example: `WD01_S1.csv` = Wind case 01, Strut 1

## Reference Conditions
- **Wave Reference**: Hs = 0.5m (significant wave height)
- **Wind Reference**: Wind speed = 10 m/s

## Sample Files Included
This directory contains a subset of 6 sample files for demonstration:
1. `W01_S1.csv` - Wave case 01, Strut 1
2. `W01_S2.csv` - Wave case 01, Strut 2
3. `W02_S1.csv` - Wave case 02, Strut 1
4. `W02_S2.csv` - Wave case 02, Strut 2
5. `WD01_S1.csv` - Wind case 01, Strut 1
6. `WD01_S2.csv` - Wind case 01, Strut 2

## Usage in Fatigue Analysis

### Processing Pipeline:
1. **Load Metadata**: Read `reference_seastate_timetraces.csv` to identify available time traces
2. **Load Time Traces**: Read individual CSV files from `sample_timetraces/` directory
3. **Apply Scaling**: Scale time traces based on target fatigue conditions
   - Wind scaling: (target_speed/10)²
   - Wave scaling: target_Hs/0.5
4. **Rainflow Counting**: Apply rainflow algorithm to scaled time traces
5. **Fatigue Calculation**: Calculate damage using S-N curves

### Key Parameters:
- **Time Duration**: 10800 seconds (3 hours) per simulation
- **Sample Rate**: 0.1 seconds (10 Hz)
- **Data Points**: 108,000 per time trace
- **Primary Load Component**: Tension (used for fatigue analysis)

## Full Dataset Structure
For complete analysis, the full dataset includes:
- **34 Reference Seastates**: 18 wave + 16 wind
- **8 Struts per Seastate**: Total 272 time trace files
- **81 Fatigue Conditions**: Combinations requiring scaling

## Notes
- Time traces represent pre-computed OrcaFlex simulation results
- Data is in engineering units (kN for forces)
- Sample files are representative of the full dataset structure
- Full dataset required for production fatigue analysis