# OrcaFlex Strut Force Identification Specification

## Key Finding: Maximum Strut Force Location

### Critical Discovery
The maximum strut force values are **always** found in `*strut_dyn.csv` files, specifically the summary files with pattern `dm*strut_dyn.csv`.

### File Structure

#### Example File: `dm_fsts_03c_0100yr_l015_hwl_strut_dyn.csv`

This file contains:
- Multiple columns with max/min values for different strut forces
- A critical column: **`fe_filename`** 
- Each row represents a different simulation run

### The `fe_filename` Column

The `fe_filename` column contains the `.sim` file basename that corresponds to that row's data. This is the key to identifying which time series files should be displayed.

#### Example:
```
Row | fe_filename                          | Strut7_Body_eff_tension_max | ...
----|--------------------------------------|----------------------------|-----
15  | fsts_l015_hwl_ncl_000deg_Jacket1.sim| 8265.55                   | ...
16  | fsts_l015_hwl_ncl_045deg_Jacket1.sim| 7892.33                   | ...
```

When row 15 has the maximum strut force (8265.55 N), the system should:
1. Extract the basename: `fsts_l015_hwl_ncl_000deg_Jacket1`
2. Use this basename to find all related CSV files:
   - `fsts_l015_hwl_ncl_000deg_Jacket1.csv`
   - `fsts_l015_hwl_ncl_000deg_Mooring1.csv`
   - `fsts_l015_hwl_ncl_000deg_Strut1.csv`
   - etc.

## Implementation Strategy

### Default Behavior (Folder Selection)
When a user selects a folder:
1. System scans for `dm*strut_dyn.csv` files
2. Finds the row with maximum absolute strut force
3. Reads the `fe_filename` from that row
4. Extracts the basename (remove .sim extension)
5. Loads all CSV files starting with that basename
6. Displays these as the default view

### Manual Override (Parameter Change)
When user changes parameters (angle, loading, tide, etc.):
1. Keep the same `fe_filename` basename pattern
2. Modify the pattern based on user selection:
   - Change `000deg` to `045deg` for heading change
   - Change `hwl` to `mwl` for tide change
   - Change `l015` to `l095` for loading change
3. Find and load files with the modified basename

### File Naming Pattern
```
<vessel>_<loading>_<tide>_<env>_<heading>_<component>
```

Example transformations:
- Base: `fsts_l015_hwl_ncl_000deg_Jacket1`
- 45° heading: `fsts_l015_hwl_ncl_045deg_Jacket1`
- 95% loading: `fsts_l095_hwl_ncl_000deg_Jacket1`
- MWL tide: `fsts_l015_mwl_ncl_000deg_Jacket1`

## Technical Implementation

### Server-side Processing
```python
# In process_single_strut_file():
if is_strut_dyn:
    # Find row with max force
    max_row_idx = df[force_columns].abs().idxmax()
    
    # Get fe_filename from that row
    fe_filename = df.loc[max_row_idx, 'fe_filename']
    
    # Extract basename
    sim_basename = os.path.splitext(fe_filename)[0]
    
    # Return this in configuration
    config['fe_filename'] = fe_filename
    config['sim_basename'] = sim_basename
```

### File Matching
```python
# Use sim_basename to find exact files
for csv_file in all_csv_files:
    if csv_file.startswith(sim_basename):
        # This file belongs to the max force configuration
        related_files.append(csv_file)
```

## Benefits

1. **Accuracy**: Directly uses the simulation filename that produced the max force
2. **Speed**: No need to scan time series files - summary has pre-calculated maxima
3. **Flexibility**: Can easily switch between configurations using basename pattern
4. **Reliability**: The `fe_filename` is the authoritative source for file association

## Notes

- The `*strut_dyn.csv` files are summary files containing max/min values
- Each row represents a complete simulation run
- The `fe_filename` column is the key to linking summary data to time series data
- This approach ensures the correct files are always displayed
- Manual parameter changes can modify the basename pattern to load different configurations

---

*This specification documents the critical discovery that maximum strut forces are found in `*strut_dyn.csv` files and the `fe_filename` column provides the exact simulation basename for loading the correct time series data.*